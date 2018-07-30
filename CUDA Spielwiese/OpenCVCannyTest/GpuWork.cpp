/**
* @file GpuWork.cpp
* @author Chris Rebbelin s0548921
* @date 2018-07-29
* @brief The main program
*/

//TODO: das ganze mal auf linux kompilieren ob es auch geht+
// dazu opencv 3.4 mit cuda support und cuda 9.1
// bei kompilieren dann die includes, die libs und c++17

// TODO: log aufräumen
// mit tabs richtige abstände
// präzisionen für komma werte richtig
// anfang und ende mit zeitstempel, parametern und score
// datei als vorheriger dateiname abspeichern

#include "GpuWork.h"

// constants for contour features
const unsigned int MIN_AREA = 6;
const unsigned int TUMOR_SIZE = 50;

const unsigned int MAX_PE = 300;
const unsigned int MIN_PE = 20;

const double MIN_INERTIA = 0.02;

const double HULL_RATIO = 1.5;

// images on cpu (host)
cv::Mat input_host, gray_host, edges_host;

// constants for canny
const unsigned int lowThreshold = 70;
const unsigned int highThreshold = 150;
const unsigned int sobelKernelSize = 3;
const bool useL2 = false;

// pointer to cuda canny detector
cv::Ptr<cv::cuda::CannyEdgeDetector> cannyPtr = cv::cuda::createCannyEdgeDetector(lowThreshold, highThreshold, sobelKernelSize, useL2);

// images on gpu (device)
cv::cuda::GpuMat input_dev, gray_dev, out_dev;


void initCuda() {

	#ifdef MY_OWN_DEBUG_FLAG
	std::cout << "OpenCV version : " << CV_VERSION << std::endl;
	#endif

	// check for cuda support
	int n = cv::cuda::getCudaEnabledDeviceCount();

	if (n < 1) {
		std::cerr << "No GPU support :(" << std::endl;
		exit(EXIT_FAILURE);
	}
	else {
		std::cout << "Found " << n << " device" << (n > 1 ? "s" : "")  << "!" << std::endl;

		int device = 0;

		// init device
		cv::cuda::setDevice(device);
		#ifdef MY_OWN_DEBUG_FLAG
		cv::cuda::printCudaDeviceInfo(device);
		#endif

		std::cout << "Device set!" << std::endl;
	}
}

int loadImage(cv::Mat* inp, std::string* relativeFilePath) {

	// read the input image with openCV
	(*inp) = cv::imread(*relativeFilePath, CV_LOAD_IMAGE_UNCHANGED);

	// check if image was loaded
	if (!(*inp).data) {
		std::cerr << "Could not open or find the image!" << std::endl;
		exit(EXIT_FAILURE);
	}

	int c = (*inp).channels();

	if (c < 3 || c > 4) {
		std::cerr << "Images with " << c << " channel" << (c > 1 ? "s" : "") << " are not supported!" << std::endl;
		exit(EXIT_FAILURE);
	}

	if ((*inp).depth() != CV_8U) {
		std::cerr << "Only 8bit images are supported!" << std::endl;
		exit(EXIT_FAILURE);
	}

	return c;
}

void doWork(std::string imgPath) {

	#ifdef MY_OWN_DEBUG_FLAG
	std::ofstream log;
	log.open("log.txt", std::ios::out | std::ios::app);
	#endif // MY_OWN_DEBUG_FLAG

	TimingCPU timer;

	int ch = loadImage(&input_host, &imgPath);

	timer.StartCounter();

	// upload input image to device memory
	input_dev.upload(input_host);

	// convert to gray on gpu
	cv::cuda::cvtColor(input_dev, gray_dev, (ch == 4 ? CV_BGRA2GRAY : CV_BGR2GRAY));

	//double timeCudaInit = timer.GetCounter();

	// do canny on gpu
	cannyPtr->detect(gray_dev, out_dev);

	// download edges image back to host memory
	out_dev.download(edges_host);

	//double timeforCudaTotal = timer.GetCounter();

	// find contours on cpu 
	///TODO: other framework for CUDA support? Or other method?
	std::vector<std::vector<cv::Point>> contours;
	std::vector<cv::Vec4i> hierachy;
	cv::findContours(edges_host, contours, hierachy, CV_RETR_TREE, CV_CHAIN_APPROX_NONE);

	#ifdef MY_OWN_DEBUG_FLAG
	double timeAll = timer.GetCounter();
	std::cout << "Found " << contours.size() << "contours after " << std::setprecision(5) << timeAll << " ms." << std::endl;
	#endif // MY_OWN_DEBUG_FLAG

	// create empty output image
	cv::Mat contourImage(input_host.size(), CV_8UC3, cv::Scalar(0));
	cv::Mat mask(input_host.size(), CV_8UC1, cv::Scalar(0));

	cv::Scalar mean_color = cv::Scalar(0, 255, 0);

	int kiPos = 0;
	int tumorCells = 0;
	double kiScore = 0.0;

	// loop over all found contours
	//TODO: use cuda to parallelize this for loop?
	//TODO: immer ein feature nacheinander berechnen und sofort continue wenn nicht erfüllt um zeit zu sparen
	for (int idx = 0; idx < contours.size(); idx++) {

		// only take contours with no parent contours
		if (hierachy[idx][3] != -1) {
			continue;
		}

		// get current contour
		const std::vector<cv::Point> cnt = contours[idx];

		double perim = cv::arcLength(cnt, true);
		double carea = cv::contourArea(cnt);

		std::vector<cv::Point> hull;
		cv::convexHull(cnt, hull);

		double hullarea = cv::contourArea(hull);

		cv::Moments moms = cv::moments(cnt);

		double denominator = std::sqrt(std::pow(2 * moms.mu11, 2) + std::pow(moms.mu20 - moms.mu02, 2));
		const double eps = 1e-2;
		double ratio;
		if (denominator > eps) {
			double cosmin = (moms.mu20 - moms.mu02) / denominator;
			double sinmin = 2 * moms.mu11 / denominator;
			double cosmax = -cosmin;
			double sinmax = -sinmin;

			double imin = 0.5 * (moms.mu20 + moms.mu02) - 0.5 * (moms.mu20 - moms.mu02) * cosmin - moms.mu11 * sinmin;
			double imax = 0.5 * (moms.mu20 + moms.mu02) - 0.5 * (moms.mu20 - moms.mu02) * cosmax - moms.mu11 * sinmax;
			ratio = imin / imax;
		}
		else {
			ratio = 1;
		}
		///TODO: offene konturen ausschließen
		if (carea > MIN_AREA && (hullarea / carea) < HULL_RATIO && perim > MIN_PE && perim < MAX_PE && ratio > MIN_INERTIA) {

			if (carea >= TUMOR_SIZE) {
				tumorCells++;

				#ifdef MEAN_COLOR_PREC

				// TODO: sehr genaue farbbestimmung, aber EXTREM langsam, >5 Sekunden!!!
				mask = 0;
				cv::drawContours(mask, contours, idx, cv::Scalar(255), CV_FILLED);
				mean_color = cv::mean(input_host, mask);

				#else

				// TODO: mit bounding rect alleine ist es ungenauer, aber viel schneller
				// TODO: was wäre wenn man nur die konturpunkte alleine nimmt? ohne die dadrin?
				cv::Rect box = cv::boundingRect(cnt);
				mean_color = cv::mean(input_host(box));

				#endif // MEAN_COLOR_PREC

				// TODO: bessere farb checker? vllt mit HSL? braun und blau haben easy anderen h wert, aber wie ist die zeit?
				if (mean_color[2] > mean_color[1] * 1.1 && mean_color[2] > mean_color[0] * 1.1) {
					kiPos++;

					#ifdef MY_OWN_DEBUG_FLAG
					std::cout << "KI67 POS: ";
					log << "KI67 POS: ";
					#endif // MY_OWN_DEBUG_FLAG
				}
				else {

					#ifdef MY_OWN_DEBUG_FLAG
					std::cout << "KI67 NEG: ";
					log << "KI67 NEG: ";
					#endif // MY_OWN_DEBUG_FLAG
				}
			}
			else {
				#ifdef MY_OWN_DEBUG_FLAG
				std::cout << "NORMAL: ";
				log << "NORMAL: ";
				#endif // MY_OWN_DEBUG_FLAG
			}

			#ifdef MY_OWN_DEBUG_FLAG
			std::cout << "Perimeter: " << std::setprecision(5) << perim << ", Area: " << carea << ", HullArea: " << hullarea << ", Ratio: " << ratio
				<< ", Color: " << mean_color << std::endl;

			log << "Perimeter: " << std::setprecision(5) << perim << ", Area: " << carea << ", HullArea: " << hullarea << ", Ratio: " << ratio
				<< ", Color: " << mean_color << std::endl;
			#endif // MY_OWN_DEBUG_FLAG

			cv::drawContours(contourImage, contours, idx, mean_color);

			// show each drawn contour individually and wait for keypress
			//cv::imshow("Contours213", contourImage);
			//cv::waitKey();
		}
	}

	#ifdef MY_OWN_DEBUG_FLAG
	log.close();
	#endif // MY_OWN_DEBUG_FLAG

	kiScore = tumorCells > 0 ? (double) kiPos / tumorCells : -1;

	// TODO: kiScore einteilung: gut <0.15, mittel 0.15<x<0.35, schlecht >0.35   

	double timeFin = timer.GetCounter();
	std::cout << "Final time: " << std::setprecision(5) << timeFin << " ms. Score: " << std::setprecision(2) << kiScore << std::endl;

	cv::imshow("Contours", contourImage);
	cv::imwrite("AMIN" + std::to_string(MIN_AREA) + "PMAX" + std::to_string(MAX_PE) + "PMIN" + std::to_string(MIN_PE) + "I" + std::to_string(MIN_INERTIA) + ".png", contourImage);
	cv::waitKey();
}

void testStains() {

	cv::cuda::setDevice(0);

	input_host = cv::imread("testImages/testKI67.png", CV_LOAD_IMAGE_UNCHANGED);

	int ros = input_host.rows;
	int cls = input_host.cols;
	int both = ros * cls;

	// normalize matrix to [0,1]
	double scl = 1.f / 255;

	// the deconvolution matrix
	double staining_matrix[3][3] = {
		{ 0.650, 0.704, 0.286 }, // haema
		{ 0.072, 0.990, 0.105 }, // eosin
		{ 0.268, 0.570, 0.776 } // dab
	};
	cv::Mat stain = cv::Mat(3, 3, CV_32FC1, staining_matrix).t();

	cv::Mat input_host_float, inp, dummy, mult, output;

	#ifdef MIT_GPU
	cv::cuda::GpuMat stain_dev, inp_dev, dummy_dev, mult_dev;
	#endif

	// remove alpha channel
	cv::cvtColor(input_host, input_host, CV_BGRA2BGR);

	TimingCPU timer;
	timer.StartCounter();

	// convert matrix to floating point
	input_host.convertTo(input_host_float, CV_32FC3, scl);

	// reshape for matrix multiplication
	inp = input_host_float.reshape(1, both);

	#ifndef MIT_GPU

	// perform convolution on cpu
	cv::gemm(inp, stain, 1.0, dummy, 0.0, mult);

	#else

	// upload data to gpu memory
	inp_dev.upload(inp);
	stain_dev.upload(stain);

	// perform convolution on gpu
	cv::cuda::gemm(inp_dev, stain_dev, 1.0, dummy_dev, 0.0, mult_dev);

	// download result to host memory
	mult_dev.download(mult);

	#endif

	// reshape back to BGR format (3 channels)
	output = mult.reshape(3, ros);

	double all = timer.GetCounter();
	std::cout << "timeAll: " << all << "ms." << std::endl;

	// show result image and separate channels
	cv::Mat bgr[3];
	cv::split(output, bgr);

	cv::imshow("newImage", output);

	///TODO: mit den einzelnen canny machen und dann zusammenfügen die listen
	//ob das schneller geht 

	cv::imshow("1.channel", bgr[0]);
	cv::imshow("2.channel", bgr[1]);
	cv::imshow("3.channel", bgr[2]);

	cv::waitKey();

	cv::cuda::resetDevice();
}

int main(int argc, char** argv) {

	#ifdef STAIN_TEST

	// test color deconvolution with opencv
	testStains();

	#else
	// path to main image directory
	std::string image_dir_path;

	if (argc > 1) {
		std::cout << "Using '" << argv[1] << "' as image directory" << std::endl;
		image_dir_path = argv[1];
	}
	else {
		std::cout << "No image directory specified, using default" << std::endl;
		image_dir_path = "testImages";
	}

	// init cuda device
	initCuda();

	// iterate over all files in directory
	for (auto p : std::filesystem::directory_iterator(image_dir_path)) {

		// get file path
		auto file_path = p.path();

		// check for image extensions
		if (file_path.extension().string() == ".jpg" || file_path.extension().string() == ".png") {
			std::cout << "Processing " << file_path << "..." << std::endl;
			doWork(file_path.string());
		}
		else {
			std::cout << "Skipping " << file_path << "..." << std::endl;
		}

	}

	// reset device
	cv::cuda::resetDevice();
	std::cout << "Finished!" << std::endl;
	#endif // STAIN_TEST

	return EXIT_SUCCESS;
}
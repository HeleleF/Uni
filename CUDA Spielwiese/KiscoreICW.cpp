/**
* @file KiscoreICW.cpp
* @author Chris Rebbelin s0548921
* @date 2018-09-15
* @brief The main program
*/

//TODO: mehrere Bilder durchlaufen lassen, Statistik zu Zeit machen
// also min, max, Durchschnitt und Standardabweichung
// Statistik zu Genauigkeit machen, für jedes Bild Scores vergleichen
// auch hier min, max, Durchschnitt und Abweichung der Differenz jeweils
// vllt. auch hier gesondert betrachten, wann ein Unterschied eine andere Bewertungsklasse gebracht hat (an den Grenzen 0.15 und 0.35)

/**
* - OpenCV Framework, weil viele Algos schon optimiert/getestet sind
* - Routinen für Bildbearbeitung müssen nicht erst selber gemacht werden
*
* - C++, weil damit schon gearbeitet wurde und die Infrastruktur da ist
* - damit läuft es dann auch unter beiden OS (Windows und Linux)
*
* - OpenCV in Version 3.4 mit Cuda Unterstützung
* - Cuda aber nur 9.1 unterstützt, nicht das neueste 9.2
* - wenn kein CUDA support vorhanden ist, Abbruch
*
* - getestet mit Win10 Prof. (64 bit) mit CPU: i5-4570 und GPU: Nvidia GTX 770 (2GB)
* - und Laptop Ubuntu 16.04 (64 bit) mit CPU:  und GPU: Nvidia GTX 950M
*
* - nur auf Bildern mit RGB oder RGBA zulässig (Grauwertbilder machen ja keinen Sinn)
*
* - Eingabeparameter kann Dateipfad oder Ordnerpfad sein
* - dazu c++17 Features verwendet, die explizit eingschaltet werden müssen
* - ginge ohne diese auch, aber viel aufwendiger
* - als Bilddateien nur .jpg und .png zulässig (wird angenommen, kann aber einfach ergänzt werden)
*
* - zunächst Konvertierung in Grau und Canny auf GPU, dazu Parameter durch Ausprobieren gefunden
* - Bild muss dazu auf GPU kopiert werden und danach wieder zurück
* - nimmt quasi kaum Zeit in Anspruch (< 6ms) -> im Vergleich zu CPU Canny starke Zeitvorteil (50ms+)
*
* - keine Implementierung von Konturfindung auf GPU gefunden, deshalb findet das auf CPU statt
* - dauert hier ca. 20 ms (laut openCV gäbe es auf GPU aber auch keinen nennenswerten Zeitvorteil)
* - andere Frameworks (ITK, python scikit) bieten auch nichts dazu an 
*
* - Großteil der Zeit ist dann das Finden der Kontureigenschaften und Bestimmen der positiven Zellen
* - dazu über alle Konturen iterieren und nach Kriterien auschließen
* - durch Ausprobieren gefunden
*
* - Farbe kann auf zwei Arten bestimmt werden:
* - Genau: Dazu muss die Kontur gemalt werden und dann als Maske benutzt werden -> sehr genau, aber dauert lange
* - Box: Bounding Box der Kontur bestimmen und als Maske verwenden -> ungenauer, aber viel schneller im Vergleich
* - Hier muss noch untersucht werden, wie "schlimm" diese Ungenauigkeit wäre
*
* Alternative:
* - zunächst Color Deconvolution mit der Stainmatrix machen und dann jeweils Canny darauf
* - aber: CD dauert auf GPU viel länger als CPU bzw. stürzt häufig ab wegen Memory Error
* - außerdem müssten gefundene Konturen in beiden Bilder zusammengeführt werden o.Ä.
* - damit wäre ein Zeitvorteil wahrscheinlich schon aufgehoben (?)
* - richtiges Testen hier nicht möglich wegen den Fehlern
*
* - Insgesamt kann Echtzeit nicht erreicht werden, dazu dauert gerade der letzte Schritt zu lange
* - Eine Verbesserung der Zeit ist auf jeden Fall zu erreichen.
* - Es bräuchte eine lauffähige Umsetzung von findContours() auf der GPU 
* - Selber aber nicht umsetzbar, weil deutlich zu komplex
*
* - es werden viele doppelte / verkürzte Fragmente gefunden
*
* - Die Konturfeatures müssten auch auf der GPU berechnet werden -> eigener CUDA Kernel?
*
* - Score wird auf -1 gesetzt wenn es irgendwelche Fehler gab
* - Ergebnis mit in den Dateinamen geschrieben
* - Datei log.txt wird angelegt/erweitert mit allen Scores und Zeiten für jede Bilddatei
*/

#include "KiscoreICW.h"

void initCuda() {

	#ifdef SHOW_DEBUG
	std::cout << "OpenCV version : " << CV_VERSION << std::endl;
	#endif

	// check for cuda support
	const int n = cv::cuda::getCudaEnabledDeviceCount();

	if (n < 1) {
		std::cerr << "No GPU support :(" << std::endl;
		exit(EXIT_FAILURE);
	}
	else {

		// init device
		const int device = 0;	
		cv::cuda::setDevice(device);

		#ifdef SHOW_DEBUG
		cv::cuda::printCudaDeviceInfo(device);
		#endif

		std::cout << "Device set!" << std::endl;

		//TODO: Perform a warmup cuda call
		
		cv::Mat1d dummyHost = cv::Mat1d(100, 100);
		cv::cuda::calcSum(dummyHost, dummyHost);
		
		std::cout << "Warmup set!" << std::endl;
	}
}

int loadImage(cv::Mat* inp, const fs::path* relativeFilePath) {

	// read the input image with openCV
	(*inp) = cv::imread((*relativeFilePath).string(), CV_LOAD_IMAGE_UNCHANGED);

	// check if image was loaded
	if (!(*inp).data) {
		std::cerr << "Could not open or find the image!" << std::endl;
		exit(EXIT_FAILURE);
	}

	const int c = (*inp).channels();

	// check for correct image format
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

double calculateKiScore(int numberOfTumorCells, int numberOfKiPosCells) {

	// check for invalid values
	if (numberOfTumorCells < 1 || numberOfKiPosCells > numberOfTumorCells) {
		std::cerr << "Invalid values for score calculation!" << std::endl;
		return -1;
	}

	// cast to double to prevent integer division
	double score = (double)numberOfKiPosCells / numberOfTumorCells;

	#ifdef SHOW_DEBUG
	std::cout << "Score: " << score;

	if (score < KI_67_CUTOFF_1) {
		std::cout << " (LOW)" << std::endl;
	}
	else if (score <= KI_67_CUTOFF_2) {
		std::cout << " (MID)" << std::endl;
	}
	else {
		std::cout << " (HIGH)" << std::endl;
	}
	#endif
	return score;

}

void doWork(const fs::path imgPath) {

	TimingCPU timer;

	// images on cpu (host)
	cv::Mat input_host, gray_host, edges_host;

	// images on gpu (device)
	cv::cuda::GpuMat input_dev, gray_dev, out_dev;

	// pointer to cuda canny detector
	cv::Ptr<cv::cuda::CannyEdgeDetector> cannyPtr = cv::cuda::createCannyEdgeDetector(lowThreshold, highThreshold, sobelKernelSize, useL2);

	const int ch = loadImage(&input_host, &imgPath);

	timer.StartCounter();

	// upload input image to device memory
	input_dev.upload(input_host);

	// convert to gray on gpu
	cv::cuda::cvtColor(input_dev, gray_dev, (ch == 4 ? CV_BGRA2GRAY : CV_BGR2GRAY));

	// do canny on gpu
	cannyPtr->detect(gray_dev, out_dev);

	// download edges image back to host memory
	out_dev.download(edges_host);

	#ifdef SHOW_DEBUG
	std::cout << "After canny: " << timer.GetCounter() << std::endl;
	#endif

	// find contours on cpu
	std::vector<std::vector<cv::Point>> contours;
	std::vector<cv::Vec4i> hierachy;
	cv::findContours(edges_host, contours, hierachy, CV_RETR_TREE, CV_CHAIN_APPROX_NONE);

	#ifdef SHOW_DEBUG
	std::cout << "After findContours: " << timer.GetCounter() << std::endl;
	#endif

	#ifdef WRITE_IMAGE
	// create empty output image
	cv::Mat contourImage(input_host.size(), CV_8UC3, cv::Scalar(0));
	#endif

	// mean color mask
	cv::Mat mask(input_host.size(), CV_8UC1, cv::Scalar(0));
	cv::Scalar mean_color = cv::Scalar(0, 255, 0);

	// counter variables for ki-score calculation
	int kiPos = 0;
	int tumorCells = 0;
	double kiScore = -1;

	const size_t contourLength = contours.size();

	// loop over all found contours
	for (int idx = 0; idx < contourLength; idx++) {

		/*
		* 1. Hierarchy: If contour has parents, its not the outmost one
		* 2. Perimeter Length: If outside of limits, return early
		* 3. Area: If too small, return early
		* 4. Hull ratio: If too big, return early
		* 5. Inertia: If too small, return early
		*
		* Always returns early when a check fails to save time, e.g.
		* don't calculate contour moments when its perimeter is already too small
		*
		* 6. Use area to determine tumor cells
		* 7. For tumor cells, determine its color for ki positive/negative
		* 8. Calculate the score
		*/

		// (1)
		if (hierachy[idx][3] != -1) continue;

		// get current contour
		const std::vector<cv::Point> cnt = contours[idx];

		// (2)
		double perim = cv::arcLength(cnt, true);
		if (perim <= MIN_PE || perim >= MAX_PE) continue;

		// (3)
		double carea = cv::contourArea(cnt);
		if (carea <= MIN_AREA) continue;

		// (4)
		std::vector<cv::Point> hull;
		cv::convexHull(cnt, hull);
		double hullarea = cv::contourArea(hull);
		if ((hullarea / carea) >= HULL_RATIO) continue;

		// (5)
		cv::Moments moms = cv::moments(cnt);
		double denominator = std::sqrt(std::pow(2 * moms.mu11, 2) + std::pow(moms.mu20 - moms.mu02, 2));
		double ratio;
		if (denominator > 0.01) {
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

		if (ratio <= MIN_INERTIA) continue;

		// (6)
		if (carea >= TUMOR_SIZE) {
			tumorCells++;

			#ifdef USE_HIGH_COLOR_PREC

			// use only pixel inside of contour, more precise
			mask = 0;
			cv::drawContours(mask, contours, idx, cv::Scalar(255), CV_FILLED);
			mean_color = cv::mean(input_host, mask);

			#else
			// use all pixels of bounding box of contour, faster
			cv::Rect box = cv::boundingRect(cnt);
			mean_color = cv::mean(input_host(box));

			#endif

			// TODO: bessere farb checker? vllt mit HSL? braun und blau haben easy anderen h wert, aber wie ist die zeit?
			// (7)
			if (mean_color[2] > mean_color[1] * 1.1 && mean_color[2] > mean_color[0] * 1.1) {
				kiPos++;

				#ifdef SHOW_DEBUG
				std::cout << "KI67 POS: ";
				#endif
			}
			else {

				#ifdef SHOW_DEBUG
				std::cout << "KI67 NEG: ";
				#endif
			}
		}
		else {
			#ifdef SHOW_DEBUG
			std::cout << "NORMAL: ";
			#endif
		}

		#ifdef SHOW_DEBUG
		std::cout << "\tPerimeter: " << std::setprecision(4) << perim << "\t Area: " << carea << "\t HullArea: " << hullarea << "\t Ratio: " << ratio << "\t Color: " << mean_color << std::endl;
		#endif 

		#ifdef WRITE_IMAGE
		cv::drawContours(contourImage, contours, idx, mean_color);
		#endif 
		// show each drawn contour individually and wait for keypress
		//cv::imshow("Contours only", contourImage);
		//cv::waitKey();
	}

	// (8)
	kiScore = calculateKiScore(tumorCells, kiPos);

	double timeFin = timer.GetCounter();
	std::cout << "Final time: " << timeFin << " ms. Score: " << kiScore << std::endl;

	// log time and score to file
	std::ofstream log;
	log.open("log.txt", std::ios::out | std::ios::app);
	log << imgPath.string() << "\tTime:" << std::setprecision(6) << timeFin << "ms\tScore: " << std::setprecision(3) << kiScore << std::endl;
	log.close();

	//cv::imshow("Contours", contourImage);

	#ifdef WRITE_IMAGE
	cv::imwrite("new/KI_" + std::to_string(kiScore) + "_" + imgPath.filename().string(), contourImage);
	#endif

	//cv::waitKey();
}

void showUsage(int exitcode) {
	std::cout << "Usage: ./KiscoreICW <PATH>" << std::endl;
	std::cout << "<PATH> can be a single image or a directory containing images." << std::endl;
	std::cout << "./KiscoreICW -h prints this message." << std::endl;
	exit(exitcode);
}

int main(int argc, char** argv) {

	fs::path img_or_dir_path;

	// check arguments
	if (argc > 1) {

		if (argv[1] == std::string("-h") || argv[1] == std::string("--help")) {
			showUsage(EXIT_SUCCESS);
		}
		else {
			img_or_dir_path = argv[1];
		}
	}
	else {
		showUsage(EXIT_FAILURE);
	}

	// init cuda device
	initCuda();

	if (fs::is_directory(img_or_dir_path)) {

		std::cout << "Processing directory " << img_or_dir_path << "..." << std::endl;

		// iterate over all files in directory
		for (auto p : fs::directory_iterator(img_or_dir_path)) {

			// get file path
			auto file_path = p.path();

			// check for image extensions
			if (file_path.extension().string() == ".jpg" || file_path.extension().string() == ".png") {
				std::cout << "Processing " << file_path.string() << "..." << std::endl;
				doWork(file_path);
			}
			else {
				std::cout << "Skipping " << file_path << "..." << std::endl;
			}
		}
	}
	else if (fs::is_regular_file(img_or_dir_path)) {

		// check for image extensions
		if (img_or_dir_path.extension().string() == ".jpg" || img_or_dir_path.extension().string() == ".png") {
			std::cout << "Processing image " << img_or_dir_path << "..." << std::endl;
			doWork(img_or_dir_path);
		}
		else {
			std::cout << img_or_dir_path << " is not a valid image!" << std::endl;
		}
	}
	else {
		std::cout << img_or_dir_path << " is neither a valid image nor a directory!" << std::endl;
	}

	// reset device
	cv::cuda::resetDevice();
	std::cout << "Finished!" << std::endl;

	return EXIT_SUCCESS;
}
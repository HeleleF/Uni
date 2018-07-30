/**
* @file CannyTester.cpp
* @brief SPIELWIESE FÜR ALLES MÖGLICHE
*/
/*#include <stdio.h> // for printf()
#include <stdlib.h> // for exit()

#include <iostream>
#include <fstream>

#include <opencv2/opencv.hpp> // include openCV
#include "opencv2/core/cuda.hpp" // needs cuda 9.1 with opencv 3.4

#include "CPUTimer.h" // include timer for CPU

cv::Mat inp, gray, edges, show_edges, dst;

int lowThreshold = 50;
int const max_lowThreshold = 100;
float ratio = 3.0f;

bool useL2 = false;

double timeAll, timeTotal, cpuAvg, gpuAvg, gpuTotalAvg = 0.0;

const ushort TRIES = 200;

std::ofstream myfile, myfile2;


void loadAndPrep() {

	// read the input image with openCV
	inp = cv::imread("testKI67.png", CV_LOAD_IMAGE_UNCHANGED);

	// check if image was loaded
	if (!inp.data) {
		printf("Could not open or find the image!\n");
		exit(EXIT_FAILURE);
	}
}

void doCannyOnGPU(bool show) {

	myfile.open("logGPU.txt");
	
	TimingCPU t;
	timeAll = 0.0;
	myfile << "===========================================================\n";

	// check for cuda support
	int n = cv::cuda::getCudaEnabledDeviceCount();
	printf("Found %d devices\n", n);

	if (n < 1) {
		printf("No GPU support :(\n");
		exit(EXIT_FAILURE);
	}

	// init device
	cv::cuda::setDevice(0);

	// repeat multiple times for average
	for (int i = 0; i < TRIES; i++) {

		printf("%3.3d Running Canny on GPU: ", i);
		myfile << i << " Running Canny on GPU: ";

		t.StartCounter();

		cv::cuda::GpuMat inp_dev(inp);
		cv::cuda::GpuMat gray_dev, out_dev;

		cv::cuda::cvtColor(inp_dev, gray_dev, CV_BGRA2GRAY);

		cv::Ptr<cv::cuda::CannyEdgeDetector> canny = cv::cuda::createCannyEdgeDetector(lowThreshold, lowThreshold * ratio, 3, useL2);

		double initTime = t.GetCounter();
	
		canny->detect(gray_dev, out_dev);

		double detectionTime = t.GetCounter() - initTime;
		timeAll += detectionTime;

		out_dev.download(edges);
		double finalTime = t.GetCounter();
		timeTotal += finalTime;

		printf("Total: %6.6f ms; ", finalTime);
		printf("Canny detection: %6.6f ms\n", detectionTime);
		myfile << "Total: " << finalTime << "ms; Canny detection: " << detectionTime << " ms\n";

		
	}
	// clean up device
	cv::cuda::resetDevice();

	// calculate average times
	gpuAvg = timeAll / TRIES;
	gpuTotalAvg = timeTotal / TRIES;

	// print and write results
	printf("Canny on GPU average %6.6f ms.\n", gpuAvg);
	printf("Canny on GPU total average %6.6f ms.\n", gpuTotalAvg);
	myfile << "Canny on GPU average " << gpuAvg << " ms.\n";
	myfile << "Canny on GPU total average " << gpuTotalAvg << " ms.\n";

	// show result image
	if (show) {
		cv::imshow("canny edges", edges);
		cv::waitKey();
	}

	myfile.close();
}

void doCannyOnCPU(bool show) {

	myfile.open("logCPU.txt");

	cv::cvtColor(inp, inp, CV_BGRA2BGR);
	cv::cvtColor(inp, gray, CV_BGR2GRAY);

	TimingCPU tim;
	timeAll = 0.0;
	myfile << "===========================================================\n";

	for (int i = 0; i < TRIES; i++) {

		printf("%3.3d Running on CPU: ", i);
		myfile << i << " Running on CPU: ";

		tim.StartCounter();
		cv::Canny(gray, edges, lowThreshold, lowThreshold * ratio, 3, useL2);
		double erg = tim.GetCounter();

		timeAll += erg;
		printf("Canny took %6.6f milliseconds\n", erg);
		myfile << "Canny took " << erg << " milliseconds\n";
	}
	cpuAvg = timeAll / TRIES;
	printf("Canny on CPU average %6.6f ms.\n", cpuAvg);
	myfile << "Canny on CPU average " << cpuAvg << " milliseconds\n";
	if (show) {
		cv::imshow("result", edges);
		cv::waitKey();
	}

	myfile.close();
}

void CannyThreshold(int, void*) {

	cv::Canny(gray, edges, lowThreshold, lowThreshold * ratio, 3, useL2);

	cv::cvtColor(edges, show_edges, CV_GRAY2BGR);

	edges.release();

	
	dst.setTo(0, show_edges);
	cv::imshow("Fenster", dst);
}

void doCannyOnCPUVisual() {

	cv::cvtColor(inp, inp, CV_BGRA2BGR);
	cv::cvtColor(inp, gray, CV_BGR2GRAY);

	cv::namedWindow("Fenster", CV_WINDOW_AUTOSIZE);
	cv::createTrackbar("Min:", "Fenster", &lowThreshold, max_lowThreshold, CannyThreshold);

	dst = inp.clone();

	CannyThreshold(0, 0);
}

void doBench() {

	myfile2.open("logBench.txt");

	doCannyOnGPU(false);
	doCannyOnCPU(false);

	if (gpuAvg > 0 && gpuTotalAvg > 0) {
		myfile2 << "===========================================================\n";
		myfile2 << "============Theoretical Speedup of approx. " << std::round(cpuAvg / gpuAvg) << "==============\n";
		myfile2 << "================Real Speedup of approx. " << std::round(cpuAvg / gpuTotalAvg) << "=================\n";
		myfile2 << "===========================================================\n";
	}

	myfile2.close();
}

int main() {

	loadAndPrep();

	//doBench();

	doCannyOnGPU(true);

	//doCannyOnCPUVisual();
	
	// keep window open until keypress
	cv::waitKey();

	return EXIT_SUCCESS;
}
*/
/*
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"
#include <stdlib.h>
#include <stdio.h>

using namespace cv;

/// Global variables

Mat src, src_gray;
Mat dst, detected_edges, addWeight;

int edgeThresh = 1;
int lowThreshold;
int const max_lowThreshold = 100;
int ratio = 3;
int kernel_size = 3;
const char* window_name = "Edge Map";


void CannyThreshold(int, void*) {
	/// Reduce noise with a kernel 3x3
	//blur(src_gray, detected_edges, Size(3, 3));

	/// Canny detector
	Canny(src_gray, detected_edges, lowThreshold, lowThreshold*ratio, kernel_size);

	/// Using Canny's output as a mask, we display our result
	dst = Scalar::all(0);

	src.copyTo(dst, detected_edges);
	imshow(window_name, dst);
}

int doCanny(std::string inp) {
	src = imread(inp);

	/// Create a matrix of the same type and size as src (for dst)
	
	for (int i = 50; i < 71; i+=10) {
		dst = src.clone();
		/// Convert the image to grayscale
		cvtColor(src, src_gray, CV_BGRA2GRAY);

		Canny(src_gray, detected_edges, i, i*ratio, kernel_size);

		cvtColor(detected_edges, detected_edges, CV_GRAY2BGR); // convert canny image to bgr
		dst.setTo(cv::Scalar(0, 0, 255), detected_edges);

		imwrite(inp + "Threshold" + std::to_string(i) + ".jpg", dst);
		dst.release();
		detected_edges.release();
	}
	return 0;
}



int main(int argc, char** argv) {
	/// Load an image

			doCanny("testKI67.png");
			doCanny("testKI67_2.jpg");
			//doCanny("testKI67_3.jpg");
			//doCanny("testKI67_4.jpg");
			waitKey(0);
			return 0;

			/// Create a window
			namedWindow(window_name, CV_WINDOW_AUTOSIZE);

			imshow("metal", src);

			/// Create a Trackbar for user to enter threshold
			createTrackbar("Min Threshold:", window_name, &lowThreshold, max_lowThreshold, CannyThreshold);

			/// Show the image
			CannyThreshold(0, 0);

			/// Wait until user exit program by pressing a key
			waitKey(0);

			imwrite("Gray_Image70.jpg", dst);


			return 0;
		}*/
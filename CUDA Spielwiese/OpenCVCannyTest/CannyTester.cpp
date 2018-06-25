#include <stdio.h> // for printf()
#include <stdlib.h> // for exit()

#include <iostream>
#include <fstream>

#include <opencv2/opencv.hpp> // include openCV
#include "opencv2/core/cuda.hpp"

#include "CPUTimer.h" // include timer for CPU

cv::Mat inp, gray, edges, show_edges, dst;

int lowThreshold = 50;
int const max_lowThreshold = 100;
float ratio = 3.0f;

bool useL2 = false;

double timeAll, timeTotal, cpuAvg, gpuAvg, gpuTotalAvg = 0.0;

const ushort TRIES = 200;

std::ofstream myfile;


void loadAndPrep() {

	// read the input image with openCV
	inp = cv::imread("testKI67.png", CV_LOAD_IMAGE_UNCHANGED);


	// check if image was loaded
	if (!inp.data) {
		printf("Could not open or find the image!\n");
		exit(EXIT_FAILURE);
	}

	cv::cvtColor(inp, inp, CV_BGRA2BGR);
	cv::cvtColor(inp, gray, CV_BGR2GRAY);
	//cv::blur(gray, gray, cv::Size(3, 3));
}

void doCannyOnGPU(bool show) {
	
	TimingCPU t;
	timeAll = 0.0;
	myfile << "===========================================================\n";

	int n = cv::cuda::getCudaEnabledDeviceCount();
	printf("Found %d devices\n", n);

	if (n < 1) {
		printf("No GPU support :(\n");
		exit(EXIT_FAILURE);
	}

	cv::cuda::setDevice(0);

	for (int i = 0; i < TRIES; i++) {

		printf("%3.3d Running Canny on GPU: ", i);
		myfile << i << " Running Canny on GPU: ";
		t.StartCounter();

		cv::Ptr<cv::cuda::CannyEdgeDetector> canny = cv::cuda::createCannyEdgeDetector(lowThreshold, lowThreshold * ratio, 3, useL2);

		cv::cuda::GpuMat inp_dev(gray);
		cv::cuda::GpuMat out_dev;

		double initTime = t.GetCounter();

		
		canny->detect(inp_dev, out_dev);

		double detectionTime = t.GetCounter() - initTime;
		timeAll += detectionTime;

		out_dev.download(edges);
		double finalTime = t.GetCounter();
		timeTotal += finalTime;

		printf("Total: %6.6f ms; ", finalTime);
		printf("Canny detection: %6.6f ms\n", detectionTime);
		myfile << "Total: " << finalTime << "ms; Canny detection: " << detectionTime << " ms\n";

		
	}
	cv::cuda::resetDevice();
	gpuAvg = timeAll / TRIES;
	gpuTotalAvg = timeTotal / TRIES;
	printf("Canny on GPU average %6.6f ms.\n", gpuAvg);
	printf("Canny on GPU total average %6.6f ms.\n", gpuTotalAvg);
	myfile << "Canny on GPU average " << gpuAvg << " ms.\n";
	myfile << "Canny on GPU total average " << gpuTotalAvg << " ms.\n";
	if (show) {
		cv::imshow("result", edges);
		cv::waitKey();
	}
}

void doCannyOnCPU(bool show) {

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
}

void CannyThreshold(int, void*) {

	cv::Canny(gray, edges, lowThreshold, lowThreshold * ratio, 3, useL2);

	cv::cvtColor(edges, show_edges, CV_GRAY2BGR);

	
	dst.setTo(0, show_edges);
	cv::imshow("Fenster", dst);
}

void doCannyOnCPUVisual() {

	cv::namedWindow("Fenster", CV_WINDOW_AUTOSIZE);
	cv::createTrackbar("Min:", "Fenster", &lowThreshold, max_lowThreshold, CannyThreshold);

	dst = inp.clone();

	CannyThreshold(0, 0);
}

int main() {

	loadAndPrep();

	myfile.open("log7.txt");
	
	doCannyOnGPU(false);
	doCannyOnCPU(false);
	
	if (gpuAvg > 0 && gpuTotalAvg > 0) {
		myfile << "===========================================================\n";
		myfile << "============Theoretical Speedup of approx. " << std::round(cpuAvg / gpuAvg) << "==============\n";
		myfile << "================Real Speedup of approx. " << std::round(cpuAvg / gpuTotalAvg) << "=================\n";
		myfile << "===========================================================\n";
	}

	myfile.close();
	
	// keep window open until keypress
	cv::waitKey(1000);

	return EXIT_SUCCESS;
}


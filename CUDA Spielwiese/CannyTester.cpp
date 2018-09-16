/**
* @file CannyTester.cpp
* @brief SPIELWIESE FÜR ALLES MÖGLICHE
*/
#include <stdio.h> // for printf()
#include <stdlib.h> // for exit()

#include <iostream>
#include <fstream>

#include <opencv2/opencv.hpp> // include openCV
#include "opencv2/core/cuda.hpp" // needs cuda 9.1 with opencv 3.4

#include "CPUTimer.h" // include timer for CPU
/*
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
*/

#define MIT_GPU

// the deconvolution matrix
float staining_matrix[3][3] = {
	{ 0.650f, 0.704f, 0.286f }, // haema
	{ 0.268f, 0.570f, 0.776f }, // dab
	{ 0.000f, 0.000f, 0.000f }
};
cv::Mat stain = cv::Mat(3, 3, CV_32FC1, staining_matrix).t();

void mach(cv::Mat* input_host) {
	int ros = (*input_host).rows;
	int cls = (*input_host).cols;
	int both = ros * cls;

	// normalize matrix to [0,1]
	double scl = 1.f / 255;

	cv::Mat input_host_float, inp, dummy, mult, output;

#ifdef MIT_GPU
	cv::cuda::GpuMat stain_dev, inp_dev, dummy_dev, mult_dev;
#endif

	// remove alpha channel
	cv::cvtColor((*input_host), (*input_host), CV_BGRA2BGR);

	TimingCPU timer;
	timer.StartCounter();

	// convert matrix to floating point
	(*input_host).convertTo(input_host_float, CV_32FC3, scl);

	std::cout << timer.GetCounter() << std::endl;

	// reshape for matrix multiplication
	inp = input_host_float.reshape(1, both);

	cv::imshow("newImage", inp);
	cv::waitKey();

	std::cout << timer.GetCounter() << std::endl;


#ifndef MIT_GPU

	// perform convolution on cpu
	cv::gemm(inp, stain, 1, dummy, 0, mult);

#else

	// upload data to gpu memory
	inp_dev.upload(inp);
	stain_dev.upload(stain);

	std::cout << "After upload: " << timer.GetCounter() << std::endl;

	// perform deconvolution on gpu
	cv::cuda::gemm(inp_dev, stain_dev, 1.0, dummy_dev, 0.0, mult_dev);

	std::cout << "After deconvolution: " << timer.GetCounter() << std::endl;

	// download result to host memory
	mult_dev.download(mult);

	std::cout << "After download:" << timer.GetCounter() << std::endl;

#endif

	// reshape back to BGR format (3 channels)
	output = mult.reshape(3, ros);


	double all = timer.GetCounter();
	std::cout << "timeAll: " << all << "ms." << std::endl;

	// show result image and separate channels
	//cv::Mat bgr[3];
	//cv::split(output, bgr);

	//cv::imshow("newImage", output);

	///TODO: mit den einzelnen canny machen und dann zusammenfügen die listen
	//ob das schneller geht 

	//cv::imshow("1.channel", bgr[0]);
	//cv::imshow("2.channel", bgr[1]);
	//cv::imshow("3.channel", bgr[2]);

	//cv::waitKey();

}

int main3() {

	cv::cuda::setDevice(0);

	TimingCPU timer2;
	timer2.StartCounter();

	cv::Mat1f dummyHost = cv::Mat1f(800, 3);
	cv::randu(dummyHost, 0, 1);

	cv::cuda::GpuMat outDev, dummy = cv::cuda::GpuMat(dummyHost);

	cv::cuda::GpuMat stain2 = cv::cuda::GpuMat(stain);

	cv::cuda::gemm(dummy, stain2, 1, dummy, 0, outDev);

	std::cout << "gemm fin " << timer2.GetCounter() << std::endl;
	outDev.download(dummyHost);

	std::cout << "Warmup set!" << std::endl;

	cv::Mat input_host = cv::imread("testImages/testKI67.png", CV_LOAD_IMAGE_UNCHANGED);

	mach(&input_host);


	cv::cuda::resetDevice();
	return 0;
}
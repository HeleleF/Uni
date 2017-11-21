#include <stdio.h>
#include "utils.h"
#include <opencv2\opencv.hpp>

// Programmierkonzepte und Algorithmen
// Beleg Aufgabe 3
// Chris Rebbelin s0548921

/**
* CUDA kernel
*/
__global__ void switchGreenBlueKernel(const uchar3* const inImage, uchar3 *const outImage, const int w) {

	// 2D Array of Blocks with the same dimension as the inImg
	// copied from ProgKo_03-04_CUDA.pdf (page 57; 109-112)
	const long idx = blockIdx.y * w + blockIdx.x;

	// get current pixel
	const uchar3 pixel = inImage[idx];

	// save new pixel value
	// openCV uses BGR order (opposed to standard RGB)
	// so switching Blue and Green means switching the first two channels
	// TODO: Writing it like this causes problems on deepgreen (Why?)
	outImage[idx] = { pixel.y, pixel.x, pixel.z };
}

void switchWithCUDA(const char *inputFile, const char *outputFile) {

	printf("CUDA start\n");

	Timer timer;

	// 3 unsigned chars for 3 channels (alpha is not needed)
	uchar3 *host_inputImage, *host_outputImage, *dev_inputImage, *dev_outputImage;

	// read the input image with openCV
	// channel order is Blue Green Red !
	// (assuming we're only dealing with color images)
	cv::Mat inImg = cv::imread(inputFile, CV_LOAD_IMAGE_COLOR);

	// check for errors
	if (!inImg.data) {
		printf("Could not open or find the image");
		return;
	}

	// allocate memory for the output image
	// (CV_8UC3 is 3 channels with 8 byte each)
	cv::Mat outImg = cv::Mat(inImg.rows, inImg.cols, CV_8UC3);

	// get uchar3 pointer to both host images
	host_inputImage = inImg.ptr<uchar3>(0);
	host_outputImage = outImg.ptr<uchar3>(0);

	// calculate buffer size
	const size_t bufSize = sizeof(uchar3) * inImg.rows * inImg.cols;

	// set up device and print properties
	int dev = 0;
	CHECK(cudaSetDevice(dev));

	cudaDeviceProp prop;
	CHECK(cudaGetDeviceProperties(&prop, dev));
	printf("Device %s\n", prop.name);
	printf("%d GPU processors\n", prop.multiProcessorCount);

	// allocate memory for device input image
	CHECK(cudaMalloc((void**)&dev_inputImage, bufSize));

	// allocate memory for device output image
	CHECK(cudaMalloc((void**)&dev_outputImage, bufSize));

	// copy host image to device image
	CHECK(cudaMemcpy(dev_inputImage, host_inputImage, bufSize, cudaMemcpyHostToDevice));

	// 1 Thread per Block, 2D Array of Blocks with the same dimension as the input image
	// copied from ProgKo_03-04_CUDA.pdf (page 57; 109-112)
	const int w = inImg.rows;
	const int h = inImg.cols;

	dim3 grid(w, h, 1);
	dim3 block(1, 1, 1);
	printf("Execution configure <<<(%d,%d), %d>>>\n", grid.x, grid.y, block.x);

	// start the timer
	timer.Start();

	// invoke the kernel
	switchGreenBlueKernel <<<grid, block>>> (dev_inputImage, dev_outputImage, w);

	// stop timer
	timer.Stop();
	printf("CUDA end. Time elapsed: %f ms.\n", timer.GetTime());

	CHECK(cudaDeviceSynchronize());

	// copy the output image from device back to host
	CHECK(cudaMemcpy(host_outputImage, dev_outputImage, bufSize, cudaMemcpyDeviceToHost));

	// write the output inImg
	cv::imwrite(outputFile, outImg);

	cv::imshow("meddl1", inImg);
	cv::imshow("meddl2", outImg);

	cv::waitKey(0);

	// free device memory
	CHECK(cudaFree(dev_inputImage));
	CHECK(cudaFree(dev_outputImage));

	// reset device and exit
	CHECK(cudaDeviceReset());
}

void switchSimple(const char *inputFile, const char *outputFile) {

	printf("CPU start\n");

	// read the input image with openCV
	cv::Mat inImg = cv::imread(inputFile, CV_LOAD_IMAGE_COLOR);

	if (!inImg.data) {
		printf("Could not open or find the image");
		return;
	}

	// allocate memory for the output image
	cv::Mat outImg = cv::Mat(inImg.rows, inImg.cols, CV_8UC3);

	// get uchar3 pointer to both host images
	uchar3 *inputPtr = inImg.ptr<uchar3>(0);
	uchar3 *outputPtr = outImg.ptr<uchar3>(0);

	//**** TIME THIS 

	for (int i = 0; i < inImg.rows * inImg.cols; i++) {

		// get current pixel	
		const uchar3 pixel = inputPtr[i];

		// save new value
		// TODO: Writing it like this causes problems on deepgreen (Why?)
		outputPtr[i] = { pixel.y, pixel.x, pixel.z };
	}

	//**** TIMER END 

	// write the output inImg
	cv::imwrite(outputFile, outImg);

	printf("CPU end. Elapsed Time: ms\n");

}

void switchWithOpenCV(const char *inputFile, const char *outputFile) {

	printf("OpenCV start\n");

	// read the input image with openCV
	cv::Mat inImg = cv::imread(inputFile, CV_LOAD_IMAGE_COLOR);

	if (!inImg.data) {
		printf("Could not open or find the image");
		return;
	}

	// allocate memory for the output image
	cv::Mat outImg = cv::Mat(inImg.rows, inImg.cols, CV_8UC3);	
	std::vector<cv::Mat> channels(3);

	//**** TIME THIS 

	// split bgr image into 3 channels
	cv::split(inImg, channels);

	// create new vector with switched channels
	std::vector<cv::Mat> switchedChannels(3);
	switchedChannels[0] = channels[1];
	switchedChannels[1] = channels[0];
	switchedChannels[2] = channels[2];

	// merge channels again
	cv::merge(switchedChannels, outImg);

	//**** TIMER END 

	// write the output inImg
	cv::imwrite(outputFile, outImg);

	printf("OpenCV end. Elapsed Time: ms\n");
}

/**
* main function
*
* Compile on deepgreen with:
* nvcc ./start.cu `pkg-config --cflags --libs opencv` -o main.out
*
* Start with: ./main.out testimages/dice.png testimages/output.png
*
* TODO: Benchmark all 3 functions (Time, Complexity, Effort) (How?)
* TODO: Find a better way for the kernel grid (?)
* TODO: Timer for non-CUDA stuff
*/
int main(int argc, char **argv) {

	// check for correct cmd args
	if (argc != 3) {
		printf("Usage: %s inputImg outputImg \n", argv[0]);
		return EXIT_FAILURE;
	}

	switchWithCUDA(argv[1], argv[2]);

	//switchWithOpenCV(argv[1], argv[2]);

	//switchSimple(argv[1], argv[2]);

	return EXIT_SUCCESS;
}
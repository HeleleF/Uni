/**
* @file GpuWork.h
* @author Chris Rebbelin s0548921
* @date 2018-07-29
* @brief header file for @c GpuWork.cpp
*/

#pragma once

#ifndef __GPUWORK_H__
#define __GPUWORK_H__

#include <stdio.h> // for printf()
#include <stdlib.h> // for exit()

#include <iostream>
#include <fstream>

#include <filesystem> // for directory_iterator(), needs C++17

#include <opencv2/opencv.hpp> // include openCV
#include "opencv2/core/cuda.hpp" // needs cuda 9.1 with opencv 3.4

#include "CPUTimer.h" // include timer for CPU
#include "GpuTimer.h" // include timer for CPU

/**
* @def MY_OWN_DEBUG_FLAG
*
* @brief A flag used for debugging
*/
#define MY_OWN_DEBUG_FLAGd

// test flags
//#define STAIN_TEST
#define MIT_GPU
//#define MEAN_COLOR_PREC

/**
* @brief Initialises the CUDA context
*/
void initCuda();

/**
* @brief Testing stains
*/
void testStains();

/**
* @brief Loads and checks an image
*
* @param inp Pointer to the input image matrix
* @param relativeFilePath Pointer to the image path
* @return The number of image channels
*/
int loadImage(cv::Mat* inp, std::string* relativeFilePath);

/**
* @brief Does the main calculation
*
* @param imgPath The image path
*/
void doWork(std::string imgPath);

#endif /* __GPUWORK_H__ */

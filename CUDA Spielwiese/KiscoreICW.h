/**
* @file KiscoreICW.h
* @author Chris Rebbelin s0548921
* @date 2018-09-15
* @brief header file for @c KiscoreICW.cpp
*/

#pragma once

#ifndef __KISCOREICW_H__
#define __KISCOREICW_H__

#include <stdio.h> // for printf()
#include <stdlib.h> // for exit()

#include <iostream>
#include <iomanip> // for setprecision()
#include <fstream>

#include <filesystem> // for directory_iterator(), needs C++17

#include <opencv2/opencv.hpp> // include openCV
#include <opencv2/core/cuda.hpp> // needs cuda 9.1 with opencv 3.4

#include "CPUTimer.h" // include timer for CPU
#include "GpuTimer.h" // include timer for GPU

#define SHOW_DEBUG
//#define USE_HIGH_COLOR_PREC
//#define WRITE_IMAGE

const double KI_67_CUTOFF_1 = 0.15;
const double KI_67_CUTOFF_2 = 0.35;

// constants for contour features
const unsigned int MIN_AREA = 6;
const unsigned int TUMOR_SIZE = 50;

const unsigned int MAX_PE = 300;
const unsigned int MIN_PE = 20;

const double MIN_INERTIA = 0.02;

const double HULL_RATIO = 1.5;

// constants for canny
const unsigned int lowThreshold = 70;
const unsigned int highThreshold = 150;
const unsigned int sobelKernelSize = 3;
const bool useL2 = false;

/**
* @brief Initialises the CUDA context
*/
void initCuda();

/**
* @brief Calculates the ki67 score
*
* @param numberOfTumorCells Number of tumor cells
* @param numberOfKiPosCells Number of ki67 positive cells
* @return The ki67 score, -1 if invalid values were passed
*/
double calculateKiScore(int numberOfTumorCells, int numberOfKiPosCells);

/**
* @brief Prints the help message
* and exits with the given code
*/
void showUsage(int exitcode);

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
* For details, see @c KiscoreICW.cpp
*
* @param imgPath The image path
*/
void doWork(const std::filesystem::path imgPath);

#endif /* __KISCOREICW_H__ */

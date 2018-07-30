/**
* @file GpuTimer.h
* @brief A Gpu timer using CUDA events
*/

#pragma once

#ifndef __GPU_TIMER_H__
#define __GPU_TIMER_H__

#include <cuda_runtime.h>

struct GpuTimer {
	cudaEvent_t start;
	cudaEvent_t stop;

	GpuTimer() {
		cudaEventCreate(&start);
		cudaEventCreate(&stop);
	}
	/*
	// this crashes after "cv::cuda::resetDevice()"
	~GpuTimer() {
		cudaEventDestroy(start);
		cudaEventDestroy(stop);
	}*/
	~GpuTimer() {}

	void Start() {
		cudaEventRecord(start, 0);
	}

	void Stop() {
		cudaEventRecord(stop, 0);
	}

	float Elapsed() {
		float elapsed;
		cudaEventSynchronize(stop);
		cudaEventElapsedTime(&elapsed, start, stop);
		return elapsed;
	}
};

#endif  /* __GPU_TIMER_H__ */

#include <cuda_runtime_api.h>

// copied from ProgKo_03-04_CUDA.pdf (page 54)
#define CHECK(call)                                                            \
{                                                                              \
    const cudaError_t error = call;                                            \
    if (error != cudaSuccess)                                                  \
    {                                                                          \
        fprintf(stderr, "Error: %s:%d, ", __FILE__, __LINE__);                 \
        fprintf(stderr, "code: %d, reason: %s\n", error,                       \
                cudaGetErrorString(error));                                    \
        exit(EXIT_FAILURE);                                                               \
    }                                                                          \
}

// copied from ProgKo_03-04_CUDA.pdf (page 55)
struct Timer {
	cudaEvent_t start;
	cudaEvent_t stop;
	float gpuTime;

	Timer() {
		cudaEventCreate(&start);
		cudaEventCreate(&stop);
	}

	~Timer() {
		cudaEventDestroy(start);
		cudaEventDestroy(stop);
	}

	void Start() {
		cudaEventRecord(start, 0);
	}

	void Stop() {
		cudaEventRecord(stop, 0);
	}

	float GetTime() {
		cudaEventSynchronize(stop);
		cudaEventElapsedTime(&gpuTime, start, stop);
		return gpuTime;
	}
};
#ifndef __CANNYTESTER_H__
#define __CANNYTESTER_H__

void loadAndPrep();

void doCannyOnGPU();

void doCannyOnCPU();

void CannyThreshold(int, void*);

void doCannyOnCPUVisual();

#endif /* __CANNYTESTER_H__ */

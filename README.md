# Task-mapping Library using a Space-filling curve

## Introduction

A task-mapping library is developed using the space-filling curve to improve the communication performance of parallel applications with structured grids in Cartesian coordinate. The library includes public APIs and tools for application developers (see table below). Main functionality of the library is to remap tasks to computational resources along the Hilbert curve.
The tools for the performance analysis are accompanied as well. 
The library is implemented in Fortran and some APIs are also developed in C language.

| Name | Functionality | Languages | 
| ------------- | ------------- | --------- | 
| remap\_tasks\_Hilbert\_2D(3D) | Function for Hilbert-curve-based task-mapping for 2D(3D) | Fortran and C |
| calculate\_fragmentation\_2D(3D) | Function to calculate the fragmentation for 2D(3D)  | Fortran | 
| record\_compute\_nodes | A tool to record list of MPI\_ID-hostname pairs | Fortran |
| generate\_job\_log | A tool to generate job log including execution info. \& fragmentation | Fortran |

Following figure illustrates the task-mapping workflow using the library APIs and tools. The workflow has two phases: (a) training and (b) execution phase. In the training phase, the $PIR$ (performance improvement) predictor is trained using the execution profile and fragmentation value. The $PIR$ is defined as the ratio of the elapsed application time between with and without the proposed strategy. 
In the execution phase, the task-remapping is performed only when the predictor expects improvement ($PIR$ > 1). Below we describe details of the task-mapping library and then how to install and run it.



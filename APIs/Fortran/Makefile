FC=mpifort
FFLAG= -cpp 

all: process.o calculate_fragmentation_2D.o calculate_fragmentation_3D.o hilbert.o

clean:
	rm *.o *.mod

process.o: process.f90
	$(FC) -c $(FFLAG) process.f90

calculate_fragmentation_2D.o: calculate_fragmentation_2D.f90
	$(FC) -c $(FFLAG) calculate_fragmentation_2D.f90 

calculate_fragmentation_3D.o: calculate_fragmentation_3D.f90
	$(FC) -c $(FFLAG) calculate_fragmentation_3D.f90 

hilbert.o: hilbert.f90
	$(FC) -c $(FFLAG) hilbert.f90 



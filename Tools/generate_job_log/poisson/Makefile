FC=gfortran
FFLAG= -cpp 

test: generate_job_log.f90 process.o calculate_fragmentation_3D.o 
	$(FC) -o generate_job_log $(FFLAG) ./generate_job_log.f90 calculate_fragmentation_3D.o process.o

clean:
	rm *.o *.mod generate_job_log

calculate_fragmentation_3D.o: calculate_fragmentation_3D.f90
	$(FC) -c $(FFLAG) calculate_fragmentation_3D.f90

process.o: process.f90
	$(FC) -c $(FFLAG) process.f90


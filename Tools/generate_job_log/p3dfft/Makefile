FC=gfortran
FFLAG= -cpp 

test: generate_job_log.f90 process.o calculate_fragmentation_2D.o  
	$(FC) -o generate_job_log $(FFLAG) ./generate_job_log.f90 process.o calculate_fragmentation_2D.o   

clean:
	rm *.o *.mod generate_job_log

process.o: process.f90
	$(FC) -c $(FFLAG) process.f90

calculate_fragmentation_2D.o: calculate_fragmentation_2D.f90
	$(FC) -c $(FFLAG) calculate_fragmentation_2D.f90




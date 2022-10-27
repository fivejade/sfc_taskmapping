FC=mpifort
FFLAG= 

record_compute_nodes: record_compute_nodes.f90 hilbert.o 
	$(FC) -o record_compute_nodes $(FFLAG) ./record_compute_nodes.f90 hilbert.o

clean:
	rm *.o record_compute_nodes

hilbert.o: hilbert.f90
	$(FC) -c $(FFLAG) hilbert.f90




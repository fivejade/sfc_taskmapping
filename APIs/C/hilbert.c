#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpi.h"

// The alogorithm of xy2d function is borrowed from the folowing research: A.R. Butz: Alternative algorithm for Hilbert’s space filling curve. IEEE Trans. On Computers, 20:424-42, April 1971.

/******************************************************************************/
/*
  Purpose:
	ROT rotates and flips a quadrant appropriately.
  Parameters:
	Input, int N, the length of a side of the square.  N must be a power of 2.
	Input/output, int *X, *Y, the old and the new coordinates.
	Input, int RX, RY
*/
void rot ( int n, int *x, int *y, int rx, int ry )
{
  int t;
  if ( ry == 0 )
  {
/*
  Reflect.
*/
    if ( rx == 1 )
    {
      *x = n - 1 - *x;
      *y = n - 1 - *y;
    }
/*
  Flip.
*/
     t = *x;
    *x = *y;
    *y =  t;
  }
  return;
}

/******************************************************************************/
/*
  Purpose:
    I4_POWER returns the value of I^J.
  Parameters:
    Input, int I, J, the base and the power.  J should be nonnegative.
    Output, int I4_POWER, the value of I^J.
*/
int i4_power ( int i, int j )

{
  int k;
  int value;
 
  if ( j < 0 )
  {
    if ( i == 1 )
    {
      value = 1;
    }
    else if ( i == 0 )
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "I4_POWER - Fatal error!\n" );
      fprintf ( stderr, "  I^J requested, with I = 0 and J negative.\n" );
      exit ( 1 );
    }
    else
    {
      value = 0;
    }
  }
  else if ( j == 0 )
  {
    if ( i == 0 )
    {
      fprintf ( stderr, "\n" );
      fprintf ( stderr, "I4_POWER - Fatal error!\n" );
      fprintf ( stderr, "  I^J requested, with I = 0 and J = 0.\n" );
      exit ( 1 );
    }
    else
    {
      value = 1;
    }
  }
  else if ( j == 1 )
  {
    value = i;
  }
  else
  {
    value = 1;
    for ( k = 1; k <= j; k++ )
    {
      value = value * i;
    }
  }
  return value;
}


/******************************************************************************/
/*
  Purpose:
    XY2D converts a 2D Cartesian coordinate to a 1D Hilbert coordinate.
  Discussion:
    It is assumed that a square has been divided into an NxN array of cells,
    where N is a power of 2.
    Cell (0,0) is in the lower left corner, and (N-1,N-1) in the upper 
    right corner.
  Parameters:
    Input, int M, the index of the Hilbert curve.
    The number of cells is N=2^M.
    0 < M.
    Input, int X, Y, the Cartesian coordinates of a cell.
    0 <= X, Y < N.
    Output, int XY2D, the Hilbert coordinate of the cell.
    0 <= D < N * N.
*/
int xy2d ( int m, int x, int y )
{
  int d = 0;
  int n;
  int rx;
  int ry;
  int s;
  n = i4_power ( 2, m );
  for ( s = n / 2; s > 0; s = s / 2 )
  {
    rx = ( x & s ) > 0;
    ry = ( y & s ) > 0;
    d = d + s * s * ( ( 3 * rx ) ^ ry );
    rot ( s, &x, &y, rx, ry );
  }
  return d;
}

int distance_from_2D_points(int p, int id1, int id2) {
	return xy2d(p, id1, id2);
}

/******************************************************************************/
/*
  Purpose:
    remap_tasks_Hilbert_2D does task-remapping using Hilbert-curve.
  Discussion:
    It is assumed that a square has been divided into an Nx x Ny array of cells,
    where Nx and Ny are a power of 2.
  Parameters:
  	cur_comm = current communicator (input)
  	new_comm = task-remapped new communicator (return)
*/
void remap_tasks_Hilbert_2D(int Nx, int Ny, MPI_Comm *cur_comm, MPI_Comm *new_comm) {
	int i = 0, j;
	int x,y;
	int p = log2(Nx*Ny)/2;
	int log2_Nx;
	int taskid, numtasks;
	MPI_Group world_group, new_group;
	int *new_tasklist;

	// assert that Nx and Ny are same and a power of 2
	if (Nx!=Ny) {
		fprintf(stderr, "Wrong dimensions: Nx(%d)!=Ny(%d)\n", Nx, Ny);
		new_comm = cur_comm;
		return;
	}
	log2_Nx = (int)log2(Nx);
	if (Nx!=(int)pow(2,log2_Nx)) {
		fprintf(stderr, "Wrong dimensions: Nx(%d) should be a power of 2\n", Nx);
		new_comm = cur_comm;
		return;
	}

	MPI_Comm_rank(*cur_comm,&taskid);
	MPI_Comm_size(*cur_comm,&numtasks);
	new_tasklist=(int *)malloc(sizeof(int)*numtasks);
    for (x=0;x<Nx;x++) {
        for (y=0;y<Ny;y++) {
            j = distance_from_2D_points(p, x, y);
            new_tasklist[i]=j;
            i=i+1;
        }
    }
/*    if (taskid == 0) {
        fprintf(stdout, "==== start of hilbert node ID info\n");
        for (i=0;i<numtasks;i++) {
            fprintf(stdout, "prev_nodeID: %d new_nodeId: %d\n", i, new_tasklist[i]);
        }
        fprintf(stdout, "==== end of hilbert node ID info\n");
    }*/

	MPI_Comm_group(*cur_comm, &world_group);
	MPI_Group_incl(world_group, numtasks, new_tasklist, &new_group);
	MPI_Comm_create_group(*cur_comm, new_group, 0, new_comm);

	free(new_tasklist);
}


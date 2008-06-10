#ifndef ENDPOINT_H

#define ENDPOINT_H
#include <R.h>
#include <Rinternals.h>
#include <vector>
#include <algorithm> // For max




//////// Endpoint class

class Endpoint{

private:
  
  static int state_array[2][2][2];

  inline int state() const { return( state_array[query][left][closed] ); }
  
public:

  static double tol;

  int index;
  double pos;
  bool query, left, closed;

  Endpoint(int i, double p, bool q, bool l, bool c); 
  
  inline bool operator< (const Endpoint& other) const {
    // We use relative difference for assessing equality of pos, and compare on
    // the basis of state in case of (effective) ties.
    double max_abs, rd;
    max_abs = std::max( fabs( other.pos ), fabs( this->pos ) );
    if ( max_abs > 0. ) {
      rd = ( this->pos - other.pos ) / max_abs;
      if ( rd > tol ) return( false );
      if ( rd < -1. * tol ) return( true );
    }
    return( this->state() < other.state() );
  }

  static void set_state_array( const int new_array[2][2][2] ) {
    int i, j, k;
    for( i = 0; i < 2; i++ ) 
      for( j = 0; j < 2; j++ ) 
	for( k = 0; k < 2; k++ ) 
	  Endpoint::state_array[i][j][k] = new_array[i][j][k];
  }

  void R_print() const;

};




//////// Endpoints class

class Endpoints : public std::vector< Endpoint > {
public:
  Endpoints( const double * pos, const int * closed, int n, bool query, bool is_full );
  void R_print() const;
};




#endif // #ifndef ENDPOINT_H

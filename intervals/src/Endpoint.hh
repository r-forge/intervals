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

  int state() const { return( state_array[query][left][closed] ); }
  
public:

  static double tol;
  
  int index;
  double pos;
  bool query, left, closed;

  Endpoint(int i, double p, bool q, bool l, bool c) { index = i; pos = p; query = q; left = l; closed = c; }

  bool operator< (const Endpoint& other) const {
    // We use relative difference for assessing equality of pos, and compare on
    // the basis of state in case of (effective) ties.
    double max_abs, rd;
    max_abs = std::max( abs( other.pos ), abs( this->pos ) );
    if ( max_abs > 0. ) {
      rd = ( this->pos - other.pos ) / max_abs;
      if ( rd > tol ) return( false );
      if ( rd < -1. * tol ) return( true );
    }
    return( this->state() < other.state() );
  }

  void R_print() const;

};




//////// Endpoints class

class Endpoints : public std::vector< Endpoint > {

public:

  Endpoints( const double * pos, const int * closed, int n, bool query, bool is_full ) {
    /*
      The pos pointer should point to an n x 2 array of endpoints, and the closed
      pointer, to either an array of booleans of the same size (if full = true)
      or an array of two booleans (if full = false). Note that R uses int, not
      bool, for logicals. Intervals with R numeric NA in either slot are
      dropped, with a warning. 
    */
    int i;
    bool na_warning = false;
    this->reserve( 2 * n );
    for ( i = 0; i < n; i++ ) {
      if ( ISNA( pos[i] ) || ISNA( pos[i+n] ) ) {
	na_warning = true; 
	continue;
      }
      this->push_back( Endpoint( i, pos[i], query, true, (bool) closed[ is_full ? i : 0 ] ) );
      this->push_back( Endpoint( i, pos[i+n], query, false, (bool) closed[ is_full ? i+n : 1 ] ) );
    }
    if ( na_warning ) warning( "Some NA endpoints encountered." );
  }

  void R_print() const;

};




//////// Default tolerance for floating point equality 

// This R all.equal default on my Macbook.

double Endpoint::tol = 1.490116e-08;




//////// Ordering for tied endpoints

/*
  Let Q/T be query/target, L/R be left/right, and O/C be open/closed. Our
  ordering, when pos is effectively tied, is then:

    QRO < TRO ... < TLC < QLC < QRC < TRC ... < TLO < QLO
     0     1         2     3     4     5         6     7

  The basic principals are, for similar closure, start targets before
  queries but finish them after queries. For abutting intervals, start new
  intervals before finishing old ones, unless one or both endpoints are
  open, in which case we should finish old intervals first.
*/

int Endpoint::state_array[2][2][2] = {
  // Target {{RO, RC}, {LO, LC}}
  {{1,5},{6,2}},
  // Query {{RO, RC}, {LO, LC}}
  {{0,4},{7,3}}
};




//////// R_print methods, for debugging

void Endpoint::R_print() const {
  Rprintf(
	  "index = %i, pos = %f (%s, %s, %s)\n",
	  index, pos,
	  query ? "query" : "target",
	  left ? "left" : "right",
	  closed ? "closed" : "open"
	  );
}

void Endpoints::R_print() const {
  Endpoints::const_iterator it;
  for ( it = this->begin(); it < this->end(); it++ ) 
    it->R_print();
}




#endif // #ifndef ENDPOINT_H

#include <vector>
#include <algorithm> // For max
#include <sstream>


//////// enums

enum type_enum { query, target };
enum side_enum { left, right };
enum closure_enum { open, closed };




//////// index type

typedef unsigned long ind_t;




//////// Endpoint class

class Endpoint{
  
private:
  
  ind_t index;
  double pos;
  type_enum type;
  side_enum side;
  closure_enum closure;

  static int state_array[2][2][2];

  int state() const { return( state_array[type][side][closure] ); }
  
public:

  static double tol;
  
  Endpoint(ind_t i, double p, type_enum t, side_enum s, closure_enum c) { 
     index = i; pos = p; type = t; side = s; closure = c; 
  }

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

  std::string str() const {
    std::stringstream ss;
    ss << 
      "index = " << index <<
      ", pos = " << pos <<
      " (" << ( type ? "target" : "query" ) <<
      ", " << ( side ? "right" : "left" ) <<
      ", " << ( closure ? "closed" : "open" ) <<
      ")";
    return( ss.str() );
  }

};




//////// Endpoints class

class Endpoints : public std::vector< Endpoint > {
public:
  Endpoints( const double * pos, const bool * closure, ind_t n, type_enum type, bool is_full ) {
    /*
      The pos pointer should point to an nx2 array of endpoints, and the closure
      pointer, to either an array of booleans of the same size (if full = true)
      or an array of two booleans (if full = false).
     */
    ind_t i;
    this->reserve( 2 * n );
    for ( i = 0; i < n; i++ ) {
      this->push_back( Endpoint( i, pos[i], type, left, (closure_enum) closure[ is_full ? i : 0 ] ) );
      this->push_back( Endpoint( i, pos[i+n], type, right, (closure_enum) closure[ is_full ? i+n : 1] ) );
    }
  }
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
  // Query {{LO, LC}, {RO, RC}}
  {{7,3},{0,4}},
  // Target {{LO, LC}, {RO, RC}}
  {{6,2},{1,5}}
};

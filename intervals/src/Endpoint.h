#include <algorithm> // For max



//////// enums

enum type_enum { query, target };
enum side_enum { left, right };
enum closure_enum { closed, open };




//////// Class definition

class Endpoint{
  
 private:
  
  double pos;
  type_enum type;
  side_enum side;
  closure_enum closure;
  // bool closed, left, query;
  unsigned long index;
  
 public:

  static double tol;
  static int state_array[2][2][2];
  
  Endpoint() {}
  Endpoint(double p, type_enum t, side_enum s, closure_enum c, unsigned long i) { 
    pos = p; type = t; side = s; closure = c; index = i; 
  }

  int state() const { return( state_array[type][side][closure] ); }

  bool operator< (const Endpoint& other) const {
    // We use relative difference for assessing equality of pos. 
    // TO DO: what if the max is 0!
    double rd = ( this->pos - other.pos ) / abs( std::max( other.pos, this->pos ) );
    std::cout << "rd = " << rd << std::endl;
    if ( rd > tol ) return( false );
    if ( rd < -1. * tol ) return( true );
    // The x values are effectively equal. Assess based on endpoint type flags.
    return( this->state() < other.state() );
  }

  void print() {
    std::cout << 
      "pos = " << pos <<
      ", " << ( type == query ? "query" : "target" ) <<
      ", " << ( side == left ? "left" : "right" ) <<
      ", " << ( closure == closed ? "closed" : "open" ) <<
      ", index = " << index <<
      std::endl;
  }

};




//////// Tolerance for floating point equality 

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
  // Query {{LC, LO}, {RC, RO}}
  {{3,7},{4,0}},
  // Target {{LC, LO}, {RC, RO}}
  {{2,6},{5,1}}
};

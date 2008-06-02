class Endpoint{
  
 private:
  
  double x;
  bool closed, left, query;
  int index;
  
 public:

  static double tol;
  
  Endpoint() {}
  Endpoint(double x, bool c, bool l, bool q, int i) { 
    x = x; closed = c; left = l; query = q, index = i; 
  }

  int score() const { 
    /*
      Let Q/T be query/target, L/R be left/right, and O/C be open/closed. Our
      ordering, when x is effectively tied, is then:

        QRO < TRO ... < TLC < QLC < QRC < TRC ... < TLO < QLO

      The basic principals are, for similar closure, start targets before
      queries but finish them after queries. For abutting intervals, start new
      intervals before finishing old ones, unless one or both endpoints are
      open, in which case we should finish old intervals first.
    */
    return( 
	   query ?
	   // Query
	   ( left ? 
	     ( closed ? 3 : 7 ) : // Left
	     ( closed ? 4 : 0 )   // Right
	     ) :
	   // Target
	   ( left ? 
	     ( closed ? 2 : 6 ) : // Left
	     ( closed ? 5 : 1 )   // Right
	     )
	    );
  }

  bool operator< (const Endpoint& other) const {
    // We use relative difference for assessing equality of x. 
    double rd = ( this->x - other.x ) / std::max( other.x, this->x );
    if ( rd > tol ) return( false );
    if ( rd < -1. * tol ) return( true );
    // The x values are effectively equal. Assess based on endpoint type flags.
    return( this->score() < other.score() );
  }

};

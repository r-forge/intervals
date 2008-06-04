#include <R.h>
#include <Rinternals.h>
#include "Endpoint.hh"
#include <vector>
#include <set>
#include <algorithm>

/*
  What we require to prevent segfaults:

    1. For Intervals_full objects, the endpoint and closure matrices must be of
       the same dimension, and have two columns each. For Intervals objects, we
       expect a closure vector of length 2.

    2. Endpoints must be type double, and closure must be R logical, which ends
       up as int in C++.

  The validity function for the classes should verify both of these properties,
  so we do no additional checks here, i.e., we assume that we've been passed
  valid objects.

  TO DO: Try hash_set instead of set, if it compiles OK.
*/

extern "C"
{

  SEXP _interval_overlap(SEXP qe, SEXP te, SEXP qc, SEXP tc, SEXP q_full, SEXP t_full, SEXP tol) {
    
    // Load data, combine
    int tn = nrows(te);
    Endpoints q ( REAL(qe), LOGICAL(qc), nrows(qe), true, *LOGICAL(q_full) );
    Endpoints t ( REAL(te), LOGICAL(tc), tn, false, *LOGICAL(t_full) );
    q.insert( q.end(), t.begin(), t.end() );

    // Set equality tolerance and sort
    Endpoint::tol = *REAL(tol);
    sort( q.begin(), q.end() );

    // Process overlaps
    Endpoints::const_iterator it;
    std::set<int> q_active, t_active;
    std::set<int>::iterator it_active;
    std::vector< std::vector<int> > indices ( tn );
    
    for ( it = q.begin(); it < q.end(); it++ ) {
      // Query endpoint
      if ( it->query ) {
    	if ( it->left ) {
    	  q_active.insert( it->index );
    	  for( it_active = t_active.begin(); it_active != t_active.end(); it_active++ )
    	    indices[ *it_active ].push_back( it->index + 1 );
    	}
    	else q_active.erase( it->index );
      }
      else {
    	// Target Endpoint
    	if ( it->left ) {
    	  t_active.insert( it->index );	  
	  for( it_active = q_active.begin(); it_active != q_active.end(); it_active++ )
	    indices[ it->index ].push_back( *it_active + 1 );
    	}
    	else t_active.erase( it->index );      
      }
    }
    
    // Prepare and return result.
    SEXP result;
    int i;

    PROTECT( result = allocVector( VECSXP, tn ) );    
    for( i = 0; i < tn; i++ ) {
      SET_VECTOR_ELT( result, i, allocVector( INTSXP, indices[i].size() ) );
      copy( 
	   indices[i].begin(), indices[i].end(), 
	   std::vector<int>::iterator ( INTEGER( VECTOR_ELT( result, i ) ) )
	    );
    }
    UNPROTECT(1);
    return( result );    
  
  }

}

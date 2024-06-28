// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include <Rcpp.h>
#include <map>


using namespace Rcpp;
using namespace std;

struct single_symbol{string e1;                      };
struct double_symbol{string e1; string e2;           };
struct triple_symbol{string e1; string e2; string e3;};

typedef map <single_symbol, long double> a1;
typedef map <double_symbol, long double> a2;
typedef map <triple_symbol, long double> a3;


inline bool operator<(const single_symbol& lhs, const single_symbol& rhs) {
    return lhs.e1 < rhs.e1;
}

inline bool operator<(const double_symbol& lhs, const double_symbol& rhs) {
    if (lhs.e1 != rhs.e1)
        return lhs.e1 < rhs.e1;
    return lhs.e2 < rhs.e2;
}

inline bool operator<(const triple_symbol& lhs, const triple_symbol& rhs) {
    if (lhs.e1 != rhs.e1)
        return lhs.e1 < rhs.e1;
    if (lhs.e2 != rhs.e2)
        return lhs.e2 < rhs.e2;
    return lhs.e3 < rhs.e3;
}



struct aaa{
    a1 single_indeterminate;
    a2 double_indeterminate;
    a3 triple_indeterminate;
};

#define K -1 // a(bc) == K(ab)c

#if K == 1
    #error "K cannot be 1 [the algebra is not nilpotent]"
#endif


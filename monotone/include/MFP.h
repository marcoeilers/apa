#ifndef MFP_H
#define MFP_H

#include <stdio.h>
#include "MFramework.h"
#include "ControlFlow.h"

// Class that provides a method to compute the maximal fix point
// of any given monotone framework (and program).
// Implementation of solve() is in src/MFP.hpp

// param T is the type on which the analysis operated (e.g. a set of expressions
// for available expressions analysis)
template<typename T> class MFP
{
public:
    MFP();
    virtual ~MFP();

    // takes a MFramework object describing the analysis
    // to be performed and a ControlFlow object describing the
    // program that is to be analyzed.
    T * solve(MFramework<T>*);
protected:
private:
};

#include "MFP.hpp"

#endif // MFP_H

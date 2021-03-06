/*
 * MFP.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

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
    // to be performed.
    // returns an array of pairs.
    // result[i].first is the context value of statement s (i.e.
    // the analysis result before the statement), result[i].second
    // is the effect value, i.e. the result after the statement
    std::pair<T, T> * solve(MFramework<T>*);
protected:
private:
};

#include "MFP.hpp"

#endif // MFP_H

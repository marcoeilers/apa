/*
 * MFramework.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef MFRAMEWORK_H
#define MFRAMEWORK_H

#include "ParserSemantics.h"
#include "ControlFlow.h"

// Abstract base class for different kinds of intraprocedural analyses.v
// Contains only information that is specific for one type of analysis
// but is independent from the program that is analyzed.
// Can be used for both forward and backward analyses.

// param T is the type on which the analysis operated (e.g. a set of expressions
// for available expressions analysis)
template<typename T> class MFramework {
public:
	//MFramework();
	//virtual ~MFramework();

	// methods defining the lattice
	virtual T join(T&, T&) = 0;
	virtual T bottom() = 0;
	virtual bool lessOrEqual(T&, T&) = 0;

	// transfer function
	virtual T f(T&, int) = 0;

	// initial value for extremal labels
	virtual T getExtremalValue() = 0;

	// set of extremal labels
	virtual std::set<int> getExtremalLabels() = 0;

	// gets the next (i.e. following or preceding) labels for a given label
	virtual std::set<int> getNext(int) = 0;

	// gets the list of all statements in the function
	// in order of their labels
	virtual std::vector<CPPParser::Statement*> getLabels() {
		return cflow->getLabels();
	}

	// creates a string that describes an object of the type
	// the lattice works on
	virtual std::string toString(T& t) {return ""; }
protected:
	ControlFlow* cflow;
};

#endif // MFRAMEWORK_H

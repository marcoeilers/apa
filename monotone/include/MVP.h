/*
 * MVP.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef MVP_H_
#define MVP_H_

#include "EMFramework.h"

/*
 * Solves a given embellished monotone framework for a forward analysis,
 * using call strings as context. Gives the Meet over Valid Paths solution.
 *
 * Type param T is the type on which the analysis operates
 *
 * implementation of 'solve' is in MVP.hpp
 */
template<typename T> class MVP {
public:
	// constructor takes the limit k to the length of the context
	// use 0 to omit context
	MVP(int);
	virtual ~MVP();

	// returns an array which, for each label, contains a pair of  maps
	// from contexts (call strings) to the result of the analysis.
	// 'first' of each pair is the context value (value before the statement)
	// 'second' is the effect value (value after the statement)
	std::pair<std::map<std::string, T>, std::map<std::string, T> >* solve(EMFramework<T>*);
protected:
	// maximal length of call strings
	int k;
	// prints a call string in a readable way
	std::string printContext(std::string);
	// prepends a label to a context (respecting the maximal length,
	// i.e. cutting off the end if necessary
	std::string prepend(int, std::string);
	// returns the current value for the given label and given context
	// in the supplied map, or bottom if there is no such value
	T getCurrentValue(std::map<std::string, T>*, int, std::string, EMFramework<T>*);
	// adds the given label with the given context and all following labels
	// to the map (first parameter) if not already present
	void addToWorkList(std::set<std::pair<int, std::string> >*, int, std::string,
			EMFramework<T>*);
};

#include "MVP.hpp"

#endif /* MVP_H_ */

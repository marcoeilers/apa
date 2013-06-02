/*
 * MVP.h
 *
 *  Created on: May 27, 2013
 *      Author: marco
 */

#ifndef MVP_H_
#define MVP_H_

#include "EMFramework.h"

/*
 * Solves a given embellished monotone framework for a forward analysis,
 * using call strings as context. Gives the Meet over Valid Paths solution.
 *
 * Type param T is the type on which the analysis operates
 */
template<typename T> class MVP {
public:
	// constructor takes the limit k to the length of the context
	// use 0 to omit context
	MVP(int);
	virtual ~MVP();
	// returns an array which, for each label, contains a map of contexts
	// to the result of the analysis.
	std::pair<std::map<std::string, T>, std::map<std::string, T> >* solve(EMFramework<T>*);
protected:
	int k;
	std::string prepend(int, std::string);
	T getResult(std::map<std::string, T>*, int, std::string, EMFramework<T>*);
	void addToWorkList(std::set<std::pair<int, std::string> >*, int, std::string,
			EMFramework<T>*);
};

#include "MVP.hpp"

#endif /* MVP_H_ */

/*
 * MVP.h
 *
 *  Created on: May 27, 2013
 *      Author: marco
 */

#ifndef MVP_H_
#define MVP_H_

#include "EMFramework.h"

template<typename T> class MVP {
public:
	MVP(int);
	virtual ~MVP();
	std::map<std::string, T>* solve(EMFramework<T>*);
protected:
	int k;
	std::string prepend(int, std::string);
};

#include "MVP.hpp"

#endif /* MVP_H_ */

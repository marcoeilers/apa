/*
 * PointerAnalysis.h
 *
 *  Created on: Jun 1, 2013
 *      Author: marco
 */

#ifndef POINTERANALYSIS_H_
#define POINTERANALYSIS_H_

#include "EMFramework.h"
#include "InterControlFlow.h"

class PointerAnalysis: virtual public EMFramework<
		std::map<std::string, std::set<std::string> > > {
public:
	PointerAnalysis(InterControlFlow*);
	virtual ~PointerAnalysis();

	// methods defining the lattice
	virtual std::map<std::string, std::set<std::string> > join(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&) = 0;
	virtual std::map<std::string, std::set<std::string> > bottom() = 0;
	virtual bool lessThan(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&) = 0;

	// transfer functions
	virtual std::map<std::string, std::set<std::string> > f(std::map<std::string, std::set<std::string> >&, CPPParser::Statement*) = 0;
	virtual std::map<std::string, std::set<std::string> > fcall(std::map<std::string, std::set<std::string> >&, CPPParser::Statement*,
			CPPParser::FunctionDeclaration*) = 0;
	virtual std::map<std::string, std::set<std::string> > fenter(std::map<std::string, std::set<std::string> >& t) = 0;
	virtual std::map<std::string, std::set<std::string> > fexit(std::map<std::string, std::set<std::string> >& t) = 0;
	virtual std::map<std::string, std::set<std::string> > freturn(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&, CPPParser::Statement*) = 0;

	// initial value for extremal labels
	virtual std::map<std::string, std::set<std::string> > getExtremalValue() = 0;

	// set of extremal (i.e. first) labels, usually a singleton set
	virtual std::set<int> getExtremalLabels() = 0;
};

#endif /* POINTERANALYSIS_H_ */

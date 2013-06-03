/*
 * PointerAnalysis.h
 *
 *  Created on: Jun 1, 2013
 *      Author: marco
 */

#ifndef POINTERANALYSIS_H_
#define POINTERANALYSIS_H_

#include <sstream>

#include "EMFramework.h"
#include "InterControlFlow.h"

class PointerAnalysis: virtual public EMFramework<
		std::map<std::string, std::set<std::string> > > {
public:
	PointerAnalysis(InterControlFlow*);
	virtual ~PointerAnalysis();

	// methods defining the lattice
	virtual std::map<std::string, std::set<std::string> > join(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&);
	virtual std::map<std::string, std::set<std::string> > bottom();
	virtual bool lessThan(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&);

	// transfer functions
	virtual std::map<std::string, std::set<std::string> > f(std::map<std::string, std::set<std::string> >&, int);
	virtual std::map<std::string, std::set<std::string> > fcall(std::map<std::string, std::set<std::string> >&, int,
			CPPParser::FunctionDeclaration*);
	virtual std::map<std::string, std::set<std::string> > fenter(std::map<std::string, std::set<std::string> >& t);
	virtual std::map<std::string, std::set<std::string> > fexit(std::map<std::string, std::set<std::string> >& t);
	virtual std::map<std::string, std::set<std::string> > freturn(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&, int);

	// initial value for extremal labels
	virtual std::map<std::string, std::set<std::string> > getExtremalValue();

	// set of extremal (i.e. first) labels, usually a singleton set
	virtual std::set<int> getExtremalLabels();

	virtual std::string toString(std::map<std::string, std::set<std::string> >&);

protected:
	virtual std::set<std::string> evaluateLhs(int, std::string, std::map<std::string, std::set<std::string> >&);
	virtual std::set<std::string> evaluateRhs(CPPParser::VariableValue*, std::map<std::string, std::set<std::string> >&);
};

#endif /* POINTERANALYSIS_H_ */

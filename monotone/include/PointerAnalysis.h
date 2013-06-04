/*
 * PointerAnalysis.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef POINTERANALYSIS_H_
#define POINTERANALYSIS_H_

#include <sstream>

#include "EMFramework.h"
#include "InterControlFlow.h"

/*
 * An embellished monotone framework for performing points-to analysis.
 * Built on the idea of Andersen's pointer analysis, extended to work
 * interprocedurally.
 */
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
	virtual void addLabels(int, std::string);
	virtual std::set<std::string> evaluateLhs(int, int, std::string, std::map<std::string, std::set<std::string> >&);
	virtual std::set<std::string> evaluateRhs(int, CPPParser::VariableValue*, std::map<std::string, std::set<std::string> >&);
	std::map<std::string, std::set<int> > functionLabels;
	std::map<std::string, std::map<std::string, std::string> > varIDs;
	std::string getID(int, std::string);
};

#endif /* POINTERANALYSIS_H_ */

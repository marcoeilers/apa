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
	// all public methods are required by EMFramework

	// constructor takes program information
	PointerAnalysis(InterControlFlow*);
	virtual ~PointerAnalysis();

	// methods defining the lattice
	virtual std::map<std::string, std::set<std::string> > join(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&);
	virtual std::map<std::string, std::set<std::string> > bottom();
	virtual bool lessOrEqual(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&);

	// transfer functions
	virtual std::map<std::string, std::set<std::string> > f(std::map<std::string, std::set<std::string> >&, int);
	virtual std::map<std::string, std::set<std::string> > fcall(std::map<std::string, std::set<std::string> >&, int,
			CPPParser::FunctionDeclaration*);
	virtual std::map<std::string, std::set<std::string> > fenter(std::map<std::string, std::set<std::string> >& t);
	virtual std::map<std::string, std::set<std::string> > fexit(std::map<std::string, std::set<std::string> >& t);
	virtual std::map<std::string, std::set<std::string> > freturn(std::map<std::string, std::set<std::string> >&, std::map<std::string, std::set<std::string> >&, int);

	// initial value for extremal labels
	virtual std::map<std::string, std::set<std::string> > getExtremalValue();

	// set of extremal (i.e. first) labels, always a singleton set
	virtual std::set<int> getExtremalLabels();

	virtual std::string toString(std::map<std::string, std::set<std::string> >&);

protected:
	// inserts the labels for each function into 'functionLabels'
	void addLabels(int, std::string);
	// from Andersen's pointer analysis: evaluates the left hand side
	// of an assignment
	std::set<std::string> evaluateLhs(int, int, std::string, std::map<std::string, std::set<std::string> >&);
	// from Andersen's pointer analysis: evaluates the right hand side
	// of an assignment
	std::set<std::string> evaluateRhs(int, CPPParser::VariableValue*, std::map<std::string, std::set<std::string> >&);
	// at the given label, gets the unique ID for the given variable name
	std::string getID(int, std::string);
	// mapping of all function names to sets of the labels in those functions
	std::map<std::string, std::set<int> > functionLabels;
	// mappings for all functions, for all variables to a string uniquely
	// identifying them in the program
	std::map<std::string, std::map<std::string, std::string> > varIDs;
};

#endif /* POINTERANALYSIS_H_ */

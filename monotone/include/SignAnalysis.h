/*
 * SignAnalysis.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef SIGNANALYSIS_H_
#define SIGNANALYSIS_H_

#include <sstream>
#include "EMFramework.h"
#include "InterControlFlow.h"
#include <array>

enum Sign {
	SIGN_PLUS, SIGN_MINUS, SIGN_ZERO
};

typedef std::array<std::array<std::set<Sign>*, 3>, 3> SignArray;

/*
 * Interprocedural Sign Analysis
 * Implemented to test the framework
 */
class SignAnalysis: virtual public EMFramework<std::map<std::string, std::set<Sign> > > {
public:
	// constructor takes interprocedural control flow
	SignAnalysis(InterControlFlow*);
	virtual ~SignAnalysis();

	// operations of the lattice
	virtual std::map<std::string, std::set<Sign> > join(
			std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&);
	virtual std::map<std::string, std::set<Sign> > bottom();
	virtual bool lessOrEqual(std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&);

	// transfer functions
	virtual std::map<std::string, std::set<Sign> > f(
			std::map<std::string, std::set<Sign> >&, int);
	virtual std::map<std::string, std::set<Sign> > fcall(
			std::map<std::string, std::set<Sign> >&, int,
			CPPParser::FunctionDeclaration*);
	virtual std::map<std::string, std::set<Sign> > fenter(
			std::map<std::string, std::set<Sign> >& t);
	virtual std::map<std::string, std::set<Sign> > fexit(
			std::map<std::string, std::set<Sign> >& t);
	virtual std::map<std::string, std::set<Sign> > freturn(
			std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&, int);

	// initial value for extremal labels
	virtual std::map<std::string, std::set<Sign> > getExtremalValue();

	virtual std::set<int> getExtremalLabels();
	virtual std::set<int> getNext(int);

	virtual std::string toString(std::map<std::string, std::set<Sign> >&);
protected:
	// computes the signs that a given expression can have in the
	// current environment
	virtual std::set<Sign> getSigns(CPPParser::VariableValue*,
			std::map<std::string, std::set<Sign> >&);
	// adds the result of the operation param2 (operator defined by param1) param3 to param4
	virtual void addAllCombinations(SignArray, std::set<Sign>&, std::set<Sign>&, std::set<Sign>*);
	// converts a char to a sign (used for initialization)
	std::set<Sign>* getSign(char c);
	// set containing only plus
	std::set<Sign> plusSet;
	// set containing only minus
	std::set<Sign> minusSet;
	// set containing only zero
	std::set<Sign> zeroSet;
	// set containing plus, minus, zero
	std::set<Sign> allSet;
	// guess what
	std::set<Sign> emptySet;

	// arrays of sets specifying the signs that may result from an operation
	// with the given operator
	SignArray op_plus;
	SignArray op_minus;
	SignArray op_mult;
	SignArray op_div;
};

#endif /* SIGNANALYSIS_H_ */

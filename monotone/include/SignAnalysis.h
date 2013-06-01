/*
 * SignAnalysis.h
 *
 *  Created on: May 28, 2013
 *      Author: marco
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
	virtual bool lessThan(std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&);

	// transfer functions
	virtual std::map<std::string, std::set<Sign> > f(
			std::map<std::string, std::set<Sign> >&, CPPParser::Statement*);
	virtual std::map<std::string, std::set<Sign> > fcall(
			std::map<std::string, std::set<Sign> >&, CPPParser::Statement*,
			CPPParser::FunctionDeclaration*);
	virtual std::map<std::string, std::set<Sign> > fenter(
			std::map<std::string, std::set<Sign> >& t);
	virtual std::map<std::string, std::set<Sign> > fexit(
			std::map<std::string, std::set<Sign> >& t);
	virtual std::map<std::string, std::set<Sign> > freturn(
			std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&, CPPParser::Statement*);

	// initial value for extremal labels
	virtual std::map<std::string, std::set<Sign> > getExtremalValue();

	virtual std::set<int> getExtremalLabels();
	virtual std::set<int> getNext(int);
protected:
	virtual std::set<Sign> getSigns(CPPParser::VariableValue*,
			std::map<std::string, std::set<Sign> >&);
	virtual void addAllCombinations(SignArray, std::set<Sign>&, std::set<Sign>&, std::set<Sign>*);
	std::set<Sign>* getSign(char c);
	std::set<Sign> plusSet;
	std::set<Sign> minusSet;
	std::set<Sign> zeroSet;
	std::set<Sign> allSet;
	std::set<Sign> emptySet;

	SignArray op_plus;
	SignArray op_minus;
	SignArray op_mult;
	SignArray op_div;
};

#endif /* SIGNANALYSIS_H_ */

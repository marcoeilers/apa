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

enum Sign {
	SIGN_PLUS, SIGN_MINUS, SIGN_ZERO
};


class SignAnalysis: virtual public EMFramework<std::map<std::string, std::set<Sign> > > {
public:
	SignAnalysis(InterControlFlow*);
	virtual ~SignAnalysis();

	virtual std::map<std::string, std::set<Sign> > join(
			std::map<std::string, std::set<Sign> >&,
			std::map<std::string, std::set<Sign> >&);
	virtual std::map<std::string, std::set<Sign> > top();
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
			std::map<std::string, std::set<Sign> >& t, CPPParser::Return* r);
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
	virtual void addAllCombinations(const std::set<Sign>*[3][3], std::set<Sign>&, std::set<Sign>&, std::set<Sign>*);
	std::set<Sign> plusSet;
	std::set<Sign> minusSet;
	std::set<Sign> zeroSet;
	std::set<Sign> allSet;
	std::set<Sign> emptySet;

	const std::set<Sign>* op_plus[3][3];
	const std::set<Sign>* op_minus[3][3];
	const std::set<Sign>* op_mult[3][3];
	const std::set<Sign>* op_div[3][3];
};

#endif /* SIGNANALYSIS_H_ */

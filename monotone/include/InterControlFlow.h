/*
 * InterControlFlow.h
 *
 *  Created on: May 26, 2013
 *      Author: marco
 */

#ifndef INTERCONTROLFLOW_H_
#define INTERCONTROLFLOW_H_

#include "ParserSemantics.h"
#include "ControlFlow.h"
#include <stdio.h>

// struct to save interFlow information
struct InterFlow
{
	int call;
	int enter;
	int exit;
	int ret;
};

enum LabelType
{
	LABEL_DEFAULT,
	LABEL_CALL,
	LABEL_ENTER,
	LABEL_EXIT,
	LABEL_RETURN
};

/*
 * Stores all the information about a program that is relevant for an
 * interprocedural analysis.
 */
class InterControlFlow : public virtual ControlFlow {
public:
	// Constructor takes the program as argument
	InterControlFlow(CPPParser::Program*);
	virtual ~InterControlFlow();

	// get information about interprocedural transitions
	// TODO see which ones are unused
	int getEntry(int);
	int getReturn(int);
	int getReturnForCall(int);
	int getCallForReturn(int);

	// for a given label, returns what type it has
	// (i.e. either call, enter, exit, return or default, meaning any other
	//  kind of instruction)
	LabelType getType(int);
	CPPParser::Program* getProg() { return prog; }

	// gets the set of labels that can follow a given label,
	// including interprocedural jumps
	std::set<int> getNext(int);
protected:
	virtual int addStatement(CPPParser::Statement*, int, std::set<int>*);
	int addFunction(std::string, int, std::set<int>*);
private:
	CPPParser::Program* prog;
	std::set<InterFlow*> inter;
	std::set<int> returnLabels;
	std::map<std::string, std::pair<int, int> > functions;
};

#endif /* INTERCONTROLFLOW_H_ */

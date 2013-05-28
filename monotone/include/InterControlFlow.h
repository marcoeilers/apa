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

class InterControlFlow : public virtual ControlFlow {
public:
	InterControlFlow(CPPParser::Program*);
	virtual ~InterControlFlow();
	int getEntry(int);
	int getReturn(int);
	int getReturnForCall(int);
	int getCallForReturn(int);
	LabelType getType(int);
	CPPParser::Program* getProg() { return prog; }
	std::set<int> getNext(int);
protected:
	virtual int addStatement(CPPParser::Statement*, int, std::set<int>*);
	int addFunction(std::string, int, std::set<int>*);
private:
	CPPParser::Program* prog;
	std::set<InterFlow*> inter;
	std::set<int> returnLabels;
};

#endif /* INTERCONTROLFLOW_H_ */

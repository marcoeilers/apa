/*
 * InterControlFlow.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
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
 * interprocedural analysis. I.e. in addition to everything ControlFlow does,
 * saves inter flow information.
 * In contrast to ControlFlow, operates on an entire program.
 */
class InterControlFlow : public virtual ControlFlow {
public:
	// Constructor takes the program as argument
	InterControlFlow(CPPParser::Program*);
	virtual ~InterControlFlow();

	// get information about interprocedural transitions
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
	// adds a statement with the given label to the list of statements
	// if it is a return statement, adds it to the set of ints as well
	virtual int addStatement(CPPParser::Statement*, int, std::set<int>*);

	// adds all statements in the function with the given name
	// return labels will be saved to the set of ints as well
	int addFunction(std::string, int, std::set<int>*);
	CPPParser::Program* prog;

	// interprocedural flow information
	std::set<InterFlow*> inter;

	// maps all function names to a pair containing their
	// entry and exit labels
	std::map<std::string, std::pair<int, int> > functions;

	// maps function names for functions which have not been completely
	// added yet to their entry labels
	std::map<std::string, int> incompleteFuncs;

	// maps all function names which have not been completely added
	// (i.e. still have to be completed) to their call and return labels
	std::map<std::string, std::set<std::pair<int, int> > > toComplete;
};

#endif /* INTERCONTROLFLOW_H_ */

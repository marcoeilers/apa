/*
 * ControlFlow.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef CONTROLFLOW_H
#define CONTROLFLOW_H

#include <stdio.h>
#include <vector>
#include <map>
#include <set>
#include "ParserSemantics.h"

class ControlFlowError {
private:
	std::string message;
public:
	ControlFlowError() {message = "Unknown error!";}
	ControlFlowError(std::string m) : message(m) {}
	std::string getMessage() { return message;}
};

// This class stores all the information about a single function
// that is relevant for an intraprocedural analysis, i.e. the instruction that
// can be found at a given label and the set of labels that can follow a given label,
// as well as the set of extremal (start/end) labels.

class ControlFlow {
public:
	ControlFlow();

	// takes the function that should be analyzed
	ControlFlow(CPPParser::FunctionDeclaration&);
	virtual ~ControlFlow();

	// vector of all statements at the index of their label
	std::vector<CPPParser::Statement*> getLabels();

	// the set of labels following a label
	std::set<int> getNext(int l);

	// the set of labels preceding a label
	std::set<int> getNextR(int l);

	// the first label of the function
	std::set<int> getFirstLabels();

	// the last labels of the program
	std::set<int> getLastLabels();
protected:
	// adds a statement with the given label to the list of statements
	virtual int addStatement(CPPParser::Statement*, int);
	// adds a statement to 'labels' at the given position
	// and prints that it is doing so
	void addLabel(int, CPPParser::Statement*);
	// adds a transition between two labels to 'transitions'
	// also calls addTransitionR
	void addTransition(int, int);
	// adds a transition to 'transitionsR'
	void addTransitionR(int, int);
	// the function's statements, sorted by their labels
	std::vector<CPPParser::Statement*> labels;
	// transitions in forward direction
	std::map<int, std::set<int> > transitions;
	// transitions in backward direction
	std::map<int, std::set<int> > transitionsR;
	// first label (can only contain one label)
	std::set<int> first;
	// last labels
	std::set<int> last;
	// set used to temporarily collect labels of return
	// statements, later added to 'last'
	std::set<int> rets;
private:
};

#endif // CONTROLFLOW_H

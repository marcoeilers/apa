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
	virtual int addStatement(CPPParser::Statement*, int);
	void addTransition(int, int);
	void addTransitionR(int, int);
	std::vector<CPPParser::Statement*> labels;
	std::map<int, std::set<int> > transitions;
	std::map<int, std::set<int> > transitionsR;
	std::set<int> first;
	std::set<int> last;
	std::set<int> rets;
private:
};

#endif // CONTROLFLOW_H

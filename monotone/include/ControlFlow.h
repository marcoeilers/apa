#ifndef CONTROLFLOW_H
#define CONTROLFLOW_H

#include <stdio.h>
#include <vector>
#include <map>
#include <set>
#include "ParserSemantics.h"

// This class stores all the information about a program
// that is relevant for an analysis, i.e. the instruction that
// can be found at a given laben and the set of labels that can follow a given label,
// as well as the set of extremal (start/end) labels.

class ControlFlow {
public:
	ControlFlow(CPPParser::FunctionDeclaration&);
	virtual ~ControlFlow();
	std::vector<CPPParser::Statement*> getLabels();
	std::set<int> getNext(int l);
	std::set<int> getNextR(int l);
	std::set<int> getFirstLabels();
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
private:
};

#endif // CONTROLFLOW_H

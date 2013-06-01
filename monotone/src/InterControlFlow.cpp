/*
 * InterControlFlow.cpp
 *
 *  Created on: May 26, 2013
 *      Author: marco
 */

#include "InterControlFlow.h"

using namespace std;

InterControlFlow::InterControlFlow(CPPParser::Program* p) {
	prog = p;
	last.insert(-1);
	set<int>* rets = new set<int>();
	addFunction("main", 0, rets);
	last.insert(rets->begin(), rets->end());
	first.insert(0);
}

InterControlFlow::~InterControlFlow() {
	// TODO Auto-generated destructor stub
}

// adds all statements and transitions in function name,
// puts labels of return statements in rets
int InterControlFlow::addFunction(string name, int label, set<int>* rets) {
	vector<CPPParser::FunctionDeclaration>::iterator it;
	bool found = false;
	for (it = (prog->functionDeclarations.begin());
			it != (prog->functionDeclarations.end()); it++) {
		if (it->name.compare(name) == 0) {
			return addStatement(it->codeBlock, label, rets);
		}
	}
	if (!found)
		printf("No function with name %s declared.", name.c_str());
	return -1;
}

// gets the entry from the call
int InterControlFlow::getEntry(int label) {
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->call == label)
			return (*it)->enter;
	}
	return -1;
}

// gets the return from the exit
int InterControlFlow::getReturn(int label) {
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->exit == label)
			return (*it)->ret;
	}
	return -1;
}

// gets return label from the call label
int InterControlFlow::getReturnForCall(int label) {
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->call == label)
			return (*it)->ret;
	}
	return -1;
}

// gets call label from the return label
int InterControlFlow::getCallForReturn(int label) {
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->ret == label)
			return (*it)->call;
	}
	return -1;
}

// adds a statement and its transitions (intra and interprocedural) to the
// control flow
int InterControlFlow::addStatement(CPPParser::Statement* s, int label,
		set<int>* rets) {

	switch (s->getType()) {
	case CPPParser::TYPE_FUNCTIONCALL: {
		int startLabel = label;
		labels.insert(labels.begin() + label, s);

		// add all transitions to this statement
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, label);
		}

		last.clear();

		label++;

		// add empty entry label (symbolizing a skip at the start of
		// a function)
		labels.insert(labels.begin() + label, NULL);

		// create InterFlow object to store control flow data
		InterFlow* i = new InterFlow();
		i->call = startLabel; // the call label
		i->enter = label;     // the skip (NULL) at the start of the function

		// the last label (i.e. the one from which all next transitions come)
		// is the skip
		last.insert(label);
		label++;

		// TODO add only once!!!!
		// add called Function
		set<int>* rets = new set<int>();
		CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
		label = addFunction(fc->name, label, rets);

		// add empty exit label /skip)
		labels.insert(labels.begin() + label, NULL);
		i->exit = label;

		//  add transitions from return statements to exit
		set<int>::iterator retIt;
		for (retIt = rets->begin(); retIt != rets->end(); retIt++) {
			addTransition(*retIt, label);
		}

		label++;
		labels.insert(labels.begin() + label, s);
		i->ret = label;

		// store interflow
		inter.insert(i);

		last.clear();
		last.insert(label);
		return ++label;

	}
	case CPPParser::TYPE_RETURN: {

		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, startLabel);
		}

		labels.insert(labels.begin() + label, s);

		// last stays empty
		last.clear();
		// add this to the returns, though
		rets->insert(label);

		return ++label;
	}
	case CPPParser::TYPE_WHILE: {
		CPPParser::While* w = (CPPParser::While*) s;

		// remember current label
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets->count(*it))
				addTransition(*it, startLabel);
		}

		last.clear();

		last.insert(startLabel);

		labels.insert(labels.begin() + label, w);

		// add while block
		label = addStatement(w->statement, ++label, rets);

		// for each one of them, add transitions to while stmt
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets->count(*it))
				addTransition(*it, startLabel);
		}

		// all following transitions start from the while stmt
		last.clear();
		last.insert(startLabel);
		return label;
	}
	case CPPParser::TYPE_IF: {
		CPPParser::If* i = (CPPParser::If*) s;
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets->count(*it))
				addTransition(*it, startLabel);
		}

		last.clear();
		last.insert(startLabel);

		labels.insert(labels.begin() + label, s);

		// add if block
		label = addStatement(i->statement, ++label, rets);

		// add if stmt to last
		last.insert(startLabel);
		return label;
	}
	case CPPParser::TYPE_CODEBLOCK: {
		CPPParser::CodeBlock* cb = (CPPParser::CodeBlock*) s;

		// add all stmts in the block
		vector<CPPParser::Statement*>::iterator it;
		for (it = cb->statements.begin(); it != cb->statements.end(); it++) {
			label = addStatement(*it, label, rets);
		}
		return label;
	}
	default: {
		// add this statement
		labels.insert(labels.begin() + label, s);

		// add transitions to this statement
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets->count(*it))
				addTransition(*it, label);
		}

		// this is now the last statement
		last.clear();
		last.insert(label);

		// increment label
		return ++label;
	}
	}

}

LabelType InterControlFlow::getType(int label) {
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->call == label)
			return LABEL_CALL;
		if ((*it)->enter == label)
			return LABEL_ENTER;
		if ((*it)->exit == label)
			return LABEL_EXIT;
		if ((*it)->ret == label)
			return LABEL_RETURN;
	}
	return LABEL_DEFAULT;
}

set<int> InterControlFlow::getNext(int label) {
	set<int> result;
	if (transitions.count(label)) {
		result.insert(transitions[label].begin(), transitions[label].end());
	}
	set<InterFlow*>::iterator it;
	for (it = inter.begin(); it != inter.end(); it++) {
		if ((*it)->call == label)
			result.insert((*it)->enter);
		if ((*it)->exit == label)
			result.insert((*it)->ret);
	}

	return result;
}

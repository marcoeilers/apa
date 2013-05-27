/*
 * InterControlFlow.cpp
 *
 *  Created on: May 26, 2013
 *      Author: marco
 */

#include "InterControlFlow.h"

using namespace std;

InterControlFlow::InterControlFlow(CPPParser::Program* p) {
	last.insert(-1);
	addFunction("main", 0, new set<int>());
	first.insert(0);
}

InterControlFlow::~InterControlFlow() {
	// TODO Auto-generated destructor stub
}

int InterControlFlow::addFunction(string name, int label) {
	vector<CPPParser::FunctionDeclaration*>::iterator it;
	bool found = false;
	for (it = prog->functionDeclarations.begin();
			it != prog->functionDeclarations.end(); it++) {
		if ((*it)->name.compare(name) == 0) {
			// TODO: add skips
			return addStatement((*it)->codeBlock, label);
		}
	}
	if (!found)
		printf("No main function declared.");
}

int InterControlFlow::addStatement(CPPParser::Statement* s, int label, set<int>* rets) {

	switch (s->getType()) {
	case CPPParser::TYPE_FUNCTIONCALL: {
		int startLabel = label;
		labels.insert(labels.begin() + label, s);

		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, label);
		}

		last.clear();

		label++;

		//add empty entry label
		labels.insert(labels.begin() + label, NULL);
		InterFlow* i = new InterFlow();
		i->call=startLabel;
		i->enter=label;

		CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
		label = addFunction(fc->name, label);

		// add empty exit label
		labels.insert(labels.begin() + label, NULL);
		i->exit = label;
		// TODO: add transitions from return statements to exit

		label++;
		labels.insert(labels.begin() + label, s);
		i->ret = label;
		inter.insert(i);

		last.clear();
		last.insert(label);
		return ++label;

	}
		// TODO: return
	case CPPParser::TYPE_WHILE: {
		CPPParser::While* w = (CPPParser::While*) s;
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, startLabel);
		}

		last.clear();

		last.insert(startLabel);

		labels.insert(labels.begin() + label, w);

		label = addStatement(w->statement, ++label);

		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, startLabel);
		}

		last.clear();
		last.insert(startLabel);
		return label;
	}
	case CPPParser::TYPE_IF: {
		CPPParser::If* i = (CPPParser::If*) s;
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, startLabel);
		}

		last.clear();
		last.insert(startLabel);

		labels.insert(labels.begin() + label, s);
		//addTransition(last, label);

		label = addStatement(i->statement, ++label);

		last.insert(startLabel);
		//addTransition(startLabel, label);
		return label;
	}
	case CPPParser::TYPE_CODEBLOCK: {
		CPPParser::CodeBlock* cb = (CPPParser::CodeBlock*) s;
		vector<CPPParser::Statement*>::iterator it;
		for (it = cb->statements.begin(); it != cb->statements.end(); it++) {
			label = addStatement(*it, label);
		}
		return label;
	}
	default: {
		labels.insert(labels.begin() + label, s);

		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			addTransition(*it, label);
		}

		last.clear();
		last.insert(label);
		//addTransition(last, label);

		return ++label;
	}
	}

}


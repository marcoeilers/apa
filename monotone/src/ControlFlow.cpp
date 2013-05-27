#include "ControlFlow.h"

using namespace std;

ControlFlow::ControlFlow(CPPParser::FunctionDeclaration& f) {
	last.insert(-1);
	addStatement(f.codeBlock, 0);
	last.insert(rets.begin(), rets.end());
	first.insert(0);
}

ControlFlow::ControlFlow() {

}

ControlFlow::~ControlFlow() {

}

int ControlFlow::addStatement(CPPParser::Statement* s, int label) {
	switch (s->getType()) {
	case CPPParser::TYPE_WHILE: {
		CPPParser::While* w = (CPPParser::While*) s;
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, startLabel);
		}

		last.clear();

		last.insert(startLabel);

		labels.insert(labels.begin() + label, w);

		label = addStatement(w->statement, ++label);

		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
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
			if (!rets.count(*it))
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
	case CPPParser::TYPE_RETURN: {
		labels.insert(labels.begin() + label, s);
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, label);
		}

		last.clear();
		last.insert(label);
		rets.insert(label);

		return ++label;
	}
	default: {
		labels.insert(labels.begin() + label, s);

		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, label);
		}

		last.clear();
		last.insert(label);
		//addTransition(last, label);

		return ++label;
	}
	}
}

void ControlFlow::addTransition(int from, int to) {
	addTransitionR(to, from);
	if (from != -1) {
		if (transitions.count(from)) {
			int sizeBf = transitions[from].size();
			set<int>* current = &(transitions[from]);
			current->insert(to);
		} else {
			set<int> toInsert;
			toInsert.insert(to);
			transitions[from] = toInsert;
		}
	}
}

void ControlFlow::addTransitionR(int from, int to) {
	if (to != -1) {
		if (transitionsR.count(from)) {
			int sizeBf = transitionsR[from].size();
			set<int>* current = &(transitionsR[from]);
			current->insert(to);
		} else {
			set<int> toInsert;
			toInsert.insert(to);
			transitionsR[from] = toInsert;
		}
	}
}

vector<CPPParser::Statement*> ControlFlow::getLabels() {
	return labels;
}

set<int> ControlFlow::getFirstLabels() {
	return first;
}

set<int> ControlFlow::getLastLabels() {
	return last;
}

set<int> ControlFlow::getNext(int l) {
	return transitions[l];
}

set<int> ControlFlow::getNextR(int l) {
	return transitionsR[l];
}

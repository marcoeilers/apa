#include "ControlFlow.h"

using namespace std;

ControlFlow::ControlFlow(CPPParser::FunctionDeclaration& f) {
	last.insert(-1);

	// add the function's code
	addStatement(f.codeBlock, 0);

	// add all return statements to the set of last statmeents
	last.insert(rets.begin(), rets.end());

	// first statement is always label 0
	first.insert(0);
}

ControlFlow::ControlFlow() {

}

ControlFlow::~ControlFlow() {

}

// adds a Statement to the control flow, i.e. gives it a label and adds
// all transitions
int ControlFlow::addStatement(CPPParser::Statement* s, int label) {
	switch (s->getType()) {
	case CPPParser::TYPE_WHILE: {
		CPPParser::While* w = (CPPParser::While*) s;

		// remember current label
		int startLabel = label;

		// add all transitions from previous label(s) to this
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, startLabel);
		}

		last.clear();

		last.insert(startLabel);

		labels.insert(labels.begin() + label, w);

		// add the contents of the while block
		label = addStatement(w->statement, ++label);

		// from the last statements of the while block, add transitions
		// back to the while statement
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, startLabel);
		}

		// all subsequent transitions should go from the while statement
		last.clear();
		last.insert(startLabel);
		return label;
	}
	case CPPParser::TYPE_IF: {
		CPPParser::If* i = (CPPParser::If*) s;

		// as before, remember current label, add transitions
		int startLabel = label;
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
				addTransition(*it, startLabel);
		}

		last.clear();
		// set last to the if statement
		last.insert(startLabel);

		labels.insert(labels.begin() + label, s);

		// add if block
		label = addStatement(i->statement, ++label);

		// add if statement to last
		last.insert(startLabel);
		return label;
	}
	case CPPParser::TYPE_CODEBLOCK: {
		CPPParser::CodeBlock* cb = (CPPParser::CodeBlock*) s;

		// add all statements in the block
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

		// last should be empty, no transitions from the return stmt
		last.clear();

		// add to list of return statements
		rets.insert(label);

		return ++label;
	}
	default: {
		// add this stmt to labels
		labels.insert(labels.begin() + label, s);

		// add transitions from last to this
		set<int>::iterator it;
		for (it = last.begin(); it != last.end(); it++) {
			if (!rets.count(*it))
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

// adds a transition to flow (and the reverse to flowR)
void ControlFlow::addTransition(int from, int to) {
	addTransitionR(to, from);
	if (from != -1) {
		if (transitions.count(from)) {
			set<int>* current = &(transitions[from]);
			current->insert(to);
		} else {
			set<int> toInsert;
			toInsert.insert(to);
			transitions[from] = toInsert;
		}
	}
}

// adds the reverse of a transition to flowR
void ControlFlow::addTransitionR(int from, int to) {
	if (to != -1) {
		if (transitionsR.count(from)) {
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

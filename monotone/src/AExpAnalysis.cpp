#include "AExpAnalysis.h"

using namespace std;
AExpAnalysis::AExpAnalysis(ControlFlow* cf) {
	// initialize

	cflow = cf;

	vector<CPPParser::Statement*>::iterator it;
	vector<CPPParser::Statement*> lbls = cf->getLabels();
	for (it = lbls.begin(); it != lbls.end(); it++) {
		switch ((*it)->getType()) {
		case CPPParser::TYPE_VAR_ASSIGNMENT: {
			CPPParser::VariableAssignment* a =
					(CPPParser::VariableAssignment*) (*it);
			addToExpressions(a->value);
			break;
		}
		case CPPParser::TYPE_IF: {
			CPPParser::If* i = (CPPParser::If*) (*it);
			if (i->condition->getType() == CPPParser::CONDITION_RELATIONAL) {
				CPPParser::RelationalCondition* rc =
						(CPPParser::RelationalCondition*) i->condition;
				addToExpressions(rc->variable1);
				addToExpressions(rc->variable2);
			}
			break;
		}
		case CPPParser::TYPE_WHILE: {
			CPPParser::While* w = (CPPParser::While*) (*it);
			if (w->condition->getType() == CPPParser::CONDITION_RELATIONAL) {
				CPPParser::RelationalCondition* rc =
						(CPPParser::RelationalCondition*) w->condition;
				addToExpressions(rc->variable1);
				addToExpressions(rc->variable2);
			}
			break;
		}
		default:
			break;
		}
	}
}

AExpAnalysis::~AExpAnalysis() {
	//dtor
}

void AExpAnalysis::addToExpressions(CPPParser::VariableValue* v) {
	switch (v->getType()) {
	case CPPParser::VALUE_COMBINATION: {
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		aexp.insert(v);
		addToExpressions(c->value1);
		addToExpressions(c->value2);
		return;
	}
	default:
		return;
	}
}

set<CPPParser::VariableValue*> AExpAnalysis::bottom() {
	// set of all subexpressions
	return aexp;
}

bool AExpAnalysis::lessThan(set<CPPParser::VariableValue*>& first,
		set<CPPParser::VariableValue*>& second) {
	set<CPPParser::VariableValue*>::iterator it;

	for (it = second.begin(); it != second.end(); it++) {
		bool tempResult = false;
		set<CPPParser::VariableValue*>::iterator firstIt;
		for (firstIt = first.begin(); firstIt != first.end(); firstIt++) {
			tempResult = tempResult || ((*it)->equals(*firstIt));
		}
		if (!tempResult)
			return false;
	}

	return true;
}

set<CPPParser::VariableValue*> AExpAnalysis::f(
		set<CPPParser::VariableValue*>& current, CPPParser::Statement* s) {
	set<CPPParser::VariableValue*> genSet = gen(s);

	set<CPPParser::VariableValue*> un = varUnion(current, genSet);

	set<CPPParser::VariableValue*> result;
	set<CPPParser::VariableValue*>::iterator it;
	if (s->getType() == CPPParser::TYPE_VAR_ASSIGNMENT) {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		if (va->derefDepth == 0) {
			for (it = un.begin(); it != un.end(); it++) {
				if (!contains(*it, va->name)) {
					result.insert(*it);
				}
			}
		}
	} else if (s->getType() == CPPParser::TYPE_VAR_DECLARATION) {
		CPPParser::VariableDeclaration* va = (CPPParser::VariableDeclaration*) s;
		for (it = un.begin(); it != un.end(); it++) {
			if (!contains(*it, va->name))
				result.insert(*it);
		}
	} else {
		result.insert(un.begin(), un.end());
	}

	return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::varUnion(
		set<CPPParser::VariableValue*>& first,
		set<CPPParser::VariableValue*>& second) {
	set<CPPParser::VariableValue*> result;
	result.insert(first.begin(), first.end());

	set<CPPParser::VariableValue*>::iterator it;
	for (it = second.begin(); it != second.end(); it++) {
		bool found = false;
		set<CPPParser::VariableValue*>::iterator secIt;
		for (secIt = result.begin(); secIt != result.end(); secIt++) {
			if ((*it)->equals(*secIt))
				found = true;
		}
		if (!found) {
			result.insert(*it);
		}
	}

	return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::gen(CPPParser::Statement* s) {
	set<CPPParser::VariableValue*> result;
	switch (s->getType()) {
	case CPPParser::TYPE_VAR_ASSIGNMENT: {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		addSubExpressions(&result, va->value);

		return result;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		CPPParser::VariableDeclaration* va = (CPPParser::VariableDeclaration*) s;
		addSubExpressions(&result, va->value);
		return result;
	}
	case CPPParser::TYPE_IF: {
		CPPParser::If* i = (CPPParser::If*) s;
		if (i->condition->getType() == CPPParser::CONDITION_RELATIONAL) {
			CPPParser::RelationalCondition* rc =
					(CPPParser::RelationalCondition*) i->condition;
			addSubExpressions(&result, rc->variable1);
			addSubExpressions(&result, rc->variable2);
		}
		return result;
	}
	case CPPParser::TYPE_WHILE: {
		CPPParser::While* w = (CPPParser::While*) s;
		if (w->condition->getType() == CPPParser::CONDITION_RELATIONAL) {
			CPPParser::RelationalCondition* rc =
					(CPPParser::RelationalCondition*) w->condition;
			addToExpressions(rc->variable1);
			addToExpressions(rc->variable2);
		}
		return result;
	}
	default:
		return result;

	}
}

void AExpAnalysis::addSubExpressions(set<CPPParser::VariableValue*>* s,
		CPPParser::VariableValue* v) {
	switch (v->getType()) {
	case CPPParser::VALUE_ALLOCATION:
		return;
	case CPPParser::VALUE_COMBINATION: {
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		s->insert(v);
		addSubExpressions(s, c->value1);
		addSubExpressions(s, c->value2);
		return;
	}
	case CPPParser::VALUE_VARIABLE:
		return;
	case CPPParser::VALUE_UNKNOWN:
		return;
	}
}

set<CPPParser::VariableValue*> AExpAnalysis::join(
		set<CPPParser::VariableValue*>& first,
		set<CPPParser::VariableValue*>& second) {

	// compute the intersection
	set<CPPParser::VariableValue*> result;
	set<CPPParser::VariableValue*>::iterator it;
	for (it = first.begin(); it != first.end(); it++) {

		set<CPPParser::VariableValue*>::iterator secIt;
		for (secIt = second.begin(); secIt != second.end(); secIt++) {

			if ((*it)->equals(*secIt))
				result.insert(*it);
		}
	}

	return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::getExtremalValue() {
	// empty set
	set<CPPParser::VariableValue*> result;
	return result;
}

bool AExpAnalysis::contains(CPPParser::VariableValue* v, string name) {
	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		return (0 == name.compare(var->value));
	}
	case CPPParser::VALUE_COMBINATION: {
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		return (contains(c->value1, name) || contains(c->value2, name));
	}
	default:
		return true;
	}
}

set<int> AExpAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

set<int> AExpAnalysis::getNext(int l) {
	return cflow->getNext(l);
}


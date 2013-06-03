#include "AExpAnalysis.h"

using namespace std;
AExpAnalysis::AExpAnalysis(ControlFlow* cf) {

	cflow = cf;

	// collect all (sub-)expressions that occur in the program
	// and save them to aexp
	int i = -1;
	vector<CPPParser::Statement*>::iterator it;
	vector<CPPParser::Statement*> lbls = cf->getLabels();
	for (it = lbls.begin(); it != lbls.end(); it++) {
		i++;
		switch ((*it)->getType()) {

		// for var declarations and assignments, just save the rhs
		case CPPParser::TYPE_VAR_DECLARATION: {
			CPPParser::VariableDeclaration* d =
					(CPPParser::VariableDeclaration*) *it;
			addToExpressions(d->value);
			break;
		}
		case CPPParser::TYPE_VAR_ASSIGNMENT: {
			CPPParser::VariableAssignment* a =
					(CPPParser::VariableAssignment*) (*it);
			addToExpressions(a->value);
			break;
		}

			// for relational conditions in if and while, save
			// the expressions in the condition
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
			// nothing to save for other kinds of statements
		default:
			break;
		}
	}
}

AExpAnalysis::~AExpAnalysis() {
	//dtor
}

// Adds expressions (= Combinations) to aexp
void AExpAnalysis::addToExpressions(CPPParser::VariableValue* v) {
	switch (v->getType()) {
	case CPPParser::VALUE_COMBINATION: {
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		if (!setContains(aexp, v))
			aexp.insert(v);
		addToExpressions(c->value1);
		addToExpressions(c->value2);
		return;
	}
		// everything that is not a Combination is not added
	default:
		return;
	}
}

set<CPPParser::VariableValue*> AExpAnalysis::bottom() {
	// set of all subexpressions
	return aexp;
}

// return (first is bigger or equal to second)
bool AExpAnalysis::lessThan(set<CPPParser::VariableValue*>& first,
		set<CPPParser::VariableValue*>& second) {
	set<CPPParser::VariableValue*>::iterator it;

	// for each exp in second
	for (it = second.begin(); it != second.end(); it++) {
		bool tempResult = false;

		// make sure it is also in first
		set<CPPParser::VariableValue*>::iterator firstIt;
		for (firstIt = first.begin(); firstIt != first.end(); firstIt++) {
			tempResult = tempResult || ((*it)->equals(*firstIt));
		}
		// otherwise return false
		if (!tempResult)
			return false;
	}

	return true;
}

// Transition function for the given statement s
set<CPPParser::VariableValue*> AExpAnalysis::f(
		set<CPPParser::VariableValue*>& current, int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	// get expressions generated by the statement
	set<CPPParser::VariableValue*> genSet = gen(s);

	// take union with old expressions
	set<CPPParser::VariableValue*> un = varUnion(current, genSet);

	set<CPPParser::VariableValue*> result;
	set<CPPParser::VariableValue*>::iterator it;
	if (s->getType() == CPPParser::TYPE_VAR_ASSIGNMENT) {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		// if were assigning to a pointer, do not assign anything to the result
		// since we do not know that the pointer refers to, so the assignment
		// could kill all expressions
		if (va->derefDepth == 0) {
			// otherwise add all that are not killed by the assignment
			for (it = un.begin(); it != un.end(); it++) {
				if (!contains(*it, va->name)) {
					result.insert(*it);
				}
			}
		}
	} else {
		// for all other statements simply add the gen set
		result.insert(un.begin(), un.end());
	}

	return result;
}

// calculates the union of two sets of expressions
set<CPPParser::VariableValue*> AExpAnalysis::varUnion(
		set<CPPParser::VariableValue*>& first,
		set<CPPParser::VariableValue*>& second) {
	set<CPPParser::VariableValue*> result;
	result.insert(first.begin(), first.end());

	set<CPPParser::VariableValue*>::iterator it;
	for (it = second.begin(); it != second.end(); it++) {
		if (!setContains(result, *it))
			result.insert(*it);
	}

	return result;
}

// calculates the genSet for a statement
set<CPPParser::VariableValue*> AExpAnalysis::gen(CPPParser::Statement* s) {
	set<CPPParser::VariableValue*> result;
	switch (s->getType()) {
	// for assignments and declarations, this simply the set of all
	// subexpressions on the rhs
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
	// for while and if, the expressions used in the condition
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

// adds all (sub-)expressions in v to *s
void AExpAnalysis::addSubExpressions(set<CPPParser::VariableValue*>* s,
		CPPParser::VariableValue* v) {
	switch (v->getType()) {
	case CPPParser::VALUE_ALLOCATION:
		// no expression an allocation
		return;
	case CPPParser::VALUE_COMBINATION: {
		// add this combination plus subexpressions
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		if (!setContains(*s, v))
			s->insert(v);
		addSubExpressions(s, c->value1);
		addSubExpressions(s, c->value2);
		return;
	}
	case CPPParser::VALUE_VARIABLE:
		// variables do not count as expressions
		return;
	case CPPParser::VALUE_UNKNOWN:
		return;
	}
}

// takes the intersection of first and second
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

// returns true iff v contains a reference to the variable name
bool AExpAnalysis::contains(CPPParser::VariableValue* v, string name) {
	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		return (0 == name.compare(var->value));
	}
	case CPPParser::VALUE_COMBINATION: {
		// check subexpressions
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		return (contains(c->value1, name) || contains(c->value2, name));
	}
	default:
		return true;
	}
}

// since this is a forward analysis, return first label
set<int> AExpAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

// forward analysis, therefore return next labels in flow
set<int> AExpAnalysis::getNext(int l) {
	return cflow->getNext(l);
}

string AExpAnalysis::toString(set<CPPParser::VariableValue*>& s){
	stringstream ss;
	set<CPPParser::VariableValue*>::iterator it;
	for (it = s.begin(); it != s.end(); it++){
		if (it != s.begin())
			ss << ", ";
		ss << (*it)->toString();
	}
	ss << "\n";
	return ss.str();
}

bool AExpAnalysis::setContains(set<CPPParser::VariableValue*>& s, CPPParser::VariableValue* v){
	set<CPPParser::VariableValue*>::iterator it;
	for (it = s.begin(); it != s.end(); it++){
		if (v->equals(*it))
			return true;
	}
	return false;
}


/*
 * SignAnalysis.cpp
 *
 *  Created on: May 28, 2013
 *      Author: marco
 */

#include "SignAnalysis.h"

using namespace std;

SignAnalysis::SignAnalysis(InterControlFlow* cf) :
		op_plus( { { &plusSet, &allSet, &plusSet }, { &allSet, &minusSet, &minusSet },
				{ &plusSet, &minusSet, &zeroSet } }), op_minus( { { &allSet,
				&plusSet, &plusSet }, { &minusSet, &allSet, &minusSet }, { &minusSet,
				&plusSet, &zeroSet } }), op_mult( {
				{ &plusSet, &minusSet, &zeroSet }, { &minusSet, &plusSet, &zeroSet },
				{ &zeroSet, &zeroSet, &zeroSet } }), op_div( { { &plusSet, &minusSet,
				&emptySet }, { &minusSet, &plusSet, &emptySet }, { &zeroSet, &zeroSet,
				&emptySet } }) {
	cflow = cf;

	plusSet.insert(SIGN_PLUS);
	minusSet.insert(SIGN_MINUS);
	zeroSet.insert(SIGN_ZERO);
	allSet.insert(SIGN_PLUS);
	allSet.insert(SIGN_MINUS);
	allSet.insert(SIGN_ZERO);

}

SignAnalysis::~SignAnalysis() {
	// TODO Auto-generated destructor stub
}

map<string, set<Sign> > SignAnalysis::top() {
	map<string, set<Sign> > result;
	return result;
}

map<string, set<Sign> > SignAnalysis::bottom() {
	map<string, set<Sign> > result;
	return result;
}

bool SignAnalysis::lessThan(map<string, set<Sign> >& first,
		map<string, set<Sign> >& second) {
	map<string, set<Sign> >::iterator it;
	for (it = first.begin(); it != first.end(); it++) {
		if (second.count(it->first)) {
			set<Sign>::iterator setIt;
			for (setIt = it->second.begin(); setIt != it->second.end();
					setIt++) {
				if (!second[it->first].count(*setIt))
					return false;
			}
		} else {
			return false;
		}
	}
	return true;
}

map<string, set<Sign> > SignAnalysis::join(map<string, set<Sign> >& first,
		map<string, set<Sign> >& second) {
	map<string, set<Sign> > result;
	result.insert(first.begin(), first.end());

	map<string, set<Sign> >::iterator it;
	for (it = second.begin(); it != second.end(); it++) {
		if (result.count(it->first)) {
			result[it->first].insert(it->second.begin(), it->second.end());
		} else {
			result[it->first] = it->second;
		}
	}
	return result;
}

map<string, set<Sign> > SignAnalysis::f(map<string, set<Sign> >& old,
		CPPParser::Statement* s) {
	map<string, set<Sign> > result;
	result.insert(old.begin(), old.end());

	switch (s->getType()) {
	case CPPParser::TYPE_VAR_ASSIGNMENT: {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		if (old.count(va->name)) {
			set<Sign> newOnes = getSigns(va->value, old);
			result[va->name].insert(newOnes.begin(), newOnes.end());
		}
		break;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		CPPParser::VariableDeclaration* vd = (CPPParser::VariableDeclaration*) s;

		if (vd->dataType->name.compare("int") == 0
				&& vd->dataType->pointerDepth == 0) {
			set<Sign> newOnes = getSigns(vd->value, old);

			result[vd->name] = newOnes;
		}
		break;
	}
	default: {
		break;
	}
	}

	return result;
}

set<Sign> SignAnalysis::getSigns(CPPParser::VariableValue* v,
		map<string, set<Sign> >& mappings) {
	set<Sign> result;

	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		if (mappings.count(var->value)) {
			result.insert(mappings[var->value].begin(),
					mappings[var->value].end());
		} else {
			int intVal;
			stringstream ss;
			ss << var->value;
			if (!(ss >> intVal).fail()) {
				if (intVal > 0)
					result.insert(SIGN_PLUS);
				else if (intVal < 0)
					result.insert(SIGN_MINUS);
				else
					result.insert(SIGN_ZERO);
			}
		}
		break;
	}
	case CPPParser::VALUE_COMBINATION: {
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		set<Sign> first = getSigns(c->value1, mappings);
		set<Sign> second = getSigns(c->value2, mappings);

		if (c->combinator.compare("+") == 0) {
			addAllCombinations(op_plus, first, second, &result);
		} else if (c->combinator.compare("-") == 0) {
			addAllCombinations(op_minus, first, second, &result);
		} else if (c->combinator.compare("*") == 0) {
			addAllCombinations(op_mult, first, second, &result);
		} else if (c->combinator.compare("/") == 0) {
			addAllCombinations(op_div, first, second, &result);
		}

		break;
	}
	default:
		break;
	}
	return result;
}

void SignAnalysis::addAllCombinations(const set<Sign>* op[][3], set<Sign>& first,
		set<Sign>& second, set<Sign>* result) {
	set<Sign>::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++) {
		set<Sign>::iterator sIt;
		for (sIt = second.begin(); sIt != second.end(); sIt++) {
			set<Sign> toAdd = (*op)[*fIt][*sIt];
			result->insert(toAdd.begin(), toAdd.end());
		}
	}
}

map<string, set<Sign> > SignAnalysis::fcall(map<string, set<Sign> >& old,
		CPPParser::Statement* s, CPPParser::FunctionDeclaration* fd) {
	map<string, set<Sign> > result;
	return result;
}

map<string, set<Sign> > SignAnalysis::fenter(map<string, set<Sign> >& old) {
	map<string, set<Sign> > result;
	return result;
}

map<string, set<Sign> > SignAnalysis::fexit(map<string, set<Sign> >& old,
		CPPParser::Return* r) {
	map<string, set<Sign> > result;
	return result;
}

map<string, set<Sign> > SignAnalysis::freturn(
		map<string, set<Sign> >& beforeCall,
		map<string, set<Sign> >& afterFunction, CPPParser::Statement* s) {
	map<string, set<Sign> > result;
	return result;
}

map<string, set<Sign> > SignAnalysis::getExtremalValue() {
	map<string, set<Sign> > result;
	return result;
}

set<int> SignAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

set<int> SignAnalysis::getNext(int label) {
	return cflow->getNext(label);
}


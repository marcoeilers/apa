/*
 * SignAnalysis.cpp
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#include "SignAnalysis.h"

using namespace std;

SignAnalysis::SignAnalysis(InterControlFlow* cf) {
	// initialize operator matrices
	std::string op_plus_s = "papammpmz";
	std::string op_minus_s = "appmammpz";
	std::string op_mult_s = "pmzmpzzzz";
	std::string op_div_s = "pmempezze";
	for (int i = 0; i < 9; i++) {
		int x = i % 3;
		int y = i / 3;
		op_plus[y][x] = getSign(op_plus_s[i]);
		op_minus[y][x] = getSign(op_minus_s[i]);
		op_mult[y][x] = getSign(op_mult_s[i]);
		op_div[y][x] = getSign(op_div_s[i]);
	}


	cflow = cf;

	// now initialize the sets in those matrices
	plusSet.insert(SIGN_PLUS);
	minusSet.insert(SIGN_MINUS);
	zeroSet.insert(SIGN_ZERO);
	allSet.insert(SIGN_PLUS);
	allSet.insert(SIGN_MINUS);
	allSet.insert(SIGN_ZERO);
}

SignAnalysis::~SignAnalysis() {
}

std::set<Sign>* SignAnalysis::getSign(char c) {
	switch (c) {
	case 'p':
		return &plusSet;
	case 'm':
		return &minusSet;
	case 'e':
		return &emptySet;
	case 'z':
		return &zeroSet;
	case 'a':
		return &allSet;
	}
	throw EMFError("This should never happen");
}

map<string, set<Sign> > SignAnalysis::bottom() {
	map<string, set<Sign> > result;
	// empty map
	return result;
}

// returns true if the second contains all variables the first contains
// and for each of those variables, the set of signs contains all the first
// contains
bool SignAnalysis::lessOrEqual(map<string, set<Sign> >& first,
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

// returns a set that has all the variables from both first and second,
// and for each var has the union of the signs for that var in first and second
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
		int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);
	map<string, set<Sign> > result;

	// add all old vars and signs
	result.insert(old.begin(), old.end());

	switch (s->getType()) {
	case CPPParser::TYPE_VAR_ASSIGNMENT: {
		// for an assignment
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		// get the signs of the rhs
		set<Sign> newOnes = getSigns(va->value, old);
		// if the assignment is to a pointer
		if (va->derefDepth != 0) {
			// since it could point anywhere, we must add the new signs
			// to ALL vars
			map<string, set<Sign> >::iterator it;
			for (it = old.begin(); it != old.end(); it++) {
				result[it->first].insert(newOnes.begin(), newOnes.end());
			}
			// since the pointer could also point to something outside the
			// current function, we must remember this
			// store it as '0all' (which is not a valid variable name,
			// therefore cannot interfere with any other variable, and which
			// is not printed)
			result["0all"].insert(newOnes.begin(), newOnes.end());
		} else {
			// otherwise these are the new signs of the lhs variable
			if (old.count(va->name)) {
				result[va->name] = newOnes;
			}
		}
		break;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		// for a declaration
		CPPParser::VariableDeclaration* vd = (CPPParser::VariableDeclaration*) s;

		// if it is a numeral, and not a pointer
		if ((vd->dataType->name.compare("int") == 0
				|| vd->dataType->name.compare("long") == 0
				|| vd->dataType->name.compare("float") == 0
				|| vd->dataType->name.compare("double") == 0)
				&& vd->dataType->pointerDepth == 0) {
			// add a new entry for the var, put its current signs in there
			set<Sign> newOnes = getSigns(vd->value, old);

			result[vd->name] = newOnes;
		}
		break;
	}
	case CPPParser::TYPE_RETURN: {
		// for a return statement,
		// store the returned value in the var "return"
		CPPParser::Return* r = (CPPParser::Return*) s;

		set<Sign> newOnes = getSigns(r->variable, old);
		result["return"] = newOnes;
		break;
	}
	default: {
		// for all other statements do not change anything
		break;
	}
	}

	return result;
}

// computes the signs that a given expression can have in the
// current environment
set<Sign> SignAnalysis::getSigns(CPPParser::VariableValue* v,
		map<string, set<Sign> >& mappings) {
	set<Sign> result;

	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		// for a variable that is already in the environment
		if (mappings.count(var->value)) {
			result.insert(mappings[var->value].begin(),
					mappings[var->value].end());
		} else {
			// for a numeral (since those are parsed as Variable objects)
			// just insert its sign
			long intVal;
			double floatVal;
			stringstream ss;
			ss << var->value;
			if (!(ss >> intVal).fail()) {
				if (intVal > 0)
					result.insert(SIGN_PLUS);
				else if (intVal < 0)
					result.insert(SIGN_MINUS);
				else
					result.insert(SIGN_ZERO);
			} else if (!(ss >> floatVal).fail()) {
				if (floatVal > 0.0)
					result.insert(SIGN_PLUS);
				else if (floatVal < 0.0)
					result.insert(SIGN_MINUS);
				else
					result.insert(SIGN_ZERO);
			}
		}
		break;
	}
	case CPPParser::VALUE_COMBINATION: {
		// for a combination, get the signs of the subexpressions
		CPPParser::Combination* c = (CPPParser::Combination*) v;
		set<Sign> first = getSigns(c->value1, mappings);
		set<Sign> second = getSigns(c->value2, mappings);

		// and apply the correct combinator
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

// for two given sets of signs and a combinator,
// returns a set of all combinations which can arise from an arithmetic
// operation
void SignAnalysis::addAllCombinations(SignArray op, set<Sign>& first,
		set<Sign>& second, set<Sign>* result) {
	set<Sign>::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++) {
		set<Sign>::iterator sIt;
		for (sIt = second.begin(); sIt != second.end(); sIt++) {
			set<Sign> toAdd = *(op[*fIt][*sIt]);
			result->insert(toAdd.begin(), toAdd.end());
		}
	}
}

map<string, set<Sign> > SignAnalysis::fcall(map<string, set<Sign> >& old,
		int label, CPPParser::FunctionDeclaration* fd) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	// start out with an empty environment
	map<string, set<Sign> > result;

	// add parameters
	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;

	if (fc->variables.size() != fd->arguments.size())
		throw EMFError("Wrong number of arguments in function call.");
	// for each of them, get the signs of the param from
	// the old environment
	for (int i = 0; i < fc->variables.size(); i++) {
		set<Sign> argSigns = getSigns(fc->variables.at(i), old);
		pair<CPPParser::Variable*, CPPParser::DataType*> var = fd->arguments.at(
				i);
		result[var.first->value] = argSigns;
	}

	return result;
}

// fenter is just the identity
map<string, set<Sign> > SignAnalysis::fenter(map<string, set<Sign> >& old) {
	map<string, set<Sign> > result;
	result.insert(old.begin(), old.end());
	return result;
}

// fexit is just the identity
map<string, set<Sign> > SignAnalysis::fexit(map<string, set<Sign> >& old) {
	map<string, set<Sign> > result;
	result.insert(old.begin(), old.end());
	return result;
}

map<string, set<Sign> > SignAnalysis::freturn(
		map<string, set<Sign> >& beforeCall,
		map<string, set<Sign> >& afterFunction, int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	// add everything from the environment before the call
	map<string, set<Sign> > result;
	result.insert(beforeCall.begin(), beforeCall.end());

	// if a pointer was accessed within the function
	if (afterFunction.count("0all")){

		// add the signs in 0all to all old variables
		map<string, set<Sign> >::iterator it;
		for (it = result.begin(); it != result.end(); it++){
			it->second.insert(afterFunction["0all"].begin(), afterFunction["0all"].end());
		}

		// and propagate, since we could be in a nested function call
		result["0all"] = afterFunction["0all"];
	}

	// add the value of "return" as the new value of the return variable
	// if there is an assignment of the result
	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
	if (fc->returnVariable != NULL) {
		result[fc->returnVariable->value] = afterFunction["return"];
	}
	return result;
}

// extremal value is an empty map
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

string SignAnalysis::toString(map<string, set<Sign> >& m) {
	stringstream ss;
	ss << "\n";

	map<string, set<Sign> >::iterator map2It;
	for (map2It = m.begin(); map2It != m.end(); map2It++) {
		// ignore '0all', since this is not an actual variable
		// and is used to transport information for the return
		if (map2It->first.compare("0all") != 0) {
			ss << "For variable ";
			ss << map2It->first;
			ss << ":\n";

			set<Sign>::iterator setIt;
			for (setIt = map2It->second.begin(); setIt != map2It->second.end();
					setIt++) {
				if (setIt != map2It->second.begin())
					ss << ", ";
				switch (*setIt) {
				case SIGN_PLUS:
					ss << "PLUS";
					break;
				case SIGN_MINUS:
					ss << "MINUS";
					break;
				case SIGN_ZERO:
					ss << "ZERO";
					break;
				}
			}
			ss << "\n";
		}
	}
	return ss.str();
}

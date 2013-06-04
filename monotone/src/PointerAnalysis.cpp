/*
 * PointerAnalysis.cpp
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#include "PointerAnalysis.h"

using namespace std;

PointerAnalysis::PointerAnalysis(InterControlFlow* icf) {
	cflow = icf;

	// make a list of the labels for each function
	addLabels(*(icf->getFirstLabels().begin()), "main");

	// for each function, for each variable name, generate a unique string
	map<string, set<int> >::iterator it;
	for (it = functionLabels.begin(); it != functionLabels.end(); it++) {
		// for each function
		set<int>::iterator setIt;

		// for each label in the function
		for (setIt = it->second.begin(); setIt != it->second.end(); setIt++) {
			CPPParser::Statement* s = cflow->getLabels().at(*setIt);
			// if the statement is not a skip
			if (s != NULL) {
				// if a variable is declared here
				if (s->getType() == CPPParser::TYPE_VAR_DECLARATION) {
					CPPParser::VariableDeclaration* fd =
							(CPPParser::VariableDeclaration*) s;
					// id is '[name] (label [currentLabel])'
					stringstream ss;
					ss << fd->name;
					ss << " (label ";
					ss << *setIt;
					ss << ")";
					varIDs[it->first][fd->name] = ss.str();
				} else if (s->getType() == CPPParser::TYPE_FUNCTIONCALL) {
					// add parameters
					CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;

					// get function
					CPPParser::FunctionDeclaration fd;
					for (int i = 0;
							i < cflow->getProg()->functionDeclarations.size();
							i++) {
						if (cflow->getProg()->functionDeclarations.at(i).name.compare(
								fc->name) == 0) {
							fd = cflow->getProg()->functionDeclarations.at(i);
							break;
						}
					}

					// for all arguments
					for (int i = 0; i < fd.arguments.size(); i++) {
						// id is '[name] (param of [funcName])'
						stringstream ss;
						ss << fd.arguments.at(i).first->value;
						ss << " (param of ";
						ss << fd.name;
						ss << ")";
						varIDs[fd.name][fd.arguments.at(i).first->value] =
								ss.str();
					}

				}
			}
		}
	}
}

PointerAnalysis::~PointerAnalysis() {
	// TODO Auto-generated destructor stub
}

// adds the labels for all functions to 'functionLabels'
void PointerAnalysis::addLabels(int label, string fName) {
	// if entry is not already in there
	if (!functionLabels[fName].count(label)) {
		// insert
		functionLabels[fName].insert(label);

		set<int> next = cflow->getNext(label);
		set<int>::iterator it;

		switch (cflow->getType(label)) {
		case LABEL_CALL: {
			// if current label is a call
			CPPParser::FunctionCall* fc =
					(CPPParser::FunctionCall*) cflow->getLabels().at(label);
			// add next labels to name of called function
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fc->name);
			}

			// and labels following the return to the current function
			int returnLbl = cflow->getReturnForCall(label);
			next = cflow->getNext(returnLbl);
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fName);
			}

			break;
		}
		case LABEL_RETURN: {
			// already dealt with in the call case
			break;
		}
		default: {
			// add all dollowing labels to current function
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fName);
			}
			break;
		}
		}
	}
}

// gets a string uniquely identiying the given variable used at the given label
string PointerAnalysis::getID(int label, string varName) {
	// get name of current function
	string fName;
	map<string, set<int> >::iterator it;
	for (it = functionLabels.begin(); it != functionLabels.end(); it++) {
		if (it->second.count(label)) {
			fName = it->first;
			break;
		}
	}

	// look up var name
	string result = varIDs[fName][varName];
	return result;
}

// result has all keys from both inputs, and for all keys the
// union of the values from both inputs
map<string, set<string> > PointerAnalysis::join(
		map<string, set<string> >& first, map<string, set<string> >& second) {
	map<string, set<string> > result;
	result.insert(first.begin(), first.end());

	map<string, set<string> >::iterator it;
	for (it = second.begin(); it != second.end(); it++) {
		if (result.count(it->first)) {
			result[it->first].insert(it->second.begin(), it->second.end());
		} else {
			result[it->first] = it->second;
		}
	}
	return result;
}

map<string, set<string> > PointerAnalysis::bottom() {
	map<string, set<string> > result;
	// empty map
	return result;
}

// checks if second has all keys that are in first, and if for all those keys,
// it has all the values in first
bool PointerAnalysis::lessOrEqual(map<string, set<string> >& first,
		map<string, set<string> >& second) {

	// for every key in first
	map<string, set<string> >::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++) {
		// check if also in second
		if (second.count(fIt->first)) {
			// check if second has all values first has for this key
			set<string>::iterator sIt;
			for (sIt = fIt->second.begin(); sIt != fIt->second.end(); sIt++) {
				if (!second[fIt->first].count(*sIt)) {
					// otherwise return false
					return false;
				}
			}
			// otherwise return false
		} else {
			return false;
		}
	}
	// return true if all tests passed
	return true;
}

// given the current label and the (normal name, not ID,  of the)
// left hand side of an assignment,
// returns a set of the unique IDs of all variables to which it evaluates
set<string> PointerAnalysis::evaluateLhs(int label, int derefDepth, string name,
		map<string, set<string> >& old) {
	set<string> result;

	string id = getID(label, name);

	// if no pointer
	if (derefDepth == 0) {
		// just insert the id
		result.insert(id);
	} else {
		// otherwise return a set of all the ids the pointer points to
		CPPParser::Variable* v = new CPPParser::Variable();
		v->derefDepth = derefDepth - 1;
		v->value = name;
		result = evaluateRhs(label, v, old);
		delete v;
	}
	return result;
}

// for the current label, the right hand side of an assignment,
// returns a set with the IDs of all variables the right hand side points to
set<string> PointerAnalysis::evaluateRhs(int label, CPPParser::VariableValue* v,
		map<string, set<string> >& old) {
	set<string> result;
	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		// if it's a variable
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		// if it's &q for some q
		if (var->value.substr(0, 1).compare("&") == 0) {
			// add q
			string varName = var->value.substr(1);
			result.insert(getID(label, varName));
		} else if (var->derefDepth > 0) {
			// if it's *e for some e
			CPPParser::Variable* newVar = new CPPParser::Variable();
			newVar->value = var->value;
			newVar->derefDepth = var->derefDepth - 1;
			set<string> temp = evaluateRhs(label, newVar, old);
			// for everything e evaluates to
			set<string>::iterator it;
			for (it = temp.begin(); it != temp.end(); it++) {
				// add old[eval e]
				if (old.count(*it)) {
					result.insert(old[*it].begin(), old[*it].end());
				}
			}
			delete newVar;

		} else {
			// if it's just a variable without & or *
			string id = getID(label, var->value);
			// if the variable has a value in old
			// (otherwise it's a number)
			if (old.count(id)) {
				// add what it points to
				result.insert(old[id].begin(), old[id].end());
			}
		}
		break;
	}
	case CPPParser::VALUE_COMBINATION: {
		// arithmetic is being performed
		// we will not evaluate this
		// add all known pointers and vars to be safe
		// since it could point to anything
		map<string, set<string> >::iterator it;
		for (it = old.begin(); it != old.end(); it++) {
			result.insert(it->first);
		}

		break;
	}
	case CPPParser::VALUE_ALLOCATION: {
		// generate new string
		// 'new [datatype] (label [label])'
		CPPParser::Allocation* a = (CPPParser::Allocation*) v;
		stringstream ss;
		ss << "new ";
		ss << a->type->name;
		ss << " (label ";
		ss << label;
		ss << ")";
		result.insert(ss.str());
		break;
	}
	case CPPParser::VALUE_UNKNOWN: {
		break;
	}
	}
	return result;
}

map<string, set<string> > PointerAnalysis::f(map<string, set<string> >& old,
		int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	map<string, set<string> > result;
	// copy old value to result
	result.insert(old.begin(), old.end());

	switch (s->getType()) {
	case CPPParser::TYPE_VAR_ASSIGNMENT: {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		// evaluate both sides
		set<string> lhs = evaluateLhs(label, va->derefDepth, va->name, old);
		set<string> rhs = evaluateRhs(label, va->value, old);

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			set<string> oldVal = old[*it];
			// assign new value rhs
			result[*it] = rhs;
		}
		break;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		CPPParser::VariableDeclaration* vd = (CPPParser::VariableDeclaration*) s;
		// if a pointer is declared
		if (vd->dataType->pointerDepth > 0) {
			string id = getID(label, vd->name);
			set<string> rhs = evaluateRhs(label, vd->value, old);

			// value for this variable is rhs
			result[id] = rhs;
		}
		break;
	}
	case CPPParser::TYPE_RETURN: {
		CPPParser::Return* r = (CPPParser::Return*) s;
		// evaluate right hand side
		set<string> rhs = evaluateRhs(label, r->variable, old);
		// assign to variable 'return'
		result["return"] = rhs;
	}
	default:
		// for other statements, do nothing
		break;
	}
	return result;
}

map<string, set<string> > PointerAnalysis::fcall(map<string, set<string> >& old,
		int label, CPPParser::FunctionDeclaration* fd) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	map<string, set<string> > result;
	// insert all current values
	// since all variable IDs are unique, the heap is a global object
	// and points-to information only makes sense if one can see all of it
	result.insert(old.begin(), old.end());
	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;

	if (fc->variables.size() != fd->arguments.size())
		throw EMFError("Wrong number of arguments in function call.");

	// get function entry label (there can only be one)
	int funcLabel = *(cflow->getNext(label).begin());

	// for all parameters
	for (int i = 0; i < fc->variables.size(); i++) {
		// if it's a pointer
		if (fd->arguments.at(i).second->pointerDepth > 0) {
			// evaluate lhs, that is the parameter variable that we assign to
			set<string> lhs = evaluateLhs(funcLabel, 0,
					fd->arguments.at(i).first->value, old);
			// evaluate rhs, that is the argument
			set<string> rhs = evaluateRhs(label, fc->variables.at(i), old);

			// for every var in lhs
			set<string>::iterator it;
			for (it = lhs.begin(); it != lhs.end(); it++) {
				set<string> oldVal = old[*it];
				// set value to rhs
				result[*it] = rhs;
			}
		}
	}

	return result;
}

map<string, set<string> > PointerAnalysis::fenter(
		map<string, set<string> >& old) {
	// identity function
	map<string, set<string> > result;
	result.insert(old.begin(), old.end());
	return result;
}

map<string, set<string> > PointerAnalysis::fexit(
		map<string, set<string> >& old) {
	// identity function
	map<string, set<string> > result;
	result.insert(old.begin(), old.end());
	return result;
}

map<string, set<string> > PointerAnalysis::freturn(
		map<string, set<string> >& beforeCall,
		map<string, set<string> >& afterFunc, int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	map<string, set<string> > result;
	// insert all values from before the call
	result.insert(beforeCall.begin(), beforeCall.end());

	// for each variable that already existed before the call
	map<string, set<string> >::iterator mapIt;
	for (mapIt = afterFunc.begin(); mapIt != afterFunc.end(); mapIt++) {
		if (result.count(mapIt->first)) {
			// insert new value
			result[mapIt->first] = mapIt->second;
		}
	}

	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
	// if return value is assigned to a variable
	if (fc->returnVariable != NULL) {
		// evaluate the variable that we assign to
		set<string> lhs = evaluateLhs(label, fc->returnVariable->derefDepth,
				fc->returnVariable->value, result);
		// return value is saved as variable 'return'
		set<string> rhs = afterFunc["return"];

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			// set new value rhs
			result[*it] = rhs;
		}
	}

	return result;
}

map<string, set<string> > PointerAnalysis::getExtremalValue() {
	map<string, set<string> > result;
	// empty map
	return result;
}

set<int> PointerAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

// creates a string describing a map from var ids to the set of
// var ids it points to
string PointerAnalysis::toString(map<string, set<string> >& m) {
	stringstream ss;
	ss << "\n";

	map<string, set<string> >::iterator map2It;
	for (map2It = m.begin(); map2It != m.end(); map2It++) {
		ss << "For variable ";
		ss << map2It->first;
		ss << ":\n";

		set<string>::iterator setIt;
		for (setIt = map2It->second.begin(); setIt != map2It->second.end();
				setIt++) {
			ss << "Points to ";
			ss << *setIt;
			ss << ".\n";
		}
	}
	return ss.str();
}

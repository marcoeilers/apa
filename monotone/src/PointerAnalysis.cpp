/*
 * PointerAnalysis.cpp
 *
 *  Created on: Jun 1, 2013
 *      Author: marco
 */

#include "PointerAnalysis.h"

using namespace std;

PointerAnalysis::PointerAnalysis(InterControlFlow* icf) {
	cflow = icf;

	// make a list of the labels for each function
	addLabels(*(icf->getFirstLabels().begin()), "main");

	// for each function, for variable name, generate a unique string
	map<string, set<int> >::iterator it;
	for (it = functionLabels.begin(); it != functionLabels.end(); it++) {

		set<int>::iterator setIt;
		for (setIt = it->second.begin(); setIt != it->second.end(); setIt++) {
			CPPParser::Statement* s = cflow->getLabels().at(*setIt);
			if (s != NULL) {
				if (s->getType() == CPPParser::TYPE_VAR_DECLARATION) {
					CPPParser::VariableDeclaration* fd =
							(CPPParser::VariableDeclaration*) s;
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

					for (int i = 0; i < fd.arguments.size(); i++) {
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

void PointerAnalysis::addLabels(int label, string fName) {
	if (!functionLabels[fName].count(label)) {
		functionLabels[fName].insert(label);

		set<int> next = cflow->getNext(label);
		set<int>::iterator it;

		switch (cflow->getType(label)) {
		case LABEL_CALL: {
			CPPParser::FunctionCall* fc =
					(CPPParser::FunctionCall*) cflow->getLabels().at(label);
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fc->name);
			}

			int returnLbl = cflow->getReturnForCall(label);
			next = cflow->getNext(returnLbl);
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fName);
			}

			break;
		}
		case LABEL_RETURN: {
			break;
		}
		default: {
			for (it = next.begin(); it != next.end(); it++) {
				addLabels(*it, fName);
			}
			break;
		}
		}
	}
}

string PointerAnalysis::getID(int label, string varName) {
	string fName;
	map<string, set<int> >::iterator it;
	for (it = functionLabels.begin(); it != functionLabels.end(); it++) {
		if (it->second.count(label)) {
			fName = it->first;
			break;
		}
	}

	string result = varIDs[fName][varName];
	return result;
}

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
	return result;
}

bool PointerAnalysis::lessThan(map<string, set<string> >& first,
		map<string, set<string> >& second) {

	map<string, set<string> >::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++) {
		if (second.count(fIt->first)) {
			set<string>::iterator sIt;
			for (sIt = fIt->second.begin(); sIt != fIt->second.end(); sIt++) {
				if (!second[fIt->first].count(*sIt)) {
					return false;
				}
			}
		} else {
			return false;
		}
	}
	return true;
}

set<string> PointerAnalysis::evaluateLhs(int label, int derefDepth, string name,
		map<string, set<string> >& old) {
	set<string> result;

	string id = getID(label, name);

	if (derefDepth == 0) {
		result.insert(id);
	} else {
		CPPParser::Variable* v = new CPPParser::Variable();
		v->derefDepth = derefDepth - 1;
		v->value = name;
		result = evaluateRhs(label, v, old);
		delete v;
	}
	return result;
}

set<string> PointerAnalysis::evaluateRhs(int label, CPPParser::VariableValue* v,
		map<string, set<string> >& old) {
	set<string> result;
	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		if (var->value.substr(0, 1).compare("&") == 0) {
			string varName = var->value.substr(1);
			result.insert(getID(label, varName));
		} else if (var->derefDepth > 0) {
			CPPParser::Variable* newVar = new CPPParser::Variable();
			newVar->value = var->value;
			newVar->derefDepth = var->derefDepth - 1;
			set<string> temp = evaluateRhs(label, newVar, old);
			set<string>::iterator it;
			for (it = temp.begin(); it != temp.end(); it++) {
				string id = getID(label, *it);
				if (old.count(id)) {
					result.insert(old[id].begin(), old[id].end());
				} else {
					//TODO ??
				}
			}
			delete newVar;

		} else {
			string id = getID(label, var->value);
			if (old.count(id)) {
				result.insert(old[id].begin(), old[id].end());
			} else {
				//TODO what???
			}
		}
		break;
	}
	case CPPParser::VALUE_COMBINATION: {
		// arithmetic is being performed
		// we will not evaluate this
		// add all known pointers and vars
		map<string, set<string> >::iterator it;
		for (it = old.begin(); it != old.end(); it++) {
			result.insert(it->first);
		}

		break;
	}
	case CPPParser::VALUE_ALLOCATION: {
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
	result.insert(old.begin(), old.end());

	switch (s->getType()) {
	case CPPParser::TYPE_VAR_ASSIGNMENT: {
		CPPParser::VariableAssignment* va = (CPPParser::VariableAssignment*) s;
		set<string> lhs = evaluateLhs(label, va->derefDepth, va->name, old);
		set<string> rhs = evaluateRhs(label, va->value, old);

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			set<string> oldVal = old[*it];

			result[*it] = rhs;
		}
		break;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		CPPParser::VariableDeclaration* vd = (CPPParser::VariableDeclaration*) s;
		if (vd->dataType->pointerDepth > 0) {
			set<string> lhs = evaluateLhs(label, 0, vd->name, old);
			set<string> rhs = evaluateRhs(label, vd->value, old);

			// for every var in lhs
			set<string>::iterator it;
			for (it = lhs.begin(); it != lhs.end(); it++) {
				set<string> oldVal = old[*it];

				bool subset = true;
				set<string>::iterator rIt;
				for (rIt = rhs.begin(); rIt != rhs.end(); rIt++) {
					if (!oldVal.count(*rIt)) {
						subset = false;
						break;
					}
				}
				result[*it].insert(oldVal.begin(), oldVal.end());
				if (!subset) {
					result[*it].insert(rhs.begin(), rhs.end());
				}
			}
		}
		break;
	}
	case CPPParser::TYPE_RETURN: {
		CPPParser::Return* r = (CPPParser::Return*) s;
		set<string> rhs = evaluateRhs(label, r->variable, old);
		result["return"].insert(rhs.begin(), rhs.end());
	}
	default:
		break;
	}
	return result;
}

map<string, set<string> > PointerAnalysis::fcall(map<string, set<string> >& old,
		int label, CPPParser::FunctionDeclaration* fd) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	map<string, set<string> > result;
	result.insert(old.begin(), old.end());
	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;

	if (fc->variables.size() != fd->arguments.size())
		throw EMFError("Wrong number of arguments in function call.");

	int funcLabel = *(cflow->getNext(label).begin());

	for (int i = 0; i < fc->variables.size(); i++) {
		set<string> lhs = evaluateLhs(funcLabel, 0,
				fd->arguments.at(i).first->value, old);
		set<string> rhs = evaluateRhs(label, fc->variables.at(i), old);

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			set<string> oldVal = old[*it];

			bool subset = true;
			set<string>::iterator rIt;
			for (rIt = rhs.begin(); rIt != rhs.end(); rIt++) {
				if (!oldVal.count(*rIt)) {
					subset = false;
					break;
				}
			}
			result[*it].insert(oldVal.begin(), oldVal.end());
			if (!subset) {
				result[*it].insert(rhs.begin(), rhs.end());
			}
		}
	}

	return result;
}

map<string, set<string> > PointerAnalysis::fenter(
		map<string, set<string> >& old) {
	map<string, set<string> > result;
	result.insert(old.begin(), old.end());
	return result;
}

map<string, set<string> > PointerAnalysis::fexit(
		map<string, set<string> >& old) {
	map<string, set<string> > result;
	result.insert(old.begin(), old.end());
	return result;
}

map<string, set<string> > PointerAnalysis::freturn(
		map<string, set<string> >& beforeCall,
		map<string, set<string> >& afterFunc, int label) {
	CPPParser::Statement* s = cflow->getLabels().at(label);

	map<string, set<string> > result;
	result.insert(beforeCall.begin(), beforeCall.end());

	// insert all new values for existing vars
	map<string, set<string> >::iterator mapIt;
	for (mapIt = afterFunc.begin(); mapIt != afterFunc.end(); mapIt++) {
		if (result.count(mapIt->first)) {
			result[mapIt->first] = mapIt->second;
		}
	}

	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
	if (fc->returnVariable != NULL) {
		set<string> lhs = evaluateLhs(label, fc->returnVariable->derefDepth,
				fc->returnVariable->value, result);
		set<string> rhs = afterFunc["return"];

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			set<string> oldVal = beforeCall[*it];

			result[*it] = rhs;

		}
	}

	map<string, set<string> >::iterator it;
	for (it = afterFunc.begin(); it != afterFunc.end(); it++) {
		if (result.count(it->first)) {

		}
	}

	return result;
}

map<string, set<string> > PointerAnalysis::getExtremalValue() {
	map<string, set<string> > result;
	return result;
}

set<int> PointerAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

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

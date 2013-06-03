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

}

PointerAnalysis::~PointerAnalysis() {
	// TODO Auto-generated destructor stub
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
	printf("lessThan first has size %i, second has size %i.\n", first.size(),
			second.size());

	map<string, set<string> >::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++) {
		if (second.count(fIt->first)) {
			set<string>::iterator sIt;
			for (sIt = fIt->second.begin(); sIt != fIt->second.end(); sIt++) {
				if (!second[fIt->first].count(*sIt)) {
					printf("lessThan is false.\n");
					return false;
				}
			}
		} else {
			printf("lessThan is false.\n");
			return false;
		}
	}
	printf("lessThan is true.\n");
	return true;
}

set<string> PointerAnalysis::evaluateLhs(int derefDepth, string name,
		map<string, set<string> >& old) {
	set<string> result;

	if (derefDepth == 0) {
		result.insert(name);
	} else {
		CPPParser::Variable* v = new CPPParser::Variable();
		v->derefDepth = derefDepth - 1;
		v->value = name;
		result = evaluateRhs(v, old);
		delete v;
	}
	return result;
}

set<string> PointerAnalysis::evaluateRhs(CPPParser::VariableValue* v,
		map<string, set<string> >& old) {
	set<string> result;
	switch (v->getType()) {
	case CPPParser::VALUE_VARIABLE: {
		CPPParser::Variable* var = (CPPParser::Variable*) v;
		if (var->value.substr(0, 1).compare("&") == 0) {
			string varName = var->value.substr(1);
			result.insert(varName);
		} else if (var->derefDepth > 0) {
			CPPParser::Variable* newVar = new CPPParser::Variable();
			newVar->value = var->value;
			newVar->derefDepth = var->derefDepth - 1;
			set<string> temp = evaluateRhs(newVar, old);
			set<string>::iterator it;
			for (it = temp.begin(); it != temp.end(); it++) {
				if (old.count(*it)) {
					result.insert(old[*it].begin(), old[*it].end());
				} else {
					//TODO ??
				}
			}
			delete newVar;

		} else {
			if (old.count(var->value)) {
				result.insert(old[var->value].begin(), old[var->value].end());
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
		set<string> lhs = evaluateLhs(va->derefDepth, va->name, old);
		set<string> rhs = evaluateRhs(va->value, old);

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
		break;
	}
	case CPPParser::TYPE_VAR_DECLARATION: {
		CPPParser::VariableDeclaration* vd = (CPPParser::VariableDeclaration*) s;
		if (vd->dataType->pointerDepth > 0) {
			set<string> lhs = evaluateLhs(0, vd->name, old);
			set<string> rhs = evaluateRhs(vd->value, old);

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
		set<string> rhs = evaluateRhs(r->variable, old);
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
	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;

	if (fc->variables.size() != fd->arguments.size())
		throw EMFError("Wrong number of arguments in function call.");

	for (int i = 0; i < fc->variables.size(); i++) {
		set<string> lhs = evaluateLhs(0, fd->arguments.at(i).first->value, old);
		set<string> rhs = evaluateRhs(fc->variables.at(i), old);

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

	CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
	if (fc->returnVariable != NULL) {
		set<string> lhs = evaluateLhs(fc->returnVariable->derefDepth,
				fc->returnVariable->value, beforeCall);
		set<string> rhs = afterFunc["return"];

		// for every var in lhs
		set<string>::iterator it;
		for (it = lhs.begin(); it != lhs.end(); it++) {
			set<string> oldVal = beforeCall[*it];

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

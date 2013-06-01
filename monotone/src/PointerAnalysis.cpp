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
	for (it = second.begin(); it != second.end(); it++){
		if (result.count(it->first)){
			result[it->first].insert(it->second.begin(), it->second.end());
		}else{
			result[it->first] = it->second;
		}
	}
	return result;
}

map<string, set<string> > PointerAnalysis::bottom() {
	map<string, set<string> > result;
	return result;
}

bool PointerAnalysis::lessThan(
		map<string, set<string> >& first, map<string, set<string> >& second) {
	map<string, set<string> >::iterator fIt;
	for (fIt = first.begin(); fIt != first.end(); fIt++){
		if (second.count(fIt->first)){
			set<string>::iterator sIt;
			for (sIt = fIt->second.begin(); sIt != fIt->second.end(); sIt++){
				if (!second[fIt->first].count(*sIt))
					return false;
			}
		}else{
			return false;
		}
		return true;
	}
}

map<string, set<string> > PointerAnalysis::f(map<string, set<string> >& old,
		CPPParser::Statement* s) {
	map<string, set<string> > result;
	// TODO
	return result;
}

map<string, set<string> > PointerAnalysis::fcall(map<string, set<string> >& old,
		CPPParser::Statement* s, CPPParser::FunctionDeclaration* fd) {
	map<string, set<string> > result;
	// TODO
	return result;
}

map<string, set<string> > PointerAnalysis::fenter(
		map<string, set<string> >& old) {
	map<string, set<string> > result;
	// TODO
	return result;
}

map<string, set<string> > PointerAnalysis::fexit(
		map<string, set<string> >& old) {
	map<string, set<string> > result;
	// TODO
	return result;
}

map<string, set<string> > PointerAnalysis::freturn(
		map<string, set<string> >& beforeCall,
		map<string, set<string> >& afterFunc, CPPParser::Statement* s) {
	map<string, set<string> > result;
	return result;
}

map<string, set<string> > PointerAnalysis::getExtremalValue() {
	map<string, set<string> > result;
	return result;
}

set<int> PointerAnalysis::getExtremalLabels() {
	return cflow->getFirstLabels();
}

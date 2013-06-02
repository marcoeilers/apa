#include <iostream>
#include "AExpAnalysis.h"
#include "MFP.h"
#include "MVP.h"
#include "Parser.h"
#include "StringUtil.h"
#include "ParserSemantics.h"
#include "SignAnalysis.h"
#include "PointerAnalysis.h"

using namespace std;

int main() {

	CPPParser::Parser parser;

	parser.parseFile("RecursionTest.txt");

	CPPParser::TokenList tokens = parser.getTokens();

	CPPParser::TokenList::iterator it;
	for (it = tokens.begin(); it < tokens.end(); it++)
		printf("Token #%lu: '%s'\n", it->id, it->name.c_str());

	CPPParser::Program* program = new CPPParser::Program();
	try {
		program->tryBuild(tokens);
	} catch (CPPParser::ParseError pe) {
		printf("Parser error: %s\n", pe.getMessage().c_str());
	}
	

	InterControlFlow* icf = new InterControlFlow(program);
	
	/*
	PointerAnalysis* p = new PointerAnalysis(icf);

	MVP<map<string, set<string> > >* mvp = new MVP<map<string, set<string> > >(2);

	pair<map<string, map<string, set<string> > >, map<string, map<string, set<string> > > >* result = mvp->solve(p);

	for (int i = 0; i < icf->getLabels().size(); i++) {
			printf("For label %i:\n", i);
			map<string, map<string, set<string> > >::iterator mapIt;
			for (mapIt = result[i].second.begin(); mapIt != result[i].second.end(); mapIt++) {
				printf("For context %s:\n", mapIt->first.c_str());

				map<string, set<string> >::iterator map2It;
				for (map2It = mapIt->second.begin(); map2It != mapIt->second.end();
						map2It++) {
					printf("For variable %s:\n", map2It->first.c_str());

					set<string>::iterator setIt;
					for (setIt = map2It->second.begin(); setIt != map2It->second.end(); setIt++){
						printf("Points to %s.\n", setIt->c_str());
					}
				}
			}
	}
	*/


	SignAnalysis* s = new SignAnalysis(icf);
	
	MVP<map<string, set<Sign> > >* mvp = new MVP<map<string, set<Sign> > >(2);
	
	pair<map<string, map<string, set<Sign> > >, map<string, map<string, set<Sign> > > >* result = mvp->solve(s);


	for (int i = 0; i < icf->getLabels().size(); i++) {
		printf("For label %i:\n", i);
		map<string, map<string, set<Sign> > >::iterator mapIt;
		for (mapIt = result[i].second.begin(); mapIt != result[i].second.end(); mapIt++) {
			printf("For context %s:\n", mapIt->first.c_str());

			map<string, set<Sign> >::iterator map2It;
			for (map2It = mapIt->second.begin(); map2It != mapIt->second.end();
					map2It++) {
				printf("For variable %s:\n", map2It->first.c_str());

				set<Sign>::iterator setIt;
				for (setIt = map2It->second.begin();
						setIt != map2It->second.end(); setIt++) {
					switch (*setIt) {
					case SIGN_PLUS:
						printf("plus\n");
						break;
					case SIGN_MINUS:
						printf("minus\n");
						break;
					case SIGN_ZERO:
						printf("zero\n");
						break;
					}
				}
			}
		}
	}

	/*
	 CPPParser::FunctionDeclaration fd = *(program.functionDeclarations.begin());

	 ControlFlow* cf = new ControlFlow(fd);
	 AExpAnalysis* a = new AExpAnalysis(cf);

	 CPPParser::VariableAssignment* s = new CPPParser::VariableAssignment();

	 s->name = "s";
	 CPPParser::Combination* c = new CPPParser::Combination();
	 s->value = c;
	 CPPParser::Variable* v1 = new CPPParser::Variable();
	 CPPParser::Variable* v2 = new CPPParser::Variable();
	 v1->value = "a";
	 v2->value = "b";
	 c->value1 = v1;
	 c->value2 = v2;
	 c->combinator="+";

	 MFP<set<CPPParser::VariableValue*> > * solver = new MFP<set<CPPParser::VariableValue*> >();

	 set<CPPParser::VariableValue*>* result = solver->solve(a);

	 for (int i = 0 ; i<cf->getLabels().size(); i++)
	 {
	 printf("Result for label %i (size %i):\n",i, result[i].size());
	 set<CPPParser::VariableValue*>::iterator it;
	 for (it = result[i].begin(); it != result[i].end(); it++)
	 {
	 printf("%s\n", (*it)->toString().c_str());
	 }
	 } */

	printf("Press any key to exit..");
	getchar();

	return 0;
}


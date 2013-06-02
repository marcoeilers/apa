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

	parser.parseFile("FibTest.txt");

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

	*/


	SignAnalysis* s = new SignAnalysis(icf);
	
	MVP<map<string, set<Sign> > >* mvp = new MVP<map<string, set<Sign> > >(1);
	
	pair<map<string, map<string, set<Sign> > >, map<string, map<string, set<Sign> > > >* result = mvp->solve(s);



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


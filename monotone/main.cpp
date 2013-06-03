#include <iostream>
#include <cstdio>

#include "AExpAnalysis.h"
#include "MFP.h"
#include "MVP.h"
#include "SignAnalysis.h"
#include "PointerAnalysis.h"

#include "Parser.h"
#include "StringUtil.h"
#include "ParserSemantics.h"


using namespace std;

int main() {
	std::string filename = "";

	while (true) {
		printf("Select the file to load:\n1.FibTest.txt\n2.PointerTest.txt\n3.RecursionTest.txt\n4.SignTest.txt\nAlternatively, type in a filename:");
		char buffer[201];
		gets(buffer);
		buffer[200] = 0;
		filename = std::string(buffer);
		if (filename.size() == 1)
			switch (filename[0]) {
			case '1':
				filename = "FibTest.txt"; break;
			case '2':
				filename = "PointerTest.txt"; break;
			case '3':
				filename = "RecursionTest.txt"; break;
			case '4':
				filename = "SignTest.txt"; break;
			}
		// See if we can open the file:
		FILE* file = ::fopen(filename.c_str(), "r");
		if (file) {
			fclose(file);
			break;
		}
		printf("Invalid input, please type a number or a filename (including the extension, e.g: MyFile.txt).\n");
		printf("\n");
	}
	printf("Selected %s.\n", filename.c_str());

	CPPParser::Parser parser;

	parser.parseFile(filename);

	CPPParser::TokenList tokens = parser.getTokens();

	/*CPPParser::TokenList::iterator it;
	for (it = tokens.begin(); it < tokens.end(); it++)
		printf("Token #%lu: '%s'\n", it->id, it->name.c_str());*/

	CPPParser::Program* program = new CPPParser::Program();
	bool success = false;
	try {
		success = program->tryBuild(tokens);
	} catch (CPPParser::ParseError pe) {
		printf("Parser error: %s\n", pe.getMessage().c_str());
	}
	if (!success) {
		printf("Parsing failed!\n");
	}

	int choice = -1;
	while (true) {
		printf("\n");
		printf("Select the type of analysis:\n1.Available Expression Analysis\n2.Sign Analysis\n3.Pointer Analysis\n:");
		char buffer[5];
		gets(buffer);
		buffer[4] = 0;
		filename = std::string(buffer);
		choice = static_cast<int>(filename[0] - '0');
		// See if we can open the file:
		if (choice > 0 && choice < 4) {
			break;
		}
		printf("Invalid input, please type a valid number.\n");
		printf("\n");
	}

	InterControlFlow* icf = new InterControlFlow(program);
	
	switch (choice) { // Note: If we add more, make sure to modify the break check in the while statement preceding this.
	case 1:
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

		for (int i = 0 ; i<cf->getLabels().size(); i++) {
			printf("Result for label %i (size %i):\n",i, result[i].size());
			set<CPPParser::VariableValue*>::iterator it;
			for (it = result[i].begin(); it != result[i].end(); it++) {
				printf("%s\n", (*it)->toString().c_str());
			}
		}
		break;
	case 2:
		SignAnalysis* s = new SignAnalysis(icf);
	
		MVP<map<string, set<Sign> > >* mvp = new MVP<map<string, set<Sign> > >(1);
	
		pair<map<string, map<string, set<Sign> > >, map<string, map<string, set<Sign> > > >* result = mvp->solve(s);
		break;
	case 3:
		PointerAnalysis* p = new PointerAnalysis(icf);

		MVP<map<string, set<string> > >* mvp = new MVP<map<string, set<string> > >(2);

		pair<map<string, map<string, set<string> > >, map<string, map<string, set<string> > > >* result = mvp->solve(p);
		break;
	}

	printf("Press any key to exit..");
	getchar();

	return 0;
}


#include <iostream>
#include <AExpAnalysis.h>
#include <MFP.h>
#include <Parser.h>
#include <StringUtil.h>
#include <ParserSemantics.h>


using namespace std;

int main()
{

    CPPParser::Parser parser;

	parser.parseFile("../../C++ Parser/Parser Input/Code.cpp");

	CPPParser::TokenList tokens = parser.getTokens();

	CPPParser::TokenList::iterator it;
	for (it = tokens.begin(); it < tokens.end(); it++)
		printf("Token #%lu: '%s'\n", it->id, it->name.c_str());

	CPPParser::Program program;
	try {
		program.tryBuild(tokens);
	} catch (CPPParser::ParseError pe) {
		printf("Parser error: %s\n", pe.getMessage().c_str());
	}


    CPPParser::FunctionDeclaration fd = *(program.functionDeclarations.begin());
    ControlFlow* cf = new ControlFlow(fd);
    AExpAnalysis* a = new AExpAnalysis(*cf);
    MFP<set<CPPParser::VariableValue*> > * solver = new MFP<set<CPPParser::VariableValue*> >();

    set<CPPParser::VariableValue*> result = solver->solve(a, cf);


    printf("Press any key to exit..");
	getchar();

    return 0;
}


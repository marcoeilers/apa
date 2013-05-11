#include <cstdlib>
#include <vector>
#include <cstdio>
#include "Parser.h"
#include "StringUtil.h"

/**	First breaks up in tokens, then builds the semantic tree.
*/
int main() {
	CPPParser::Parser parser;

	parser.parseFile("Parser Input/Example.cpp");

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

	printf("Press enter to exit..");
	getchar();

	return 0;
}
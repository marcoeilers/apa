#include <cstdlib>
#include <vector>
#include <cstdio>
#include "Parser.h"
#include "StringUtil.h"

/** Right now, this program reads a file and breaks it up in 'Tokens'. It's not completed yet.
	About Linux: If some functions don't work for linux, for example I can imagine file io may
	be slightly different (/ vs \), please just email me or fix it and comment!
*/
int main() {
	CPPParser::Parser parser;

	parser.parseFile("Parser Input/Example.cpp");

	CPPParser::TokenList tokens = parser.getTokens();

	CPPParser::Program program;
	try {
		program.tryBuild(tokens);
	} catch (CPPParser::ParseError pe) {
		printf("Parser error: %s\n", pe.getMessage().c_str());
	}

	/*CPPParser::TokenList::iterator it;
	for (it = tokens.begin(); it < tokens.end(); it++)
		printf("Token #%lu: '%s'\n", it->id, it->name.c_str());*/

	printf("Press enter to exit..");
	getchar();

	return 0;
}
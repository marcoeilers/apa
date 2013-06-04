/*
 * Parser.h
 *
 *      Author: Bas in het Veld (3710971)
 *              Marco Eilers (F121763)
 *
 */

#ifndef PARSER_H
#define PARSER_H

#include <cstdlib>
#include <cstdio>
#include <vector>
#include <string>

#include "ParserSemantics.h"

namespace CPPParser {

class Parser {
private:
	TokenList tokens;
public:
	/** Parses the file, and stores all tokens (words, symbols) in the tokens vector.
		@param filename The file's location/name.
	*/
	void parseFile(std::string filename);
	TokenList getTokens();
};

}

#endif

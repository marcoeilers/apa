#ifndef PARSER_H
#define PARSER_H

#include <cstdlib>
#include <cstdio>
#include <vector>
#include <string>


namespace CPPParser {

/** A token is a small building block of the language: 'if', 'while', '1', '=', etc.
*/
struct Token {
	std::string name;
	unsigned long id; // We don't really use this?
};

typedef std::vector<Token> TokenList;

std::vector<Token> extractTokens(std::string);

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

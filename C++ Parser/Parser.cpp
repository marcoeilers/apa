#include "Parser.h"
#include "StringUtil.h"
#include <string>
//#include <regex> HI I COMMITTED THIS (BUT NOT A CRIME)
#include <boost/regex.hpp>

namespace CPPParser {

/** Splits string into words/symbols. Removes spaces and tabs.
	@Author Bas
*/
std::vector<std::string> split(std::string string) {
	std::vector<std::string> result;
	Utils::StringUtil::trim(string, true, true);
	int max = string.size();
	if (max == 0) return result;
	// We now know there's at least a word.

	boost::regex r1;

	r1.assign(" *((//).+?[\\r\\n])|(/\\*.+?\\*/)"); // Remove any comments

	string = boost::regex_replace(string, r1, " ");

	r1.assign("[\\r\\n]+"); // Remove newlines and returns (Maybe need to add linux/mac return?)

	string = boost::regex_replace(string, r1, " ");

	r1.assign("\\s*;\\s*"); // Matches any ';' and any spaces behind it.

	string = boost::regex_replace(string, r1, " ; ");

	r1.assign("\\s*(\\*+)\\s*"); // Matches any sequential '*' symbols, and spaces around them

	string = boost::regex_replace(string, r1, " $1 "); // Replaces just the * symbols, and one space before and after.

	//r1.assign("(?<=[a-zA-Z0-9])\\("); // Find any '(' without a space preceding it

	//string = boost::regex_replace(string, r1, " (");

	//r1.assign("(?<=[^=!<>]=[^=])(.*?)(?= *;)"); // Match any assignment (from i = i - 1, [i - 1] is returned.)

	//string = boost::regex_replace(string, r1, "($1)"); // Surround any assignments with (): i = i + 1; becomes i = (i + 1);  The reason we do this is that it'll be grouped before it's tokenized.

	r1.assign(" *\\( *");

	string = boost::regex_replace(string, r1, " ( ");

	r1.assign(" *\\) *");

	string = boost::regex_replace(string, r1, " ) ");

	r1.assign("\\s\\s+"); // Match any group of spaces, or tabs (2+)

	string = boost::regex_replace(string, r1, " ");

	r1.assign("[\\s]$"); // End of the string: Remove space if there is one there

	string = boost::regex_replace(string, r1, "");

	// Next, we group together anything within ( ) symbols by removing spaces within those groups:
	int index1 = -1; // The location of the first (
	/*while (true) {
		index1 = string.find("(", index1+1);
		if (index1 == -1) break;
		int index2 = index1;
		int depth = 0;
		while (string[++index2] != ')' || depth != 0) {
			if (string[index2] == ' ')
				string.replace(index2--, 1, "");
			else if (string[index2] == '(') depth++;
			else if (string[index2] == ')') depth--;
		}
		index1 = index2;
	}*/
	//printf("'%s'\n", string.c_str());
	// Find next space:
	index1 = 0;
	while (true) {
		int index2 = string.find(' ', index1 + 1);
		result.push_back(string.substr(index1, index2-index1));
		if (index2 == -1) break;
		index1 = index2 + 1;
	}
	return result;
}

std::vector<Token> extractTokens(std::string string) {
	std::vector<std::string> tokenNames = split(string); // Note: This is not Utils::StringUtil::split, but the special version defined in this file.
	std::vector<Token> tokens;

	for (unsigned int i = 0; i < tokenNames.size(); i++) {
		Token token;
		token.name = tokenNames[i];
		token.id = i;
		tokens.push_back(token);
	}
	return tokens;
}

void Parser::parseFile(std::string filename) {
	//FILE* file = fopen(filename.c_str(), "r");

	std::string fullProgram; // We save the whole program in one string, not perfect memory wise, but more convenient and flexible.

	/*char b[200];
	while (fgets(b, 200, file) != NULL)
		fullProgram += std::string(b);// + " "; */
	//fclose(file);
	//file = NULL;
    fullProgram = "int main() { int i = 8; int j = 0; int k = 0; int z = 5; k = j + 5; while (k > j) { i = i + 1; j = k + i; z = 6; if (j > 5) k = j + i; } k = j + i; }";
	tokens = extractTokens(fullProgram);
}

TokenList Parser::getTokens() {
	return tokens;
}

}

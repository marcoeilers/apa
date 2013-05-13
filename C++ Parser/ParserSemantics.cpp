#include "ParserSemantics.h"

namespace CPPParser {

/** Generally, all tryBuild() methods defined here should return false if no match was possible, and throw a ParseError if 
	they modified the token list before finding out no match was possible. If this happens, the parsing has failed.
*/

/** Returns the first element from the list, and removes that element
*/
Token pop(TokenList& tokens) {
	Token res = tokens.front();
	tokens.erase(tokens.begin());
	return res;
}

// These are the possible semantics: 
//
// [program] = [include]* main() [functionDecl]+
// [statement] = [while] | [if] | [varDecl] | [assignment] | [functionCall] | [codeBlock]
// [while] = while [condition] [statement]
// [if] = if [condition] [statement]
// [varDecl] = [type] varName = [value];
// [varAssignment] = [name] = [type];
// [functionCall] = fName();
// [functionDecl] = [type] fName() [codeBlock]
// [codeBlock] = { [Statement]* }
// [include] = #include "[filename]"

bool Program::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	includes.clear();
	functionDeclarations.clear();
	bool res;
	do {
		// Parse any number of includes:
		Include include;
		res = include.tryBuild(tokens);
		if (res) includes.push_back(include);
	} while (res);

	do {
		FunctionDeclaration functionDeclaration;
		res = functionDeclaration.tryBuild(tokens);
		if (res) functionDeclarations.push_back(functionDeclaration);
	} while (res);

	if (functionDeclarations.size() == 0) {
		throw ParseError("Expected FunctionDeclaration in Program");
		return false;
	}

	return true;
}
bool DataType::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	// Data type:
	name = tokens[0].name;

	int i;
	for (i = 0; i < nAllowedTypes; i++)
		if (name.compare(allowedTypes[i]) == 0) break;
	if (i == nAllowedTypes) return false; // 'Safe' return

	pop(tokens); // Remove it now (so the above return is safe)
	// Data type pointer 'depth'
	if (tokens.size() > 0 && tokens[0].name[0] == '*') {
		// Parse a pointer:
		pointerDepth = pop(tokens).name.size();
	} else pointerDepth = 0;
	return true;
}
/** Note, this function allocates 1 Statement object without releasing it.
	However, in the program it doesn't really matter since we will never release it anyway.
*/
Statement* tryStatement(TokenList& tokens) {
	// Try while, if, varDecl and function call:
	Statement* statement = new While();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new If();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new VariableDeclaration();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new VariableAssignment();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new FunctionCall();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new CodeBlock();
	if (statement->tryBuild(tokens))
		return statement;
	throw ParseError("Undefined Statement!");
	return NULL;
}
/** Note: If this function returns false, it must not have modified the token list.
	If the function throws an error, it may have.
*/
bool While::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	if (tokens[0].name.compare("while") != 0) return false; // 'Safe' return
	pop(tokens); // while
	
	condition = new RelationalCondition();
	if (!condition->tryBuild(tokens)) {
		delete condition;
		condition = new ConstantCondition();
		if (!condition->tryBuild(tokens)) {
			throw ParseError("Expected Condition in While");
			return false;
		}
	}

	statement = tryStatement(tokens);
	if (statement == NULL) throw ParseError("Expected Statement in While");

	return true;
}
/** Note: If this function returns false, it must not have modified the token list.
	If the function throws an error, it may have.
*/
bool If::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	if (tokens[0].name.compare("if") != 0) return false; // 'Safe' return
	pop(tokens); // if
	
	condition = new RelationalCondition();
	if (!condition->tryBuild(tokens)) {
		delete condition;
		condition = new ConstantCondition();
		if (!condition->tryBuild(tokens)) {
			throw ParseError("Expected Condition in While");
			return false;
		}
	}

	statement = tryStatement(tokens);
	if (statement == NULL) throw ParseError("Expected Statement in If");

	return true;
}
/** Note: If this function returns false, it must not have modified the token list.
	If the function throws an error, it may have.
*/
bool VariableDeclaration::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	dataType = new DataType();
	if (!dataType->tryBuild(tokens)) return false; // 'Safe' return: If dataType->tryBuild returns false it's been a safe return.
	
	name = pop(tokens).name;

	Token token = pop(tokens); // '=' or ';'
	if (token.name.compare(";") == 0) {
		value = "";
		return true;
	}

	value = pop(tokens).name;

	token = pop(tokens); // ';'

	if (token.name.compare(";") != 0) {
		throw ParseError("Expected \';\' in VariableDeclaration");
		return false;
	}

	return true;
}
/** Note: If this function returns false, it must not have modified the token list.
	If the function throws an error, it may have.
*/
bool VariableAssignment::tryBuild(TokenList& tokens) {
	if (tokens.size() < 2) return false;
	if (tokens[1].name.compare("=") != 0) return false; // 'Safe' return
	name = pop(tokens).name;
	pop(tokens); // '='
	value = pop(tokens).name;

	if (pop(tokens).name.compare(";") != 0) {
		throw ParseError("Expected \';\' in VariableAssignment");
		return false;
	}
	return true;
}
/** Note: If this function returns false, it must not have modified the token list.
	If the function throws an error, it may have.
*/
bool FunctionCall::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	if (tokens[1].name[0] != '(' || tokens[2].name.compare(";") != 0) return false;
	name = pop(tokens).name;
	arguments = pop(tokens).name;
	pop(tokens); // ';'
	return true;
}
bool CodeBlock::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	if (tokens[0].name.compare("{") != 0) return false; // 'Safe' return

	pop(tokens);
	Token token;
	while (tokens[0].name.compare("}") != 0) {
		Statement* statement = tryStatement(tokens);
		if (statement == NULL) {
			throw ParseError("Undefined statement in CodeBlock");
			return false;
		}
		statements.push_back(statement);
	}

	/*if (tokens[0].name.compare("}") != 0) {
		throw ParseError("Codeblock must end with \'}\'");
		return false;
	}
	pop(tokens);*/

	pop(tokens); // Remove the last '}'

	return true;
}
bool FunctionDeclaration::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	// Function datatype:
	dataType = new DataType();
	if (!dataType->tryBuild(tokens)) {
		//delete dataType;
		return false;
	}
	// Function name:
	name = pop(tokens).name;

	// Function arguments:
	pop(tokens); // (

	arguments = "";
	Token token = pop(tokens);
	while (token.name.compare(")") != 0) {
		arguments += token.name;
		token = pop(tokens);
		if (token.name.compare(")") != 0)
			arguments += " ";
		else break;
	}

	// Function body:
	codeBlock = new CodeBlock();
	if (!codeBlock->tryBuild(tokens)) {
		//delete codeBlock;
		throw ParseError("Expected CodeBlock in FunctionDeclaration");
		return false;
	}
	return true;
}
bool Include::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	if (tokens[0].name.compare("#include") == 0) {
		pop(tokens); // Discard
		this->filename = pop(tokens).name;
		return true;
	}
	return false;
}



bool RelationalCondition::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	// This is one token, always starting with '(' and ending in ')'. There must be at least one conditional in it.
	String str = tokens[0].name;
	if (str[0] != '(' || str[str.size()-1] != ')') {
		// Illegal token
		return false;
	}

	for (unsigned int i = 1; i < str.size() - 1; i++) {
		for (int j = 0; j < nRelationals; j++) {
			if (str[i] == relationals[j][0]) {
				// First letters match
				bool match = true;
				int k = 1;
				for (; k < relationals[j].size(); k++)
					if (str[i+k] != relationals[j][k]) {
						match = false;
						break;
					}
				if (match) {
					// k is the length of the found match
					this->conditional = str.substr(i, k);
					this->value1 = str.substr(1, i-1);
					this->value2 = str.substr(i+k, str.size() - (i+k) - 1);
					pop(tokens);
					return true;
				}
			}
		}
	}
	return false; // Safe return
}

bool ConstantCondition::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0) return false;
	// This is one token, always starting with '(' and ending in ')'.
	value = tokens[0].name;
	if (value[0] != '(' || value[value.size()-1] != ')') {
		// Illegal token
		return false;
	}
	if (value[1] == '!') {
		negative = true;
		value = value.substr(2, value.size() - 3);
	} else value = value.substr(1, value.size() - 2);
	pop(tokens);
	return true;
}

}
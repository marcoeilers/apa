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
	for (i = 0; i < nAllowedTypes; i++) {
		if (name.compare(allowedTypes[i]) == 0) break;
	}
	if (i == nAllowedTypes) return false; // 'Safe' return

	pop(tokens); // Remove it now (so the above return is safe)
	// Data type pointer 'depth'
	pointerDepth = 0;
	while (tokens[0].name.compare("*") == 0) {
		pointerDepth++;
		pop(tokens);
	}
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
	statement = new Return();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new VariableDeclaration();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new FunctionCall();
	if (statement->tryBuild(tokens))
		return statement;
	delete statement;
	statement = new VariableAssignment();
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
			throw ParseError("Expected Condition in If");
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
		value = new Unknown(); // Undefined variable value
		return true;
	}

	// Try all possible variable value types:
	value = new Allocation();
	if (!value->tryBuild(tokens)) {
		delete value;
		value = new Combination();
		if (!value->tryBuild(tokens)) {
			delete value;
			value = new Variable();
			if (!value->tryBuild(tokens)) {
				delete value;
				value = new Unknown(); // No need to try build
			}
		}
	}


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

	derefDepth = 0;
	while (tokens[derefDepth].name.compare("*") == 0)
		derefDepth++;

	if (tokens[derefDepth + 1].name.compare("=") != 0) return false; // 'Safe' return

	// Remove all the *s that represent derefDepth:
	for (int i = 0; i < derefDepth; i++)
		pop(tokens);

	name = pop(tokens).name;
	pop(tokens); // '='

	// Try all possible variable value types:
	value = new Allocation();
	if (!value->tryBuild(tokens)) {
		delete value;
		value = new Combination();
		if (!value->tryBuild(tokens)) {
			delete value;
			value = new Variable();
			if (!value->tryBuild(tokens)) {
				delete value;
				value = new Unknown(); // No need to try build
			}
		}
	}

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
	if (tokens.size() < 3) return false;
	
	// First, check whether this is a functioncall (and not a variable assignment)
	if (tokens[1].name[0] == '=' && tokens[3].name[0] != '(') return false;
	if (tokens[1].name[0] != '=' && tokens[1].name[0] != '(') return false;

	if (tokens[1].name[0] == '=') {
		returnVariable = new Variable();
		if (!returnVariable->tryBuild(tokens)) {
			return false; // Safe (variable->tryBuild is presumed safe)
		}
		pop(tokens); // = 
	} else returnVariable = NULL;

	name = pop(tokens).name;

	pop(tokens); // (

	variables.clear();
	Token token = tokens[0];
	while (token.name.compare(")") != 0) {
		VariableValue* variable;
		variable = new Combination();
		if (!variable->tryBuild(tokens)) {
			delete variable;
			variable = new Variable();
			if (!variable->tryBuild(tokens)) {
				delete variable;
				throw ParseError("Expected VariableValue in FunctionCall");
				return false;
			}
		}
		variables.push_back(variable);

		if (tokens[0].name.compare(",") == 0) pop(tokens);
		else break;
	}
	if (tokens[0].name.compare(")") != 0) {
		throw ParseError("Expected \')\' in FunctionCall");
		return false;
	}
	pop(tokens); // )

	pop(tokens); // ';'
	
	return true;
}
bool Return::tryBuild(TokenList& tokens) {
	if (tokens.size() < 2) return false;
	if (tokens[0].name.compare("return") != 0) return false; // safe
	pop(tokens); // return
	if (tokens[0].name.compare(";") == 0) {
		this->variable = NULL;
		pop(tokens);
		return true;
	}
	variable = new Variable();
	if (!variable->tryBuild(tokens)) {
		throw ParseError("Invalid return variable in Return");
		return false;
	}
	if (tokens[0].name.compare(";") != 0) {
		throw ParseError("Expected \';\' in Return");
		return false;
	}
	pop(tokens); // ;
	
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

	pop(tokens); // Remove the last '}'
	
	return true;
}

bool Variable::tryBuild(TokenList& tokens) {
	if (tokens.size() == 0 || tokens[0].name.compare(";") == 0) return false;
	derefDepth = 0;
	while (tokens[derefDepth].name.compare("*") == 0) {
		derefDepth++;
		pop(tokens);
	}

	value = pop(tokens).name;
	
	return true;
}
bool Combination::tryBuild(TokenList& tokens) {
    // not safe

    combinator = tokens[1].name;

    bool found = false;
	for (int i = 0; i < nCombinators; i++)
		if (combinators[i].compare(combinator) == 0) {
			found = true;
			break;
		}
	if (!found) return false; // Safe return

    value1 = new Variable();
    if (!value1->tryBuild(tokens))
        return false;

    //pop combinator
        pop(tokens);

    if (tokens[0].name.compare("(")==0)
    {
        //pop opening parenthesis
        pop(tokens);

        value2 = new Combination();
        if (!value2->tryBuild(tokens))
            throw ParseError("Something went wrong");

        if (tokens[0].name.compare(")")!=0)
            throw ParseError("Something else went wrong");
        //pop closing parenthesis
        pop(tokens);
    }else{
        value2 = new Variable();
        if (!value2->tryBuild(tokens))
            throw ParseError("Something entirely different went wrong");
    }
	
	return true;
}
bool Allocation::tryBuild(TokenList& tokens) {
	if (tokens.size() < 5) return false;
	if (tokens[0].name.compare("new") != 0) return false;
	pop(tokens); // new

	type = new DataType();
	if (!type->tryBuild(tokens)) throw ParseError("Expected datatype in Allocation");

	if (tokens[0].name.compare("(") != 0) throw ParseError("Expected ( in Allocation");
	pop(tokens); // (

	if (tokens[0].name.compare(")") == 0) value = new Unknown();
	else {
		value = new Variable();
		value->tryBuild(tokens);
	}
	if (tokens[0].name.compare(")") != 0) throw ParseError("Expected ) in Allocation");
	pop(tokens); // )
	
	return true;
}
bool Unknown::tryBuild(TokenList& tokens) {
	return true; // Always succeeds
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

	arguments.clear();
	Token token = tokens[0];
	while (token.name.compare(")") != 0) {
		std::pair<Variable*, DataType*> pair;
		pair.second = new DataType();
		if (!pair.second->tryBuild(tokens)) {
			throw ParseError("Expected DataType in FunctionDeclaration");
			return false;
		}
		pair.first = new Variable();
		if (!pair.first->tryBuild(tokens)) {
			throw ParseError("Expected Variable in FunctionDeclaration");
			return false;
		}
		arguments.push_back(pair);

		if (tokens[0].name.compare(",") == 0) pop(tokens);
		else break;
	}
	if (tokens[0].name.compare(")") != 0) {
		throw ParseError("Expected \')\' in FunctionDeclaration");
		return false;
	}
	pop(tokens); // )

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
	if (tokens.size() < 3) return false;

	if (tokens[0].name.compare("(") != 0) return false;

	// tokens[0] is '(', now find the corresponding ')':
	bool foundCondition = false;
	unsigned int endPos;
	int depth = 0;
	for (endPos = 1; endPos < tokens.size(); endPos++) {
		if (tokens[endPos].name.compare("(") == 0) depth++;
		if (tokens[endPos].name.compare(")") == 0) {
			if (depth == 0)
				break;
			else depth--;
			continue;
		}
		if (depth == 0)
			for (int j = 0; j < nRelationals; j++) {
				if (relationals[j].compare(tokens[endPos].name) == 0) {
					foundCondition = true;
					break;
				}
			}
	}
	if (endPos == tokens.size()) {
		throw ParseError("Expected \')\' in RelationalCondition.");
		return false;
	}
	if (!foundCondition) return false; // Means this must be a ConstantCondition.

	pop(tokens); // (

	// Parse a combination, or a variable.
	this->variable1 = new Combination();
	if (!variable1->tryBuild(tokens)) {
		delete variable1;
		variable1 = new Variable();
		if (!variable1->tryBuild(tokens)) {
			throw ParseError("Expected Combination or Variable in RelationalCondition");
			return false;
		}
	}
	this->conditional = pop(tokens).name; // The relational

	// Parse a combination, or a variable.
	this->variable2 = new Combination();
	if (!variable2->tryBuild(tokens)) {
		delete variable2;
		variable2 = new Variable();
		if (!variable2->tryBuild(tokens)) {
			throw ParseError("Expected Combination or Variable in RelationalCondition");
			return false;
		}
	}

	pop(tokens); // )

	return true;
}

bool ConstantCondition::tryBuild(TokenList& tokens) {
	if (tokens.size() < 3) return false;
	if (tokens[0].name.compare("(") != 0 || tokens[2].name.compare(")") != 0) return false;

	pop(tokens); // (

	this->variable = pop(tokens).name;
	if (variable[0] == '!') {
		negative = true;
		variable = variable.substr(1);
	} else negative = false;

	pop(tokens); // )
	
	return true;
}

String Program::toString() {
	char c[201];
	sprintf(c, "Program|%i includes|%i functionDeclarations", this->includes.size(), this->functionDeclarations.size());
	c[200] = 0;
	return String(c);
}

String DataType::toString() {
	char c[201];
	sprintf(c, "DataType|%s", this->name.c_str());
	String result = String(c);
	for (int i = 0; i < this->pointerDepth; i++) 
		result += "*";
	c[200] = 0;
	return result;
}

String While::toString() {
	char c[201];
	sprintf(c, "While(%s)", this->condition->toString().c_str());
	c[200] = 0;
	return String(c);
}
String If::toString() {
	char c[201];
	sprintf(c, "If(%s)", this->condition->toString().c_str());
	c[200] = 0;
	return String(c);
}
String VariableDeclaration::toString() {
	char c[201];
	sprintf(c, "VariableDecl|(%s) %s = (%s)", this->dataType->toString().c_str(), this->name.c_str(), this->value->toString().c_str());
	c[200] = 0;
	return String(c);
}
String VariableAssignment::toString() {
	char c[201];
	String deref = "";
	for (int i = 0; i < this->derefDepth; i++)
		deref += "&";
	sprintf(c, "VariableAssignment|%s%s = (%s)", deref.c_str(), this->name.c_str(), this->value->toString().c_str());
	c[200] = 0;
	return String(c);
}
String FunctionCall::toString() {
	char c[201];
	String ret;
	if (this->returnVariable == NULL) ret = "";
	else ret = "(" + returnVariable->toString() + ") = ";
	String vars = "";
	for (int i = 0; i < this->variables.size(); i++) {
		if (i != 0) vars += ",";
		vars += "(" + variables[i]->toString() + ")";
	}
	sprintf(c, "FunctionCall|%s%s(%s)", ret.c_str(), this->name.c_str(), vars.c_str());
	c[200] = 0;
	return String(c);
}
String Return::toString() {
	char c[201];
	String val;
	if (this->variable == NULL) val = "void";
	else val = variable->toString();
	sprintf(c, "Return|val:(%s)", val.c_str());
	c[200] = 0;
	return String(c);
}
String CodeBlock::toString() {
	return "CodeBlock";
}
String Variable::toString() {
	char c[201];
	String deref = "";
	for (int i = 0; i < this->derefDepth; i++)
		deref += "*";
	sprintf(c, "Variable|val:%s%s", deref.c_str(), this->value.c_str());
	c[200] = 0;
	return String(c);
}
String Combination::toString() {
	char c[201];
	sprintf(c, "Combination|(%s) %s (%s)", this->value1->toString().c_str(), combinator.c_str(), this->value2->toString().c_str());
	c[200] = 0;
	return String(c);
}
String Allocation::toString() {
	char c[201];
	sprintf(c, "Allocation|(%s) (%s)", this->type->toString().c_str(),this->value->toString().c_str());
	c[200] = 0;
	return String(c);
}
String Unknown::toString() {
	return "Unknown";
}
String FunctionDeclaration::toString() {
	char c[201];
	String args = "";
	for (int i = 0; i < arguments.size(); i++) {
		if (i != 0) args += ",";
		sprintf(c, "(%s %s)", arguments[i].second->toString().c_str(), arguments[i].first->toString().c_str());
		args += String(c);
	}
	sprintf(c, "FunctionDeclaration|(%s) %s(%s)", this->dataType->toString().c_str(), this->name.c_str(), args.c_str());
	c[200] = 0;
	return String(c);
}
String Include::toString() {
	return "Include|fName:" + this->filename;
}
String RelationalCondition::toString() {
	char c[201];
	sprintf(c, "RelationalCondition|(%s) %s (%s)", this->variable1->toString().c_str(), this->conditional.c_str(), this->variable2->toString().c_str());
	c[200] = 0;
	return String(c);
}
String ConstantCondition::toString() {
	char c[201];
	sprintf(c, "ConstantCondition|%s%s", this->negative ? "!" : "", this->variable.c_str());
	c[200] = 0;
	return String(c);
}

}

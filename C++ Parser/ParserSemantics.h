#ifndef PARSERSEMANTICS_H
#define PARSERSEMANTICS_H

#include <cstdlib>
#include <vector>

namespace CPPParser {

// Forward declarations
struct Program;
struct Statement;
struct While;
struct If;
struct VariableDeclaration;
struct VariableAssignment;
struct FunctionCall;
struct CodeBlock;
struct FunctionDeclaration;
struct Include;
struct Condition;
struct RelationalCondition;
struct ConstantCondition;

typedef std::string String; // From now on we can call std::string 'String'.

/** A token is a small building block of the language: 'if', 'while', '1', '=', etc.
*/
struct Token {
	std::string name;
	unsigned long id; // We don't really use this?
};

typedef std::vector<Token> TokenList;

class ParseError {
private:
	std::string message;
public:
	ParseError() {message = "Unknown error!";}
	ParseError(std::string m) : message(m) {}
	std::string getMessage() { return message;}
};

/////////////////////////////////////
// Definitions of the semantics:
/////////////////////////////////////

// These are the possible semantics:
//
// [program] = [include]* main() [functionDecl]+
// [statement] = [while] | [if] | [varDecl] | [varAssignment] | [functionCall] | [codeBlock]
// [while] = while [condition] [statement]
// [if] = if [condition] [statement]
// [varDecl] = [type] varName = [value];
// [varAssignment] = [name] = [type];
// [functionCall] = fName();
// [functionDecl] = [type] fName() [codeBlock]
// [codeBlock] = { [Statement]* }
// [include] = #include "[filename]"
// [relational] = < | > | <= | >= | == | != | && | ||
// [condition] = varName [relational] varName | varName

struct Program {
	std::vector<Include> includes;
	std::vector<FunctionDeclaration> functionDeclarations;
	bool tryBuild(TokenList& tokens);
};
const int nAllowedTypes = 5;
const std::string allowedTypes[] = {"int", "long", "char", "float", "double"};
const int nRelationals = 8;
const std::string relationals[] = {">", "<", ">=", "<=", "==", "!=", "&&", "||"};
struct DataType {
	std::string name;
	int pointerDepth; // 0: 'no' pointer, 1: pointer, 2: pointer to pointer, etc
	bool tryBuild(TokenList& tokens);
};

// Abstract
struct Statement {
virtual bool tryBuild(TokenList& tokens) = 0;
protected:
	Statement() {} // Make the class abstract by hiding the constructor
};

struct While : Statement {
	Condition* condition;
	Statement* statement;
	virtual bool tryBuild(TokenList& tokens);
};
struct If : Statement {
	Condition* condition;
	Statement* statement;
	virtual bool tryBuild(TokenList& tokens);
};
struct VariableDeclaration : Statement {
	DataType* dataType;
	String name;
	String value; // Value = "" if the var is uninitialized
	virtual bool tryBuild(TokenList& tokens);
};
struct VariableAssignment : Statement {
	String name;
	String value;
	virtual bool tryBuild(TokenList& tokens);
};
struct FunctionCall : Statement {
	String name;
	String arguments;
	virtual bool tryBuild(TokenList& tokens);
};

struct CodeBlock : Statement {
	std::vector<Statement*> statements;
	virtual bool tryBuild(TokenList& tokens);
};

struct FunctionDeclaration {
	DataType* dataType;
	String name;
	std::string arguments;
	CodeBlock* codeBlock;
	bool tryBuild(TokenList& tokens);
};

struct Include {
	String filename;
	bool tryBuild(TokenList& tokens);
};

struct Condition {
	virtual bool tryBuild(TokenList& tokens) = 0;
};

struct RelationalCondition : Condition {
	String value1;
	String value2;
	String conditional;
	virtual bool tryBuild(TokenList& tokens);
};

struct ConstantCondition : Condition {
	String value;
	virtual bool tryBuild(TokenList& tokens);
};

}

#endif

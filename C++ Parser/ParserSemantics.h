#ifndef PARSERSEMANTICS_H
#define PARSERSEMANTICS_H

#include <cstdlib>
#include <vector>

namespace CPPParser {

// Forward declarations
struct Program;
struct Statement;
struct While; // : Statement
struct If; // : Statement
struct VariableDeclaration; // : Statement
struct VariableAssignment; // : Statement
struct FunctionCall; // : Statement
struct CodeBlock; // : Statement
struct FunctionDeclaration;
struct Include;
struct Condition;
struct RelationalCondition; // : Condition
struct ConstantCondition; // : Condition
struct VariableValue; // : Condition
struct Constant; // : VariableValue
struct Variable; // : VariableValue
struct Combination; // : VariableValue
struct Allocation; // : VariableValue
struct Unknown; // : VariableValue // An uninitialized value

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

enum StatementType {
	TYPE_WHILE,
	TYPE_IF,
	TYPE_VAR_DECLARATION,
	TYPE_VAR_ASSIGNMENT,
	TYPE_FUNCTIONCALL,
	TYPE_CODEBLOCK
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
// [varDecl] = [type] varName = [variableValue];
// [varAssignment] = [name] = [variableValue];
// [combinator] = + | - | * | /
// [combination] = [variableValue] [combinator] [variableValue] // Note, for variableValue, only  variable/unknown are allowed.
// [allocation] = new [type]([variableValue]) // Note, for variableValue, only variable/unknown are allowed.
// [variableValue] = [variable] | [combination] | [allocation] | [unknown]
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
const int nCombinators = 4;
const std::string combinators[] = {"+", "-", "*", "/"};
struct DataType {
	std::string name;
	int pointerDepth; // 0: 'no' pointer, 1: pointer, 2: pointer to pointer, etc
	bool tryBuild(TokenList& tokens);
};

// Abstract
struct Statement {
virtual bool tryBuild(TokenList& tokens) = 0;
virtual StatementType getType() = 0;
};

struct While : Statement {
	Condition* condition;
	Statement* statement;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_WHILE; }
};
struct If : Statement {
	Condition* condition;
	Statement* statement;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_IF; }
};
struct VariableDeclaration : Statement {
	DataType* dataType;
	String name;
	VariableValue* value;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_VAR_DECLARATION; }
};
struct VariableAssignment : Statement {
	String name;
	VariableValue* value;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_VAR_ASSIGNMENT; }
};
struct FunctionCall : Statement {
	String name;
	String arguments;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_FUNCTIONCALL; }
};

struct CodeBlock : Statement {
	std::vector<Statement*> statements;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_CODEBLOCK; }
};

struct VariableValue {
	virtual bool tryBuild(TokenList& tokens) = 0;
};

struct Variable : VariableValue {
	String value;
	virtual bool tryBuild(TokenList& tokens);
};
struct Combination : VariableValue {
	VariableValue* value1;
	VariableValue* value2;
	String combinator;
	virtual bool tryBuild(TokenList& tokens);
};
struct Allocation : VariableValue {
	DataType* type;
	VariableValue* value; // That is, value of the object, not the pointer.
	virtual bool tryBuild(TokenList& tokens);
};
struct Unknown : VariableValue { // Uninitialized variables get this value.
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
	bool negative;
	virtual bool tryBuild(TokenList& tokens);
};

}

#endif
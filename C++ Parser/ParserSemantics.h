#ifndef PARSERSEMANTICS_H
#define PARSERSEMANTICS_H

#include <stdio.h>
#include <cstdlib>
#include <vector>
#include <string>

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
	TYPE_RETURN,
	TYPE_CODEBLOCK
};

enum ValueType {
    VALUE_VARIABLE,
    VALUE_COMBINATION,
    VALUE_ALLOCATION,
    VALUE_UNKNOWN
};

enum ConditionType {
	CONDITION_CONSTANT,
	CONDITION_RELATIONAL
};

/////////////////////////////////////
// Definitions of the semantics:
/////////////////////////////////////

// These are the possible semantics:
//
// [program] = [include]* main() [functionDecl]+
// [statement] = [while] | [if] | [varDecl] | [varAssignment] | [functionCall] | [return] | [codeBlock]
// [while] = while [condition] [statement]
// [if] = if [condition] [statement]
// [varDecl] = [type] varName = [variableValue];
// [varAssignment] = [name] = [variableValue];
// [combinator] = + | - | * | /
// [combination] = [variableValue] [combinator] [variableValue] // Note, for variableValue, only  variable/unknown are allowed.
// [allocation] = new [type]([variableValue]) // Note, for variableValue, only variable/unknown are allowed.
// [variableValue] = [variable] | [combination] | [allocation] | [unknown]
// [functionCall] = fName(); | [variable] = fName();
// [functionDecl] = [type] fName() [codeBlock]
// [return] = return [variableValue]? ;
// [codeBlock] = { [Statement]* }
// [include] = #include "[filename]"
// [relational] = < | > | <= | >= | == | != | && | ||
// [condition] = [variableValue] [relational] [variableValue] | [variableValue] // EG i+1 > 15-j

struct Program {
	std::vector<Include> includes;
	std::vector<FunctionDeclaration> functionDeclarations;
	bool tryBuild(TokenList& tokens);
};
const int nAllowedTypes = 6;
const std::string allowedTypes[] = {"int", "long", "char", "float", "double", "bool"};
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
	int derefDepth;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_VAR_ASSIGNMENT; }
};
struct FunctionCall : Statement {
	String name;
	String arguments;
	Variable* variable; // NULL if no variable. [variable] = fName();
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_FUNCTIONCALL; }
};

struct Return : Statement {
	VariableValue* variable; // NULL if the return does not return anything.
	virtual StatementType getType() { return TYPE_RETURN; }
	virtual bool tryBuild(TokenList& tokens);
};

struct CodeBlock : Statement {
	std::vector<Statement*> statements;
	virtual bool tryBuild(TokenList& tokens);
	virtual StatementType getType() { return TYPE_CODEBLOCK; }
};

struct VariableValue {
	virtual bool tryBuild(TokenList& tokens) = 0;
	virtual ValueType getType() = 0;
	virtual std::string toString() = 0;
	virtual bool equals(VariableValue* other) = 0;
};

struct Variable : VariableValue {
	String value;
	virtual bool tryBuild(TokenList& tokens);
	virtual ValueType getType() { return VALUE_VARIABLE; }
	virtual std::string toString() { return value; }
	virtual bool equals (VariableValue* other)
	{
	    if (other->getType() == VALUE_VARIABLE)
	    {
	        Variable* v = (Variable*) other;
	        return (value.compare(v->value) == 0);
	    }else{
            return false;
	    }
	}
};
struct Combination : VariableValue {
	VariableValue* value1;
	VariableValue* value2;
	String combinator;
	virtual bool tryBuild(TokenList& tokens);
	virtual ValueType getType() { return VALUE_COMBINATION; }
	virtual std::string toString() { return (value1->toString() + combinator.c_str()+value2->toString());}
	virtual bool equals (VariableValue* other)
	{
	    if (other->getType() == VALUE_COMBINATION)
	    {
	        Combination* c = (Combination*) other;

	        return (value1->equals(c->value1) && combinator.compare(c->combinator) == 0 && value2->equals(c->value2));
	    }else{
            return false;
	    }
	}
};
struct Allocation : VariableValue {
	DataType* type;
	VariableValue* value; // That is, value of the object, not the pointer.
	virtual bool tryBuild(TokenList& tokens);
    virtual ValueType getType() { return VALUE_ALLOCATION; }
    virtual std::string toString() { return "alloc";}
    virtual bool equals (VariableValue* other)
    {
        return (other->getType() == VALUE_ALLOCATION);
    }
};
struct Unknown : VariableValue { // Uninitialized variables get this value.
	virtual bool tryBuild(TokenList& tokens);
	virtual ValueType getType() { return VALUE_UNKNOWN; }
	virtual std::string toString() { return "unknown"; }
	virtual bool equals (VariableValue* other)
	{
	    return (other->getType() == VALUE_UNKNOWN);
	}
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
	virtual ConditionType getType() = 0;
};

struct RelationalCondition : Condition {
	VariableValue* variable1;
	VariableValue* variable2;
	String conditional;
	virtual bool tryBuild(TokenList& tokens);
	virtual ConditionType getType() { return CONDITION_RELATIONAL; }
};

struct ConstantCondition : Condition {
	bool negative;
	String variable;
	virtual bool tryBuild(TokenList& tokens);
	virtual ConditionType getType() { return CONDITION_CONSTANT; }
};

}

#endif

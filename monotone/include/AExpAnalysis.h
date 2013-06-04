/*
 * AExpAnalysis.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef AEXPANALYSIS_H
#define AEXPANALYSIS_H

#include <stdio.h>
#include <set>
#include <string>
#include <sstream>

#include "MFramework.h"
#include "ControlFlow.h"
#include "ParserSemantics.h"

// Available expressions analysis is one instance of a monotone framework

// type param T is instantiated to be a set of expressions ( = VariableValues)
class AExpAnalysis : public MFramework<std::set<CPPParser::VariableValue*> >
{
    public:
		// constructor takes a ControlFlow to collect all expressions
		// used in the function
        AExpAnalysis(ControlFlow*);
        virtual ~AExpAnalysis();

        // implement methods required by MFramework
        std::set<CPPParser::VariableValue*> bottom();
        std::set<CPPParser::VariableValue*> join(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        bool lessOrEqual(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        std::set<CPPParser::VariableValue*> f (std::set<CPPParser::VariableValue*>&, int);
        std::set<CPPParser::VariableValue*> getExtremalValue();
        std::set<int> getExtremalLabels();
        std::set<int> getNext(int);
        std::string toString(std::set<CPPParser::VariableValue*>&);
    protected:
    private:
        // calculates the gen set for an expression
        std::set<CPPParser::VariableValue*> gen(CPPParser::Statement*);
        // collection of all expressions used in the function
        std::set<CPPParser::VariableValue*> aexp;
        // adds an expression to aexp
        void addToExpressions(CPPParser::VariableValue*);
        // checks if an expression contains a variable with the given name
        bool contains(CPPParser::VariableValue*, std::string);
        // computes the union of two sets
        std::set<CPPParser::VariableValue*> varUnion(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        // adds all subexpressions of the given VariableVar to the
        // given set
        void addSubExpressions(std::set<CPPParser::VariableValue*>*, CPPParser::VariableValue*);
        // checks if a set contains an expression equal to the given one
        bool setContains(std::set<CPPParser::VariableValue*>&, CPPParser::VariableValue*);
};

#endif // AEXPANALYSIS_H

#ifndef AEXPANALYSIS_H
#define AEXPANALYSIS_H

#include <stdio.h>
#include "MFramework.h"
#include <set>
#include <string>
#include "ControlFlow.h"
#include "ParserSemantics.h"

// Available expressions analysis is one instance of a monotone framework
// implemented for testing reasons because of its relative simplicity.

// type param T is instantiated to be a set of expressions, where the expressions are
// represented as strings.
class AExpAnalysis : public MFramework<std::set<CPPParser::VariableValue*> >
{
    public:
        AExpAnalysis(ControlFlow*);
        virtual ~AExpAnalysis();
        std::set<CPPParser::VariableValue*> top();
        std::set<CPPParser::VariableValue*> bottom();
        std::set<CPPParser::VariableValue*> join(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        bool lessThan(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        std::set<CPPParser::VariableValue*> f (std::set<CPPParser::VariableValue*>&, CPPParser::Statement*);
        std::set<CPPParser::VariableValue*> getExtremalValue();
        std::set<int> getExtremalLabels();
        std::set<int> getNext(int);
    protected:
    private:
        std::set<CPPParser::VariableValue*> gen(CPPParser::Statement*);
        std::set<CPPParser::VariableValue*> aexp;
        void addToExpressions(CPPParser::VariableValue*);
        bool contains(CPPParser::VariableValue*, std::string);
        std::set<CPPParser::VariableValue*> varUnion(std::set<CPPParser::VariableValue*>&, std::set<CPPParser::VariableValue*>&);
        void addSubExpressions(std::set<CPPParser::VariableValue*>*, CPPParser::VariableValue*);
};

#endif // AEXPANALYSIS_H

#include "AExpAnalysis.h"

using namespace std;
AExpAnalysis::AExpAnalysis(ControlFlow& cf)
{
    // initialize
    vector<CPPParser::Statement*>::iterator it;
    for (it = cf.getLabels().begin(); it != cf.getLabels().end(); it++)
    {
        // TODO: add expressions to aexp
    }
}

AExpAnalysis::~AExpAnalysis()
{
    //dtor
}

set<CPPParser::VariableValue*> AExpAnalysis::top()
{
    // empty set
    set<CPPParser::VariableValue*> result;
    return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::bottom()
{
    // set of all subexpressions
    return aexp;
}

bool AExpAnalysis::lessThan(set<CPPParser::VariableValue*>& first, set<CPPParser::VariableValue*>& second)
{
    set<CPPParser::VariableValue*>::iterator it;
    bool result = true;
    for (it = second.begin(); it != second.end(); it++)
    {
        result = result && (first.find(*it) != first.end());
    }
    return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::f(set<CPPParser::VariableValue*>& current, CPPParser::Statement* s)
{
    //TODO
    set<CPPParser::VariableValue*> result;
    return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::join(set<CPPParser::VariableValue*>& first, set<CPPParser::VariableValue*>& second)
{
    // compute the intersection
    set<CPPParser::VariableValue*> result;
    set<CPPParser::VariableValue*>::iterator it;
    for (it = first.begin(); it != first.end(); it++)
    {
        if (second.find(*it) != second.end())
            result.insert(*it);
    }
    return result;
}

set<CPPParser::VariableValue*> AExpAnalysis::getExtremalValue()
{
    // empty set
    set<CPPParser::VariableValue*> result;
    return result;
}

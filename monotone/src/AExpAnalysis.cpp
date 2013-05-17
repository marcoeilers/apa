#include "AExpAnalysis.h"

using namespace std;
AExpAnalysis::AExpAnalysis(ControlFlow& cf)
{
    // initialize
    vector<CPPParser::Statement*>::iterator it;
    for (it = cf.getLabels().begin(); it != cf.getLabels().end(); it++)
    {
        switch ((*it)->getType())
        {
            case CPPParser::TYPE_VAR_ASSIGNMENT:
            {
                CPPParser::VariableAssignment* a = (CPPParser::VariableAssignment*) (*it);
                addToExpressions(a->value);
                break;
            }
            case CPPParser::TYPE_IF:
            {
                CPPParser::If* i = (CPPParser::If*) (*it);
                // addToExpressions(i->condition);
                // TODO: conditions should have VariableValues as arguments
                break;
            }
            case CPPParser::TYPE_WHILE:
            {
                CPPParser::While* w = (CPPParser::While*) (*it);
                // addToExpressions(w->condition);
                // TODO: conditions should have VariableValues as arguments
                break;
            }
            default: break;
        }
    }
}

AExpAnalysis::~AExpAnalysis()
{
    //dtor
}

void AExpAnalysis::addToExpressions(CPPParser::VariableValue* v)
{
    switch (v->getType())
        {
            case CPPParser::VALUE_COMBINATION:
            {
                CPPParser::Combination* c = (CPPParser::Combination*) v;
                aexp.insert(v);
                addToExpressions(c->value1);
                addToExpressions(c->value2);
                return;
            }
            default: return;
        }
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

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

set<string> AExpAnalysis::top()
{
    // empty set
    set<string> result;
    return result;
}

set<string> AExpAnalysis::bottom()
{
    // set of all subexpressions
    return aexp;
}

bool AExpAnalysis::lessThan(set<string>& first, set<string>& second)
{
    set<string>::iterator it;
    bool result = true;
    for (it = first.begin(); it != first.end(); it++)
    {
        result = result && (second.find(*it) != second.end());
    }
    return result;
}

set<string> AExpAnalysis::f(set<string>& current, CPPParser::Statement* s)
{
    //TODO
    set<string> result;
    result.insert("tralala");
    return result;
}

set<string> AExpAnalysis::join(set<string>& first, set<string>& second)
{
    // compute the intersection
    set<string> result;
    set<string>::iterator it;
    for (it = first.begin(); it != first.end(); it++)
    {
        if (second.find(*it) != second.end())
            result.insert(*it);
    }
    return result;
}

set<string> AExpAnalysis::getExtremalValue()
{
    // empty set
    set<string> result;
    return result;
}

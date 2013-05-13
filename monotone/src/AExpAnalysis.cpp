#include "AExpAnalysis.h"

using namespace std;
// TODO: This class currently only has dummy functions
AExpAnalysis::AExpAnalysis()
{
    //ctor
}

AExpAnalysis::~AExpAnalysis()
{
    //dtor
}

set<string> AExpAnalysis::top()
{
    set<string> result;
    result.insert("tralala");
    return result;
}

set<string> AExpAnalysis::bottom()
{
    set<string> result;
    result.insert("tralala");
    return result;
}

bool AExpAnalysis::lessThan(set<string> first, set<string> second)
{
    return false;
}

set<string> AExpAnalysis::f(set<string> current, Statement s)
{
    set<string> result;
    result.insert("tralala");
    return result;
}

set<string> AExpAnalysis::join(set<string> first, set<string> second)
{
    set<string> result;
    result.insert("tralala");
    return result;
}

set<string> AExpAnalysis::getExtremalValue()
{
    set<string> result;
    result.insert("tralala");
    return result;
}

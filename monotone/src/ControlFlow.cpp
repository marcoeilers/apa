#include <ControlFlow.h>

ControlFlow::ControlFlow(CPPParser::FunctionDeclaration& f)
{
    vector<CPPParser::Statement*>::iterator it;
    for (it= f.codeBlock->statements.begin(); it != f.codeBlock->statements.begin(); it++)
    {
        //TODO
    }
}

ControlFlow::~ControlFlow()
{

}

ControlFlow::getLabels()
{
    return labels;
}

ControlFlow::getExtremalLabels()
{
    return extremals;
}

ControlFlow::getNext(int l)
{
    return transitions[l];
}

#include <ControlFlow.h>

using namespace std;

ControlFlow::ControlFlow(CPPParser::FunctionDeclaration& f)
{
    vector<CPPParser::Statement*>::iterator it;
    int index = 0;

    index = addStatement(f.codeBlock, index);
}

ControlFlow::~ControlFlow()
{

}

int ControlFlow::addStatement(CPPParser::Statement* s, int label)
{
    switch (s->getType())
        {
            case CPPParser::TYPE_WHILE :
            {
                CPPParser::While* w = (CPPParser::While*) s;
                labels.insert(labels.begin()+label, w);
                label++;
                return addStatement(w->statement, label);
            }
            case CPPParser::TYPE_IF : return label;
            {
                CPPParser::If* i = (CPPParser::If*) s;
                labels.insert(labels.begin()+label, s);
                label++;
                return addStatement(i->statement, label);
            }
            case CPPParser::TYPE_CODEBLOCK:
            {
                CPPParser::CodeBlock* cb = (CPPParser::CodeBlock*) s;
                vector<CPPParser::Statement*>::iterator it;
                for (it = cb->statements.begin(); it != cb->statements.end(); it++)
                {
                    label = addStatement(*it, label);
                }
                return label;
            }
            default:
            {
                labels.insert(labels.begin()+label, s);
                label++;
                return label;
            }
        }
}

vector<CPPParser::Statement*> ControlFlow::getLabels()
{
    return labels;
}

set<int> ControlFlow::getExtremalLabels()
{
    return extremals;
}

set<int> ControlFlow::getNext(int l)
{
    return transitions[l];
}

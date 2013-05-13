#include <ControlFlow.h>

using namespace std;

ControlFlow::ControlFlow(CPPParser::FunctionDeclaration& f)
{
    vector<CPPParser::Statement*>::iterator it;
    int index = 0;
    last = -1;
    index = addStatement(f.codeBlock, index);

    extremals.insert(0);
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
                int startLabel = label;
                labels.insert(labels.begin()+label, w);
                addTransition(last, label);
                last=label++;
                label = addStatement(w->statement, label);
                addTransition(startLabel, label);
                addTransition(label-1, startLabel);
                return label;
            }
            case CPPParser::TYPE_IF : return label;
            {
                CPPParser::If* i = (CPPParser::If*) s;
                int startLabel = label;
                labels.insert(labels.begin()+label, s);
                addTransition(last, label);
                last=label++;
                label =  addStatement(i->statement, label);
                addTransition(startLabel, label);
                return label;
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
                addTransition(last, label);
                last=label++;
                return label;
            }
        }
}

void ControlFlow::addTransition(int from, int to)
{
    if (from != -1)
    {
        if (transitions.find(from) != transitions.end())
        {
            set<int> current = transitions[from];
            current.insert(to);
        }
        else
        {
            set<int> toInsert;
            toInsert.insert(to);
            transitions[from] = toInsert;
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

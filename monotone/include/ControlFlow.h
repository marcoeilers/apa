#ifndef CONTROLFLOW_H
#define CONTROLFLOW_H

#include <vector>
#include <map>
#include <set>
#include <ParserSemantics.h>

// This class stores all the information about a program
// that is relevant for an analysis, i.e. the instruction that
// can be found at a given laben and the set of labels that can follow a given label,
// as well as the set of extremal (start/end) labels.

class ControlFlow
{
    public:
        ControlFlow(CPPParser::FunctionDeclaration&);
        virtual ~ControlFlow();
        std::vector<CPPParser::Statement*> getLabels();
        std::set<int> getNext(int l);
        std::set<int> getExtremalLabels();

    protected:
    private:
        int addStatement(CPPParser::Statement*, int);
        void addTransition(int, int);
        std::vector<CPPParser::Statement*> labels;
        std::map<int, std::set<int> > transitions;
        std::set<int> extremals;
        int last;
};

#endif // CONTROLFLOW_H

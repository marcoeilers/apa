#ifndef CONTROLFLOW_H
#define CONTROLFLOW_H

#include <vector>
#include <map>

// This class stores all the information about a program
// that is relevant for an analysis, i.e. the instruction that
// can be found at a given laben and the set of labels that can follow a given label,
// as well as the set of extremal (start/end) labels.

// param I is the type of a statement/an instruction. Could be hard coded to be
// the statement coming from our parser.
template<typename I> class ControlFlow
{
    public:
        ControlFlow()
        {
            // TODO
        }
        virtual ~ControlFlow()
        {
            // TODO
        }
        std::vector<I> getLabels()
        {
            return labels;
        }
        std::set<int> getNext(int l)
        {
            return transitions[l];
        }
        std::set<int> getExtremalLabels()
        {
            return extremals;
        }
    protected:
    private:
        std::vector<I> labels;
        std::map<int, std::set<int> > transitions;
        std::set<int> extremals;
};

#endif // CONTROLFLOW_H

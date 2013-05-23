#ifndef MFRAMEWORK_H
#define MFRAMEWORK_H

#include "ParserSemantics.h"
#include "ControlFlow.h"

// Abstract base class for different kinds of analyses
// Contains only information that is specific for one type of analysis
// but is independent from the program that is analyzed.

// param T is the type on which the analysis operated (e.g. a set of expressions
// for available expressions analysis)
template<typename T> class MFramework
{
    public:
        //MFramework();
        //virtual ~MFramework();

        // methods defining the lattice
        virtual T join(T&,T&) = 0;
        virtual T top() = 0;
        virtual T bottom() = 0;
        virtual bool lessThan(T&, T&) = 0;

        // transfer function
        virtual T f(T&, CPPParser::Statement*) = 0;

        // initial value for extremal labels
        virtual T getExtremalValue() = 0;

        virtual std::set<int> getExtremalLabels() = 0;
        virtual std::set<int> getNext(int) = 0;

        virtual std::vector<CPPParser::Statement*> getLabels() {return cflow->getLabels();}
    protected:
        ControlFlow* cflow;
    private:
};

#endif // MFRAMEWORK_H

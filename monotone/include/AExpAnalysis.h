#ifndef AEXPANALYSIS_H
#define AEXPANALYSIS_H

#include <MFramework.h>
#include <set>
#include <string>
#include <ControlFlow.h>
#include <../../C++ Parser/ParserSemantics.h>

// Available expressions analysis is one instance of a monotone framework
// implemented for testing reasons because of its relative simplicity.

// type param T is instantiated to be a set of expressions, where the expressions are
// represented as strings. I is instantated to be a Statement (TODO: placeholder, should be replaced
// by the actual type used by the parser)
class AExpAnalysis : public MFramework<std::set<std::string>, CPPParser::Statement>
{
    public:
        AExpAnalysis(ControlFlow<CPPParser::Statement>&);
        virtual ~AExpAnalysis();
        std::set<std::string> top();
        std::set<std::string> bottom();
        std::set<std::string> join(std::set<std::string>&, std::set<std::string>&);
        bool lessThan(std::set<std::string>&, std::set<std::string>&);
        std::set<std::string> f (std::set<std::string>&, CPPParser::Statement*);
        std::set<std::string> getExtremalValue();
    protected:
    private:
        std::set<std::string> aexp;
};

#endif // AEXPANALYSIS_H

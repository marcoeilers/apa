/*
 * EMFramework.h
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

#ifndef EMFRAMEWORK_H_
#define EMFRAMEWORK_H_

#include "InterControlFlow.h"


class EMFError {
private:
	std::string message;
public:
	EMFError() {message = "Unknown error!";}
	EMFError(std::string m) : message(m) {}
	std::string getMessage() { return message;}
};

/*
 * Abstract superclass for all embellished monotone frameworks.
 *
 * Since freturn takes two arguments and fcall takes only one, this
 * is the version for forward analyses.
 *
 * Parameter T is the data type on which the lattice operates,
 * This class does NOT know about context, that is handled in MVP,
 */
template<typename T> class EMFramework
{
public:

       // methods defining the lattice
       virtual T join(T&,T&) = 0;
       virtual T bottom() = 0;
       virtual bool lessOrEqual(T&, T&) = 0;

       // transfer functions
       virtual T f(T&, int) = 0;
       virtual T fcall(T&, int, CPPParser::FunctionDeclaration*) = 0;
       virtual T fenter(T& t) = 0;
       virtual T fexit(T& t) = 0;
       virtual T freturn(T&, T&, int) = 0;


       // initial value for extremal labels
       virtual T getExtremalValue() = 0;

       // set of extremal (i.e. first) labels, usually a singleton set
       virtual std::set<int> getExtremalLabels() = 0;

       // gets the set of labels following label l
       // (including interprocedural transitions)
       virtual std::set<int> getNext(int l) { return cflow->getNext(l); }

       // gets the type of a label (e.g. call, enter, exit, return, default)
       virtual LabelType getLabelType(int label) {return cflow->getType(label);}

       // gets the respective call label that belongs to a return
       virtual int getCallFromReturn(int label) {return cflow->getCallForReturn(label); }

       // gets the respective return label that belongs to a call
       virtual int getReturnFromCall(int label) {return cflow->getReturnForCall(label); }

       // list of all statements sorted by their labels
       virtual std::vector<CPPParser::Statement*> getLabels() {return cflow->getLabels();}

       virtual CPPParser::Program* getProg() { return cflow->getProg(); }

       // creates a string describing an object of the lattice's type
       virtual std::string toString(T& t) { return ""; }
protected:
       // information about the analyzed program
       InterControlFlow* cflow;
   private:
};




#endif /* EMFRAMEWORK_H_ */

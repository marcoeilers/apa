/*
 * EMFramework.h
 *
 *  Created on: May 27, 2013
 *      Author: marco
 */

#ifndef EMFRAMEWORK_H_
#define EMFRAMEWORK_H_

#include "InterControlFlow.h"


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
       virtual bool lessThan(T&, T&) = 0;

       // transfer functions
       virtual T f(T&, CPPParser::Statement*) = 0;
       virtual T fcall(T&, CPPParser::Statement*, CPPParser::FunctionDeclaration*) = 0;
       virtual T fenter(T& t) = 0;
       virtual T fexit(T& t) = 0;
       virtual T freturn(T&, T&, CPPParser::Statement*) = 0;


       // initial value for extremal labels
       virtual T getExtremalValue() = 0;

       // set of extremal (i.e. first) labels, usually a singleton set
       virtual std::set<int> getExtremalLabels() = 0;
       virtual std::set<int> getNext(int l) { return cflow->getNext(l); }

       virtual LabelType getLabelType(int label) {return cflow->getType(label);}

       virtual int getCallFromReturn(int label) {return cflow->getCallForReturn(label); }

       virtual std::vector<CPPParser::Statement*> getLabels() {return cflow->getLabels();}

       virtual CPPParser::Program* getProg() { return cflow->getProg(); }
protected:
       InterControlFlow* cflow;
   private:
};




#endif /* EMFRAMEWORK_H_ */

/*
 * EMFramework.h
 *
 *  Created on: May 27, 2013
 *      Author: marco
 */

#ifndef EMFRAMEWORK_H_
#define EMFRAMEWORK_H_

#include "InterControlFlow.h"

template<typename T> class EMFramework
{
public:
       //MFramework();
       //virtual ~MFramework();

       // methods defining the lattice
       virtual T join(T&,T&) = 0;
       virtual T top() = 0;
       virtual T bottom() = 0;
       virtual bool lessThan(T&, T&) = 0;

       // transfer functions
       virtual T f(T&, CPPParser::Statement*) = 0;
       virtual T fcall(T&, CPPParser::Statement*, CPPParser::FunctionDeclaration*) = 0;
       virtual T fenter(T&, CPPParser::FunctionDeclaration*) = 0;
       virtual T fexit(T&, CPPParser::Return*) = 0;
       virtual T freturn(T&, CPPParser::Statement*) = 0;


       // initial value for extremal labels
       virtual T getExtremalValue() = 0;

       virtual std::set<int> getExtremalLabels() = 0;
       virtual std::set<int> getNext(int) = 0;

       virtual std::vector<CPPParser::Statement*> getLabels() {return cflow->getLabels();}
       virtual LabelType getLabelType(int label) {return cflow->getType(label);}

       virtual CPPParser::Program* getProg();
protected:
       InterControlFlow* cflow;
   private:
};




#endif /* EMFRAMEWORK_H_ */

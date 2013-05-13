#include <iostream>
#include <AExpAnalysis.h>
#include <MFP.h>
#include <../../C++ Parser/ParserSemantics.h>

using namespace std;

int main()
{
    cout << "Hello world!" << endl;
    ControlFlow<CPPParser::Statement>* cf = new ControlFlow<CPPParser::Statement>();
    AExpAnalysis* a = new AExpAnalysis(*cf);
    MFP<set<string>, CPPParser::Statement> * solver = new MFP<set<string>, CPPParser::Statement>();

    return 0;
}



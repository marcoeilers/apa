#include <iostream>
#include <AExpAnalysis.h>
#include <MFP.h>
#include <../../C++ Parser/ParserSemantics.h>

using namespace std;

int main()
{
    cout << "Hello world!" << endl;
    CPPParser::FunctionDeclaration fd;
    ControlFlow* cf = new ControlFlow(fd);
    AExpAnalysis* a = new AExpAnalysis(*cf);
    MFP<set<string> > * solver = new MFP<set<string> >();

    return 0;
}



#include <iostream>
#include <AExpAnalysis.h>
#include <MFP.h>
#include <../../C++ Parser/ParserSemantics.h>

using namespace std;

int main()
{
    cout << "Hello world!" << endl;
    ControlFlow* cf = new ControlFlow();
    AExpAnalysis* a = new AExpAnalysis(*cf);
    MFP<set<string>> * solver = new MFP<set<string>>();

    return 0;
}



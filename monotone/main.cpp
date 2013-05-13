#include <iostream>
#include <AExpAnalysis.h>
#include <MFP.h>

using namespace std;

int main()
{
    cout << "Hello world!" << endl;
    AExpAnalysis* a = new AExpAnalysis();
    ControlFlow<Statement>* cf = new ControlFlow<Statement>();
    MFP<set<string>, Statement> * solver = new MFP<set<string>, Statement>();

    return 0;
}



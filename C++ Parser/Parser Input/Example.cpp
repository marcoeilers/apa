#include "MyHeader.h"
#include "MyOtherHeader.h"

// This is a comment
// Only simple condition are allowed now: if (i < 4), while (false), etc. No things like if (i > 9 && a > b)

int main() {
	int i = 8; // Comment behind vars
	char j = 0;
	int * pointer; // Integer pointer
	float ** pointer; // Pointer to pointer
	// int * * pointer; // This is illegal, * symbols must be sequential
	while(i > 8) {
		i = i - 1;
	}
	while (true) sayHi();
	if (false != true) i = 0;
	func120(i);
}

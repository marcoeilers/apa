#include "MyHeader.h"
#include "MyOtherHeader.h"

// This is a comment
// Note: Only simple comments are allowed right now, so no /* */ comments.

int main() {
	int i = 8; // Comment behind vars
	char j = 0;
	int * pointer; // Integer pointer
	float ** pointer; // Pointer to pointer
	// int * * pointer; // This is illegal, * symbols must be sequential
	while(i > 8) {
		i = i - 1;
	}
	if (true && (i == 1 && i != 0)) i = 0;
	func120(i);
}

#include "StringUtil.h"
#include <string>

namespace Utils {

std::string& StringUtil::trim(std::string& s, bool begin, bool end) {
	int max = s.size();
	if (max == 0) return s;

	int index;
	if (begin) {
		// Remove front spaces:
		index = 0;
		while (s[index] == ' ' || s[index] == '\t') {
			index++;
			if (index == max) {
				// String was spaces only
				s = "";
				return s;
			}
		}
		s = s.substr(index);
	}
	if (end) {
		// Remove rear spaces:
		index = s.size()-1;
		while (s[index] == ' ' || s[index] == '\t')
			index--; // Note: We can never reach index 0
		s = s.substr(0, index+1);
	}
	return s;
}

void StringUtil::removeComments(std::string& string) {
	int index = string.find("//");
	if (index == -1) return;
	int index2 = string.find("\r", index+1);
	int index3 = string.find("\n", index+1);
	if (index2 == -1) index2 = index3;
	else if (index3 != -1 && index3 < index2) index2 = index3;
	if (index2 == -1) {
		// Both index2 and index3 were -1, so there's no return in this line, the comment extends to the end.
		string = string.substr(0, index);
		removeComments(string);
		return;
	} else {
		// index2 is now the end of the comment.
		if (index2 == string.size()-1) {
			string = string.substr(0, index); // Return is at the end of the string
			removeComments(string);
			return;
		}
		std::string begin = string.substr(0, index);
		string = begin + string.substr(index2);
		removeComments(string);
		return;
	}

}

std::vector<std::string> StringUtil::split(std::string string) {
	std::vector<std::string> result;
	StringUtil::trim(string, true, true);
	int max = string.size();
	if (max == 0) return result;
	// We now know there's at least a word.
	// Remove any double spaces:
	bool found;
	do {
		found = false;
		int i = string.find("  ");
		int l = 1;
		if (i == -1) i = string.find(" \t");
		if (i == -1) i = string.find("\t ");
		if (i != -1) {
			found = true;
			string.replace(i, 2, " ");
		}
		i = string.find("\t");
		if (i != -1) {
			found = true;
			string.replace(i, 1, " ");
		}
	} while(found);

	// Find next space:
	int index1 = 0, index2;
	while (true) {
		index2 = string.find(' ', index1 + 1);
		result.push_back(string.substr(index1, index2-index1));
		if (index2 == -1) break;
		index1 = index2 + 1;
	}
	return result;
}
std::string StringUtil::ftos(float f)  {
	char b[30];
	sprintf(b, "%f", f);
	int i = 0;
	for (; i < 30; i++)
		if (b[i] == '\0') {
			do
				i--;
			while(b[i] == '0');
			if (b[i] == '.') b[i+2] = '\0'; // 0 after .
			else b[i+1] = '\0'; // 0 after any other number than 0, still delete the 0.
			break;
		}
	return std::string(b);
}

}
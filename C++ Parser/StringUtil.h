/*
 * StringUtil.h
 *
 *      Author: Bas in het Veld (3710971)
 *              Marco Eilers (F121763)
 *
 */

#ifndef NIGHTFALL_STRINGUTIL_H_
#define NIGHTFALL_STRINGUTIL_H_

#include <cstdlib>
#include <cstdio>
#include <vector>
#include <string>

namespace Utils {

class StringUtil {
public:
	/** Removes all spaces and tabs from the beginning and the end of a string, and returns a reference to that string (but also modifies the original string).
        @param
            s The string to trim.
		 @param
			begin Whether to trim the beginning
		 @param
			end Whether to trim the end
    */
	static std::string& trim(std::string& string, bool begin = true, bool end = true);
	/**	Removes all comments from a string.
	*/
	static void removeComments(std::string& string);
	/** Split up a string into a vector of string, divided by the spaces in it.
		Example: "Hello World" becomes ["Hello", "World"].
		@param
			string The string to split
		@return
			A vector of strings
	 */
	static std::vector<std::string> split(std::string string);
	/** Converts a float to a string that has at most one 0 at the end, like 1.0.
		@param
			f The float to convert to string
	*/
	static std::string ftos(float f);
};

}

#endif

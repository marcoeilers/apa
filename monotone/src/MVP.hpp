#include <sstream>

using namespace std;

template<typename T>
MVP<T>::MVP(int limit) {
	k = limit;
}

template<typename T>
MVP<T>::~MVP() {
	// Auto-generated destructor stub
}

template<typename T>
pair<map<string, T>, map<string, T> >* MVP<T>::solve(EMFramework<T>* mf) {
	map<string, T> *result = new map<string, T>[mf->getLabels().size()];

	for (int i = 0; i < mf->getLabels().size(); i++) {
		if (mf->getExtremalLabels().count(i)) {
			// for extremal labels, assign extremal value for empty context
			result[i][""] = mf->getExtremalValue();
		}
	}

	// worklist contains of pairs of label and context
	set<pair<int, string> > workList;
	set<int>::iterator setIt;
	for (setIt = mf->getExtremalLabels().begin();
			setIt != mf->getExtremalLabels().end(); setIt++) {
		// insert extremal labels with empty context into workList
		pair<int, string> p(*setIt, "");
		workList.insert(p);
	}

	// while there is stuff to do
	while (!workList.empty()) {
		// current.first is the label we are working on,
		// current.second is the current call string
		pair<int, string> current = *(workList.begin());

		// get all possible next labels
		set<int> next = mf->getNext(current.first);

		workList.erase(current);

		// for each subsequent label
		set<int>::iterator it;
		for (it = next.begin(); it != next.end(); it++) {
			// s is the current statement
			CPPParser::Statement* s = mf->getLabels().at(current.first);

			// what we do now depends on the type of the current statement
			switch (mf->getLabelType(current.first)) {
			case LABEL_DEFAULT: {
				// if it is a normal statement, i.e. no function call or return

				// get current value
				T old = getResult(result, current.first, current.second, mf);

				// apply transfer function
				T iterated = mf->f(old, s);

				// if changed
				if (!mf->lessThan(iterated, result[*it][current.second])) {
					// update with join of old and new
					result[*it][current.second] = mf->join(
							result[*it][current.second], iterated);

					// add next labels with current context to work list
					set<int> toRevisit = mf->getNext(*it);

					set<int>::iterator trIt;
					for (trIt = toRevisit.begin(); trIt != toRevisit.end();
							trIt++) {
						pair<int, string> workItem(*it, current.second);
						workList.insert(workItem);
					}
				}
				break;
			}
			case LABEL_CALL: {
				// for a function call

				// calculate the context after the call
				string newContext = prepend(current.first, current.second);

				// find the declaration of the called function
				CPPParser::FunctionDeclaration* calledFun;
				CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
				vector<CPPParser::FunctionDeclaration>::iterator funIt;
				for (funIt = mf->getProg()->functionDeclarations.begin();
						funIt != mf->getProg()->functionDeclarations.end();
						funIt++) {
					if (funIt->name.compare(fc->name) == 0) {
						calledFun = &(*funIt);
						break;
					}
				}

				// get current value
				T old = getResult(result, current.first, current.second, mf);

				// apply transfer function fcall
				T iterated = mf->fcall(old, s, calledFun);

				// as before
				if (!mf->lessThan(iterated, result[*it][newContext])) {
					result[*it][newContext] = mf->join(result[*it][newContext],
							iterated);

					set<int> toRevisit = mf->getNext(*it);

					// add next labels with NEW context to work list
					set<int>::iterator trIt;
					for (trIt = toRevisit.begin(); trIt != toRevisit.end();
							trIt++) {
						pair<int, string> workItem(*it, newContext);
						workList.insert(workItem);
					}
				}
				break;
			}
			case LABEL_ENTER: {
				// enter is like default
				T old = getResult(result, current.first, current.second, mf);

				// except we use fenter as the transfer function
				T iterated = mf->fenter(old);
				if (!mf->lessThan(iterated, result[*it][current.second])) {
					result[*it][current.second] = mf->join(
							result[*it][current.second], iterated);
					set<int> toRevisit = mf->getNext(*it);

					set<int>::iterator trIt;
					for (trIt = toRevisit.begin(); trIt != toRevisit.end();
							trIt++) {
						pair<int, string> workItem(*it, current.second);
						workList.insert(workItem);
					}
				}
				break;
			}
			case LABEL_EXIT: {
				// exit is like default
				T old = getResult(result, current.first, current.second, mf);

				// except we use fexit as the transfer function
				T iterated = mf->fexit(old);
				if (!mf->lessThan(iterated, result[*it][current.second])) {
					result[*it][current.second] = mf->join(
							result[*it][current.second], iterated);
					set<int> toRevisit = mf->getNext(*it);

					set<int>::iterator trIt;
					for (trIt = toRevisit.begin(); trIt != toRevisit.end();
							trIt++) {
						pair<int, string> workItem(*it, current.second);
						workList.insert(workItem);
					}
				}
				break;
			}
			case LABEL_RETURN: {
				// okay, this one is complicated

				// get the label from which this function was called
				int callLbl = mf->getCallFromReturn(current.first);

				// iterate through all existing contexts at the call label
				typename map<string, T>::iterator conIt;
				for (conIt = result[callLbl].begin();
						conIt != result[callLbl].end(); conIt++) {
					// for each such context, calculate the new context that would result from the jump
					string callContext = prepend(callLbl, conIt->first);

					// if this is the context we are currently working on
					if (callContext.compare(current.second) == 0) {

						// oldContext is the context BEFORE the call was made
						// remember, current.second is the context inside the call
						string oldContext = conIt->first;

						// get the value before the call
						T beforeCall = getResult(result, callLbl, oldContext,
								mf);

						// and the current one, i.e. the one at the return
						T afterFunc = getResult(result, current.first,
								current.second, mf);

						// apply transfer function freturn
						T iterated = mf->freturn(beforeCall, afterFunc, s);

						// if changed
						if (!mf->lessThan(iterated,
								result[*it][oldContext])) {
							// add to the result with the context from before the call
							result[*it][oldContext] = mf->join(
									result[*it][oldContext], iterated);
							set<int> toRevisit = mf->getNext(*it);

							// add all next labels with context from before the call
							set<int>::iterator trIt;
							for (trIt = toRevisit.begin();
									trIt != toRevisit.end(); trIt++) {
								pair<int, string> workItem(*it, oldContext);
								workList.insert(workItem);
							}
						}
					}
				}
				break;
			}

			}
		}
	}

	// phew, now apply the transfer function one more time for each label to get the effect value
	pair<map<string, T>, map<string, T> >* final = new pair<map<string, T>,
			map<string, T> > [mf->getLabels().size()];

	// for each label
	for (int i = 0; i < mf->getLabels().size(); i++) {
		// the context value is what we have already calculated
		map<string, T> context = result[i];
		map<string, T> effect;

		typename map<string, T>::iterator finalIt;

		// again, depending on the type of the label
		switch (mf->getLabelType(i)) {
		case LABEL_CALL: {
			// for a call

			// again, get the declaration of the called function
			CPPParser::FunctionDeclaration* calledFun;

			CPPParser::FunctionCall* fc =
					(CPPParser::FunctionCall*) mf->getLabels().at(i);
			vector<CPPParser::FunctionDeclaration>::iterator funIt;
			for (funIt = mf->getProg()->functionDeclarations.begin();
					funIt != mf->getProg()->functionDeclarations.end();
					funIt++) {
				if (funIt->name.compare(fc->name) == 0) {
					calledFun = &(*funIt);
					break;
				}
			}

			// apply fcall
			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {
				effect[finalIt->first] = mf->fcall(context[finalIt->first],
						mf->getLabels().at(i), calledFun);
			}
			break;
		}
		case LABEL_DEFAULT:
			// for default, enter and exit, just apply the transfer function and
			// save the result in effect
			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {
				effect[finalIt->first] = mf->f(context[finalIt->first],
						mf->getLabels().at(i));
			}
			break;
		case LABEL_ENTER:
			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {
				effect[finalIt->first] = mf->fenter(context[finalIt->first]);
			}
			break;
		case LABEL_EXIT:
			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {
				effect[finalIt->first] = mf->fexit(context[finalIt->first]);
			}
			break;
		case LABEL_RETURN: {
			// get label from which the call was made
			int callLbl = mf->getCallFromReturn(i);

			// for every context at this label
			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {

				// for every context at the call label
				typename map<string, T>::iterator conIt;
				for (conIt = result[callLbl].begin();
						conIt != result[callLbl].end(); conIt++) {
					// callContext is the context that would result from the function call
					string callContext = prepend(callLbl, conIt->first);

					// if this is the one we are currently working on
					if (finalIt->first.compare(callContext) == 0) {
						// apply freturn , save result in effect
						T beforeCall = result[callLbl][conIt->first];
						T afterFunc = context[finalIt->first];

						T old = getResult(result, callLbl, conIt->first, mf);
						T newVal = mf->freturn(beforeCall, afterFunc,
								mf->getLabels().at(callLbl));
						effect[conIt->first] = newVal;
					}
				}

			}

			break;

		}

		}
		// add to result
		pair<map<string, T>, map<string, T> > p(context, effect);
		final[i] = p;
	}

	return final;
}

// gets the value (of type T) for a specific label and a specific context from an array of maps
// if there is no value for the selected context, returns bottom
template<typename T>
T MVP<T>::getResult(map<string, T>* result, int label, string context,
		EMFramework<T>* mf) {
	if (result[label].count(context)) {
		return result[label][context];
	} else {
		return mf->bottom();
	}
}

// prepends a label to the current call string
// cuts of the end to make the result at most k chars long
template<typename T>
string MVP<T>::prepend(int label, string context) {
	stringstream ss;
	ss << label;
	ss << context;
	string result = ss.str();
	result.resize(k);
	return result;
}


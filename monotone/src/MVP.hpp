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
	map<string, T> *result = new map<string, T> [mf->getLabels().size()];
	for (int i = 0; i < mf->getLabels().size(); i++) {
		if (mf->getExtremalLabels().count(i)) {

			result[i][""] = mf->getExtremalValue();
		}

	}

	set<pair<int, string> > workList;
	set<int>::iterator setIt;
	for (setIt = mf->getExtremalLabels().begin();
			setIt != mf->getExtremalLabels().end(); setIt++) {
		pair<int, string> p(*setIt, "");
		workList.insert(p);
	}

	while (!workList.empty()) {
		pair<int, string> current = *(workList.begin());

		set<int> next = mf->getNext(current.first);

		workList.erase(current);
		set<int>::iterator it;
		for (it = next.begin(); it != next.end(); it++) {
			CPPParser::Statement* s = mf->getLabels().at(current.first);

			switch (mf->getLabelType(current.first)) {
			case LABEL_DEFAULT: {

				T old = getResult(result, current.first, current.second, mf);
				T iterated = mf->f(old, s);
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
			case LABEL_CALL: {
				string newContext = prepend(current.first, current.second);

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

				T old = getResult(result, current.first, current.second, mf);
				T iterated = mf->fcall(old, s, calledFun);
				if (!mf->lessThan(iterated, result[*it][newContext])) {
					result[*it][newContext] = mf->join(result[*it][newContext],
							iterated);
					set<int> toRevisit = mf->getNext(*it);

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

				T old = getResult(result, current.first, current.second, mf);
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

				T old = getResult(result, current.first, current.second, mf);
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
				int callLbl = mf->getCallFromReturn(current.first);

				typename map<string, T>::iterator conIt;
				for (conIt = result[callLbl].begin();
						conIt != result[callLbl].end(); conIt++) {
					string callContext = prepend(callLbl, conIt->first);
					if (callContext.compare(current.second) == 0) {
						string oldContext = conIt->first;
						T beforeCall = getResult(result, callLbl, oldContext,
								mf);

						T afterFunc = getResult(result, current.first,
								current.second, mf);

						T iterated = mf->freturn(beforeCall, afterFunc, s);

						if (!mf->lessThan(iterated,
								result[*it][current.second])) {
							result[*it][current.second] = mf->join(
									result[*it][current.second], iterated);
							set<int> toRevisit = mf->getNext(*it);

							set<int>::iterator trIt;
							for (trIt = toRevisit.begin();
									trIt != toRevisit.end(); trIt++) {
								pair<int, string> workItem(*it, current.second);
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

	pair<map<string, T>, map<string, T> >* final = new pair<map<string, T>,
			map<string, T> > [mf->getLabels().size()];

	// apply one last time
	for (int i = 0; i < mf->getLabels().size(); i++) {
		map<string, T> context = result[i];
		map<string, T> effect;

		typename map<string, T>::iterator finalIt;
		switch (mf->getLabelType(i)) {
		case LABEL_CALL: {
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

			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {
				effect[finalIt->first] = mf->fcall(context[finalIt->first],
						mf->getLabels().at(i), calledFun);
			}
			break;
		}
		case LABEL_DEFAULT:
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
			int callLbl = mf->getCallFromReturn(i);

			for (finalIt = context.begin(); finalIt != context.end();
					finalIt++) {

				typename map<string, T>::iterator conIt;
				for (conIt = result[callLbl].begin();
						conIt != result[callLbl].end(); conIt++) {
					string callContext = prepend(callLbl, conIt->first);

					if (finalIt->first.compare(callContext) == 0) {

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
		pair<map<string, T>, map<string, T> > p(context, effect);
		final[i] = p;
	}

	return final;
}

template<typename T>
T MVP<T>::getResult(map<string, T>* result, int label, string context,
		EMFramework<T>* mf) {
	if (result[label].count(context)) {
		return result[label][context];
	} else {
		return mf->bottom();
	}
}

template<typename T>
string MVP<T>::prepend(int label, string context) {
	stringstream ss;
	ss << label;
	ss << context;
	string result = ss.str();
	result.resize(k);
	return result;
}


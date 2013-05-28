#include <sstream>

using namespace std;

template<typename T>
MVP<T>::MVP(int limit) {
	k = limit;
}

template<typename T>
MVP<T>::~MVP() {
	// TODO Auto-generated destructor stub
}

template<typename T>
map<string, T>* MVP<T>::solve(EMFramework<T>* mf) {
	T * result = new T[mf->getLabels().size()];
	for (int i = 0; i < mf->getLabels().size(); i++) {
		if (mf->getExtremalLabels().count(i)) {

			result[i] = mf->getExtremalValue();
		} else {
			result[i] = mf->bottom();
		}

	}

	set<pair<int, string> > workList;
	for (int i = 0; i < mf->getLabels().size(); i++) {
		pair<int, string> p(i, "");
		workList.insert(p);
	}

	while (!workList.empty()) {
		pair<int, string> current = *(workList.begin());

		// TODO: next has to get all next labels
		set<int> next = mf->getNext(current);
		workList.erase(current);
		set<int>::iterator it;
		for (it = next.begin(); it != next.end(); it++) {
			CPPParser::Statement* s = mf->getLabels().at(current.first);

			switch (mf->getLabelType(current.first)) {
			case LABEL_DEFAULT: {
				T iterated = mf->f(result[current.first][current.second], s);
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

				// TODO other call
				CPPParser::FunctionCall* fc = (CPPParser::FunctionCall*) s;
				vector<CPPParser::FunctionDeclaration>::iterator funIt;
				for (funIt = mf->getProg()->functionDeclarations.begin();
						funIt != mf->getProg()->functionDeclarations.end();
						funIt++) {
					if (funIt->name.compare(fc->name) == 0) {
						fc = it;
						break;
					}
				}

				T iterated = mf->fcall(result[current.first][current.second], s,
						fc);
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
				T iterated = mf->fenter(result[current.first][current.second]);
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
				CPPParser::Return* r = (CPPParser::Return*) s;

				T iterated = mf->fexit(result[current.first][current.second],
						r);
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
				for (conIt = result[current.first].begin();
						conIt != result[current.first].end(); conIt++) {
					string callContext = prepend(callLbl, conIt->first);

					if (callContext.compare(callContext) == 0) {
						string oldContext = conIt->first;

						T iterated = mf->freturn(result[callLbl][oldContext],
								result[current.first][current.second], s);
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

	//return result;
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


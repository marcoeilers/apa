/*
 * MFP.hpp
 *
 *      Author: Marco Eilers (F121763)
 *              Bas in het Veld (3710971)
 *
 */

using namespace std;

template<typename T>
MFP<T>::MFP() {

}

template<typename T>
MFP<T>::~MFP() {

}

template<typename T>

pair<T, T> * MFP<T>::solve(MFramework<T>* mf) {
	// initialize
	T * result = new T[mf->getLabels().size()];
	for (int i = 0; i < mf->getLabels().size(); i++) {
		if (mf->getExtremalLabels().count(i)) {
			// extremal labels get extremal values
			result[i] = mf->getExtremalValue();
		} else {
			// others get bottom
			result[i] = mf->bottom();
		}

	}

	// put each label in the worklist once
	set<int> workList;
	for (int i = 0; i < mf->getLabels().size(); i++) {
		workList.insert(i);
	}

	// perform fixpoint iteration
	while (!workList.empty()) {
		int current = *(workList.begin());
		set<int> next = mf->getNext(current);
		workList.erase(current);

		// for all next labels
		set<int>::iterator it;
		for (it = next.begin(); it != next.end(); it++) {
			// apply transfer function
			T iterated = mf->f(result[current], current);

			// if there is a change
			if (!mf->lessOrEqual(iterated, result[*it])) {
				// join with old value
				result[*it] = mf->join(result[*it], iterated);

				// add next label to work list
				workList.insert(*it);
			}
		}
	}

	// now we have the context values.
	// to get the effect values, apply the transfer function one more
	// time for each label
	pair<T, T>* final = new pair<T, T> [mf->getLabels().size()];

	for (int i = 0; i < mf->getLabels().size(); i++) {
		T context = result[i];
		T effect = mf->f(result[i], i);
		pair<T, T> p(context, effect);
		final[i] = p;
	}

	// print results
	for (int i = 0; i < mf->getLabels().size(); i++){
		printf("For label %i:\n", i);

		printf("Context value:\n%s\n", mf->toString(final[i].first).c_str());

		printf("Effect value:\n%s\n", mf->toString(final[i].second).c_str());
	}

	return final;
}

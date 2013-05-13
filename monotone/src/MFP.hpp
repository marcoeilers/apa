using namespace std;

template<typename T, typename I>
MFP<T, I>::MFP()
{

}

template<typename T, typename I>
MFP<T, I>::~MFP()
{

}


template<typename T, typename I>

T * MFP<T, I>::solve(MFramework<T, I>& mf, ControlFlow<I> cf)
{

    T * result = new T[cf.getLabels().size()];
    for (int i = 0; i<cf.getLabels().size(); i++)
    {
        if (cf.getExtremalLabels().find(i) != cf.getExtremalLabels().end())
            result[i] = mf->getExtremalValue();
        else
            result[i] = mf->bottom();
    }

    set<int> workList;
    for (int i = 0;
            i<cf.getLabels().size();
            i++)
    {
        workList.insert(i);
    }

    while(!workList.empty())
    {
        int current = workList.begin();
        set<int> next = cf.getNext(current);
        workList.erase(current);
        set<int>::iterator it;
        for (it = workList.begin(); it != workList.end(); it++)
        {
            T iterated = mf->f(result[current], cf.getLabels().at(current));
            if (!mf->lessThan(iterated, result[*it]))
            {
                result[*it] = mf->join(result[*it], iterated);
                set<int> toRevisit = cf.getNext(*it);
                workList.insert(toRevisit.begin(), toRevisit.end());
            }
        }
    }

    return result;
}

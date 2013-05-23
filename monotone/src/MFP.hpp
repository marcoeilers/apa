using namespace std;

template<typename T>
MFP<T>::MFP()
{

}

template<typename T>
MFP<T>::~MFP()
{

}


template<typename T>

T * MFP<T>::solve(MFramework<T>* mf)
{

    T * result = new T[mf->getLabels().size()];
    for (int i = 0; i<mf->getLabels().size(); i++)
    {
        if (mf->getExtremalLabels().count(i))
        {

            result[i] = mf->getExtremalValue();
        }
        else
        {
            result[i] = mf->bottom();
        }

    }

    for (int i = 0; i<mf->getLabels().size(); i++)
    {
        CPPParser::Statement* s= mf->getLabels().at(i);
    }

    set<int> workList;
    for (int i = 0;
            i<mf->getLabels().size();
            i++)
    {
        workList.insert(i);
    }

    while(!workList.empty())
    {
        int current = *(workList.begin());
        set<int> next = mf->getNext(current);
        workList.erase(current);
        set<int>::iterator it;
        for (it = next.begin(); it != next.end(); it++)
        {
            T iterated = mf->f(result[current], mf->getLabels().at(current));
            if (!mf->lessThan(iterated, result[*it]))
            {
                result[*it] = mf->join(result[*it], iterated);
                set<int> toRevisit = mf->getNext(*it);

                workList.insert(toRevisit.begin(), toRevisit.end());
            }
        }
    }

    return result;
}

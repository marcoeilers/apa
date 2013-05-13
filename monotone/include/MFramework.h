#ifndef MFRAMEWORK_H
#define MFRAMEWORK_H

// Abstract base class for different kinds of analyses
// Contains only information that is specific for one type of analysis
// but is independent from the program that is analyzed.

// param T is the type on which the analysis operated (e.g. a set of expressions
// for available expressions analysis)
// param I is the type of a statement/an instruction. Could be hard coded to be
// the statement coming from our parser.
template<typename T, typename I> class MFramework
{
    public:
        //MFramework();
        //virtual ~MFramework();

        // methods defining the lattice
        virtual T join(T,T) = 0;
        virtual T top() = 0;
        virtual T bottom() = 0;
        virtual bool lessThan(T, T) = 0;

        // transfer function
        virtual T f(T, I) = 0;

        // initial value for extremal labels
        virtual T getExtremalValue() = 0;
    protected:
    private:
};

#endif // MFRAMEWORK_H

#ifndef NOVELTY_HEURISTIC_H
#define NOVELTY_HEURISTIC_H

#include "../heuristic.h"
#include "state_var_t.h"
#include "state.h"

class NoveltyHeuristic : public Heuristic {
	Heuristic* novelty_heuristic;
    bool solution_found_by_heuristic;
    std::vector<std::vector<int> > novelty_per_variable_value;

protected:
    virtual void initialize();
    virtual int compute_heuristic(const State &state);
public:
    NoveltyHeuristic(const Options &options);
    ~NoveltyHeuristic();

    virtual bool found_solution();
    virtual const std::vector<const Operator*>& get_solution() const;
};

#endif

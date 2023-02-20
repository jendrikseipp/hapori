#include "novelty_heuristic.h"

#include "option_parser.h"
#include "plugin.h"
#include "globals.h"

using namespace std;

NoveltyHeuristic::NoveltyHeuristic(const Options &opts)
    : Heuristic(opts), novelty_heuristic(opts.get<Heuristic *>("eval")),
        solution_found_by_heuristic(false)
{
}

NoveltyHeuristic::~NoveltyHeuristic() {
}

void NoveltyHeuristic::initialize() {
    cout << "Initializing novelty heuristic..." << endl;
    // Setting the value to DEAD_END initially.
    novelty_per_variable_value.assign(g_variable_domain.size(), std::vector<int>());
    for (size_t var = 0; var < g_variable_domain.size(); ++var) {
        novelty_per_variable_value[var].assign(g_variable_domain[var], DEAD_END);
    }
}

int NoveltyHeuristic::compute_heuristic(const State &state) {
    solution_found_by_heuristic = false;
    novelty_heuristic->evaluate(state);
    int heuristic_value = novelty_heuristic->get_value();
    if (heuristic_value == DEAD_END)
        return DEAD_END;

    int strictly_better_novelty_facts_estimate = 0;
    int strictly_worse_novelty_facts_estimate = 0;
    for (int var = 0; var < g_variable_domain.size(); ++var) {
        state_var_t value = state[var];
        int curr_value = novelty_per_variable_value[var][value];
        if (curr_value == DEAD_END || curr_value > heuristic_value) {
            novelty_per_variable_value[var][value] = heuristic_value;
            strictly_better_novelty_facts_estimate++;
        } else if (curr_value < heuristic_value) {
            strictly_worse_novelty_facts_estimate++;
        }
    }
    int ret = g_variable_domain.size();
    if (strictly_better_novelty_facts_estimate > 0) {
        ret -= strictly_better_novelty_facts_estimate;
    } else {
        ret += strictly_worse_novelty_facts_estimate;
    }
    return ret;
}

bool NoveltyHeuristic::found_solution() {
    if (novelty_heuristic->found_solution()) {
        solution_found_by_heuristic = true;
        return true;
    }
    return false;
}

const std::vector<const Operator*>& NoveltyHeuristic::get_solution() const {
      if (solution_found_by_heuristic)
            return novelty_heuristic->get_solution();
      return Heuristic::get_solution();
}

static ScalarEvaluator *_parse(OptionParser &parser) {
    Heuristic::add_options_to_parser(parser);
    parser.add_option<Heuristic *>("eval");

    Options opts = parser.parse();
    if (parser.dry_run())
        return 0;
    else {
        return new NoveltyHeuristic(opts);
    }
}

static Plugin<ScalarEvaluator> _plugin("novelty", _parse);

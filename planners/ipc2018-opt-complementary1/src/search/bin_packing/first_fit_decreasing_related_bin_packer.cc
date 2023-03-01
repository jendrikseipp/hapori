#include "first_fit_decreasing_related_bin_packer.h"

#include <iostream>

#include "bin_packer.h"
#include "../task_proxy.h"
#include "../causal_graph.h"
#include "../utils/timer.h"
#include "../utils/math.h"

using namespace pdbs;
using namespace std;

FirstFitDecreasingRelatedBinPacker::FirstFitDecreasingRelatedBinPacker(double pdb_size, int num_collections) :
        pdb_max_size (pdb_size),
        num_pbd_collections (num_collections) {
}

vector<Pattern> FirstFitDecreasingRelatedBinPacker::bin_packing(std::shared_ptr<TaskProxy> task_proxy) {

    vector<Pattern> pattern_collection;
    double current_size;

    //int temp = rand()%(max_target_size-min_target_size+1);
    //temp += min_target_size;

    //pdb_max_size=9*pow(10,temp);
    //pdb_max_size=min(pdb_max_size,pow(10,initial_max_target_size));
    //pdb_max_size=max(pdb_max_size,pow(10,min_target_size));

    cout << "Starting bin packing First Fit Decresing Related, pdb_max_size:" << pdb_max_size << endl;

    VariablesProxy variables = task_proxy->get_variables();

    const CausalGraph &cg = task_proxy->get_causal_graph();

    vector<pair<int,double>> remaining_vars;

    for (size_t i = 0; i < variables.size(); ++i) {
        if (variables[i].get_domain_size() <= pdb_max_size) {
            remaining_vars.push_back(make_pair(i, variables[i].get_domain_size()));
        }
    }

    sort(remaining_vars.begin(), remaining_vars.end(), compare_domain_size_variable);

    //Init pattern
    vector<bool> pattern(variables.size(), false);
    current_size = 1;

    size_t pos = 0;
    size_t current_var = 0;

    vector<vector<int>> rel_vars;

    while (remaining_vars.size() > 0) {

        if(utils::is_product_within_limit(current_size, remaining_vars[pos].second, pdb_max_size)) {

            current_var = remaining_vars[pos].first;
            current_size *= remaining_vars[pos].second;
            pattern[remaining_vars[pos].first] = true;

            remaining_vars.erase(remaining_vars.begin() + pos);

            const vector<int> &rel_vars = cg.get_eff_to_pre(current_var);

            for (size_t j = 0; j < rel_vars.size(); ++j) {
                size_t k = 0;
                while (k < remaining_vars.size()) {
                    if ((remaining_vars[k].first == rel_vars[j]) &&
                        (utils::is_product_within_limit(current_size, remaining_vars[k].second, pdb_max_size))) {
                        current_size *= remaining_vars[k].second;
                        pattern[remaining_vars[k].first] = true;
                        remaining_vars.erase(remaining_vars.begin() + k);
                        break;
                    }
                    k++;
                }
            }
        }
        else {

            //cout << pdb_max_size << endl;
            //cout << current_size << endl;

            //Add pattern (bin)
            pattern_collection.push_back(transform_to_pattern_normal_form(pattern));

            vector<int> trans_pattern=transform_to_pattern_normal_form(pattern);

            //Init pattern
            pattern.clear();
            pattern.resize(variables.size(), false);
            current_size = 1;
        }
    }

    if (current_size > 1) {
        //Add pattern (bin)
        pattern_collection.push_back(transform_to_pattern_normal_form(pattern));
        vector<int> trans_pattern=transform_to_pattern_normal_form(pattern);
    }

    sort(pattern_collection.begin(), pattern_collection.end(), compare_pattern_length);

    //cout << " binpacking time: " << utils::g_timer << " with " << pattern_collection.back().size() << endl;
    return pattern_collection;
}

#ifndef CHOOSE_SUBSET_HH
#define CHOOSE_SUBSET_HH

#include <vector>
#include "next_combination.h"
#include "blitz/array.h"
#include <iterator>

float choose_min_subset(const blitz::Array<float, 2> &table,
                         int subset_size,
                         std::vector<int> &best_subset) {

    // create list of indices for all configs (columns)
    std::vector<int> configs(table.extent(1));
    for (int i = 0; i < table.extent(1); ++i)
        configs[i] = i;

    // make sure the solution fits in result vector
    best_subset.resize(subset_size);

    float min_config = std::numeric_limits<float>::max();
    // iterate over all subsets
    do {
        float sum = 0;
        // sum over problems (rows)
        for (int prob_idx = 0; prob_idx < table.extent(0); ++prob_idx) {
            // take minimum of current subset
            float min = std::numeric_limits<float>::max();
            for (int comb_idx = 0; comb_idx < subset_size; ++comb_idx) {
                int config_idx = configs[comb_idx];
                float value = table(prob_idx, config_idx);
                if (value < min)
                    min = value;
            }
            sum += min;
        }
        if (sum <= min_config) {
            std::copy(configs.begin(), configs.begin() + subset_size, best_subset.begin());
            min_config = sum;
        }
    } while (next_combination(configs.begin(), configs.begin() + subset_size, configs.end()));
    return min_config;
}




float choose_max_subset(const blitz::Array<float, 2> &table,
                         int subset_size,
                         std::vector<int> &best_subset) {

    // create list of indices for all configs (columns)
    std::vector<int> configs(table.extent(1));
    for (int i = 0; i < table.extent(1); ++i)
        configs[i] = i;

    // make sure the solution fits in result vector
    best_subset.resize(subset_size);

    float max_config = std::numeric_limits<float>::min();
    // iterate over all subsets
    do {
        float sum = 0;
        // sum over problems (rows)
        for (int prob_idx = 0; prob_idx < table.extent(0); ++prob_idx) {
            // take maximum of current subset
            float max = std::numeric_limits<float>::min();
            for (int comb_idx = 0; comb_idx < subset_size; ++comb_idx) {
                int config_idx = configs[comb_idx];
                float value = table(prob_idx, config_idx);
                if (value > max)
                    max = value;
            }
            sum += max;
        }
        if (sum >= max_config) {
            std::copy(configs.begin(), configs.begin() + subset_size, best_subset.begin());
            max_config = sum;
        }
    } while (next_combination(configs.begin(), configs.begin() + subset_size, configs.end()));
    return max_config;
}

#endif //CHOOSE_SUBSET_HH

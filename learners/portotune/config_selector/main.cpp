#include <iostream>
#include "choose_subset.hh"

int main(int argc, char **argv) {


    blitz::Array<float, 2> table(100, 30);
    table = 0;
    std::vector<int> best_subset;
    float value = choose_best_subset(table, 10, best_subset);
    std::cout << "new best config with value " << value << ": ";
    std::copy(best_subset.begin(), best_subset.end(), std::ostream_iterator<int>(std::cout, ", "));
    std::cout << std::endl;

    return 0;
}

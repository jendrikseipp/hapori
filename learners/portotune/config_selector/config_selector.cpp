// python module
#include <boost/python.hpp>
#include "choose_subset.hh"
#include "blitz/tinyvec-et.h"
namespace bp=boost::python;
namespace ar=boost::python::numeric;

int test(int i) {
    return i*2;
}

/**
 convert result to python tuple (cost, subset indices)
*/
bp::tuple convert_result(float costs, std::vector<int> &best_subset){

    bp::list subset_list;
    for (size_t i = 0; i < best_subset.size(); ++i)
        subset_list.append(best_subset[i]);
    return bp::make_tuple(costs, subset_list);
}

/**
 convert python input to blitz array
*/
void convert_input(ar::array &array, blitz::Array<float, 2> &table){
    const bp::tuple &shape = bp::extract<bp::tuple>(array.attr("shape"));
    bp::len(shape);
    int rows = bp::extract<int>(shape[0]);
    int cols = bp::extract<int>(shape[1]);
    table.resize(rows, cols);
    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < cols; ++j)
            table(i,j) = bp::extract<float>(array[bp::make_tuple(i, j)]);
}

bp::tuple py_choose_min_subset(ar::array &array, int subset_size) {
    blitz::Array<float, 2> table;
    convert_input(array, table);
    std::vector<int> best_subset;
    float costs = choose_min_subset(table, subset_size, best_subset);
    return convert_result(costs, best_subset);
}

bp::tuple py_choose_max_subset(ar::array &array, int subset_size) {
    blitz::Array<float, 2> table;
    convert_input(array, table);
    std::vector<int> best_subset;
    float costs = choose_max_subset(table, subset_size, best_subset);
    return convert_result(costs, best_subset);
}

BOOST_PYTHON_MODULE(libselector)
{
    using namespace boost::python;
    ar::array::set_module_and_type("numpy", "ndarray");
    def("test", test);
    def("choose_min_subset", py_choose_min_subset);
    def("choose_max_subset", py_choose_max_subset);
}

// python module
#include <boost/python.hpp>
#include <boost/python/numpy.hpp>
#include "choose_subset.hh"
#include <blitz/array.h>
namespace p = boost::python;
namespace np = boost::python::numpy;
using NumpyArray = np::ndarray;

/**
 convert result to python tuple (cost, subset indices)
*/
p::tuple convert_result(float costs, std::vector<int> &best_subset){

    p::list subset_list;
    for (size_t i = 0; i < best_subset.size(); ++i)
        subset_list.append(best_subset[i]);
    return p::make_tuple(costs, subset_list);
}

/**
 convert python input to blitz array
*/
void convert_input(NumpyArray &array, blitz::Array<float, 2> &table){
    const p::tuple &shape = p::extract<p::tuple>(array.attr("shape"));
    p::len(shape);
    int rows = p::extract<int>(shape[0]);
    int cols = p::extract<int>(shape[1]);
    table.resize(rows, cols);
    for (int i = 0; i < rows; ++i)
        for (int j = 0; j < cols; ++j)
            table(i,j) = p::extract<float>(array[p::make_tuple(i, j)]);
}

p::tuple py_choose_min_subset(NumpyArray &array, int subset_size) {
    blitz::Array<float, 2> table;
    convert_input(array, table);
    std::vector<int> best_subset;
    float costs = choose_min_subset(table, subset_size, best_subset);
    return convert_result(costs, best_subset);
}

p::tuple py_choose_max_subset(NumpyArray &array, int subset_size) {
    blitz::Array<float, 2> table;
    convert_input(array, table);
    std::vector<int> best_subset;
    float costs = choose_max_subset(table, subset_size, best_subset);
    return convert_result(costs, best_subset);
}

BOOST_PYTHON_MODULE(libselector)
{
    Py_Initialize();
    np::initialize();
    p::def("choose_min_subset", py_choose_min_subset);
    p::def("choose_max_subset", py_choose_max_subset);
}

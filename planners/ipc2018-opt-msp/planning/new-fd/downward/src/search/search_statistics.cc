#include "search_statistics.h"

#include "utils/timer.h"
#include "utils/system.h"

#include "math.h"

#include <iostream>

using namespace std;


SearchStatistics::SearchStatistics() {
    expanded_states = 0;
    reopened_states = 0;
    evaluated_states = 0;
    evaluations = 0;
    generated_states = 0;
    dead_end_states = 0;
    generated_ops = 0;

    lastjump_expanded_states = 0;
    lastjump_reopened_states = 0;
    lastjump_evaluated_states = 0;
    lastjump_generated_states = 0;

    lastjump_f_value = -1;
}

void SearchStatistics::report_f_value_progress(int f) {
    if (f > lastjump_f_value) {
        lastjump_f_value = f;
        print_f_line();
        lastjump_expanded_states = expanded_states;
        lastjump_reopened_states = reopened_states;
        lastjump_evaluated_states = evaluated_states;
        lastjump_generated_states = generated_states;
    }
}

void SearchStatistics::print_f_line() const {
    cout << "f = " << lastjump_f_value
         << " [";
    pat_print(1,lastjump_f_value);
    print_basic_statistics();
    cout << "]" << endl;
}

void SearchStatistics::pat_print(int x, int y) const {
  static FILE* lisp;
  int pat_expanded;
  static int last_level = 0;
  static int second_last_level = 0;
  static int old_expanded = 0;
  if (lisp == NULL){
    lisp = fopen("estimation.lisp", "w");
    fprintf(lisp,"(");
      }
  //  fprintf(lisp,"x, %d y,%d,old,%d",x,y,old_expanded);
  if (x == 1) {
    fprintf(lisp,"(f_bound %d)",y );
    second_last_level = last_level;
    last_level = y;
    fflush(lisp);
      }
  else if (x == 2){
    if (y == 0){
      pat_expanded = 1;}
    else {
      pat_expanded = y;}
    if (old_expanded == 0){
      fprintf(lisp, "(hbf %d)",1);
      fprintf(lisp,"(InitialMemory %d)",utils::get_peak_memory_in_kb());
      }
    else {
      fprintf(lisp, "(hbf %f)",(float) pow( (double) y/old_expanded,1.0/(last_level - second_last_level)));
      fprintf(lisp, "(avg_total_time_costs_per_expanded_node %f)", utils::g_timer()/y);
      fprintf(lisp,"(Memory %d)",utils::get_peak_memory_in_kb());
    }
    old_expanded = pat_expanded;
    fflush(lisp);}
  
}

void SearchStatistics::print_basic_statistics() const {
      pat_print(2,expanded_states);
    cout << evaluated_states << " evaluated, "
         << expanded_states << " expanded, ";
      
    if (reopened_states > 0) {
        cout << reopened_states << " reopened, ";
    }
    cout << "t=" << utils::g_timer;
    cout << ", " << utils::get_peak_memory_in_kb() << " KB";
}

void SearchStatistics::print_detailed_statistics() const {
    cout << "Expanded " << expanded_states << " state(s)." << endl;
    cout << "Reopened " << reopened_states << " state(s)." << endl;
    cout << "Evaluated " << evaluated_states << " state(s)." << endl;
    cout << "Evaluations: " << evaluations << endl;
    cout << "Generated " << generated_states << " state(s)." << endl;
    cout << "Dead ends: " << dead_end_states << " state(s)." << endl;

    if (lastjump_f_value >= 0) {
        cout << "Expanded until last jump: "
             << lastjump_expanded_states << " state(s)." << endl;
        cout << "Reopened until last jump: "
             << lastjump_reopened_states << " state(s)." << endl;
        cout << "Evaluated until last jump: "
             << lastjump_evaluated_states << " state(s)." << endl;
        cout << "Generated until last jump: "
             << lastjump_generated_states << " state(s)." << endl;
    }
}

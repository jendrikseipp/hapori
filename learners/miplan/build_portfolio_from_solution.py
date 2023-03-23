# Put the MIP solution inside this file
infile = open('solution.txt', 'r')
solution_dict = {}

for line in infile:
    planner = line.split('=')[0].strip().replace('time_','').replace('_','-')
    allocated_time = int(float(line.split('=')[1].strip()))
    solution_dict[planner] = allocated_time

portfolio_schedule = reversed(sorted(solution_dict.items(), key=lambda x:x[1]))
total_time = 0
for item in portfolio_schedule:
    total_time += item[1]
    print(f"({item[1]}, ['{item[0]}']),")

print(total_time)



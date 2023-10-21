import os

from lab.tools import Properties, import_python_file

from downward.reports import PlanningReport


class PortfolioSimulator(PlanningReport):
    def __init__(self, portfolio_file, time_limit, *args, **kwargs):
        PlanningReport.__init__(self, *args, **kwargs)
        self.portfolio_file = portfolio_file
        self.temp_file = '/tmp/temp-portfolio.py'
        self.portfolio_name = os.path.basename(self.portfolio_file)
        self.time_limit = time_limit

    def get_runtimes(self):
        # Write new file that doesn't import the generic portfolio modules
        with open(self.portfolio_file) as f:
            lines = [line for line in f if 'import portfolio' not in line and
                                           'portfolio.' not in line]
        with open(self.temp_file, 'w') as f:
            f.write(''.join(lines))

        # Import filtered portfolio file
        portfolio_module = import_python_file(self.temp_file)
        runtimes = {}
        total_rel_time = sum(time for time, config in portfolio_module.CONFIGS)
        for rel_time, config in portfolio_module.CONFIGS:
             config = [part.replace(',bound=BOUND', '') for part in config]
             abs_time = (rel_time / float(total_rel_time)) * self.time_limit
             runtimes[tuple(config)] = int(abs_time)
        return runtimes

    def get_text(self):
        props = Properties()
        runtimes = self.get_runtimes()
        # Iterate over problems
        for (domain, problem), problem_runs in self.problem_runs.items():
            ext_config = 'WORK-simulate-%d-%s' % (self.time_limit, self.portfolio_name)
            run_id = '-'.join([ext_config, domain, problem])
            cost = None
            # problem_runs contains solutions of all tuned configs
            for run in problem_runs:
                config = run['commandline_config']
                # Timeout of portfolio config
                timeout = runtimes.get(tuple(config), 0)
                # Config not run by portfolio
                if not timeout:
                    continue
                # Time reqired by config to solve this problem
                run_total_time = run.get('total_time')
                # Problem not solved at all?
                if run_total_time is None:
                    continue
                if run['coverage'] == 1 and run_total_time <= timeout:
                    run_cost = run.get('cost')
                    assert run_cost is not None
                    if cost is None:
                        cost = run_cost
                    else:
                        cost = min(cost, run_cost)
            props[run_id] = {'cost': cost, 'coverage': cost is not None,
                             'config': ext_config, 'domain': domain,
                             'problem': problem}
        return str(props)

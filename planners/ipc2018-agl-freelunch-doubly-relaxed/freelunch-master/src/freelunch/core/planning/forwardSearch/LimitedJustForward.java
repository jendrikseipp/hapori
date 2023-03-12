package freelunch.core.planning.forwardSearch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import freelunch.core.planning.AbstractSolver;
import freelunch.core.planning.NonexistentPlanException;
import freelunch.core.planning.Solver;
import freelunch.core.planning.TimeoutException;
import freelunch.core.planning.model.BasicSettings;
import freelunch.core.planning.model.SasAction;
import freelunch.core.planning.model.SasParallelPlan;
import freelunch.core.planning.model.SasProblem;
import freelunch.core.utilities.Stopwatch;

public class LimitedJustForward extends AbstractSolver implements Solver {

    protected final ForwardSearchSettings settings;

	public LimitedJustForward(SasProblem problem, ForwardSearchSettings settings) {
		super(problem);
		this.settings = settings;
	}

	@Override
	public SasParallelPlan solve() throws TimeoutException,	NonexistentPlanException {
		Stopwatch time = new Stopwatch();
		List<SasAction> plan = new ArrayList<SasAction>();
		int iterations = 0;
		int canUse;
        mainLoop:
        for (canUse = 1; true; canUse++) {
        	//System.out.print(", " + canUse);
            int[] state = Arrays.copyOf(initialState, initialState.length);
            Set<SasAction> applicableActions = getApplicableActions(state);
            int[] actionsUsed = new int[problem.getOperators().size()];
            Arrays.fill(actionsUsed, 0);
            plan.clear();

            while (true) {
            	if (isGoalState(state)) {
            		break mainLoop;
            	}
	        	iterations++;
	        	if (time.limitExceeded(settings.getTimelimit())) {
	        		System.out.print(String.format(" iters = %d, can use = %d ", iterations, canUse));
	        		throw new TimeoutException();
	        	}
	        	List<SasAction> candidates = new ArrayList<SasAction>();
	        	for (SasAction a : applicableActions) {
	        		int used = ++actionsUsed[a.getId()];
	        		if (used <= canUse) {
	        			candidates.add(a);
	        		}
	        	}
	        	if (candidates.isEmpty()) {
	        		// restart
	        		break;
	        	}
	            SasAction best = settings.getHeuristic().select(state, candidates);
	            plan.add(best);
	            int[] change = applyActionInPlace(best, state);
	            updateApplicableActionsChanges(applicableActions, state, change);
	        }
        }
		
		List<List<SasAction>> pplan = new ArrayList<List<SasAction>>();
        for (SasAction op : plan) {
            List<SasAction> step = new ArrayList<SasAction>(1);
            step.add(op);
            pplan.add(step);
        }
		System.out.print(String.format(" iters = %d, can use = %d ", iterations, canUse));
        return new SasParallelPlan(pplan);
	}

	@Override
	public BasicSettings getSettings() {
		return settings;
	}

}

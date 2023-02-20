#include <sys/resource.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <signal.h>
#include <boruvka/timer.h>
#include <boruvka/tasks.h>
#include <pddl/pddl.h>
#include <plan/problem.h>
#include <plan/search.h>
/*
#include <boruvka/alloc.h>
#include <plan/ma_msg.h>
#include <plan/problem.h>
#include <plan/ma_search.h>
*/

#include "options.h"

plan_heur_t *planHeurMGroupMerge(const pddl_t *pddl,
                                 const plan_problem_t *prob,
                                 int use_cost_part,
                                 int use_max,
                                 size_t max_mem,
                                 int *op_cost_scale);

plan_heur_t *planHeurFlowMGroupLdmNew(const plan_problem_t *p, unsigned flags,
                                      const pddl_t *pddl,
                                      size_t max_mem);

struct _progress_t {
    int max_time;
    int max_mem;
    int agent_id;
};
typedef struct _progress_t progress_t;

static struct {
    int initialized;
    pthread_t th;
    pthread_mutex_t lock;
    int sleeptime;
    int max_time;
    int max_mem;

    bor_timer_t timer;
    plan_search_t *search;
    int ma_search_size;
} limit_monitor;

bor_timer_t timer;
pddl_t *pddl;
plan_problem_t problem;
plan_heur_t *heur;
plan_search_t *search;
progress_t progress_data;
plan_path_t path;
int op_cost_scale = 1;

static int progress(const plan_search_stat_t *stat, void *data)
{
    progress_t *p = (progress_t *)data;

    borTimerStop(&timer);
    bor_real_t elapsed = borTimerElapsedInSF(&timer);
    fprintf(stderr, "[%.3fs] [%.3f s, %ld MB] %ld steps, %ld evaluated,"
                    " %ld expanded, %ld generated, found: %d\n",
            elapsed,
            stat->elapsed_time, stat->peak_memory,
            stat->steps, stat->evaluated_states, stat->expanded_states,
            stat->generated_states,
            stat->found);
    fflush(stderr);

    if (p->max_time > 0 && stat->elapsed_time > p->max_time){
        fprintf(stderr, "Abort: Exceeded max-time.\n");
        fflush(stderr);
        printf("Abort: Exceeded max-time.\n");
        return PLAN_SEARCH_ABORT;
    }

    if (p->max_mem > 0 && stat->peak_memory > p->max_mem){
        fprintf(stderr, "Abort: Exceeded max-mem.\n");
        fflush(stderr);
        printf("Abort: Exceeded max-mem.\n");
        return PLAN_SEARCH_ABORT;
    }

    return PLAN_SEARCH_CONT;
}

static void limitMonitorAbort(void)
{
    int aborted = 0;

    pthread_mutex_lock(&limit_monitor.lock);
    if (limit_monitor.search){
        planSearchAbort(limit_monitor.search);
        aborted = 1;
    }
    pthread_mutex_unlock(&limit_monitor.lock);

    if (!aborted)
        exit(-1);
}

static void *limitMonitorTh(void *_)
{
    struct rusage usg;
    int peak_mem;
    float elapsed;

    // Enable cancelability and enable cancelation in sleep() call
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
    pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);

    while (1){
        borTimerStop(&limit_monitor.timer);
        elapsed = borTimerElapsedInSF(&limit_monitor.timer);
        if ((int)elapsed > limit_monitor.max_time){
            fprintf(stderr, "Aborting due to exceeded hard time limit"
                            " (elapsed %f, limit: %d).\n",
                    elapsed, limit_monitor.max_time);
            fflush(stderr);
            limitMonitorAbort();
            break;
        }

        if (getrusage(RUSAGE_SELF, &usg) == 0){
            peak_mem = usg.ru_maxrss / 1024L;
            if (peak_mem > limit_monitor.max_mem){
                fprintf(stderr, "Aborting due to exceeded hard mem limit"
                                " (peak-mem: %d, limit: %d).\n",
                        peak_mem, limit_monitor.max_mem);
                fflush(stderr);
                limitMonitorAbort();
                break;
            }
        }

        sleep(limit_monitor.sleeptime);
    }
    return NULL;
}


static void limitMonitorSignalKill(int signum)
{
    fprintf(stderr, "Caught %s signal.\n", strsignal(signum));
    limitMonitorAbort();
}

static void limitMonitorStart(int sleeptime, int max_time, int max_mem)
{
    struct sigaction s;

    limit_monitor.initialized = 1;
    pthread_mutex_init(&limit_monitor.lock, NULL);

    limit_monitor.sleeptime = sleeptime;
    // Give the hard limit monitor 2 minutes more
    limit_monitor.max_time = max_time + (2 * 60);
    limit_monitor.max_mem = max_mem;
    borTimerStart(&limit_monitor.timer);

    limit_monitor.search = NULL;
    limit_monitor.ma_search_size = 0;

    bzero(&s, sizeof(s));
    s.sa_handler = limitMonitorSignalKill;
    sigaction(SIGTERM, &s, NULL);
    sigaction(SIGINT, &s, NULL);

    pthread_create(&limit_monitor.th, NULL, limitMonitorTh, NULL);
}

static void limitMonitorJoin(void)
{
    pthread_cancel(limit_monitor.th);
    pthread_join(limit_monitor.th, NULL);
    pthread_mutex_destroy(&limit_monitor.lock);
}

static void limitMonitorSetSearch(plan_search_t *search)
{
    if (!limit_monitor.initialized)
        return;

    pthread_mutex_lock(&limit_monitor.lock);
    limit_monitor.search = search;
    pthread_mutex_unlock(&limit_monitor.lock);
}

static void problemInit(plan_problem_t *p, const pddl_fdr_t *fdr)
{
    plan_state_t *state;

    planProblemInit(p);
    p->var_size = fdr->var.size;
    p->var = BOR_ALLOC_ARR(plan_var_t, p->var_size);
    for (int i = 0; i < p->var_size; ++i)
        planVarInit(p->var + i, "", fdr->var.var[i].size);
    p->ma_privacy_var = -1;
    p->state_pool = planStatePoolNew(p->var, p->var_size);

    state = planStateNew(p->state_pool->num_vars);
    for (int i = 0; i < fdr->init.size; ++i){
        if (fdr->init.val[i] == PDDL_FDR_VAL_UNDEF){
            fprintf(stderr, "Fatal Error: Invalid init state!\n");
            exit(-1);
        }
        planStateSet(state, i, fdr->init.val[i]);
    }
    p->initial_state = planStatePoolInsert(p->state_pool, state);
    planStateDel(state);

    p->goal = planPartStateNew(p->state_pool->num_vars);
    for (int i = 0; i < fdr->goal.size; ++i){
        if (fdr->goal.val[i] != PDDL_FDR_VAL_UNDEF)
            planPartStateSet(p->goal, i, fdr->goal.val[i]);
    }

    p->op_size = fdr->op_size;
    p->op = BOR_ALLOC_ARR(plan_op_t, p->op_size);
    for (int i = 0; i < fdr->op_size; ++i){
        const pddl_fdr_op_t *fop = fdr->op + i;
        plan_op_t *op = p->op + i;

        planOpInit(op, p->var_size);
        op->name = BOR_STRDUP(fop->name);
        op->cost = fop->cost;
        op->global_id = i;
        for (int i = 0; i < fop->pre.size; ++i){
            if (fop->pre.val[i] != PDDL_FDR_VAL_UNDEF)
                planOpSetPre(op, i, fop->pre.val[i]);
        }
        for (int i = 0; i < fop->eff.size; ++i){
            if (fop->eff.val[i] != PDDL_FDR_VAL_UNDEF)
                planOpSetEff(op, i, fop->eff.val[i]);
        }
    }

    p->succ_gen = planSuccGenNew(p->op, p->op_size, NULL);
}

static void printProblem(const plan_problem_t *prob)
{
    borTimerStop(&timer);
    bor_real_t elapsed = borTimerElapsedInSF(&timer);

    printf("\n");
    printf("[%.3fs] Translator variables: %d\n", elapsed, prob->var_size);
    printf("[%.3fs] Translator operators: %d\n", elapsed, prob->op_size);
    printf("[%.3fs] Bytes per state: %d\n", elapsed,
           planStatePackerBufSize(prob->state_pool->packer));
    printf("[%.3fs] Size of state id: %d\n", elapsed,
            (int)sizeof(plan_state_id_t));
    printf("[%.3fs] Duplicate operators removed: %d\n", elapsed,
            prob->duplicate_ops_removed);
    printf("\n");
    fflush(stdout);
}

static void printResults(const options_t *o, int res, plan_path_t *path)
{
    FILE *fout;

    borTimerStop(&timer);
    bor_real_t elapsed = borTimerElapsedInSF(&timer);

    if (res == PLAN_SEARCH_FOUND){
        printf("[%.3fs] Solution found.\n", elapsed);

        if (o->output != NULL){
            if (strcmp(o->output, "-") == 0){
                planPathPrint(path, stdout);
                printf("[%.3fs] Plan written to stdout\n", elapsed);
            }else{
                fout = fopen(o->output, "w");
                if (fout != NULL){
                    planPathPrint(path, fout);
                    fclose(fout);
                    printf("[%.3fs] Plan written to `%s'\n",
                           elapsed, o->output);
                }else{
                    fprintf(stderr, "Error: Could not plan write to `%s'\n",
                            o->output);
                }
            }
        }

        printf("[%.3fs] Plan Cost: %d\n", elapsed, (int)planPathCost(path));
        printf("[%.3fs] Plan Length: %d\n", elapsed, planPathLen(path));

    }else if (res == PLAN_SEARCH_NOT_FOUND){
        printf("[%.3fs] Solution NOT found.\n", elapsed);

    }else if (res == PLAN_SEARCH_ABORT){
        printf("[%.3fs] Search Aborted.\n", elapsed);
    }
    fflush(stdout);
}

static void printInitHeur(const options_t *o, plan_search_t *search)
{
    borTimerStop(&timer);
    bor_real_t elapsed = borTimerElapsedInSF(&timer);
    printf("[%.3fs] Init State Heur: %d (scale: %d)\n", elapsed,
            (int)planSearchStateHeur(search, search->initial_state),
            op_cost_scale);
    printf("\n");
    fflush(stdout);
}

static void printStat(const plan_search_stat_t *stat)
{
    borTimerStop(&timer);
    bor_real_t elapsed = borTimerElapsedInSF(&timer);
    printf("[%.3fs] Search Time: %f\n", elapsed, stat->elapsed_time);
    printf("[%.3fs] Steps: %ld\n", elapsed, stat->steps);
    printf("[%.3fs] Evaluated %ld state(s)\n",
           elapsed, stat->evaluated_states);
    printf("[%.3fs] Expanded %ld state(s)\n", elapsed, stat->expanded_states);
    printf("[%.3fs] Generated %ld state(s)\n",
           elapsed, stat->generated_states);
    printf("[%.3fs] Peak Memory: %ld kb\n", elapsed, stat->peak_memory);
    printf("\n");
    fflush(stdout);
}

static plan_heur_t *_heurNew(const options_t *o,
                             const char *name,
                             const plan_problem_t *prob)
{
    plan_state_t *state;
    plan_heur_t *heur = NULL;
    unsigned flags = 0;
    unsigned flags2 = 0;

    if (optionsHeurOpt(o, "op-cost1"))
        flags |= PLAN_HEUR_OP_UNIT_COST;
    if (optionsHeurOpt(o, "op-cost+1"))
        flags |= PLAN_HEUR_OP_COST_PLUS_ONE;

    if (strcmp(name, "goalcount") == 0){
        heur = planHeurGoalCountNew(prob->goal);
    }else if (strcmp(name, "add") == 0){
        heur = planHeurAddNew(prob, flags);
    }else if (strcmp(name, "relax-add") == 0){
        heur = planHeurRelaxAddNew(prob, flags);
    }else if (strcmp(name, "max") == 0){
        heur = planHeurMaxNew(prob, flags);
    }else if (strcmp(name, "relax-max") == 0){
        heur = planHeurRelaxMaxNew(prob, flags);
    }else if (strcmp(name, "ff") == 0){
        heur = planHeurRelaxFFNew(prob, flags);
    }else if (strcmp(name, "dtg") == 0){
        heur = planHeurDTGNew(prob, 0);
    }else if (strcmp(name, "max2") == 0){
        heur = planHeurH2MaxNew(prob, flags);
    }else if (strcmp(name, "lm-cut") == 0){
        heur = planHeurLMCutNew(prob, flags);
    }else if (strcmp(name, "relax-lm-cut") == 0){
        heur = planHeurRelaxLMCutNew(prob, flags);
    }else if (strcmp(name, "lm-cut-inc-local") == 0){
        heur = planHeurLMCutIncLocalNew(prob, flags);
    }else if (strcmp(name, "relax-lm-cut-inc-local") == 0){
        heur = planHeurRelaxLMCutIncLocalNew(prob, flags);
    }else if (strcmp(name, "lm-cut-inc-cache") == 0){
        if (optionsHeurOpt(o, "prune"))
            flags2 |= PLAN_LANDMARK_CACHE_PRUNE;
        heur = planHeurLMCutIncCacheNew(prob, flags, flags2);
    }else if (strcmp(name, "relax-lm-cut-inc-cache") == 0){
        if (optionsHeurOpt(o, "prune"))
            flags2 |= PLAN_LANDMARK_CACHE_PRUNE;
        heur = planHeurRelaxLMCutIncCacheNew(prob, flags, flags2);
    }else if (strcmp(name, "lm-cut2") == 0){
        heur = planHeurLMCut2New(prob, flags);
    }else if (strcmp(name, "flow") == 0){
        if (optionsHeurOpt(o, "ilp"))
            flags |= PLAN_HEUR_FLOW_ILP;
        if (optionsHeurOpt(o, "lm-cut"))
            flags |= PLAN_HEUR_FLOW_LANDMARKS_LM_CUT;
        heur = planHeurFlowNew(prob, flags);
    }else if (strcmp(name, "flow-mgroup-ldm") == 0){
        if (optionsHeurOpt(o, "ilp"))
            flags |= PLAN_HEUR_FLOW_ILP;
        if (optionsHeurOpt(o, "lm-cut"))
            flags |= PLAN_HEUR_FLOW_LANDMARKS_LM_CUT;
        size_t max_mem = o->max_mem * 1024ul * 1024ul;
        max_mem -= 1024ul * 1024ul * 1024ul;
        heur = planHeurFlowMGroupLdmNew(prob, flags, pddl, max_mem);
    }else if (strcmp(name, "pot") == 0){
        if (optionsHeurOpt(o, "all-synt-states"))
            flags |= PLAN_HEUR_POT_ALL_SYNTACTIC_STATES;
        state = planStateNew(prob->state_pool->num_vars);
        planStatePoolGetState(prob->state_pool, prob->initial_state, state);
        heur = planHeurPotentialNew(prob, state, flags);
        planStateDel(state);
    }else if (strcmp(name, "ma-max") == 0){
        heur = planHeurMARelaxMaxNew(prob, flags);
    }else if (strcmp(name, "ma-ff") == 0){
        heur = planHeurMARelaxFFNew(prob);
    }else if (strcmp(name, "ma-lm-cut") == 0){
        heur = planHeurMALMCutNew(prob);
    }else if (strcmp(name, "ma-dtg") == 0){
        heur = planHeurMADTGNew(prob);
    }else if (strcmp(name, "ma-pot") == 0){
        if (optionsHeurOpt(o, "all-synt-states"))
            flags |= PLAN_HEUR_POT_ALL_SYNTACTIC_STATES;
        if (optionsHeurOpt(o, "encrypt-off"))
            flags |= PLAN_HEUR_POT_ENCRYPTION_OFF;
        if (optionsHeurOpt(o, "print-init-time"))
            flags |= PLAN_HEUR_POT_PRINT_INIT_TIME;
        heur = planHeurMAPotNew(prob, flags);
    }else if (strcmp(name, "ma-pot-proj") == 0){
        if (optionsHeurOpt(o, "all-synt-states"))
            flags |= PLAN_HEUR_POT_ALL_SYNTACTIC_STATES;
        heur = planHeurMAPotProjNew(prob, flags);

    }else if (strcmp(name, "mgroup-merge") == 0){
        size_t max_mem = o->max_mem * 1024ul * 1024ul;
        max_mem -= 1024ul * 1024ul * 1024ul;
        int cp = 0;
        int max = 0;
        if (optionsHeurOpt(o, "cp"))
            cp = 1;
        if (optionsHeurOpt(o, "max"))
            max = 1;
        heur = planHeurMGroupMerge(pddl, &problem, cp, max,
                                   max_mem, &op_cost_scale);

    }else{
        fprintf(stderr, "Error: Invalid heuristic type: `%s'\n", name);
    }

    return heur;
}

static plan_heur_t *heurNew(const options_t *o,
                            const plan_problem_t *prob)
{
    plan_heur_t *heur;

    heur = _heurNew(o, o->heur, prob);
    return heur;
}

static plan_list_lazy_t *listLazyCreate(const options_t *o)
{
    plan_list_lazy_t *list = NULL;

    if (optionsSearchOpt(o, "list-heap")){
        list = planListLazyHeapNew();
    }else if (optionsSearchOpt(o, "list-bucket")){
        list = planListLazyBucketNew();
    }else if (optionsSearchOpt(o, "list-rb")){
        list = planListLazyRBTreeNew();
    }else if (optionsSearchOpt(o, "list-splay")){
        list = planListLazySplayTreeNew();
    }else{
        list = planListLazySplayTreeNew();
    }

    return list;
}

static plan_search_t *searchNew(const options_t *o,
                                plan_problem_t *prob,
                                plan_heur_t *heur,
                                void *progress_data)
{
    plan_search_t *search = NULL;
    plan_search_params_t *params;
    plan_search_ehc_params_t ehc_params;
    plan_search_lazy_params_t lazy_params;
    plan_search_astar_params_t astar_params;
    int use_preferred_ops = PLAN_SEARCH_PREFERRED_NONE;
    int use_pathmax = 0;

    if (optionsSearchOpt(o, "pref")){
        use_preferred_ops = PLAN_SEARCH_PREFERRED_PREF;
    }else if (optionsSearchOpt(o, "pref_only")){
        use_preferred_ops = PLAN_SEARCH_PREFERRED_ONLY;
    }

    if (optionsSearchOpt(o, "pathmax")){
        use_pathmax = 1;
    }

    if (strcmp(o->search, "ehc") == 0){
        planSearchEHCParamsInit(&ehc_params);
        ehc_params.use_preferred_ops = use_preferred_ops;
        params = &ehc_params.search;

    }else if (strcmp(o->search, "lazy") == 0){
        planSearchLazyParamsInit(&lazy_params);
        lazy_params.use_preferred_ops = use_preferred_ops;
        lazy_params.list = listLazyCreate(o);
        lazy_params.list_del = 1;
        params = &lazy_params.search;

    }else if (strcmp(o->search, "astar") == 0){
        planSearchAStarParamsInit(&astar_params);
        astar_params.pathmax = use_pathmax;
        params = &astar_params.search;

    }else{
        return NULL;
    }

    params->heur = heur;
    params->heur_del = 1;
    params->progress.fn = progress;
    params->progress.freq = o->progress_freq;
    params->progress.data = progress_data;
    params->prob = prob;

    if (strcmp(o->search, "ehc") == 0){
        search = planSearchEHCNew(&ehc_params);
    }else if (strcmp(o->search, "lazy") == 0){
        search = planSearchLazyNew(&lazy_params);
    }else if (strcmp(o->search, "astar") == 0){
        search = planSearchAStarNew(&astar_params);
    }

    return search;
}

int main(int argc, char *argv[])
{
    options_t *opts;
    int res;

    borTimerStart(&timer);

    if ((opts = options(argc, argv)) == NULL)
        return -1;

    limitMonitorStart(opts->hard_limit_sleeptime,
                      opts->max_time, opts->max_mem);

    pddl_config_t cfg = PDDL_CONFIG_INIT;
    cfg.force_adl = 1;
    cfg.compile_away_cond_eff = 1;
    cfg.strips = 1;
    cfg.strips_cfg.h_mutex = 2;
    cfg.strips_cfg.fa_mgroup = 1;
    cfg.strips_cfg.prune.enable = 1;
    cfg.strips_cfg.prune.fixpoint = 1;
    cfg.strips_cfg.prune.static_facts = 1;
    cfg.strips_cfg.prune.irrelevance = 1;
    cfg.strips_cfg.prune.h_mutex = 2;
    cfg.strips_cfg.prune.h_mutex_bw = 2;
    cfg.strips_cfg.prune.fa_mgroup = 1;
    cfg.strips_cfg.prune.fa_mgroup_dead_end = 1;
    cfg.strips_cfg.prune.disambiguation = 1;
    cfg.fdr = 1;

    pddlErrEnableInfo(1);
    pddlErrEnableWarn(1);
    pddl = pddlNew(opts->domain_fn, opts->problem_fn, &cfg);
    if (pddl == NULL || pddl->fdr == NULL){
        pddlErrPrintWithTraceback();
        return -1;
    }

    problemInit(&problem, pddl->fdr);
    printProblem(&problem);
    heur = heurNew(opts, &problem);

    for (int i = 0; i < problem.op_size; ++i)
        problem.op[i].cost *= op_cost_scale;


    progress_data.max_time = opts->max_time;
    progress_data.max_mem = opts->max_mem;
    progress_data.agent_id = 0;
    search = searchNew(opts, &problem, heur, &progress_data);
    limitMonitorSetSearch(search);

    planPathInit(&path);
    res = planSearchRun(search, &path);

    for (int i = 0; i < problem.op_size; ++i)
        problem.op[i].cost /= op_cost_scale;
    planPathOpCostScaleDown(&path, op_cost_scale);

    printf("\n");
    printResults(opts, res, &path);
    printInitHeur(opts, search);
    printf("\n");
    printStat(&search->stat);
    fflush(stdout);

    limitMonitorJoin();
    planPathFree(&path);
    planSearchDel(search);
    planProblemFree(&problem);
    pddlDel(pddl);


    borTimerStop(&timer);
    printf("\nOverall Time: %f\n", borTimerElapsedInSF(&timer));
    return 0;
}

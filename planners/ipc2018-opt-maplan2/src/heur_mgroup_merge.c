/***
 * maplan
 * -------
 * Copyright (c)2018 Daniel Fiser <danfis@danfis.cz>,
 * Department of Computer Science,
 * Faculty of Electrical Engineering, Czech Technical University in Prague.
 * All rights reserved.
 *
 * This file is part of maplan.
 *
 * Distributed under the OSI-approved BSD License (the "License");
 * see accompanying file BDS-LICENSE for details or see
 * <http://www.opensource.org/licenses/bsd-license.php>.
 *
 * This software is distributed WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the License for more information.
 */

#include <boruvka/alloc.h>
#include <boruvka/sort.h>
#include <boruvka/htable.h>
#include <boruvka/hfunc.h>
#include <plan/heur.h>
#include <plan/search.h>
#include <pddl/pddl.h>
#include <pddl/heur_mgroup_merge.h>

struct var_val {
    int var;
    int val;
};
typedef struct var_val var_val_t;

struct part_state {
    int *val;
    int hval;
    bor_list_t htable;
};
typedef struct part_state part_state_t;

struct merge {
    bor_iset_t mgroup;
    bor_iset_t var;
    bor_iset_t *var_val;
    int var_val_size;
    part_state_t *part_state;
    int part_state_size;
    bor_htable_t *htable;
    part_state_t part_state_find;
};
typedef struct merge merge_t;

struct _plan_heur_merge_t {
    plan_heur_t heur;
    int op_cost_scale;
    merge_t *merge;
    int merge_size;
    int use_max;
};
typedef struct _plan_heur_merge_t plan_heur_merge_t;
#define HEUR(parent) \
    bor_container_of((parent), plan_heur_merge_t, heur)

static void heurDel(plan_heur_t *_heur);
static void heur(plan_heur_t *_heur, const plan_state_t *state,
                 plan_heur_res_t *res);

static void mergeInitMGroup(merge_t *m,
                            const pddl_mgroup_t *mg,
                            const pddl_t *pddl)
{
    const pddl_strips_t *strips = pddl->strips;
    const pddl_fdr_t *fdr = pddl->fdr;
    int fact;

    BOR_ISET_FOR_EACH(&mg->fact, fact){
        if (fact < strips->fact.fact_size){
            const pddl_fdr_val_t *v = fdr->var.strips_fact_id_to_val[fact];
            borISetAdd(&m->var, v->var_id);
            borISetAdd(&m->var_val[v->var_id], v->val_id);
        }
    }
}

static bor_htable_key_t partStateHash(const bor_list_t *key, void *_m)
{
    merge_t *m = _m;
    const part_state_t *ps = BOR_LIST_ENTRY(key, part_state_t, htable);
    return borCityHash_64(ps->val, sizeof(int) * borISetSize(&m->var));
}

static int partStateEq(const bor_list_t *k1, const bor_list_t *k2, void *_m)
{
    merge_t *m = _m;
    const part_state_t *p1 = BOR_LIST_ENTRY(k1, part_state_t, htable);
    const part_state_t *p2 = BOR_LIST_ENTRY(k2, part_state_t, htable);
    return memcmp(p1->val, p2->val, sizeof(int) * borISetSize(&m->var)) == 0;
}

static int partStateCmp(const void *a, const void *b, void *_m)
{
    const merge_t *m = _m;
    const part_state_t *p1 = a;
    const part_state_t *p2 = b;
    return memcmp(p1->val, p2->val, sizeof(int) * borISetSize(&m->var));
}

static void mergeAllocPartStates(merge_t *m,
                                 const pddl_heur_mgroup_merge_merge_t *mm,
                                 const pddl_t *pddl)
{
    const pddl_strips_t *strips = pddl->strips;
    const pddl_fdr_t *fdr = pddl->fdr;
    const int var_size = borISetSize(&m->var);
    int i, *var_to_idx, vid, fact;

    // Determine the number of non-dead-end partial states
    m->part_state_size = 0;
    for (int i = 0; i < mm->value_size; ++i){
        if (mm->value[i].value >= 0)
            ++m->part_state_size;
    }

    // Allocate and initialize array of part states
    m->part_state = BOR_ALLOC_ARR(part_state_t, m->part_state_size);
    for (int i = 0; i < m->part_state_size; ++i){
        m->part_state[i].val = BOR_ALLOC_ARR(int, var_size);
        for (int j = 0; j < var_size; ++j)
            m->part_state[i].val[j] = -1;
    }

    // Create mapping from var-id to idx in part_state_t.val[]
    var_to_idx = BOR_ALLOC_ARR(int, m->var_val_size);
    i = 0;
    BOR_ISET_FOR_EACH(&m->var, vid)
        var_to_idx[vid] = i++;

    // Copy part states and their heur values
    for (int i = 0, ins = 0; i < mm->value_size; ++i){
        if (mm->value[i].value < 0)
            continue;
        const pddl_heur_mgroup_merge_value_t *v = mm->value + i;
        part_state_t *dst = m->part_state + ins++;
        dst->hval = v->value;

        BOR_ISET_FOR_EACH(&v->fact, fact){
            if (fact >= strips->fact.fact_size)
                continue;
            pddl_fdr_val_t *fv = fdr->var.strips_fact_id_to_val[fact];
            dst->val[var_to_idx[fv->var_id]] = fv->val_id;
        }
    }

    borSort(m->part_state, m->part_state_size, sizeof(part_state_t),
            partStateCmp, m);

    BOR_FREE(var_to_idx);
}

static void mergeInit(merge_t *m,
                      const pddl_heur_mgroup_merge_t *mm,
                      const pddl_heur_mgroup_merge_merge_t *mmm,
                      const pddl_t *pddl)
{
    const pddl_fdr_t *fdr = pddl->fdr;
    int mi;

    bzero(m, sizeof(*m));
    m->var_val = BOR_CALLOC_ARR(bor_iset_t, fdr->var.size);
    m->var_val_size = fdr->var.size;

    borISetUnion(&m->mgroup, &mmm->mgroup);
    BOR_ISET_FOR_EACH(&m->mgroup, mi)
        mergeInitMGroup(m, mm->strips->mgroup.mgroup + mi, pddl);

    mergeAllocPartStates(m, mmm, pddl);

    m->htable = borHTableNew(partStateHash, partStateEq, m);
    for (int i = 0; i < m->part_state_size; ++i)
        borHTableInsert(m->htable, &m->part_state[i].htable);

    m->part_state_find.val = BOR_ALLOC_ARR(int, borISetSize(&m->var));
}

static void mergeFree(merge_t *m)
{
    borHTableDel(m->htable);

    borISetFree(&m->mgroup);
    borISetFree(&m->var);

    for (int i = 0; i < m->var_val_size; ++i)
        borISetFree(m->var_val + i);
    if (m->var_val != NULL)
        BOR_FREE(m->var_val);

    for (int i = 0; i < m->part_state_size; ++i)
        BOR_FREE(m->part_state[i].val);
    if (m->part_state != NULL)
        BOR_FREE(m->part_state);
}

plan_heur_t *planHeurMGroupMerge(const pddl_t *pddl,
                                 const plan_problem_t *prob,
                                 int use_cost_part,
                                 int use_max,
                                 size_t max_mem,
                                 int *op_cost_scale)
{
    plan_heur_merge_t *h;
    pddl_heur_mgroup_merge_t merge;

    h = BOR_ALLOC(plan_heur_merge_t);
    _planHeurInit(&h->heur, heurDel, heur, NULL);
    h->op_cost_scale = 1;
    h->use_max = use_max;

    pddlHeurMGroupMergeInit(&merge, pddl->strips, use_cost_part, max_mem);
    h->op_cost_scale = merge.op_cost_scale;
    *op_cost_scale = h->op_cost_scale;

    h->merge_size = merge.merge_size;
    h->merge = BOR_ALLOC_ARR(merge_t, h->merge_size);
    for (int i = 0; i < h->merge_size; ++i)
        mergeInit(h->merge + i, &merge, merge.merge + i, pddl);

    pddlHeurMGroupMergeFree(&merge);

    return &h->heur;
}

static void heurDel(plan_heur_t *_heur)
{
    plan_heur_merge_t *h = HEUR(_heur);

    for (int i = 0; i < h->merge_size; ++i)
        mergeFree(h->merge + i);
    if (h->merge != NULL)
        BOR_FREE(h->merge);

    _planHeurFree(&h->heur);
    BOR_FREE(h);
}

static void setState(const merge_t *m, const plan_state_t *state)
{
    int vari, i;

    for (i = 0; i < borISetSize(&m->var); ++i)
        m->part_state_find.val[i] = -1;

    i = 0;
    BOR_ISET_FOR_EACH(&m->var, vari){
        int val = planStateGet(state, vari);
        if (borISetIn(val, &m->var_val[vari]))
            m->part_state_find.val[i] = val;
        i++;
    }
}

static int mergeHVal(plan_heur_merge_t *h,
                     const merge_t *m,
                     const plan_state_t *state)
{
    bor_list_t *psl;
    const part_state_t *ps;

    setState(m, state);

    psl = borHTableFind(m->htable, &m->part_state_find.htable);
    if (psl == NULL){
        //fprintf(stderr, "NOTFOUND\n");
        return -1;
    }

    ps = BOR_LIST_ENTRY(psl, part_state_t, htable);
    return ps->hval;
}

static void heur(plan_heur_t *_heur, const plan_state_t *state,
                 plan_heur_res_t *res)
{
    plan_heur_merge_t *h = HEUR(_heur);
    int hval;

    res->heur = 0;
    for (int i = 0; i < h->merge_size; ++i){
        hval = mergeHVal(h, h->merge + i, state);
        if (hval < 0){
            res->heur = PLAN_HEUR_DEAD_END;
            return;
        }
        if (h->use_max){
            res->heur = BOR_MAX(res->heur, hval);
        }else{
            res->heur += hval;
        }
    }
}

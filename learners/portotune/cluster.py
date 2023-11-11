#! /usr/bin/env python

from collections import defaultdict

from scipy.cluster.vq import kmeans, vq

from portfolio import Portfolio


class ClusterPortfolio(Portfolio):
    def __init__(self, clusters, *args, **kwargs):
        Portfolio.__init__(self, *args, **kwargs)
        self.clusters = clusters

        self.portfolio_name = "Cluster portfolio"
        self.report_descr = "A cluster portfolio."
        self.settings.append("Clusters: %i" % self.clusters)

    def compute_portfolio(self):
        self.schedule_runtimes = [0 for config in self.algorithms]
        self.schedule_config_ids = list(range(len(self.algorithms)))

        print(self.algorithms)
        obs = self.qualities.transpose()
        print(obs)
        res = kmeans(obs, self.clusters)
        code_book, distance = res
        print("DISTANCE", distance)

        assignment, distances = vq(obs, code_book)
        print(assignment)
        clusters = defaultdict(list)
        for config_id, config in enumerate(self.algorithms):
            group_id = assignment[config_id]
            print(
                config_id,
                config.split("-")[1].ljust(22),
                group_id,
                distances[config_id],
            )
            clusters[group_id].append(config)

        runtime = self.plantime / len(clusters)
        representatives = []

        print("GROUPS")
        for group_id, cluster in sorted(clusters.items()):
            best_config = self.get_best_config(cluster, runtime)
            representatives.append(best_config)
            print(group_id, str(len(cluster)).rjust(2), end=" ")
            print(", ".join(config.split("-")[1] for config in cluster), end=" ")
            print("->", best_config.split("-")[1])

        for config in representatives:
            config_id = self.algorithms.index(config)
            self.schedule_runtimes[config_id] = int(runtime)

    def get_best_config(self, cluster, runtime):
        return max(cluster, key=lambda config: self.get_config_score(config, runtime))

    def get_config_score(self, config, runtime):
        config_id = self.algorithms.index(config)
        return self.evaluator.configs_scores(runtime)[config_id]

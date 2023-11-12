from collections import defaultdict

from scipy.cluster.vq import kmeans, vq

from portfolio import Portfolio


class ClusterPortfolio(Portfolio):
    def __init__(self, track, clusters, **kwargs):
        Portfolio.__init__(self, track, **kwargs)
        self.clusters = clusters

        self.portfolio_name = "Cluster portfolio"
        self.settings.append(f"Clusters: {self.clusters}")

    def compute_portfolio(self):
        self.schedule_runtimes = [0 for _ in self.algorithms]
        self.schedule_config_ids = list(range(len(self.algorithms)))

        obs = self.scores.transpose()
        res = kmeans(obs, self.clusters)
        code_book, distance = res

        assignment, distances = vq(obs, code_book)
        clusters = defaultdict(list)
        for algo_id, algo in enumerate(self.algorithms):
            group_id = assignment[algo_id]
            print(
                f"{algo_id:2d}",
                algo.ljust(60),
                group_id,
                f"{distances[algo_id]:.2f}".rjust(6),
            )
            clusters[group_id].append(algo)

        runtime = self.plantime / len(clusters)
        representatives = []

        print()
        for group_id, cluster in sorted(clusters.items()):
            best_algo = self.get_best_algo(cluster, runtime)
            representatives.append(best_algo)
            print(f"Cluster {group_id} with {len(cluster)} algorithms:")
            for algo in cluster:
                print(f"  {algo}")
            print(f"highest score in cluster: {best_algo}")
            print()

        for algo in representatives:
            algo_id = self.algorithms.index(algo)
            self.schedule_runtimes[algo_id] = int(runtime)

    def get_best_algo(self, cluster, runtime):
        return max(cluster, key=lambda algo: self.get_algo_score(algo, runtime))

    def get_algo_score(self, algo, runtime):
        algo_id = self.algorithms.index(algo)
        return self.evaluator.configs_scores(runtime)[algo_id]

(define (problem sliterlink-435500) (:domain slitherlink)
(:init
  (cell-capacity-inc cap-0 cap-1)
  (cell-capacity-inc cap-1 cap-2)
  (cell-capacity-inc cap-2 cap-3)
  (cell-capacity-inc cap-3 cap-4)
  (cell-capacity cell-outside-0-left cap-1)
  (cell-capacity cell-outside-0-right cap-1)
  (cell-capacity cell-outside-1-left cap-1)
  (cell-capacity cell-outside-1-right cap-1)
  (cell-capacity cell-outside-2-left cap-1)
  (cell-capacity cell-outside-2-right cap-1)
  (cell-capacity cell-outside-3-left cap-1)
  (cell-capacity cell-outside-3-right cap-1)
  (cell-capacity cell-outside-4-left cap-1)
  (cell-capacity cell-outside-4-right cap-1)
  (cell-capacity cell-outside-5-left cap-1)
  (cell-capacity cell-outside-5-right cap-1)
  (cell-capacity cell-outside-6-left cap-1)
  (cell-capacity cell-outside-6-right cap-1)
  (cell-capacity cell-outside-0-up cap-1)
  (cell-capacity cell-outside-0-down cap-1)
  (cell-capacity cell-outside-1-up cap-1)
  (cell-capacity cell-outside-1-down cap-1)
  (cell-capacity cell-outside-2-up cap-1)
  (cell-capacity cell-outside-2-down cap-1)
  (cell-capacity cell-outside-3-up cap-1)
  (cell-capacity cell-outside-3-down cap-1)
  (cell-capacity cell-0-0 cap-4)
  (cell-capacity cell-0-1 cap-1)
  (cell-capacity cell-0-2 cap-4)
  (cell-capacity cell-0-3 cap-3)
  (cell-capacity cell-1-0 cap-3)
  (cell-capacity cell-1-1 cap-4)
  (cell-capacity cell-1-2 cap-3)
  (cell-capacity cell-1-3 cap-4)
  (cell-capacity cell-2-0 cap-3)
  (cell-capacity cell-2-1 cap-0)
  (cell-capacity cell-2-2 cap-4)
  (cell-capacity cell-2-3 cap-4)
  (cell-capacity cell-3-0 cap-4)
  (cell-capacity cell-3-1 cap-4)
  (cell-capacity cell-3-2 cap-3)
  (cell-capacity cell-3-3 cap-2)
  (cell-capacity cell-4-0 cap-2)
  (cell-capacity cell-4-1 cap-4)
  (cell-capacity cell-4-2 cap-4)
  (cell-capacity cell-4-3 cap-4)
  (cell-capacity cell-5-0 cap-4)
  (cell-capacity cell-5-1 cap-3)
  (cell-capacity cell-5-2 cap-4)
  (cell-capacity cell-5-3 cap-3)
  (cell-capacity cell-6-0 cap-3)
  (cell-capacity cell-6-1 cap-4)
  (cell-capacity cell-6-2 cap-4)
  (cell-capacity cell-6-3 cap-4)
  (node-degree0 n-0-0)
  (node-degree0 n-0-1)
  (node-degree0 n-0-2)
  (node-degree0 n-0-3)
  (node-degree0 n-0-4)
  (node-degree0 n-1-0)
  (node-degree0 n-1-1)
  (node-degree0 n-1-2)
  (node-degree0 n-1-3)
  (node-degree0 n-1-4)
  (node-degree0 n-2-0)
  (node-degree0 n-2-1)
  (node-degree0 n-2-2)
  (node-degree0 n-2-3)
  (node-degree0 n-2-4)
  (node-degree0 n-3-0)
  (node-degree0 n-3-1)
  (node-degree0 n-3-2)
  (node-degree0 n-3-3)
  (node-degree0 n-3-4)
  (node-degree0 n-4-0)
  (node-degree0 n-4-1)
  (node-degree0 n-4-2)
  (node-degree0 n-4-3)
  (node-degree0 n-4-4)
  (node-degree0 n-5-0)
  (node-degree0 n-5-1)
  (node-degree0 n-5-2)
  (node-degree0 n-5-3)
  (node-degree0 n-5-4)
  (node-degree0 n-6-0)
  (node-degree0 n-6-1)
  (node-degree0 n-6-2)
  (node-degree0 n-6-3)
  (node-degree0 n-6-4)
  (node-degree0 n-7-0)
  (node-degree0 n-7-1)
  (node-degree0 n-7-2)
  (node-degree0 n-7-3)
  (node-degree0 n-7-4)
  (cell-edge cell-0-0 cell-1-0 n-1-0 n-1-1)
  (cell-edge cell-0-1 cell-1-1 n-1-1 n-1-2)
  (cell-edge cell-0-2 cell-1-2 n-1-2 n-1-3)
  (cell-edge cell-0-3 cell-1-3 n-1-3 n-1-4)
  (cell-edge cell-1-0 cell-2-0 n-2-0 n-2-1)
  (cell-edge cell-1-1 cell-2-1 n-2-1 n-2-2)
  (cell-edge cell-1-2 cell-2-2 n-2-2 n-2-3)
  (cell-edge cell-1-3 cell-2-3 n-2-3 n-2-4)
  (cell-edge cell-2-0 cell-3-0 n-3-0 n-3-1)
  (cell-edge cell-2-1 cell-3-1 n-3-1 n-3-2)
  (cell-edge cell-2-2 cell-3-2 n-3-2 n-3-3)
  (cell-edge cell-2-3 cell-3-3 n-3-3 n-3-4)
  (cell-edge cell-3-0 cell-4-0 n-4-0 n-4-1)
  (cell-edge cell-3-1 cell-4-1 n-4-1 n-4-2)
  (cell-edge cell-3-2 cell-4-2 n-4-2 n-4-3)
  (cell-edge cell-3-3 cell-4-3 n-4-3 n-4-4)
  (cell-edge cell-4-0 cell-5-0 n-5-0 n-5-1)
  (cell-edge cell-4-1 cell-5-1 n-5-1 n-5-2)
  (cell-edge cell-4-2 cell-5-2 n-5-2 n-5-3)
  (cell-edge cell-4-3 cell-5-3 n-5-3 n-5-4)
  (cell-edge cell-5-0 cell-6-0 n-6-0 n-6-1)
  (cell-edge cell-5-1 cell-6-1 n-6-1 n-6-2)
  (cell-edge cell-5-2 cell-6-2 n-6-2 n-6-3)
  (cell-edge cell-5-3 cell-6-3 n-6-3 n-6-4)
  (cell-edge cell-outside-0-up cell-0-0 n-0-0 n-0-1)
  (cell-edge cell-6-0 cell-outside-0-down n-7-0 n-7-1)
  (cell-edge cell-outside-1-up cell-0-1 n-0-1 n-0-2)
  (cell-edge cell-6-1 cell-outside-1-down n-7-1 n-7-2)
  (cell-edge cell-outside-2-up cell-0-2 n-0-2 n-0-3)
  (cell-edge cell-6-2 cell-outside-2-down n-7-2 n-7-3)
  (cell-edge cell-outside-3-up cell-0-3 n-0-3 n-0-4)
  (cell-edge cell-6-3 cell-outside-3-down n-7-3 n-7-4)
  (cell-edge cell-0-0 cell-0-1 n-0-1 n-1-1)
  (cell-edge cell-1-0 cell-1-1 n-1-1 n-2-1)
  (cell-edge cell-2-0 cell-2-1 n-2-1 n-3-1)
  (cell-edge cell-3-0 cell-3-1 n-3-1 n-4-1)
  (cell-edge cell-4-0 cell-4-1 n-4-1 n-5-1)
  (cell-edge cell-5-0 cell-5-1 n-5-1 n-6-1)
  (cell-edge cell-6-0 cell-6-1 n-6-1 n-7-1)
  (cell-edge cell-0-1 cell-0-2 n-0-2 n-1-2)
  (cell-edge cell-1-1 cell-1-2 n-1-2 n-2-2)
  (cell-edge cell-2-1 cell-2-2 n-2-2 n-3-2)
  (cell-edge cell-3-1 cell-3-2 n-3-2 n-4-2)
  (cell-edge cell-4-1 cell-4-2 n-4-2 n-5-2)
  (cell-edge cell-5-1 cell-5-2 n-5-2 n-6-2)
  (cell-edge cell-6-1 cell-6-2 n-6-2 n-7-2)
  (cell-edge cell-0-2 cell-0-3 n-0-3 n-1-3)
  (cell-edge cell-1-2 cell-1-3 n-1-3 n-2-3)
  (cell-edge cell-2-2 cell-2-3 n-2-3 n-3-3)
  (cell-edge cell-3-2 cell-3-3 n-3-3 n-4-3)
  (cell-edge cell-4-2 cell-4-3 n-4-3 n-5-3)
  (cell-edge cell-5-2 cell-5-3 n-5-3 n-6-3)
  (cell-edge cell-6-2 cell-6-3 n-6-3 n-7-3)
  (cell-edge cell-outside-0-left cell-0-0 n-0-0 n-1-0)
  (cell-edge cell-0-3 cell-outside-0-right n-0-4 n-1-4)
  (cell-edge cell-outside-1-left cell-1-0 n-1-0 n-2-0)
  (cell-edge cell-1-3 cell-outside-1-right n-1-4 n-2-4)
  (cell-edge cell-outside-2-left cell-2-0 n-2-0 n-3-0)
  (cell-edge cell-2-3 cell-outside-2-right n-2-4 n-3-4)
  (cell-edge cell-outside-3-left cell-3-0 n-3-0 n-4-0)
  (cell-edge cell-3-3 cell-outside-3-right n-3-4 n-4-4)
  (cell-edge cell-outside-4-left cell-4-0 n-4-0 n-5-0)
  (cell-edge cell-4-3 cell-outside-4-right n-4-4 n-5-4)
  (cell-edge cell-outside-5-left cell-5-0 n-5-0 n-6-0)
  (cell-edge cell-5-3 cell-outside-5-right n-5-4 n-6-4)
  (cell-edge cell-outside-6-left cell-6-0 n-6-0 n-7-0)
  (cell-edge cell-6-3 cell-outside-6-right n-6-4 n-7-4)
  (NOT-node-degree1 n-0-0)
  (NOT-node-degree1 n-0-1)
  (NOT-node-degree1 n-0-2)
  (NOT-node-degree1 n-0-3)
  (NOT-node-degree1 n-0-4)
  (NOT-node-degree1 n-1-0)
  (NOT-node-degree1 n-1-1)
  (NOT-node-degree1 n-1-2)
  (NOT-node-degree1 n-1-3)
  (NOT-node-degree1 n-1-4)
  (NOT-node-degree1 n-2-0)
  (NOT-node-degree1 n-2-1)
  (NOT-node-degree1 n-2-2)
  (NOT-node-degree1 n-2-3)
  (NOT-node-degree1 n-2-4)
  (NOT-node-degree1 n-3-0)
  (NOT-node-degree1 n-3-1)
  (NOT-node-degree1 n-3-2)
  (NOT-node-degree1 n-3-3)
  (NOT-node-degree1 n-3-4)
  (NOT-node-degree1 n-4-0)
  (NOT-node-degree1 n-4-1)
  (NOT-node-degree1 n-4-2)
  (NOT-node-degree1 n-4-3)
  (NOT-node-degree1 n-4-4)
  (NOT-node-degree1 n-5-0)
  (NOT-node-degree1 n-5-1)
  (NOT-node-degree1 n-5-2)
  (NOT-node-degree1 n-5-3)
  (NOT-node-degree1 n-5-4)
  (NOT-node-degree1 n-6-0)
  (NOT-node-degree1 n-6-1)
  (NOT-node-degree1 n-6-2)
  (NOT-node-degree1 n-6-3)
  (NOT-node-degree1 n-6-4)
  (NOT-node-degree1 n-7-0)
  (NOT-node-degree1 n-7-1)
  (NOT-node-degree1 n-7-2)
  (NOT-node-degree1 n-7-3)
  (NOT-node-degree1 n-7-4)
)
(:goal (and (NOT-node-degree1 n-0-0) (NOT-node-degree1 n-0-1) (NOT-node-degree1 n-0-2) (NOT-node-degree1 n-0-3) (NOT-node-degree1 n-0-4) (NOT-node-degree1 n-1-0) (NOT-node-degree1 n-1-1) (NOT-node-degree1 n-1-2) (NOT-node-degree1 n-1-3) (NOT-node-degree1 n-1-4) (NOT-node-degree1 n-2-0) (NOT-node-degree1 n-2-1) (NOT-node-degree1 n-2-2) (NOT-node-degree1 n-2-3) (NOT-node-degree1 n-2-4) (NOT-node-degree1 n-3-0) (NOT-node-degree1 n-3-1) (NOT-node-degree1 n-3-2) (NOT-node-degree1 n-3-3) (NOT-node-degree1 n-3-4) (NOT-node-degree1 n-4-0) (NOT-node-degree1 n-4-1) (NOT-node-degree1 n-4-2) (NOT-node-degree1 n-4-3) (NOT-node-degree1 n-4-4) (NOT-node-degree1 n-5-0) (NOT-node-degree1 n-5-1) (NOT-node-degree1 n-5-2) (NOT-node-degree1 n-5-3) (NOT-node-degree1 n-5-4) (NOT-node-degree1 n-6-0) (NOT-node-degree1 n-6-1) (NOT-node-degree1 n-6-2) (NOT-node-degree1 n-6-3) (NOT-node-degree1 n-6-4) (NOT-node-degree1 n-7-0) (NOT-node-degree1 n-7-1) (NOT-node-degree1 n-7-2) (NOT-node-degree1 n-7-3) (NOT-node-degree1 n-7-4) (cell-capacity cell-0-1 cap-0) (cell-capacity cell-0-3 cap-0) (cell-capacity cell-1-0 cap-0) (cell-capacity cell-1-2 cap-0) (cell-capacity cell-2-0 cap-0) (cell-capacity cell-2-1 cap-0) (cell-capacity cell-3-2 cap-0) (cell-capacity cell-3-3 cap-0) (cell-capacity cell-4-0 cap-0) (cell-capacity cell-5-1 cap-0) (cell-capacity cell-5-3 cap-0) (cell-capacity cell-6-0 cap-0)))
)

(define (problem sliterlink-662527) (:domain slitherlink)
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
  (cell-capacity cell-outside-7-left cap-1)
  (cell-capacity cell-outside-7-right cap-1)
  (cell-capacity cell-outside-0-up cap-1)
  (cell-capacity cell-outside-0-down cap-1)
  (cell-capacity cell-outside-1-up cap-1)
  (cell-capacity cell-outside-1-down cap-1)
  (cell-capacity cell-outside-2-up cap-1)
  (cell-capacity cell-outside-2-down cap-1)
  (cell-capacity cell-outside-3-up cap-1)
  (cell-capacity cell-outside-3-down cap-1)
  (cell-capacity cell-outside-4-up cap-1)
  (cell-capacity cell-outside-4-down cap-1)
  (cell-capacity cell-outside-5-up cap-1)
  (cell-capacity cell-outside-5-down cap-1)
  (cell-capacity cell-outside-6-up cap-1)
  (cell-capacity cell-outside-6-down cap-1)
  (cell-capacity cell-0-0 cap-3)
  (cell-capacity cell-0-1 cap-4)
  (cell-capacity cell-0-2 cap-4)
  (cell-capacity cell-0-3 cap-4)
  (cell-capacity cell-0-4 cap-4)
  (cell-capacity cell-0-5 cap-4)
  (cell-capacity cell-0-6 cap-3)
  (cell-capacity cell-1-0 cap-2)
  (cell-capacity cell-1-1 cap-4)
  (cell-capacity cell-1-2 cap-4)
  (cell-capacity cell-1-3 cap-2)
  (cell-capacity cell-1-4 cap-4)
  (cell-capacity cell-1-5 cap-4)
  (cell-capacity cell-1-6 cap-1)
  (cell-capacity cell-2-0 cap-2)
  (cell-capacity cell-2-1 cap-1)
  (cell-capacity cell-2-2 cap-4)
  (cell-capacity cell-2-3 cap-3)
  (cell-capacity cell-2-4 cap-4)
  (cell-capacity cell-2-5 cap-3)
  (cell-capacity cell-2-6 cap-4)
  (cell-capacity cell-3-0 cap-4)
  (cell-capacity cell-3-1 cap-4)
  (cell-capacity cell-3-2 cap-4)
  (cell-capacity cell-3-3 cap-4)
  (cell-capacity cell-3-4 cap-4)
  (cell-capacity cell-3-5 cap-1)
  (cell-capacity cell-3-6 cap-3)
  (cell-capacity cell-4-0 cap-3)
  (cell-capacity cell-4-1 cap-4)
  (cell-capacity cell-4-2 cap-2)
  (cell-capacity cell-4-3 cap-4)
  (cell-capacity cell-4-4 cap-2)
  (cell-capacity cell-4-5 cap-2)
  (cell-capacity cell-4-6 cap-4)
  (cell-capacity cell-5-0 cap-2)
  (cell-capacity cell-5-1 cap-2)
  (cell-capacity cell-5-2 cap-3)
  (cell-capacity cell-5-3 cap-4)
  (cell-capacity cell-5-4 cap-4)
  (cell-capacity cell-5-5 cap-4)
  (cell-capacity cell-5-6 cap-1)
  (cell-capacity cell-6-0 cap-4)
  (cell-capacity cell-6-1 cap-4)
  (cell-capacity cell-6-2 cap-2)
  (cell-capacity cell-6-3 cap-4)
  (cell-capacity cell-6-4 cap-4)
  (cell-capacity cell-6-5 cap-4)
  (cell-capacity cell-6-6 cap-4)
  (cell-capacity cell-7-0 cap-3)
  (cell-capacity cell-7-1 cap-2)
  (cell-capacity cell-7-2 cap-4)
  (cell-capacity cell-7-3 cap-4)
  (cell-capacity cell-7-4 cap-4)
  (cell-capacity cell-7-5 cap-3)
  (cell-capacity cell-7-6 cap-2)
  (node-degree0 n-0-0)
  (node-degree0 n-0-1)
  (node-degree0 n-0-2)
  (node-degree0 n-0-3)
  (node-degree0 n-0-4)
  (node-degree0 n-0-5)
  (node-degree0 n-0-6)
  (node-degree0 n-0-7)
  (node-degree0 n-1-0)
  (node-degree0 n-1-1)
  (node-degree0 n-1-2)
  (node-degree0 n-1-3)
  (node-degree0 n-1-4)
  (node-degree0 n-1-5)
  (node-degree0 n-1-6)
  (node-degree0 n-1-7)
  (node-degree0 n-2-0)
  (node-degree0 n-2-1)
  (node-degree0 n-2-2)
  (node-degree0 n-2-3)
  (node-degree0 n-2-4)
  (node-degree0 n-2-5)
  (node-degree0 n-2-6)
  (node-degree0 n-2-7)
  (node-degree0 n-3-0)
  (node-degree0 n-3-1)
  (node-degree0 n-3-2)
  (node-degree0 n-3-3)
  (node-degree0 n-3-4)
  (node-degree0 n-3-5)
  (node-degree0 n-3-6)
  (node-degree0 n-3-7)
  (node-degree0 n-4-0)
  (node-degree0 n-4-1)
  (node-degree0 n-4-2)
  (node-degree0 n-4-3)
  (node-degree0 n-4-4)
  (node-degree0 n-4-5)
  (node-degree0 n-4-6)
  (node-degree0 n-4-7)
  (node-degree0 n-5-0)
  (node-degree0 n-5-1)
  (node-degree0 n-5-2)
  (node-degree0 n-5-3)
  (node-degree0 n-5-4)
  (node-degree0 n-5-5)
  (node-degree0 n-5-6)
  (node-degree0 n-5-7)
  (node-degree0 n-6-0)
  (node-degree0 n-6-1)
  (node-degree0 n-6-2)
  (node-degree0 n-6-3)
  (node-degree0 n-6-4)
  (node-degree0 n-6-5)
  (node-degree0 n-6-6)
  (node-degree0 n-6-7)
  (node-degree0 n-7-0)
  (node-degree0 n-7-1)
  (node-degree0 n-7-2)
  (node-degree0 n-7-3)
  (node-degree0 n-7-4)
  (node-degree0 n-7-5)
  (node-degree0 n-7-6)
  (node-degree0 n-7-7)
  (node-degree0 n-8-0)
  (node-degree0 n-8-1)
  (node-degree0 n-8-2)
  (node-degree0 n-8-3)
  (node-degree0 n-8-4)
  (node-degree0 n-8-5)
  (node-degree0 n-8-6)
  (node-degree0 n-8-7)
  (cell-edge cell-0-0 cell-1-0 n-1-0 n-1-1)
  (cell-edge cell-0-1 cell-1-1 n-1-1 n-1-2)
  (cell-edge cell-0-2 cell-1-2 n-1-2 n-1-3)
  (cell-edge cell-0-3 cell-1-3 n-1-3 n-1-4)
  (cell-edge cell-0-4 cell-1-4 n-1-4 n-1-5)
  (cell-edge cell-0-5 cell-1-5 n-1-5 n-1-6)
  (cell-edge cell-0-6 cell-1-6 n-1-6 n-1-7)
  (cell-edge cell-1-0 cell-2-0 n-2-0 n-2-1)
  (cell-edge cell-1-1 cell-2-1 n-2-1 n-2-2)
  (cell-edge cell-1-2 cell-2-2 n-2-2 n-2-3)
  (cell-edge cell-1-3 cell-2-3 n-2-3 n-2-4)
  (cell-edge cell-1-4 cell-2-4 n-2-4 n-2-5)
  (cell-edge cell-1-5 cell-2-5 n-2-5 n-2-6)
  (cell-edge cell-1-6 cell-2-6 n-2-6 n-2-7)
  (cell-edge cell-2-0 cell-3-0 n-3-0 n-3-1)
  (cell-edge cell-2-1 cell-3-1 n-3-1 n-3-2)
  (cell-edge cell-2-2 cell-3-2 n-3-2 n-3-3)
  (cell-edge cell-2-3 cell-3-3 n-3-3 n-3-4)
  (cell-edge cell-2-4 cell-3-4 n-3-4 n-3-5)
  (cell-edge cell-2-5 cell-3-5 n-3-5 n-3-6)
  (cell-edge cell-2-6 cell-3-6 n-3-6 n-3-7)
  (cell-edge cell-3-0 cell-4-0 n-4-0 n-4-1)
  (cell-edge cell-3-1 cell-4-1 n-4-1 n-4-2)
  (cell-edge cell-3-2 cell-4-2 n-4-2 n-4-3)
  (cell-edge cell-3-3 cell-4-3 n-4-3 n-4-4)
  (cell-edge cell-3-4 cell-4-4 n-4-4 n-4-5)
  (cell-edge cell-3-5 cell-4-5 n-4-5 n-4-6)
  (cell-edge cell-3-6 cell-4-6 n-4-6 n-4-7)
  (cell-edge cell-4-0 cell-5-0 n-5-0 n-5-1)
  (cell-edge cell-4-1 cell-5-1 n-5-1 n-5-2)
  (cell-edge cell-4-2 cell-5-2 n-5-2 n-5-3)
  (cell-edge cell-4-3 cell-5-3 n-5-3 n-5-4)
  (cell-edge cell-4-4 cell-5-4 n-5-4 n-5-5)
  (cell-edge cell-4-5 cell-5-5 n-5-5 n-5-6)
  (cell-edge cell-4-6 cell-5-6 n-5-6 n-5-7)
  (cell-edge cell-5-0 cell-6-0 n-6-0 n-6-1)
  (cell-edge cell-5-1 cell-6-1 n-6-1 n-6-2)
  (cell-edge cell-5-2 cell-6-2 n-6-2 n-6-3)
  (cell-edge cell-5-3 cell-6-3 n-6-3 n-6-4)
  (cell-edge cell-5-4 cell-6-4 n-6-4 n-6-5)
  (cell-edge cell-5-5 cell-6-5 n-6-5 n-6-6)
  (cell-edge cell-5-6 cell-6-6 n-6-6 n-6-7)
  (cell-edge cell-6-0 cell-7-0 n-7-0 n-7-1)
  (cell-edge cell-6-1 cell-7-1 n-7-1 n-7-2)
  (cell-edge cell-6-2 cell-7-2 n-7-2 n-7-3)
  (cell-edge cell-6-3 cell-7-3 n-7-3 n-7-4)
  (cell-edge cell-6-4 cell-7-4 n-7-4 n-7-5)
  (cell-edge cell-6-5 cell-7-5 n-7-5 n-7-6)
  (cell-edge cell-6-6 cell-7-6 n-7-6 n-7-7)
  (cell-edge cell-outside-0-up cell-0-0 n-0-0 n-0-1)
  (cell-edge cell-7-0 cell-outside-0-down n-8-0 n-8-1)
  (cell-edge cell-outside-1-up cell-0-1 n-0-1 n-0-2)
  (cell-edge cell-7-1 cell-outside-1-down n-8-1 n-8-2)
  (cell-edge cell-outside-2-up cell-0-2 n-0-2 n-0-3)
  (cell-edge cell-7-2 cell-outside-2-down n-8-2 n-8-3)
  (cell-edge cell-outside-3-up cell-0-3 n-0-3 n-0-4)
  (cell-edge cell-7-3 cell-outside-3-down n-8-3 n-8-4)
  (cell-edge cell-outside-4-up cell-0-4 n-0-4 n-0-5)
  (cell-edge cell-7-4 cell-outside-4-down n-8-4 n-8-5)
  (cell-edge cell-outside-5-up cell-0-5 n-0-5 n-0-6)
  (cell-edge cell-7-5 cell-outside-5-down n-8-5 n-8-6)
  (cell-edge cell-outside-6-up cell-0-6 n-0-6 n-0-7)
  (cell-edge cell-7-6 cell-outside-6-down n-8-6 n-8-7)
  (cell-edge cell-0-0 cell-0-1 n-0-1 n-1-1)
  (cell-edge cell-1-0 cell-1-1 n-1-1 n-2-1)
  (cell-edge cell-2-0 cell-2-1 n-2-1 n-3-1)
  (cell-edge cell-3-0 cell-3-1 n-3-1 n-4-1)
  (cell-edge cell-4-0 cell-4-1 n-4-1 n-5-1)
  (cell-edge cell-5-0 cell-5-1 n-5-1 n-6-1)
  (cell-edge cell-6-0 cell-6-1 n-6-1 n-7-1)
  (cell-edge cell-7-0 cell-7-1 n-7-1 n-8-1)
  (cell-edge cell-0-1 cell-0-2 n-0-2 n-1-2)
  (cell-edge cell-1-1 cell-1-2 n-1-2 n-2-2)
  (cell-edge cell-2-1 cell-2-2 n-2-2 n-3-2)
  (cell-edge cell-3-1 cell-3-2 n-3-2 n-4-2)
  (cell-edge cell-4-1 cell-4-2 n-4-2 n-5-2)
  (cell-edge cell-5-1 cell-5-2 n-5-2 n-6-2)
  (cell-edge cell-6-1 cell-6-2 n-6-2 n-7-2)
  (cell-edge cell-7-1 cell-7-2 n-7-2 n-8-2)
  (cell-edge cell-0-2 cell-0-3 n-0-3 n-1-3)
  (cell-edge cell-1-2 cell-1-3 n-1-3 n-2-3)
  (cell-edge cell-2-2 cell-2-3 n-2-3 n-3-3)
  (cell-edge cell-3-2 cell-3-3 n-3-3 n-4-3)
  (cell-edge cell-4-2 cell-4-3 n-4-3 n-5-3)
  (cell-edge cell-5-2 cell-5-3 n-5-3 n-6-3)
  (cell-edge cell-6-2 cell-6-3 n-6-3 n-7-3)
  (cell-edge cell-7-2 cell-7-3 n-7-3 n-8-3)
  (cell-edge cell-0-3 cell-0-4 n-0-4 n-1-4)
  (cell-edge cell-1-3 cell-1-4 n-1-4 n-2-4)
  (cell-edge cell-2-3 cell-2-4 n-2-4 n-3-4)
  (cell-edge cell-3-3 cell-3-4 n-3-4 n-4-4)
  (cell-edge cell-4-3 cell-4-4 n-4-4 n-5-4)
  (cell-edge cell-5-3 cell-5-4 n-5-4 n-6-4)
  (cell-edge cell-6-3 cell-6-4 n-6-4 n-7-4)
  (cell-edge cell-7-3 cell-7-4 n-7-4 n-8-4)
  (cell-edge cell-0-4 cell-0-5 n-0-5 n-1-5)
  (cell-edge cell-1-4 cell-1-5 n-1-5 n-2-5)
  (cell-edge cell-2-4 cell-2-5 n-2-5 n-3-5)
  (cell-edge cell-3-4 cell-3-5 n-3-5 n-4-5)
  (cell-edge cell-4-4 cell-4-5 n-4-5 n-5-5)
  (cell-edge cell-5-4 cell-5-5 n-5-5 n-6-5)
  (cell-edge cell-6-4 cell-6-5 n-6-5 n-7-5)
  (cell-edge cell-7-4 cell-7-5 n-7-5 n-8-5)
  (cell-edge cell-0-5 cell-0-6 n-0-6 n-1-6)
  (cell-edge cell-1-5 cell-1-6 n-1-6 n-2-6)
  (cell-edge cell-2-5 cell-2-6 n-2-6 n-3-6)
  (cell-edge cell-3-5 cell-3-6 n-3-6 n-4-6)
  (cell-edge cell-4-5 cell-4-6 n-4-6 n-5-6)
  (cell-edge cell-5-5 cell-5-6 n-5-6 n-6-6)
  (cell-edge cell-6-5 cell-6-6 n-6-6 n-7-6)
  (cell-edge cell-7-5 cell-7-6 n-7-6 n-8-6)
  (cell-edge cell-outside-0-left cell-0-0 n-0-0 n-1-0)
  (cell-edge cell-0-6 cell-outside-0-right n-0-7 n-1-7)
  (cell-edge cell-outside-1-left cell-1-0 n-1-0 n-2-0)
  (cell-edge cell-1-6 cell-outside-1-right n-1-7 n-2-7)
  (cell-edge cell-outside-2-left cell-2-0 n-2-0 n-3-0)
  (cell-edge cell-2-6 cell-outside-2-right n-2-7 n-3-7)
  (cell-edge cell-outside-3-left cell-3-0 n-3-0 n-4-0)
  (cell-edge cell-3-6 cell-outside-3-right n-3-7 n-4-7)
  (cell-edge cell-outside-4-left cell-4-0 n-4-0 n-5-0)
  (cell-edge cell-4-6 cell-outside-4-right n-4-7 n-5-7)
  (cell-edge cell-outside-5-left cell-5-0 n-5-0 n-6-0)
  (cell-edge cell-5-6 cell-outside-5-right n-5-7 n-6-7)
  (cell-edge cell-outside-6-left cell-6-0 n-6-0 n-7-0)
  (cell-edge cell-6-6 cell-outside-6-right n-6-7 n-7-7)
  (cell-edge cell-outside-7-left cell-7-0 n-7-0 n-8-0)
  (cell-edge cell-7-6 cell-outside-7-right n-7-7 n-8-7)
  (NOT-node-degree1 n-0-0)
  (NOT-node-degree1 n-0-1)
  (NOT-node-degree1 n-0-2)
  (NOT-node-degree1 n-0-3)
  (NOT-node-degree1 n-0-4)
  (NOT-node-degree1 n-0-5)
  (NOT-node-degree1 n-0-6)
  (NOT-node-degree1 n-0-7)
  (NOT-node-degree1 n-1-0)
  (NOT-node-degree1 n-1-1)
  (NOT-node-degree1 n-1-2)
  (NOT-node-degree1 n-1-3)
  (NOT-node-degree1 n-1-4)
  (NOT-node-degree1 n-1-5)
  (NOT-node-degree1 n-1-6)
  (NOT-node-degree1 n-1-7)
  (NOT-node-degree1 n-2-0)
  (NOT-node-degree1 n-2-1)
  (NOT-node-degree1 n-2-2)
  (NOT-node-degree1 n-2-3)
  (NOT-node-degree1 n-2-4)
  (NOT-node-degree1 n-2-5)
  (NOT-node-degree1 n-2-6)
  (NOT-node-degree1 n-2-7)
  (NOT-node-degree1 n-3-0)
  (NOT-node-degree1 n-3-1)
  (NOT-node-degree1 n-3-2)
  (NOT-node-degree1 n-3-3)
  (NOT-node-degree1 n-3-4)
  (NOT-node-degree1 n-3-5)
  (NOT-node-degree1 n-3-6)
  (NOT-node-degree1 n-3-7)
  (NOT-node-degree1 n-4-0)
  (NOT-node-degree1 n-4-1)
  (NOT-node-degree1 n-4-2)
  (NOT-node-degree1 n-4-3)
  (NOT-node-degree1 n-4-4)
  (NOT-node-degree1 n-4-5)
  (NOT-node-degree1 n-4-6)
  (NOT-node-degree1 n-4-7)
  (NOT-node-degree1 n-5-0)
  (NOT-node-degree1 n-5-1)
  (NOT-node-degree1 n-5-2)
  (NOT-node-degree1 n-5-3)
  (NOT-node-degree1 n-5-4)
  (NOT-node-degree1 n-5-5)
  (NOT-node-degree1 n-5-6)
  (NOT-node-degree1 n-5-7)
  (NOT-node-degree1 n-6-0)
  (NOT-node-degree1 n-6-1)
  (NOT-node-degree1 n-6-2)
  (NOT-node-degree1 n-6-3)
  (NOT-node-degree1 n-6-4)
  (NOT-node-degree1 n-6-5)
  (NOT-node-degree1 n-6-6)
  (NOT-node-degree1 n-6-7)
  (NOT-node-degree1 n-7-0)
  (NOT-node-degree1 n-7-1)
  (NOT-node-degree1 n-7-2)
  (NOT-node-degree1 n-7-3)
  (NOT-node-degree1 n-7-4)
  (NOT-node-degree1 n-7-5)
  (NOT-node-degree1 n-7-6)
  (NOT-node-degree1 n-7-7)
  (NOT-node-degree1 n-8-0)
  (NOT-node-degree1 n-8-1)
  (NOT-node-degree1 n-8-2)
  (NOT-node-degree1 n-8-3)
  (NOT-node-degree1 n-8-4)
  (NOT-node-degree1 n-8-5)
  (NOT-node-degree1 n-8-6)
  (NOT-node-degree1 n-8-7)
)
(:goal (and (NOT-node-degree1 n-0-0) (NOT-node-degree1 n-0-1) (NOT-node-degree1 n-0-2) (NOT-node-degree1 n-0-3) (NOT-node-degree1 n-0-4) (NOT-node-degree1 n-0-5) (NOT-node-degree1 n-0-6) (NOT-node-degree1 n-0-7) (NOT-node-degree1 n-1-0) (NOT-node-degree1 n-1-1) (NOT-node-degree1 n-1-2) (NOT-node-degree1 n-1-3) (NOT-node-degree1 n-1-4) (NOT-node-degree1 n-1-5) (NOT-node-degree1 n-1-6) (NOT-node-degree1 n-1-7) (NOT-node-degree1 n-2-0) (NOT-node-degree1 n-2-1) (NOT-node-degree1 n-2-2) (NOT-node-degree1 n-2-3) (NOT-node-degree1 n-2-4) (NOT-node-degree1 n-2-5) (NOT-node-degree1 n-2-6) (NOT-node-degree1 n-2-7) (NOT-node-degree1 n-3-0) (NOT-node-degree1 n-3-1) (NOT-node-degree1 n-3-2) (NOT-node-degree1 n-3-3) (NOT-node-degree1 n-3-4) (NOT-node-degree1 n-3-5) (NOT-node-degree1 n-3-6) (NOT-node-degree1 n-3-7) (NOT-node-degree1 n-4-0) (NOT-node-degree1 n-4-1) (NOT-node-degree1 n-4-2) (NOT-node-degree1 n-4-3) (NOT-node-degree1 n-4-4) (NOT-node-degree1 n-4-5) (NOT-node-degree1 n-4-6) (NOT-node-degree1 n-4-7) (NOT-node-degree1 n-5-0) (NOT-node-degree1 n-5-1) (NOT-node-degree1 n-5-2) (NOT-node-degree1 n-5-3) (NOT-node-degree1 n-5-4) (NOT-node-degree1 n-5-5) (NOT-node-degree1 n-5-6) (NOT-node-degree1 n-5-7) (NOT-node-degree1 n-6-0) (NOT-node-degree1 n-6-1) (NOT-node-degree1 n-6-2) (NOT-node-degree1 n-6-3) (NOT-node-degree1 n-6-4) (NOT-node-degree1 n-6-5) (NOT-node-degree1 n-6-6) (NOT-node-degree1 n-6-7) (NOT-node-degree1 n-7-0) (NOT-node-degree1 n-7-1) (NOT-node-degree1 n-7-2) (NOT-node-degree1 n-7-3) (NOT-node-degree1 n-7-4) (NOT-node-degree1 n-7-5) (NOT-node-degree1 n-7-6) (NOT-node-degree1 n-7-7) (NOT-node-degree1 n-8-0) (NOT-node-degree1 n-8-1) (NOT-node-degree1 n-8-2) (NOT-node-degree1 n-8-3) (NOT-node-degree1 n-8-4) (NOT-node-degree1 n-8-5) (NOT-node-degree1 n-8-6) (NOT-node-degree1 n-8-7) (cell-capacity cell-0-0 cap-0) (cell-capacity cell-0-6 cap-0) (cell-capacity cell-1-0 cap-0) (cell-capacity cell-1-3 cap-0) (cell-capacity cell-1-6 cap-0) (cell-capacity cell-2-0 cap-0) (cell-capacity cell-2-1 cap-0) (cell-capacity cell-2-3 cap-0) (cell-capacity cell-2-5 cap-0) (cell-capacity cell-3-5 cap-0) (cell-capacity cell-3-6 cap-0) (cell-capacity cell-4-0 cap-0) (cell-capacity cell-4-2 cap-0) (cell-capacity cell-4-4 cap-0) (cell-capacity cell-4-5 cap-0) (cell-capacity cell-5-0 cap-0) (cell-capacity cell-5-1 cap-0) (cell-capacity cell-5-2 cap-0) (cell-capacity cell-5-6 cap-0) (cell-capacity cell-6-2 cap-0) (cell-capacity cell-7-0 cap-0) (cell-capacity cell-7-1 cap-0) (cell-capacity cell-7-5 cap-0) (cell-capacity cell-7-6 cap-0)))
)

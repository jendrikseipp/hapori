(define (problem typed-bomberman-rows13-cols7)
(:domain gold-miner-typed)
(:objects 
        f0-0f f0-1f f0-2f f0-3f f0-4f f0-5f f0-6f 
        f1-0f f1-1f f1-2f f1-3f f1-4f f1-5f f1-6f 
        f2-0f f2-1f f2-2f f2-3f f2-4f f2-5f f2-6f 
        f3-0f f3-1f f3-2f f3-3f f3-4f f3-5f f3-6f 
        f4-0f f4-1f f4-2f f4-3f f4-4f f4-5f f4-6f 
        f5-0f f5-1f f5-2f f5-3f f5-4f f5-5f f5-6f 
        f6-0f f6-1f f6-2f f6-3f f6-4f f6-5f f6-6f 
        f7-0f f7-1f f7-2f f7-3f f7-4f f7-5f f7-6f 
        f8-0f f8-1f f8-2f f8-3f f8-4f f8-5f f8-6f 
        f9-0f f9-1f f9-2f f9-3f f9-4f f9-5f f9-6f 
        f10-0f f10-1f f10-2f f10-3f f10-4f f10-5f f10-6f 
        f11-0f f11-1f f11-2f f11-3f f11-4f f11-5f f11-6f 
        f12-0f f12-1f f12-2f f12-3f f12-4f f12-5f f12-6f  - LOC
)
(:init
(arm-empty)
(connected f0-0f f0-1f)
(connected f0-1f f0-2f)
(connected f0-2f f0-3f)
(connected f0-3f f0-4f)
(connected f0-4f f0-5f)
(connected f0-5f f0-6f)
(connected f1-0f f1-1f)
(connected f1-1f f1-2f)
(connected f1-2f f1-3f)
(connected f1-3f f1-4f)
(connected f1-4f f1-5f)
(connected f1-5f f1-6f)
(connected f2-0f f2-1f)
(connected f2-1f f2-2f)
(connected f2-2f f2-3f)
(connected f2-3f f2-4f)
(connected f2-4f f2-5f)
(connected f2-5f f2-6f)
(connected f3-0f f3-1f)
(connected f3-1f f3-2f)
(connected f3-2f f3-3f)
(connected f3-3f f3-4f)
(connected f3-4f f3-5f)
(connected f3-5f f3-6f)
(connected f4-0f f4-1f)
(connected f4-1f f4-2f)
(connected f4-2f f4-3f)
(connected f4-3f f4-4f)
(connected f4-4f f4-5f)
(connected f4-5f f4-6f)
(connected f5-0f f5-1f)
(connected f5-1f f5-2f)
(connected f5-2f f5-3f)
(connected f5-3f f5-4f)
(connected f5-4f f5-5f)
(connected f5-5f f5-6f)
(connected f6-0f f6-1f)
(connected f6-1f f6-2f)
(connected f6-2f f6-3f)
(connected f6-3f f6-4f)
(connected f6-4f f6-5f)
(connected f6-5f f6-6f)
(connected f7-0f f7-1f)
(connected f7-1f f7-2f)
(connected f7-2f f7-3f)
(connected f7-3f f7-4f)
(connected f7-4f f7-5f)
(connected f7-5f f7-6f)
(connected f8-0f f8-1f)
(connected f8-1f f8-2f)
(connected f8-2f f8-3f)
(connected f8-3f f8-4f)
(connected f8-4f f8-5f)
(connected f8-5f f8-6f)
(connected f9-0f f9-1f)
(connected f9-1f f9-2f)
(connected f9-2f f9-3f)
(connected f9-3f f9-4f)
(connected f9-4f f9-5f)
(connected f9-5f f9-6f)
(connected f10-0f f10-1f)
(connected f10-1f f10-2f)
(connected f10-2f f10-3f)
(connected f10-3f f10-4f)
(connected f10-4f f10-5f)
(connected f10-5f f10-6f)
(connected f11-0f f11-1f)
(connected f11-1f f11-2f)
(connected f11-2f f11-3f)
(connected f11-3f f11-4f)
(connected f11-4f f11-5f)
(connected f11-5f f11-6f)
(connected f12-0f f12-1f)
(connected f12-1f f12-2f)
(connected f12-2f f12-3f)
(connected f12-3f f12-4f)
(connected f12-4f f12-5f)
(connected f12-5f f12-6f)
(connected f0-0f f1-0f)
(connected f0-1f f1-1f)
(connected f0-2f f1-2f)
(connected f0-3f f1-3f)
(connected f0-4f f1-4f)
(connected f0-5f f1-5f)
(connected f0-6f f1-6f)
(connected f1-0f f2-0f)
(connected f1-1f f2-1f)
(connected f1-2f f2-2f)
(connected f1-3f f2-3f)
(connected f1-4f f2-4f)
(connected f1-5f f2-5f)
(connected f1-6f f2-6f)
(connected f2-0f f3-0f)
(connected f2-1f f3-1f)
(connected f2-2f f3-2f)
(connected f2-3f f3-3f)
(connected f2-4f f3-4f)
(connected f2-5f f3-5f)
(connected f2-6f f3-6f)
(connected f3-0f f4-0f)
(connected f3-1f f4-1f)
(connected f3-2f f4-2f)
(connected f3-3f f4-3f)
(connected f3-4f f4-4f)
(connected f3-5f f4-5f)
(connected f3-6f f4-6f)
(connected f4-0f f5-0f)
(connected f4-1f f5-1f)
(connected f4-2f f5-2f)
(connected f4-3f f5-3f)
(connected f4-4f f5-4f)
(connected f4-5f f5-5f)
(connected f4-6f f5-6f)
(connected f5-0f f6-0f)
(connected f5-1f f6-1f)
(connected f5-2f f6-2f)
(connected f5-3f f6-3f)
(connected f5-4f f6-4f)
(connected f5-5f f6-5f)
(connected f5-6f f6-6f)
(connected f6-0f f7-0f)
(connected f6-1f f7-1f)
(connected f6-2f f7-2f)
(connected f6-3f f7-3f)
(connected f6-4f f7-4f)
(connected f6-5f f7-5f)
(connected f6-6f f7-6f)
(connected f7-0f f8-0f)
(connected f7-1f f8-1f)
(connected f7-2f f8-2f)
(connected f7-3f f8-3f)
(connected f7-4f f8-4f)
(connected f7-5f f8-5f)
(connected f7-6f f8-6f)
(connected f8-0f f9-0f)
(connected f8-1f f9-1f)
(connected f8-2f f9-2f)
(connected f8-3f f9-3f)
(connected f8-4f f9-4f)
(connected f8-5f f9-5f)
(connected f8-6f f9-6f)
(connected f9-0f f10-0f)
(connected f9-1f f10-1f)
(connected f9-2f f10-2f)
(connected f9-3f f10-3f)
(connected f9-4f f10-4f)
(connected f9-5f f10-5f)
(connected f9-6f f10-6f)
(connected f10-0f f11-0f)
(connected f10-1f f11-1f)
(connected f10-2f f11-2f)
(connected f10-3f f11-3f)
(connected f10-4f f11-4f)
(connected f10-5f f11-5f)
(connected f10-6f f11-6f)
(connected f11-0f f12-0f)
(connected f11-1f f12-1f)
(connected f11-2f f12-2f)
(connected f11-3f f12-3f)
(connected f11-4f f12-4f)
(connected f11-5f f12-5f)
(connected f11-6f f12-6f)
(connected f0-1f f0-0f)
(connected f0-2f f0-1f)
(connected f0-3f f0-2f)
(connected f0-4f f0-3f)
(connected f0-5f f0-4f)
(connected f0-6f f0-5f)
(connected f1-1f f1-0f)
(connected f1-2f f1-1f)
(connected f1-3f f1-2f)
(connected f1-4f f1-3f)
(connected f1-5f f1-4f)
(connected f1-6f f1-5f)
(connected f2-1f f2-0f)
(connected f2-2f f2-1f)
(connected f2-3f f2-2f)
(connected f2-4f f2-3f)
(connected f2-5f f2-4f)
(connected f2-6f f2-5f)
(connected f3-1f f3-0f)
(connected f3-2f f3-1f)
(connected f3-3f f3-2f)
(connected f3-4f f3-3f)
(connected f3-5f f3-4f)
(connected f3-6f f3-5f)
(connected f4-1f f4-0f)
(connected f4-2f f4-1f)
(connected f4-3f f4-2f)
(connected f4-4f f4-3f)
(connected f4-5f f4-4f)
(connected f4-6f f4-5f)
(connected f5-1f f5-0f)
(connected f5-2f f5-1f)
(connected f5-3f f5-2f)
(connected f5-4f f5-3f)
(connected f5-5f f5-4f)
(connected f5-6f f5-5f)
(connected f6-1f f6-0f)
(connected f6-2f f6-1f)
(connected f6-3f f6-2f)
(connected f6-4f f6-3f)
(connected f6-5f f6-4f)
(connected f6-6f f6-5f)
(connected f7-1f f7-0f)
(connected f7-2f f7-1f)
(connected f7-3f f7-2f)
(connected f7-4f f7-3f)
(connected f7-5f f7-4f)
(connected f7-6f f7-5f)
(connected f8-1f f8-0f)
(connected f8-2f f8-1f)
(connected f8-3f f8-2f)
(connected f8-4f f8-3f)
(connected f8-5f f8-4f)
(connected f8-6f f8-5f)
(connected f9-1f f9-0f)
(connected f9-2f f9-1f)
(connected f9-3f f9-2f)
(connected f9-4f f9-3f)
(connected f9-5f f9-4f)
(connected f9-6f f9-5f)
(connected f10-1f f10-0f)
(connected f10-2f f10-1f)
(connected f10-3f f10-2f)
(connected f10-4f f10-3f)
(connected f10-5f f10-4f)
(connected f10-6f f10-5f)
(connected f11-1f f11-0f)
(connected f11-2f f11-1f)
(connected f11-3f f11-2f)
(connected f11-4f f11-3f)
(connected f11-5f f11-4f)
(connected f11-6f f11-5f)
(connected f12-1f f12-0f)
(connected f12-2f f12-1f)
(connected f12-3f f12-2f)
(connected f12-4f f12-3f)
(connected f12-5f f12-4f)
(connected f12-6f f12-5f)
(connected f1-0f f0-0f)
(connected f1-1f f0-1f)
(connected f1-2f f0-2f)
(connected f1-3f f0-3f)
(connected f1-4f f0-4f)
(connected f1-5f f0-5f)
(connected f1-6f f0-6f)
(connected f2-0f f1-0f)
(connected f2-1f f1-1f)
(connected f2-2f f1-2f)
(connected f2-3f f1-3f)
(connected f2-4f f1-4f)
(connected f2-5f f1-5f)
(connected f2-6f f1-6f)
(connected f3-0f f2-0f)
(connected f3-1f f2-1f)
(connected f3-2f f2-2f)
(connected f3-3f f2-3f)
(connected f3-4f f2-4f)
(connected f3-5f f2-5f)
(connected f3-6f f2-6f)
(connected f4-0f f3-0f)
(connected f4-1f f3-1f)
(connected f4-2f f3-2f)
(connected f4-3f f3-3f)
(connected f4-4f f3-4f)
(connected f4-5f f3-5f)
(connected f4-6f f3-6f)
(connected f5-0f f4-0f)
(connected f5-1f f4-1f)
(connected f5-2f f4-2f)
(connected f5-3f f4-3f)
(connected f5-4f f4-4f)
(connected f5-5f f4-5f)
(connected f5-6f f4-6f)
(connected f6-0f f5-0f)
(connected f6-1f f5-1f)
(connected f6-2f f5-2f)
(connected f6-3f f5-3f)
(connected f6-4f f5-4f)
(connected f6-5f f5-5f)
(connected f6-6f f5-6f)
(connected f7-0f f6-0f)
(connected f7-1f f6-1f)
(connected f7-2f f6-2f)
(connected f7-3f f6-3f)
(connected f7-4f f6-4f)
(connected f7-5f f6-5f)
(connected f7-6f f6-6f)
(connected f8-0f f7-0f)
(connected f8-1f f7-1f)
(connected f8-2f f7-2f)
(connected f8-3f f7-3f)
(connected f8-4f f7-4f)
(connected f8-5f f7-5f)
(connected f8-6f f7-6f)
(connected f9-0f f8-0f)
(connected f9-1f f8-1f)
(connected f9-2f f8-2f)
(connected f9-3f f8-3f)
(connected f9-4f f8-4f)
(connected f9-5f f8-5f)
(connected f9-6f f8-6f)
(connected f10-0f f9-0f)
(connected f10-1f f9-1f)
(connected f10-2f f9-2f)
(connected f10-3f f9-3f)
(connected f10-4f f9-4f)
(connected f10-5f f9-5f)
(connected f10-6f f9-6f)
(connected f11-0f f10-0f)
(connected f11-1f f10-1f)
(connected f11-2f f10-2f)
(connected f11-3f f10-3f)
(connected f11-4f f10-4f)
(connected f11-5f f10-5f)
(connected f11-6f f10-6f)
(connected f12-0f f11-0f)
(connected f12-1f f11-1f)
(connected f12-2f f11-2f)
(connected f12-3f f11-3f)
(connected f12-4f f11-4f)
(connected f12-5f f11-5f)
(connected f12-6f f11-6f)
(clear f0-0f)
(hard-rock-at f0-1f)
(soft-rock-at f0-2f)
(soft-rock-at f0-3f)
(hard-rock-at f0-4f)
(soft-rock-at f0-5f)
(gold-at f0-6f)
(soft-rock-at f0-6f)
(clear f1-0f)
(hard-rock-at f1-1f)
(hard-rock-at f1-2f)
(soft-rock-at f1-3f)
(soft-rock-at f1-4f)
(soft-rock-at f1-5f)
(soft-rock-at f1-6f)
(clear f2-0f)
(hard-rock-at f2-1f)
(hard-rock-at f2-2f)
(soft-rock-at f2-3f)
(soft-rock-at f2-4f)
(hard-rock-at f2-5f)
(soft-rock-at f2-6f)
(clear f3-0f)
(soft-rock-at f3-1f)
(hard-rock-at f3-2f)
(soft-rock-at f3-3f)
(soft-rock-at f3-4f)
(soft-rock-at f3-5f)
(soft-rock-at f3-6f)
(clear f4-0f)
(hard-rock-at f4-1f)
(soft-rock-at f4-2f)
(soft-rock-at f4-3f)
(soft-rock-at f4-4f)
(soft-rock-at f4-5f)
(soft-rock-at f4-6f)
(clear f5-0f)
(soft-rock-at f5-1f)
(soft-rock-at f5-2f)
(soft-rock-at f5-3f)
(hard-rock-at f5-4f)
(hard-rock-at f5-5f)
(soft-rock-at f5-6f)
(clear f6-0f)
(soft-rock-at f6-1f)
(soft-rock-at f6-2f)
(soft-rock-at f6-3f)
(hard-rock-at f6-4f)
(soft-rock-at f6-5f)
(soft-rock-at f6-6f)
(clear f7-0f)
(soft-rock-at f7-1f)
(soft-rock-at f7-2f)
(hard-rock-at f7-3f)
(hard-rock-at f7-4f)
(soft-rock-at f7-5f)
(soft-rock-at f7-6f)
(robot-at f8-0f)
(clear f8-0f)
(soft-rock-at f8-1f)
(hard-rock-at f8-2f)
(hard-rock-at f8-3f)
(soft-rock-at f8-4f)
(soft-rock-at f8-5f)
(soft-rock-at f8-6f)
(clear f9-0f)
(soft-rock-at f9-1f)
(soft-rock-at f9-2f)
(soft-rock-at f9-3f)
(soft-rock-at f9-4f)
(hard-rock-at f9-5f)
(soft-rock-at f9-6f)
(bomb-at f10-0f)
(laser-at f10-0f)
(clear f10-0f)
(hard-rock-at f10-1f)
(hard-rock-at f10-2f)
(soft-rock-at f10-3f)
(soft-rock-at f10-4f)
(soft-rock-at f10-5f)
(hard-rock-at f10-6f)
(clear f11-0f)
(soft-rock-at f11-1f)
(hard-rock-at f11-2f)
(soft-rock-at f11-3f)
(soft-rock-at f11-4f)
(soft-rock-at f11-5f)
(soft-rock-at f11-6f)
(clear f12-0f)
(soft-rock-at f12-1f)
(soft-rock-at f12-2f)
(soft-rock-at f12-3f)
(soft-rock-at f12-4f)
(hard-rock-at f12-5f)
(hard-rock-at f12-6f)
)
(:goal
(holds-gold)
))

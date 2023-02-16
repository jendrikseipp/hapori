(define (problem typed-bomberman-rows5-cols10)
(:domain gold-miner-typed)
(:objects 
        f0-0f f0-1f f0-2f f0-3f f0-4f f0-5f f0-6f f0-7f f0-8f f0-9f 
        f1-0f f1-1f f1-2f f1-3f f1-4f f1-5f f1-6f f1-7f f1-8f f1-9f 
        f2-0f f2-1f f2-2f f2-3f f2-4f f2-5f f2-6f f2-7f f2-8f f2-9f 
        f3-0f f3-1f f3-2f f3-3f f3-4f f3-5f f3-6f f3-7f f3-8f f3-9f 
        f4-0f f4-1f f4-2f f4-3f f4-4f f4-5f f4-6f f4-7f f4-8f f4-9f  - LOC
)
(:init
(arm-empty)
(connected f0-0f f0-1f)
(connected f0-1f f0-2f)
(connected f0-2f f0-3f)
(connected f0-3f f0-4f)
(connected f0-4f f0-5f)
(connected f0-5f f0-6f)
(connected f0-6f f0-7f)
(connected f0-7f f0-8f)
(connected f0-8f f0-9f)
(connected f1-0f f1-1f)
(connected f1-1f f1-2f)
(connected f1-2f f1-3f)
(connected f1-3f f1-4f)
(connected f1-4f f1-5f)
(connected f1-5f f1-6f)
(connected f1-6f f1-7f)
(connected f1-7f f1-8f)
(connected f1-8f f1-9f)
(connected f2-0f f2-1f)
(connected f2-1f f2-2f)
(connected f2-2f f2-3f)
(connected f2-3f f2-4f)
(connected f2-4f f2-5f)
(connected f2-5f f2-6f)
(connected f2-6f f2-7f)
(connected f2-7f f2-8f)
(connected f2-8f f2-9f)
(connected f3-0f f3-1f)
(connected f3-1f f3-2f)
(connected f3-2f f3-3f)
(connected f3-3f f3-4f)
(connected f3-4f f3-5f)
(connected f3-5f f3-6f)
(connected f3-6f f3-7f)
(connected f3-7f f3-8f)
(connected f3-8f f3-9f)
(connected f4-0f f4-1f)
(connected f4-1f f4-2f)
(connected f4-2f f4-3f)
(connected f4-3f f4-4f)
(connected f4-4f f4-5f)
(connected f4-5f f4-6f)
(connected f4-6f f4-7f)
(connected f4-7f f4-8f)
(connected f4-8f f4-9f)
(connected f0-0f f1-0f)
(connected f0-1f f1-1f)
(connected f0-2f f1-2f)
(connected f0-3f f1-3f)
(connected f0-4f f1-4f)
(connected f0-5f f1-5f)
(connected f0-6f f1-6f)
(connected f0-7f f1-7f)
(connected f0-8f f1-8f)
(connected f0-9f f1-9f)
(connected f1-0f f2-0f)
(connected f1-1f f2-1f)
(connected f1-2f f2-2f)
(connected f1-3f f2-3f)
(connected f1-4f f2-4f)
(connected f1-5f f2-5f)
(connected f1-6f f2-6f)
(connected f1-7f f2-7f)
(connected f1-8f f2-8f)
(connected f1-9f f2-9f)
(connected f2-0f f3-0f)
(connected f2-1f f3-1f)
(connected f2-2f f3-2f)
(connected f2-3f f3-3f)
(connected f2-4f f3-4f)
(connected f2-5f f3-5f)
(connected f2-6f f3-6f)
(connected f2-7f f3-7f)
(connected f2-8f f3-8f)
(connected f2-9f f3-9f)
(connected f3-0f f4-0f)
(connected f3-1f f4-1f)
(connected f3-2f f4-2f)
(connected f3-3f f4-3f)
(connected f3-4f f4-4f)
(connected f3-5f f4-5f)
(connected f3-6f f4-6f)
(connected f3-7f f4-7f)
(connected f3-8f f4-8f)
(connected f3-9f f4-9f)
(connected f0-1f f0-0f)
(connected f0-2f f0-1f)
(connected f0-3f f0-2f)
(connected f0-4f f0-3f)
(connected f0-5f f0-4f)
(connected f0-6f f0-5f)
(connected f0-7f f0-6f)
(connected f0-8f f0-7f)
(connected f0-9f f0-8f)
(connected f1-1f f1-0f)
(connected f1-2f f1-1f)
(connected f1-3f f1-2f)
(connected f1-4f f1-3f)
(connected f1-5f f1-4f)
(connected f1-6f f1-5f)
(connected f1-7f f1-6f)
(connected f1-8f f1-7f)
(connected f1-9f f1-8f)
(connected f2-1f f2-0f)
(connected f2-2f f2-1f)
(connected f2-3f f2-2f)
(connected f2-4f f2-3f)
(connected f2-5f f2-4f)
(connected f2-6f f2-5f)
(connected f2-7f f2-6f)
(connected f2-8f f2-7f)
(connected f2-9f f2-8f)
(connected f3-1f f3-0f)
(connected f3-2f f3-1f)
(connected f3-3f f3-2f)
(connected f3-4f f3-3f)
(connected f3-5f f3-4f)
(connected f3-6f f3-5f)
(connected f3-7f f3-6f)
(connected f3-8f f3-7f)
(connected f3-9f f3-8f)
(connected f4-1f f4-0f)
(connected f4-2f f4-1f)
(connected f4-3f f4-2f)
(connected f4-4f f4-3f)
(connected f4-5f f4-4f)
(connected f4-6f f4-5f)
(connected f4-7f f4-6f)
(connected f4-8f f4-7f)
(connected f4-9f f4-8f)
(connected f1-0f f0-0f)
(connected f1-1f f0-1f)
(connected f1-2f f0-2f)
(connected f1-3f f0-3f)
(connected f1-4f f0-4f)
(connected f1-5f f0-5f)
(connected f1-6f f0-6f)
(connected f1-7f f0-7f)
(connected f1-8f f0-8f)
(connected f1-9f f0-9f)
(connected f2-0f f1-0f)
(connected f2-1f f1-1f)
(connected f2-2f f1-2f)
(connected f2-3f f1-3f)
(connected f2-4f f1-4f)
(connected f2-5f f1-5f)
(connected f2-6f f1-6f)
(connected f2-7f f1-7f)
(connected f2-8f f1-8f)
(connected f2-9f f1-9f)
(connected f3-0f f2-0f)
(connected f3-1f f2-1f)
(connected f3-2f f2-2f)
(connected f3-3f f2-3f)
(connected f3-4f f2-4f)
(connected f3-5f f2-5f)
(connected f3-6f f2-6f)
(connected f3-7f f2-7f)
(connected f3-8f f2-8f)
(connected f3-9f f2-9f)
(connected f4-0f f3-0f)
(connected f4-1f f3-1f)
(connected f4-2f f3-2f)
(connected f4-3f f3-3f)
(connected f4-4f f3-4f)
(connected f4-5f f3-5f)
(connected f4-6f f3-6f)
(connected f4-7f f3-7f)
(connected f4-8f f3-8f)
(connected f4-9f f3-9f)
(bomb-at f0-0f)
(laser-at f0-0f)
(clear f0-0f)
(soft-rock-at f0-1f)
(soft-rock-at f0-2f)
(soft-rock-at f0-3f)
(soft-rock-at f0-4f)
(soft-rock-at f0-5f)
(soft-rock-at f0-6f)
(soft-rock-at f0-7f)
(soft-rock-at f0-8f)
(soft-rock-at f0-9f)
(clear f1-0f)
(soft-rock-at f1-1f)
(soft-rock-at f1-2f)
(hard-rock-at f1-3f)
(hard-rock-at f1-4f)
(hard-rock-at f1-5f)
(soft-rock-at f1-6f)
(soft-rock-at f1-7f)
(hard-rock-at f1-8f)
(soft-rock-at f1-9f)
(clear f2-0f)
(soft-rock-at f2-1f)
(hard-rock-at f2-2f)
(soft-rock-at f2-3f)
(soft-rock-at f2-4f)
(soft-rock-at f2-5f)
(hard-rock-at f2-6f)
(hard-rock-at f2-7f)
(soft-rock-at f2-8f)
(gold-at f2-9f)
(soft-rock-at f2-9f)
(clear f3-0f)
(soft-rock-at f3-1f)
(soft-rock-at f3-2f)
(soft-rock-at f3-3f)
(soft-rock-at f3-4f)
(soft-rock-at f3-5f)
(soft-rock-at f3-6f)
(hard-rock-at f3-7f)
(hard-rock-at f3-8f)
(hard-rock-at f3-9f)
(robot-at f4-0f)
(clear f4-0f)
(soft-rock-at f4-1f)
(soft-rock-at f4-2f)
(soft-rock-at f4-3f)
(hard-rock-at f4-4f)
(soft-rock-at f4-5f)
(soft-rock-at f4-6f)
(soft-rock-at f4-7f)
(soft-rock-at f4-8f)
(hard-rock-at f4-9f)
)
(:goal
(holds-gold)
))

# redatared: First-Principles Outline

## Goal
Small, pure data layer for games. FRP is the core model of time and behavior.
ECS exists only for ergonomic iteration over entities.

## Core Values
- Purity: no hidden effects; all changes are data.
- Continuity: programs run across frames; "tick" is just a resume.
- Ergonomics: minimal surface area; short names; avoid boilerplate.
- Composition: steps, events, patches compose predictably.

## Pieces (ECP)
ECP = **Entity, Component, Program**.

- **World**: the complete game state.
- **Entity**: identity for grouping components.
- **Component**: typed data attached to entities.
- **Program**: a coroutine that can `await` and access World.
- **Query**: a predicate + projection over components.
- **Patch**: pure description of changes to World.
- **Event**: list of occurrences per tick.
- **Step / Signal / Tween**: FRP time primitives.

Everything else is derived convenience.

## Ground Rules (decisions locked)
- **Programs are the only control surface** (legacy naming removed).
- **Per-entity programs are tied to the program** (stored in program locals, not on entities).
- **`await` is the only control primitive**.
- **Cross-entity patches are not allowed**; only messages/events may cross entities.
- Programs run until they **finish** or **hit an await boundary** that cannot resolve.

## Why ECS Exists Here
ECS is not the core model; it is an iteration convenience:
"apply this patch to every entity that matches a query."

## Programs and Await
Programs are continuous behaviors that can suspend.
They are resumed with new inputs each frame.
For replay/serialization, **inputs are the source of truth**:
we re-run the program and feed it the inputs it has already consumed.

`await` introduces a gate:
- program waits for an event/another program's completion
- per-entity program waits for inputs or timers

No other control primitive exists; this is the only suspension mechanism.

## Runtime Semantics (frame)
1. **Gather inputs** for this frame (events/messages).
2. **Run programs** until they either finish or block on `await`.
3. **Apply patches** produced by runnable programs.
4. **Emit events/messages** produced this frame.
5. **Retry blocked programs** if new events/messages make them runnable.
6. **Stop** when no program can make progress; frame is done.

This is a fixed-point: run/retry until no progress is possible.

## Query & Iteration
Queries are static (required/forbidden) + projection.
Iteration should be:
- **Pure by default** (return patches).
- **Monadic only when needed** (per-entity state machines that `await`).

## Time (FRP)
Time is first-class and compositional:
- absolute time, ranges, windows
- event-relative time
- tweens and easing

Steps can be sampled in programs or used directly in pure logic.

## Non-Goals (optional/derived)
- Scheduler and phases (can be layered on)
- Parallelism (optional, not core)
- Archetype engine (not needed for correctness)
- Serialization (requires external logging/replay of inputs)

## Open Questions (not yet decided)
- How to serialize mid-flight programs safely.
- Whether to add a compact "batch" DSL for fusing entity iteration.
- Whether to provide optional spatial indexes or leave them to user code.

---
name: how
description: "Trace a subsystem, explain its architecture and runtime flow, or locate ownership before a change."
disable-model-invocation: true
---

# How

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Bound the question from the user request and current code. State an interpretation when needed and begin exploring. [review]
2. For a narrow question, trace the entry point, callers, state changes, and tests directly using [the explorer prompt](references/explorer-prompt.md). For a cross-cutting subsystem, split into distinct exploration angles and dispatch `how explorer` seats through the adapter. [review]
3. Verify the cited symbols and call paths. Resolve `how explainer` when an independent synthesis adds value; otherwise synthesize directly using [the explainer prompt](references/explainer-prompt.md). [review]
4. Present the overview, key concepts, runtime sequence, file ownership, and relevant gotchas. Cite actual file lines and distinguish observed behavior from inference. Carry the resulting invariants into the implementation brief when this precedes a change. [review]

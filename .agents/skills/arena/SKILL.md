---
name: arena
description: "Compare independent candidates, select a base, and combine the strongest ideas into one verified artifact."
disable-model-invocation: true
---

# Arena

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Frame the artifact and its done predicate. Create a rubric with concrete criteria before generating candidates. Give candidates the same task without the scoring rubric. [review]
2. Resolve the `arena runners` panel from `.agents/pstack-models.json`. For architectural sketches use `architect runners`. Give each writable native runner an isolated worktree or scratch directory. CLI runners return text; the parent saves each result to its own artifact file. [review]
3. Launch candidates through the adapter. Record actual provider/model and any failed seat. A failed seat is a gap. Same-provider alternatives can compare distinct approaches but do not provide cross-provider evidence. [review]
4. After generation finishes, resolve one independent judge from `arena cross-judge pool`, preferring the provider different from the candidate being judged. Provide anonymized outputs and the rubric. Read all outputs yourself while the judge reviews them. [review]
5. Score every criterion. Select the base whose boundaries and invariants make future changes easiest. Reconcile disagreements with the judge using evidence. [review]
6. Re-read the other candidates and integrate useful ideas into the base. Record accepted and rejected ideas with their source. Reframe if the candidates disagree because the task was underspecified. [review]
7. Verify the synthesized artifact through the applicable Midgard checks. Return the artifact, selection rationale, grafts, dropouts, and verification results. [review]

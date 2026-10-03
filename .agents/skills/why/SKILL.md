---
name: why
description: "Investigate design rationale using code history and available read-only evidence sources."
disable-model-invocation: true
---

# Why

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

Read [the confidence framework](references/epistemics.md) before assigning confidence.

1. Pin the code paths, symbols, user question, and time window. Use `git blame`, `git log --follow`, and relevant PR discussions as the source-control anchor. [review]
2. Discover available read tools from the actual host tool inventory. Map them to source control, issue tracker, long-form documents, team chat, observability, exception tracking, and analytics. Record each unavailable category as a gap. Source control can use local git even when `gh` is unavailable. [review]
3. Give each useful source a bounded investigation using `why investigators`. Use [the investigator prompt](references/investigator-prompt.md) and [the source index](references/source-playbook.md) to choose the relevant source reference. The source files are examples to adapt to the connected service's schema, not proof that a service or database exists. For defensive behavior also read [incident evidence](references/sources/incident-postmortem.md). [review]
4. Investigators use read operations only. Keep MCP access only when the host can restrict it to reads. The CLI adapter has no MCP access; supply sanitized excerpts as input and record the resulting coverage limit. [review]
5. Reconcile findings using `why synthesizer` and [the synthesis prompt](references/synthesizer-prompt.md). Spot-check primary sources. Preserve the separation between facts, reasonable inferences, competing hypotheses, and unknowns. [review]
6. Answer with sources consulted and explicit null results or gaps. If this precedes a change, produce Preserve / Change / Avoid / Risk constraints for planning. [review]

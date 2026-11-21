---
name: reflect
description: "Extract durable lessons from the current task and propose or apply evidence-backed skill improvements."
disable-model-invocation: true
---

# Reflect

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Use this conversation, an explicitly identified host chat, or a sanitized digest. Bound the evidence to the current task. Read [editing-agent-instructions](../editing-agent-instructions/SKILL.md) before proposing permanent rules. [review]
2. Resolve `reflect tooling` and `reflect judgment, divergent, synthesizer`. Review the session from [tooling](references/tooling-reviewer.md), [judgment](references/judgment-reviewer.md), and [divergent](references/divergent-reviewer.md) angles. Distinct lenses can share a provider; label their actual independence accurately. [review]
3. Synthesize with [the synthesis reference](references/synthesizer.md). Return accepted, rejected, and backlog items with evidence and the narrowest owner. [review]
4. Encode a mechanically detectable lesson in a check where practical. Apply skill edits when the user requested them; a request to reflect alone produces concrete proposed edits for review. Keep backlog local unless issue creation is authorized. [review]
5. Validate changed skills and any scripts. Report exact edits, checks, deferred proposals, and rejected findings with reasons. [review]

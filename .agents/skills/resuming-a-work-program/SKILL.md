---
name: resuming-a-work-program
description: Resume a multi-session Midgard work program from its durable task and decision records. Use when reconciling candidate worktrees, ownership, reviews, integration state or required evidence after an interrupted program; when a handoff omits original obligations; or before integrating a reviewed candidate packet.
---

# Resuming a work program

Read [the program and integration tools](../../../docs/agents/contrib.md#integration-and-program-resumes)
before reconstructing scope or ownership with shell/Python scripts.

1. Locate the program's authoritative manifest and linked decision records.
   Validate it with `contrib program validate --input <path>` and render its
   task view. A related issue is not an exact closure obligation.
2. Inspect `contrib workspace inspect` and `contrib resources list`. Compare
   base/candidate identities and dirty/staged overlap before choosing a source.
3. Validate required receipts against current input identities. Preserve failed,
   skipped and missing evidence; resume the pending gate rather than upgrading
   implemented/reviewed/published into accepted. [review]
4. For integration, create and verify an exact-path packet against the current
   destination base; use the existing committing-safely skill after application.

If no manifest exists, recover scope from source transcripts and current owner
decisions, then save it in the validated schema. Historical transcripts are
evidence of what was said, not authority over a later superseding decision.
External issue closure or publication retains the user's authorization scope.
Completion is a reconciled manifest, preserved work and a clear next unmet gate.

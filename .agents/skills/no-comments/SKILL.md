---
name: no-comments
description: "Review comments and suppressions for stale narration while preserving protocol rationale and public contracts."
disable-model-invocation: true
---

# No comments

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Pin the caller's files or diff. Read [the comment-review prompt](../poteto-mode/references/comment-reviewer.md), then obtain an independent read-only review when useful. [review]
2. Inspect every proposed deletion against surrounding code. Keep legal notices, public API contracts, protocol invariants, evidence links, and non-obvious rationale. Ambiguity is a reason to investigate with [how](../how/SKILL.md) or [why](../why/SKILL.md), not proof that a comment is disposable. [review]
3. Remove stale or redundant narration within scope. A suppression that hides a correctness defect needs a root-cause fix and verification. For a structural change use [architect](../architect/SKILL.md). [review]
4. Encode a proven constraint in the cheapest suitable type, check, or test when within scope. Preserve the explanation until its replacement enforces the constraint. Report unresolved or out-of-scope work without weakening it. [review]
5. Return the accepted deletions, preserved comments and reasons, root-cause fixes, and verification results. [review]

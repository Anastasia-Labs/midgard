---
name: interrogate
description: "Adversarially review a diff using independent Codex and Claude perspectives and synthesize an evidence-backed verdict."
disable-model-invocation: true
---

# Interrogate

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatch.
This workflow reviews a change. It applies fixes only when the caller authorized
implementation as well as review.

1. Pin the scope to the user-selected diff, files, or PR. Record base and head
   SHAs and include owned working-tree changes when those are the target. [review]
2. State the intent from the user request, code, and PR description. Package
   exact diff/source excerpts, surrounding contracts, and relevant Midgard
   instructions. For consensus changes, read
   [reviewing-consensus-changes](../reviewing-consensus-changes/SKILL.md). [review]
3. Resolve `interrogate reviewers` from `.agents/pstack-models.json`. Give each
   seat the same bounded input using [the reviewer prompt](references/reviewer-prompt.md),
   [rubric](references/rubric.md), and
   [code-quality lens](references/code-quality-review.md). Follow the adapter's
   actual native schema or external read-only invocation. Record provider and
   observed model; a missing seat is an explicit coverage gap. [review]
4. Read every result, deduplicate findings, and record agreement and divergence.
   A single-provider retry does not replace missing cross-provider evidence.
   Check each claimed defect against actual code and executable evidence. [review]
5. Apply [lead judgment](references/lead-judgment.md). Classify findings as
   Act on, Consider, Noted, or Dismissed, with one reason and source attribution.
   Consensus raises attention; correctness comes from evidence. [review]
6. Return the intent, actual reviewers, actionable findings with file lines,
   tradeoffs, dismissals, disagreements, and gaps. A claimed pass excludes
   unverified or missing coverage. [review]

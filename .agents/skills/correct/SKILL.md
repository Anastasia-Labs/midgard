---
name: correct
description: "Find recurring agent mistakes in Midgard and prevent them through architecture, types, lint, CI, and behavioral tests, with prose last. Prove each new check catches a real past mistake. Use for /correct."
disable-model-invocation: true
license: MIT (see LICENSE)
metadata:
  upstream: https://github.com/cursor/plugins/tree/2f8377461fd533f5af782e1fd643783b1067b847/pstack/skills/correct
---

# Correct

Read [the host adapter](../poteto-mode/references/host-adapter.md) before using models, delegation, or history. It owns these operations for both Codex and Claude. [review]

The operator keeps correcting agents in this repo for the same mistakes. Change the repo so the next agent can't make them. Follow the requested scope: an analysis or plan produces evidence and proposed enforcement; implementation happens when authorized. [review]

Assume every contributor is an agent that sees only the files it opened, copies the nearest example, and takes the shortest path that compiles. Design the repo so a change that looks right from one file is right for the whole repo. [review]

## Find the mistake classes

First, read recent commits, reverts, review comments, agent instruction files, and comments that explain workarounds. Use [recall](../recall/SKILL.md) when transcripts are in scope. State the repository, time range, and available evidence. [review]

Group the mistakes into classes. A recurring class has at least two distinct incidents; repeated mentions of one incident count once. Link each incident to its source and check the current code so already-fixed problems stay separate from open ones. [review]

## Fix each class at the highest level that works

1. **Eliminate it with architecture.** Give each piece of state one owner and each task one supported way. Hide internals so the wrong import fails. Replace hand-synced lists with one source of truth. Delete obsolete paths an agent would copy. Use [architect](../architect/SKILL.md) for consequential ownership or boundary changes, within the requested deliverable. [review]
2. **Enforce it with types so the bad state can't be written.** If bad code still compiles, add a lint or CI check whose error names the file, type, or function to use instead. An incremental baseline can stop new occurrences while existing ones are migrated; preserve existing strict checks. [review]
3. **Test the behavior.** Read [writing-tests](../writing-tests/SKILL.md) before changing assertions. Fix or delete tests that still pass when their behavior breaks, or fail when only internals change. [review]
4. **Write docs or agent rules last, only for judgment calls.** Read [editing-agent-instructions](../editing-agent-instructions/SKILL.md) before adding prose. Nothing fails when an agent skips it. [review]

## Fix and prove

For an implementation request, fix the most frequent classes in independently verifiable changes. Before committing, read [committing-safely](../committing-safely/SKILL.md). [review]

Prove each new check fails on a real past mistake and passes on the corrected case. Reproduce historical mistakes in isolated fixtures or scratch checkouts. Run the same check locally and in CI, plus the required checks in [verification](../../../docs/agents/verification.md). For a plan, specify that proof and label unexecuted checks as proposed. [review]

Exceptions use the owning check's mechanism and retain a reason and removal condition. Apply the repository's actual approval requirements and existing authorization. [review]

## Keep enforcement visible

Midgard already records enforcement with inline tags. Use [the existing tag vocabulary](../../../docs/agents/README.md) when updating rules; keep the findings table in the task report instead of duplicating the inventory in AGENTS.md. Each row pairs a mistake class, incident evidence, current status, chosen prevention level, and executed or proposed proof. [review]

When corrected during this work, check for a repeat and address it at the highest level within the authorized scope. Remove redundant instructional prose once the structure or check carries the rule. [review]

**Reply:** each class with its evidence, the level you picked, why a higher level didn't work, exact checks and results, and coverage gaps. [review]

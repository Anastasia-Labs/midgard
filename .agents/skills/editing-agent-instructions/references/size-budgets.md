# Size budgets

Sizes measured with `wc -l -c` on 2026-09-25. Budgets are proposals: only the
two skill limits marked `[script]` are enforced today. A per-file budget check
is part of task W2.3 of the
[hardening plan](../../../../docs/exec-plans/agent-contribution-hardening.md),
which is in flight; until it lands, every other budget here is `[review]`.

The cost of a line depends on when it loads. Always-loaded files are paid for
by every session, so their budgets are the tightest.

## Always loaded

| File                 | Lines | Bytes | Proposed budget | Notes                                                                                |
| -------------------- | ----- | ----- | --------------- | ------------------------------------------------------------------------------------ |
| `AGENTS.md`          | 69    | 3,447 | 100 lines       | Rung 4 only; every addition says what it buys every session.                         |
| `CLAUDE.md`          | 27    | 902   | 30 lines        | A router to `AGENTS.md` and a few docs; no rules of its own.                         |
| Skill `description`s | —     | —     | 1,024 chars     | `[script: scripts/ci/check-agent-skills.mjs]`. Loaded every turn, so prune triggers. |

The skill descriptions in this tree ranged from 189 to 618 characters.

## Loaded by directory

| File                                       | Lines | Bytes | Proposed budget |
| ------------------------------------------ | ----- | ----- | --------------- |
| `demo/AGENTS.md`                           | 22    | 1,034 | 60 lines        |
| `demo/midgard-node/AGENTS.md`              | 17    | 676   | 60 lines        |
| `onchain/aiken/AGENTS.md`                  | 15    | 691   | 60 lines        |
| `demo/midgard-node-tools/devnet/AGENTS.md` | 13    | 736   | 60 lines        |

A nested file that outgrows 60 lines is usually carrying a procedure that
belongs in a skill.

The main checkout also holds 26 copies of `onchain/aiken/AGENTS.md` under the
ignored `artifacts/event-history/` tree, where directory-scoped loaders and a
plain `grep -r` find them (plan task W2.8).

## Loaded on demand

| File                                                      | Lines       | Proposed budget                                                             |
| --------------------------------------------------------- | ----------- | --------------------------------------------------------------------------- |
| `CONTEXT.md`                                              | 163         | 250 lines                                                                   |
| `docs/agents/*.md` (largest: `withdraw-zero-yielding.md`) | 10–146      | 150 lines each                                                              |
| `.agents/skills/*/SKILL.md`                               | 68–234      | 250 lines aimed, 500 enforced `[script: scripts/ci/check-agent-skills.mjs]` |
| `.agents/skills/*/references/*.md`                        | up to 1,002 | 400 lines each                                                              |

One file already exceeds its proposed budget:
`midgard-e2e-acceptance/references/live-acceptance.md` at 1,002 lines. Split
it by branch of the runbook when next edited, not in a drive-by change.

## Out of scope

`docs/exec-plans/GOAL_SPEC.md` (1,436 lines) is a specification, not an
instruction file, and gets no budget here. What needs budgeting is the
instruction that sends agents to read it whole; see the context-bloat instance
in [smells.md](smells.md).

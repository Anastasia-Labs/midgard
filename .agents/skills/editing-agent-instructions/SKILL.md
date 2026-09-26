---
name: editing-agent-instructions
description: Decide where a rule or a hard-won lesson lives, and write it so it holds. Use before adding, editing or removing text in AGENTS.md, a nested AGENTS.md, CLAUDE.md, docs/agents/*.md, anything under .agents/skills/ or .claude/agents/; when someone proposes "document this convention" after it broke again; when a lesson, trap or owner ruling needs a permanent home; when reviewing a diff that touches those files; or when one of them has grown and needs a trim.
---

# Editing agent instructions

`AGENTS.md` and `CLAUDE.md` load in full at the start of every session. A
line added there is paid for by every task in the repository, forever. So the
question is not "is this true?" but "what is the lowest rung that can carry
it?" Text is the weakest carrier: it is read once, obeyed sometimes, and
not red when the tree moves under it.

## 1. Pick the rung

Walk down the ladder and stop at the first rung that fits.

| Rung | Carrier                                                        | Use when                                                        |
| ---- | -------------------------------------------------------------- | --------------------------------------------------------------- |
| 1    | **Check**: ESLint rule, test, CI step, or script wired into CI | A matcher or test can see the violation                         |
| 2    | **Pre-commit hook** (`.githooks/pre-commit`)                   | It needs the staged file list or the working tree, not the code |
| 3    | **Skill** (`.agents/skills/<name>/`)                           | It matters for one kind of task, not every task                 |
| 4    | **Line in an always-loaded file** (`AGENTS.md`, `CLAUDE.md`)   | It applies to every task and nothing above can carry it         |

Rung 4 is the last resort, not the default. `[review]`

- **The fix lives in the failure message.** An author reads the message when
  the check fires, not the doc that explains it. `[review]` Two models to copy:
  the `workspacePackageBoundary` message in
  [demo/eslint.config.mjs](../../../demo/eslint.config.mjs) names the right
  import and the missing `exports` entry; the plutus.json refusal in
  [.githooks/pre-commit](../../../.githooks/pre-commit) prints numbered steps.
- **A hook rung needs a CI twin.** `[review]` The hook is opt-in
  (`bash .githooks/install`) and `MIDGARD_SKIP_HOOKS=1` skips it
  (`.githooks/pre-commit:28`), so a rule enforced only there is enforced only
  for whoever installed it. Its current CI twins are the lint, format and Aiken
  formatter steps named at `.githooks/pre-commit:3-6`.
- **A rule that stays `[review]` is a candidate for rung 1.** That visibility is
  half the reason the tags exist. `[review]`

## 2. Pick the home

Rungs say how strongly a piece of knowledge is carried; this table says where
it is written down. Read it top to bottom and take the first match.

| Kind of knowledge                                             | Home                                                                                                                                          |
| ------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| A pattern a tool can detect                                   | A check (rung 1), with its remediation in the message                                                                                         |
| A procedure or trap for one kind of task                      | A skill, or a `references/` file of the skill that owns the task                                                                              |
| A consensus property a reviewer must re-check on every change | An invariants file, such as [invariants-state-queue.md](../reviewing-consensus-changes/references/invariants-state-queue.md) and its siblings |
| An owner ruling, a rejected design, a measured cap            | A decision record (below)                                                                                                                     |
| A rule for one directory tree                                 | The nearest nested `AGENTS.md`                                                                                                                |
| A term of the domain                                          | [CONTEXT.md](../../../CONTEXT.md)                                                                                                             |
| A rule for every task that no check can carry                 | Root [AGENTS.md](../../../AGENTS.md)                                                                                                          |

- **Knowledge another contributor needs lands in the repository.** `[review]`
  Private agent memory, chat and `.claude/agents/*.md` reach nobody else:
  `.gitignore:16` ignores everything under `.claude/` except the
  `.claude/skills` link. If a lesson came from memory, verify it against the
  tree before moving it; memory goes stale in both directions.
- **Decision records.** `[review]` [docs/agents/domain.md](../../../docs/agents/domain.md)
  is the router: protocol decisions go in `docs/midgard/decisions/` (no index
  file; `domain.md` links the directory), fault-proof decisions in
  `docs/fault-proofs/decisions/`, whose [README](../../../docs/fault-proofs/decisions/README.md)
  table needs a new row. Each record states context, decision, consequences,
  status and links. A superseded decision keeps its rationale and links to its
  replacement.
- **Nested `AGENTS.md` wins locally.** Four exist as of 2026-09-25:
  `demo/`, `demo/midgard-node/`, `demo/midgard-node-tools/devnet/`,
  `onchain/aiken/`. Move a directory-specific rule down, never up. [review]
- **One meaning, one place.** Never state a rule in two files; point at the
  owner instead. Grep for the subject before adding. Two copies drift into the
  conflicting-instructions smell. [review]

## 3. Write the rule

- **Write an invariant, not an instruction.** "Every validator has a passing
  and a failing emulator scenario", not "remember to add scenarios". Name the
  trigger so a reader scanning a diff knows when it applies. [review]
- **Tag what enforces it, inline, and state the blind spot beside it.** Use
  the forms `[eslint: <rule>]`, `[hook: pre-commit]`,
  `[ci: <workflow>/<step name>]`, `[script: <path>]`,
  `[aiken-test: <module>/]`, `[runtime: <symbol>]` or `[review]`, defined in
  [docs/agents/README.md](../../../docs/agents/README.md). Never claim a check
  that does not exist: a stale tag is worse than none, because a reviewer
  trusts it and skips the look. Example blind spot:
  `onchain/aiken/scripts/run-focused-check.mjs` fails unless exactly the named
  tests are collected; a raw `aiken check -m` still exits 0 when it collects
  none. [ci: repo-tools-ci/Check agent rule enforcement tags]
- **Every link says when to read it.** "Read `contracts.md` before editing
  contracts, builders or fixtures", not "see `contracts.md`". Without the
  clause an agent has no reason to spend a tool call on it. [review]
- **Date every measurement.** A count without a date cannot be told from a
  stale one. [review]

## 4. Skills

- **One skill tree, two readers.** `.claude/skills` is a symlink to
  `../.agents/skills`, so tools that look under either directory read the
  same files.
  `[ci: agent-skills-ci.yml/Check skills and the .claude/skills link]`
- **Frontmatter, size and links are checked.** `name` equals the directory
  name, `description` is present and at most 1,024 characters, `SKILL.md` is at
  most 500 lines, and every relative Markdown link in the skill resolves.
  `[script: scripts/ci/check-agent-skills.mjs]` Blind spot: it does not read
  backticked paths or judge whether the skill is still true.
- **Codex metadata.** Each skill has `agents/openai.yaml` with `display_name`,
  `short_description` and `default_prompt`, in the shape of
  [aiken-contract-build's](../aiken-contract-build/agents/openai.yaml).
  `[review]` The checker does not look for it.
- **A skill script ships a test that can fail.** A sibling `<name>.test.mjs`
  under `scripts/`, with at least one negative case; a verifier that cannot
  fail looks exactly like a healthy one.
  `[ci: agent-skills-ci.yml/Test scripts shipped inside skills]` Blind spot: CI
  runs the tests it finds; a script with no test, or a test with no negative
  case, passes.
- **Fix the tooling, then update the skill in the same change.** A skill that
  describes a workaround outlives the fix and teaches it forever. `[review]`

## 5. Check the smells

Before committing, check the edited file for the six smells: lint leakage,
context bloat, skill leakage, conflicting instructions, init fossilization and
blind references. Read [references/smells.md](references/smells.md) when you
need the definition of a smell or the verified Midgard instance of it.

Read [references/size-budgets.md](references/size-budgets.md) when a file
grows, when you add to a file every session loads, or when deciding whether content
should move into a reference. The budgets there are proposals, not checks;
enforcing them is task W2.3 of the hardening plan.

## Done when

- The rule sits on the lowest rung that can carry it, and if that is rung 1,
  the check's failure message says how to fix the violation.
- Every rule you added or touched carries one enforcement tag, and every
  non-`[review]` tag names a check you have seen fail or read.
- Every link you added says when to read it, and `node
scripts/ci/check-agent-skills.mjs` passes if you touched a skill.
- No other file states the same rule.
- The file is within its budget, or it got shorter, or you can say what the
  added lines buy every session that loads them.

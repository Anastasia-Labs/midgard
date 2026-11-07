# The six smells, with Midgard instances

Check an edited instruction file against each smell. Every instance below was
verified in the tree on 2026-09-25; some may be fixed by the time you read
this, so re-check before quoting one. Several are already listed for repair in
task W2.8 of the
[hardening plan](../../../../docs/exec-plans/agent-contribution-hardening.md).

## Lint leakage

The text restates what a check already blocks. Keep the reason, cut the rest
to the rule plus its tag: the check is the gate, the prose is only the why.

Instance: [demo/AGENTS.md](../../../../demo/AGENTS.md) line 8, "Import
workspace packages by name … never through `../<package>/src` or `dist`",
states in full what the `workspacePackageBoundary` restriction in
`demo/eslint.config.mjs` blocks, with a message that already carries the fix.
It carries no tag, so a reader cannot tell it is enforced. The fix is one line
tagged `[eslint: no-restricted-imports]` (the rule that carries the pattern).

## Context bloat

Always-loaded text that most sessions do not need.

Instance: [CLAUDE.md](../../../../CLAUDE.md) sends Goal-program work to
`docs/exec-plans/GOAL_SPEC.md`, 1,436 lines, whose §4.1 opens "Read this file
completely" (`GOAL_SPEC.md:548`). The pointer is cheap; the read it triggers
is not. A digest that routes to the needed sections belongs in between.

## Skill leakage

Instructions for one kind of task sitting in an always-loaded file.

Instance: [CLAUDE.md](../../../../CLAUDE.md) spends its "Agent skills" section
on the issue tracker and the five triage label strings. Only issue and triage
work needs them, and [docs/agents/issue-tracker.md](../../../../docs/agents/issue-tracker.md)
and [docs/agents/triage-labels.md](../../../../docs/agents/triage-labels.md)
already hold them. One pointer line with a "read this when" clause would do.

## Conflicting instructions

Two statements that disagree, usually because one aged. The tree can be one
of the two parties.

Instance: [docs/agents/naming-and-versioning.md](../../../../docs/agents/naming-and-versioning.md)
says to put "versions in wire, storage, or manifest values", not in names,
while `git ls-files | grep -cE -- '-v[0-9]+(\.|/|-|$)'` counted 402 tracked
paths with a `-vN` segment. The doc exempts "pinned evidence paths" without
saying which. Resolving it is an owner decision (plan task W2.5); until then,
do not cite the rule as settled either way.

## Init fossilization

Text generated or written once and never re-verified as the tree grew.

Instances:

- The "Repo Shape" section of [AGENTS.md](../../../../AGENTS.md) lists
  `onchain/aiken`, `demo`, `technical-spec` and `docs/agents`. It was added on
  2026-07-07 (`git log -S "## Repo Shape" -- AGENTS.md`). `offchain/` already
  existed (first added 2024-12-05), `docs-site/` arrived two days later, and
  `onchain/plutarch` exists too; none has ever been listed.
- The "Agent skills" section of [CLAUDE.md](../../../../CLAUDE.md) came from a
  one-time setup run (commit `0661e4783`, 2026-08-05). Its domain-docs line has
  since been rewritten; the rest has not been reviewed as instruction text.

If you edit next to a fossil, verify it and fix it in the same change.

## Blind references

A link or path with no statement of what it holds or when to read it.

Instances:

- [CLAUDE.md](../../../../CLAUDE.md): "Current release readiness is tracked
  in `docs/public_testnet_readiness.md`." Nothing says when an agent should
  open it.
- [AGENTS.md](../../../../AGENTS.md) Repo Shape: "`docs/agents`: progressive
  guidance; open only the relevant domain doc." It does not say which doc
  answers what; [docs/agents/README.md](../../../../docs/agents/README.md) does,
  and the line could link it.

The opposite failure, an orphan, is as bad:
[docs/agents/component-configuration.md](../../../../docs/agents/component-configuration.md)
is tracked but missing from the `docs/agents/README.md` index, and it carries
a dated "Local migration status, 2026-09-24" section, which is session state,
not guidance.

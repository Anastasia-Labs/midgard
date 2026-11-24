# The six smells, with Midgard instances

Check an edited instruction file against each smell. The instances below were
verified in the tree on 2026-11-23; re-check one before quoting it.

## Lint leakage

The text restates what a check already blocks. Keep the reason, cut the rest
to the rule plus its tag: the check is the gate, the prose is only the why.

## Context bloat

Always-loaded text that most sessions do not need. A pointer in `CLAUDE.md`
or `AGENTS.md` is cheap; the read it triggers may not be. Point at the section
a task needs, or drop the pointer when no task needs the document.

## Skill leakage

Instructions for one kind of task sitting in an always-loaded file. Move them
to the skill or `docs/agents/` file that owns the task and leave one pointer
line with a "read this when" clause.

## Conflicting instructions

Two statements that disagree, usually because one aged. The tree can be one
of the two parties.

Instance: [docs/agents/naming-and-versioning.md](../../../../docs/agents/naming-and-versioning.md)
says to put "versions in wire, storage, or manifest values", not in names,
while `git ls-files | grep -cE -- '-v[0-9]+(\.|/|-|$)'` counted 523 tracked
paths with a `-vN` segment on 2026-11-23. The doc exempts "pinned evidence
paths" without saying which. Resolving it is an owner decision; until then,
do not cite the rule as settled either way.

## Init fossilization

Text generated or written once and never re-verified as the tree grew. If you
edit next to a fossil, verify it and fix it in the same change.

## Blind references

A link or path with no statement of what it holds or when to read it.

Instance: [CLAUDE.md](../../../../CLAUDE.md): "Release readiness is tracked in
`docs/public_testnet_readiness.md`." Nothing says when an agent should open
it.

The opposite failure, an orphan, is as bad: a file under `docs/agents/` that
the [docs/agents/README.md](../../../../docs/agents/README.md) index does not
list.

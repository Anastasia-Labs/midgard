# CLAUDE.md

Follow the repository engineering guide in [AGENTS.md](AGENTS.md) — it is the
authoritative contributor instruction file for this repo (principles, repo
shape, tradeoff order, verification expectations). Read it before making
changes.

For Goal-program work, read `docs/exec-plans/GOAL_SPEC.md` for acceptance
criteria. Current release readiness is tracked in `docs/public_testnet_readiness.md`.

## Agent skills

### Issue tracker

Issues are tracked as GitHub issues on `Anastasia-Labs/midgard` via the `gh`
CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

The five canonical triage roles use their default label strings
(`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`,
`wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Read `CONTEXT.md` for terminology and `docs/agents/domain.md` for the
protocol and fault-proof decision directories.

# CLAUDE.md

Follow the repository engineering guide in [AGENTS.md](AGENTS.md) — it is the
authoritative contributor instruction file for this repo (principles, repo
shape, tradeoff order, verification expectations). Read it before making
changes.

For Goal-program work, `GOAL_SPEC.md` is the authoritative execution
specification. `GOAL_PROGRESS.md` is historical human context only; never parse
it or treat it as a task, readiness, or completion authority.

## Agent skills

### Issue tracker

Issues are tracked as GitHub issues on `Anastasia-Labs/midgard` via the `gh`
CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

The five canonical triage roles use their default label strings
(`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`,
`wontfix`). See `docs/agents/triage-labels.md`.

### Domain docs

Single-context layout — one `CONTEXT.md` and `docs/adr/` at the repo root
(created lazily by `/domain-modeling`; proceed silently while absent). See
`docs/agents/domain.md`.

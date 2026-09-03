# Agent Documentation Structure

Use this directory for guidance that is important but not needed in every
single agent turn.

## Current Structure

- `docs/agents/production-l2.md`: repository-wide safety, correctness, and
  tradeoff policy.
- `docs/agents/state-reset.md`: durable-state deletion and on-chain redeploy
  requirements.
- `docs/agents/transaction-finalization.md`: transaction completion rules.
- `docs/agents/midgard-node.md`: `demo/midgard-node`-specific compatibility
  policy.
- `docs/agents/naming-and-versioning.md`: identifier, file, and version naming
  policy (what carries a version, what never does).

Add a new domain file only when repeated repository-specific guidance cannot be
derived from code, tests, or an existing skill. Do not create placeholder files
or cite proposed paths as though they exist.

Keep the root `AGENTS.md` limited to context and rules that apply to every task
in the repository.

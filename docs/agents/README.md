# Agent Documentation Structure

Use this directory for guidance that is important but not needed in every
single agent turn.

## Suggested Structure

- `docs/agents/production-l2.md`: repository-wide safety, correctness, and
  tradeoff policy.
- `docs/agents/state-reset.md`: durable-state deletion and on-chain redeploy
  requirements.
- `docs/agents/transaction-finalization.md`: transaction completion rules.
- `docs/agents/midgard-node.md`: `demo/midgard-node`-specific compatibility
  policy.
- `docs/agents/deletion-candidates.md`: instructions that should be removed,
  merged, or rewritten before adding more guidance.

Prefer adding new files by domain, for example:

- `docs/agents/typescript.md`
- `docs/agents/testing.md`
- `docs/agents/aiken.md`
- `docs/agents/github-workflow.md`
- `docs/agents/api-design.md`

Keep the root `AGENTS.md` limited to context and rules that apply to every task
in the repository.

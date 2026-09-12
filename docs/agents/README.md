# Agent Documentation Structure

Use this directory for guidance that is important but not needed in every
single agent turn.

## Current Structure

- `docs/agents/production-l2.md`: repository-wide safety, correctness, and
  tradeoff policy.
- `docs/agents/state-reset.md`: durable-state deletion and on-chain redeploy
  requirements.
- `docs/agents/transaction-finalization.md`: transaction completion rules.
- `docs/agents/contracts.md`: trusted deployment parameters and required
  positive/negative emulator scenarios for contract changes.
- `docs/agents/naming-and-versioning.md`: identifier, file, and version naming
  policy (what carries a version, what never does).

- `docs/agents/domain.md`: terminology and architectural decision records.
- `docs/agents/withdraw-zero-yielding.md`: spending-to-rewarding delegation.
- `docs/agents/issue-tracker.md`: GitHub issue workflows.
- `docs/agents/triage-labels.md`: repository triage roles.

## Directory Guidance

- `demo/AGENTS.md`: workspace imports, builds, and operational guidance pointers.
- `demo/midgard-node/AGENTS.md`: operator command placement and source consumers.
- `demo/midgard-node-tools/devnet/AGENTS.md`: devnet acceptance configuration.
- `onchain/aiken/AGENTS.md`: Aiken constraints, builds, and validator guidance.

Keep directory-specific rules in the nearest applicable `AGENTS.md`. Shared
procedures such as contract parameter changes stay here because they span
validators, SDK builders, and deployment fixtures.

Add a new domain file only when repeated repository-specific guidance cannot be
derived from code, tests, or an existing skill. Do not create placeholder files
or cite proposed paths as though they exist.

Keep the root `AGENTS.md` limited to context and rules that apply to every task
in the repository.

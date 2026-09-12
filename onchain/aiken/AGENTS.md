# Aiken Contracts

These rules apply to `onchain/aiken`. Paths below are relative to the repository
root.

- Document readability tradeoffs needed to meet measured script-size or
  execution-budget constraints.
- Validator changes or deployment parameter application: read
  `docs/agents/contracts.md` for the shared builder, fixture, and emulator
  obligations before editing.
- Compiling or debugging contracts: use
  `.agents/skills/aiken-contract-build/SKILL.md` for the pinned compiler, build
  environment, trace settings, and focused test commands.
- Spending-to-rewarding delegation: read `docs/agents/withdraw-zero-yielding.md`
  before adding or changing a delegated redeemer arm.

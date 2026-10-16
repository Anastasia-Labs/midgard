# Aiken Contracts

These rules apply to `onchain/aiken`. Paths below are relative to the repository
root.

- Document readability tradeoffs needed to meet measured script-size or
  execution-budget constraints. [review]
- Compile, format and test only with the pinned compiler fork, run as
  `scripts/aiken` (from here `../../scripts/aiken`): it builds the commit
  `demo/flake.lock` pins with nix. The repository scripts, the pre-commit hook
  and CI reach no other aiken; never use a bare `aiken` from `PATH`.
  [script: scripts/lib/aiken.mjs]
- Never commit `onchain/aiken/plutus.json`; it is the build output of whichever
  compiler and profile last ran. Blind spot: only the pre-commit hook refuses
  it, so a commit made with hooks skipped or not installed lands it, and no CI
  step checks. [hook: pre-commit]
- Validator changes or deployment parameter application: read
  `docs/agents/contracts.md` for the shared builder, fixture, and emulator
  obligations before editing.
- Compiling or debugging contracts: use
  `.agents/skills/aiken-contract-build/SKILL.md` for the pinned compiler, build
  environment, trace settings, and focused test commands.
- Spending-to-rewarding delegation: read `docs/agents/withdraw-zero-yielding.md`
  before adding or changing a delegated redeemer arm.

# Aiken Contracts

These rules apply to `onchain/aiken`. Paths below are relative to the repository
root.

- Document readability tradeoffs needed to meet measured script-size or
  execution-budget constraints. [review]
- Compile, format and test only with the pinned compiler fork
  (`AIKEN_FORK_VERSION` in the Aiken and node workflows); stock v1.1.22 is
  unsound. `onchain/aiken/scripts/pinned-compiler.mjs` asserts the identity
  before the repository scripts and the pre-commit hook run `aiken`. Blind spot:
  a raw `aiken` command typed by hand is never checked, and `aikup` can
  silently repoint `aiken` at a stock release.
  [script: onchain/aiken/scripts/pinned-compiler.mjs]
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

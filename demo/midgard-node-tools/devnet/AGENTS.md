# Devnet Acceptance

These rules apply to the devnet generators, configuration, and acceptance
harnesses under this directory, including watcher journeys and process recovery.

- Match the target Cardano network's verified consensus and protocol parameters,
  including cost models and execution limits; Preprod is the default target.
- Document necessary local identity, genesis-start, funding, and topology
  differences. Historical test assumptions must not redefine deployment
  requirements.
- Contract builds use the Aiken `testnet` environment unless the task explicitly
  targets another environment. Before compiling or debugging contracts, use
  `.agents/skills/aiken-contract-build/SKILL.md` (path from the repository root).

# Always-succeeds emulator fixture

Status: Active

Last reviewed: 2026-09-07.

This Aiken project supplies the permissive script fixture used by SDK and node
emulator scenarios. It is not a Midgard protocol validator or a deployment
security check. The scenario under test must supply the actual protocol scripts.

From this directory, `aiken build` regenerates the tracked `plutus.json` from
`validators/`. Review generated script changes together with their test consumers.
The primary protocol blueprint is generated separately under `onchain/aiken`.

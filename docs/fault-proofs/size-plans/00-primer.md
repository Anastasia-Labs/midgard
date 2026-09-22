# L1 publication and execution acceptance

The [resolver decomposition decision](../decisions/0003-publishable-semantic-resolvers.md)
records the design choices from delivered size work. The
[availability-challenge plan](availability-challenge.md) is still active.

## Limits and evidence

Measure complete signed transactions using the applied testnet blueprint and
shared emulator protocol parameters. The 16,384-byte transaction ceiling includes
parameters and publication wrapping; raw script sizes are diagnostics only.
Reference publication must retain the 512-byte reserve (at most 15,872 signed
bytes). Execution uses 16,500,000 memory and 10,000,000,000 CPU units; evaluate all
scripts in a transaction together and preserve the applicable ledger reserve.
Reference-script bytes also incur fees and the aggregate reference-script limit.

Prefer pruning unreachable branches, then authenticated yields, then bounded
continuations where aggregate execution cannot fit. Every yield must authenticate
its role and unique dispatcher and verify the exact claim/successor. Read the
[withdraw-zero guide](../../agents/withdraw-zero-yielding.md) for that handshake.

Any new physical validator changes the applied-parameter graph. Update the SDK,
role tables, deployment manifest, submit routes, funding requirements, cancellation
and recovery, and emulator scenarios together. Asset names must remain within
Cardano's 32-byte bound and match between Aiken and TypeScript. Rebuild and derive
the catalogue/deployment identity from the resulting applied scripts.

## Verification

Follow the repository Aiken build skill for compiler selection, isolated probes,
and build locking. Do not copy an old build cache or reuse a historical blueprint
hash as current evidence. Use the declared Node/pnpm toolchain for emulator work.

A fit ledger must bind compiler and blueprint identity and include every physical
publication plus maximum-shape registered lifecycle transactions. Exercise honest
refusal, substitutions, cancellation/recovery, permanent proof, and removal.
Never establish fit through raised limits, disabled evaluation, or oversized
publication exemptions. Run the ledger's dedicated verifier after regeneration;
never change recorded digests to disguise measurements from another build.

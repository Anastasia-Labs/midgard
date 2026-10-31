# 0006 — DA attestation owns transaction-size admission

- Status: Accepted
- Scope: DA admission of retained Midgard transactions and their committed field
  preimages
- Recorded: 2026-09-10

## Decision

Transaction-size bounds are DA-attestation admission rules, not standalone
non-interactive fault statements. Before signing a block's availability
commitment, every committee member must independently verify the exact retained
payload and refuse to sign when any of these size limits is exceeded:

- the canonical full-CBOR limit for any normal or forced transaction;
- the applicable limit for any of that transaction's nine committed field
  preimages; or
- the aggregate canonical-transaction-byte limit for the block.

A quorum-attested block may therefore rely on these size facts. The fault-proof
catalogue does not need categories whose sole accusation is an oversized
transaction or oversized field preimage. Any existing oversize verdict or guard
is defense in depth and must not be cited as the security or release-acceptance
route for this boundary.

This decision is limited to byte-size admission. It does not make a malformed
envelope, a declared-length mismatch, an illegal item encoding, or any other
content-level validation fault a DA-only responsibility.

## Consequences

DA attestation tests must cover the exact maximum and the adjacent oversized
case for each limit, for both normal and forced transaction payloads, and must
show that no signature is produced after refusal. A change to any bound is a
consensus-profile and deployment-identity change and must update the attester,
operator, watcher, specification, and release evidence together.

## Authorities

- [DA payload validation](../../../demo/da-committee-node/src/da/payload.ts)
- [Consensus transaction limits](../../../demo/midgard-core/src/consensus-validation.ts)
- [Canonical V1 consensus profile](../../../demo/midgard-core/src/consensus-profile.ts)
- [DA rules](../../../technical-spec/5-ledger-rules/3-da-rules.tex)

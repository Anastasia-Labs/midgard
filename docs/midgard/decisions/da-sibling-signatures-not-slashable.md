# DA committee: signing sibling headers is not slashable

- **Status:** ACCEPTED. On-chain finding settled. Off-chain rule ruled by the
  owner on 2026-10-07 and applied (see "Off-chain rule").
- **Ticket:** C4 (#792), L1 chain-follower program (#784). C4 gates C1.
- **Question:** Does any on-chain validator or off-chain rule treat two
  content-bound commitments for sibling headers as equivocation or as
  slashable? Siblings here means the same parent but different header hashes,
  signed by the same committee member.

## On-chain finding: no

No validator compares committee signatures across headers. The only path that
takes the committee's bond is an availability timeout on a header that landed
in the state queue.

- **The bond has one slash.** `onchain/aiken/validators/da-bond-pool.ak:5`
  states that only an availability timeout slashes the pool. The
  `pool.Slash` branch (`:182`) requires two things. The state-queue mint
  redeemer must be `RemoveUnavailableBlockAfterTimeout`. The correction lock
  must be `Idle` (`:205`). The remaining branches are TopUp, which anyone can
  submit, and Begin/Cancel/CompleteWithdraw, which need the owner quorum.
  None of them reads a committee signature.
- **Attestations exist per landed header.** The attestation asset is
  `"DAAT" ++ header_hash` (`onchain/aiken/validators/da-attestation.ak:30-33`).
  `Init` (`:390`) needs a state-queue node that is already on chain and still
  `no_da_attestation` (`:413`). The output must carry that node's header hash
  (`:128-129`). `verify_indexed_signatures` (`:154`) checks each signature
  against one message (`:181`). That message is `attestation_message_v1` over
  this attestation's own commitment (`:366`). `ApplyToStateQueue` requires
  `commitment.header_hash == attestation_datum.header_hash` (`:503`).
- **Challenges target landed, attested headers.** `TimeoutChallenge`
  (`onchain/aiken/lib/midgard/availability-challenge.ak:123`) is keyed by
  header hash. `validate_open_challenge`
  (`lib/midgard/availability-challenge-validation.ak:211`, `:262`) requires a
  state-queue node in status `Attested{commitment_hash}` for that header
  (`lib/midgard/state-queue.ak`, `validate_da_challenge_open_transition_v1`).
  A sibling that never landed has no node, so nothing can challenge it.
- **Members have no standing that evidence can remove.** The committee lives
  in the DA params datum. `da-params-governor.ak:848` ignores its redeemer and
  changes the datum only under `owner_quorum_met` (`:897`).
- **Operator slashing never reads committee signatures.** These paths slash
  operators only: `scheduler.ak:892`, `settlement.ak:273-301`, and the
  state-queue `SlashOperatorForBadState` path. In the fraud-proof families,
  `verify_ed25519_signature` checks L2 transaction witnesses, not committee
  signatures.

## Off-chain rule: equivocation is one header (owner ruling 2026-10-07)

**Ruling.** A pair of committee signatures over different header hashes is
not equivocation. Equivocation means one signer, one header hash and two
different availability commitments.

**Before the ruling**, off-chain code treated a sibling pair as equivocation.
The builder returned `undefined` only for identical `headerHash ||
commitmentDigest` identities. Gossip ingest and the shared codec accepted a
cross-header pair, and the core golden vector used two header hashes. No
honest local caller produced such a pair, but a peer could relay one. The
stored record then blocked the recovery CLI
(`prior_da_conflict_evidence_requires_reconciliation`,
`demo/da-committee-node/src/l1/recovery-incident.ts:58`, since deleted with
the recovery CLI in ticket C1, #793). It also blocked bounded retention when
the pair straddled the retired set. <!-- doc-links:historical -->

**The change:**

- The shared codec refuses a cross-header pair on encode and on decode, with
  "conflicting signature/commitment evidence must name one header hash"
  (`demo/midgard-core/src/da-transport.validate-conflicting-signature-header-evidence.ts:211-216`).
  The gossip CBOR shape is unchanged; this is a validation rule only.
- `buildDaSignatureConflictEvidence`
  (`demo/da-committee-node/src/peer/signatures.ts:111`) returns `undefined`
  when the header hashes differ (`:136`). Equal headers with different
  digests still produce equivocation evidence. The guard that requires one
  deployment and one signer is kept.
- Gossip ingest
  (`demo/da-committee-node/src/committee-service.ingest-da-conflict-evidence.ts:286`)
  decodes through that codec. A cross-header pair is therefore refused with
  the same error before anything is stored.
- Retirement pinning is single-header
  (`demo/da-committee-node/src/store/retirement-transition.ts`). A stored
  record is retired with its one header. The cross-header pin expansion and
  the straddle refusal are deleted.
- Recovery gating is unchanged. Only records that can reach it are narrower.
- Stores written before the ruling may hold a relayed cross-header record.
  The codec refuses such a record on read, so a running committee could not
  read its own store without a fix. The committee store is Postgres only
  (C2), and its schema upgrade removes these records when the store opens
  (`demo/da-committee-node/src/store/postgres.schema.ts`):
  - While the `conflicting_header_hash` column still exists, the upgrade
    deletes every row whose `header_hash` differs from it.
  - It then drops the column. The digest ordering check that a new table
    carries is added in its place.
  - All of this runs in the one transaction that opens the store, and it does
    nothing on a store that is already upgraded.
  - The drop-on-open module that C4 used for this, and its JSON-store
    filtering, were deleted in C2 along with the JSON backend.

## Consequence for C1

Signing siblings is not equivocation. It does carry a retention duty. Suppose
a signed sibling lands and its bytes are not served before the response window
closes. The availability timeout then slashes the pooled bond.

**Requirement on C1:** retain the payload of every header the member signed
until that header is final (k) or provably unable to land.

## Tests

- `demo/midgard-node/tests/da-sibling-signature-emulator.test.ts` runs on the
  availability-challenge emulator harness. One member signs siblings A and B,
  and only A lands.
  - Honest polarity: every use of the B signature is refused by the validator
    that owns the check. Init for B against A's node is refused by the
    `da-attestation` mint. AddSignatures with the B signature on A is refused
    by the `da-attestation` spend. Opening a challenge on A's node with B's
    commitment is refused by the open-challenge withdrawal. Each refusal sits
    beside an accepted control transaction. A then merges, and the pool and
    the committee are unchanged.
  - Adversarial polarity: when A's data is withheld, the timeout slash takes
    exactly `da_bond` from the pool and removes A's node.
- `demo/da-committee-node/tests/conflict-evidence.test.ts` covers the builder
  and gossip ingest. Siblings return `undefined`, and ingest refuses them with
  nothing stored. One header with two digests still yields evidence and is
  stored.
- `demo/da-committee-node/tests/conflict-evidence-cross-header-upgrade.test.ts`
  rebuilds the conflict-evidence table as it was before the ruling. It seeds a
  cross-header record and a same-header record, then reopens the store. The
  cross-header record and the column are gone and the same-header record is
  kept. The conflict-evidence read and the retirement snapshot read both
  succeed, and the digest ordering check holds.
- `demo/midgard-core/tests/da-transport-conflict-evidence.test.ts` checks that
  the codec refuses a cross-header pair on both encode and decode. The golden
  vector in `da-transport-vectors.test.ts` now uses one header hash with two
  commitments.

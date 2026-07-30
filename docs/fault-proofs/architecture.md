# Fault-Proof System Architecture

> Audited 2026-07-10 against branch `tx-validation` (HEAD `269bf6b3`) plus its
> contemporaneous working tree; reconstructed on clean base `55afdc54`. Companion docs: [`catalogue-status.md`](catalogue-status.md),
> [`coverage-matrix.md`](coverage-matrix.md), [`onchain-reference.md`](onchain-reference.md),
> [`offchain-reference.md`](offchain-reference.md), [`testing-status.md`](testing-status.md),
> [`execution-plan.md`](execution-plan.md).

## 1. Purpose and security model

Midgard is an optimistic rollup: operators commit block headers to Cardano L1 and anyone
may, during the block's maturity (challenge) window, prove on L1 that the block violates a
ledger rule. A successful proof mints a permanent `fault_proof` token, which authorizes
removing the faulty header (and its descendants) from the state queue and slashing the
operator's bond.

The completeness invariant the system must satisfy is stated in
`technical-spec/C-considerations/1-protocol-invariants.tex:28`:

> _"Every invalid block can be invalidated by an L1-verified fault proof before its
> maturity period elapses, allowing it to be removed from the state queue."_

with the soundness half at `:25` ("these scripts can never succeed when targeting valid
blocks"). The spec claims the mechanism is "an onchain verification script for every
possible violation of Midgard's ledger rules" (`:33`). **That claim does not yet hold in
the implementation** — see [`coverage-matrix.md`](coverage-matrix.md) for the gap analysis.

## 2. Terminology

| Term                                     | Meaning                                                                                                                                                                                                                                                             | Anchor                                                                                                                              |
| ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| **Fault proof**                          | Permanent L1 token proving a ledger-rule violation in a committed block. Historically called "fraud proof"; clean-base source paths retain that older name pending roadmap item W-O8.                                                                               | `technical-spec/4-proof-protocol/2-fraud-proof-tokens.tex:8,31`                                                                     |
| **Fault-proof catalogue**                | Init-time MPF root committing `{4-byte category id → first-step script hash}`; immutable after genesis (spend validator always fails). Legitimacy of a proof category = MPF membership against this root.                                                           | `technical-spec/4-proof-protocol/1-fraud-proof-catalogue.tex:10-20,38-39`; `onchain/aiken/validators/fraud-proof-catalogue.ak:7-27` |
| **Computation thread**                   | CPS-style state machine splitting a proof into sequential spend-validator steps; a thread NFT named `category_id ‖ header_hash(28B)` traverses them. Redeemers: `Init` / `Success` / `BurnForCancellation`.                                                         | `technical-spec/4-proof-protocol/3-computation-thread.tex:8-9,44-46`; `onchain/aiken/validators/computation-thread.ak:20-149`       |
| **State queue**                          | L1 linked list of committed block headers awaiting confirmation; FIFO merge after maturity. `RemoveFaultyBlockHeader` is the correction path.                                                                                                                       | `technical-spec/3-consensus-protocol/4-state-queue.tex:8,106-153`; `onchain/aiken/validators/state-queue.ak:524-712`                |
| **Maturity period**                      | Canonical V1 fixes the challenge, merge, and operator bond-hold window at seven days (`604,800,000` ms); it is not environment-selectable.                                                                                                                          | `onchain/aiken/lib/midgard/ledger-state.ak`; canonical consensus profile                                                            |
| **Operator bond / slashing**             | Bond forfeited when a block is proven faulty; split historical source identifier `fraud_prover_reward` + `slashing_penalty` = `required_bond`. **All four economics params are `0` in both envs.**                                                                  | `technical-spec/3-consensus-protocol/2-operator-directory.tex:19-24`; `onchain/aiken/env/default.ak:21-35`                          |
| **Transition trace**                     | Per-block dense map `step_index → TransitionStep{event_key, phase, pre/post_utxos_root}` committed via `transition_trace_root` + `event_to_step_root`; enables one-step re-execution disputes.                                                                      | `technical-spec/1-ledger-state/1-block.tex:186-234`                                                                                 |
| **Counted / domain-tagged root**         | `blake2b_256(tag ‖ cbor(domain) ‖ raw_root ‖ cbor(count))` — commits member count and domain alongside an MPF root, so proofs can open membership, non-membership, and count.                                                                                       | `onchain/aiken/lib/midgard/transition-trace.ak:9-16,64-80`                                                                          |
| **phas / pexcludes**                     | Withdraw-zero "merkelized validator" scripts performing MPF membership (`mpf.has`) and non-membership (`mpf.insert` must succeed) checks, invoked by proof steps via reference scripts.                                                                             | `onchain/aiken/validators/phas.ak:15`, `pexcludes.ak:22`; `lib/midgard/common/utils.ak:597-719`                                     |
| **DA attestation**                       | Committee threshold-signs `"MidgardDAAttestationV1" ‖ header_hash`; at threshold the `DAAT` token is burned and the state-queue node's `da_attestation` field set, gating maturation. The validator never inspects payload bytes.                                   | `onchain/aiken/validators/da-attestation.ak:29-32,336-376`                                                                          |
| **Inclusion time / event interval**      | L1 user events (deposits, withdrawal orders, tx orders) get inclusion times; each block's non-overlapping, gapless event interval obligates inclusion — the censorship-protection invariant.                                                                        | `technical-spec/2-user-event-protocol/1-deposit.tex:27-33`; `C-considerations/1-protocol-invariants.tex:56-62`                      |
| **Witness staking script**               | Per-event registered staking credential whose (non-)registration lets a Plutus script disprove the existence of an L1 event UTxO — the intended hook for fabricated-deposit/withdrawal proofs (no construction implemented).                                        | `technical-spec/2-user-event-protocol/1-deposit.tex:12`; `4-withdrawal-order.tex:106-120`                                           |
| **Phase A / Phase B (local validation)** | The node's two-phase mempool admission: Phase A stateless per-tx checks; Phase B stateful UTxO/graph/script-execution checks. Reject codes are "operational evidence, not L1 fault proofs" (`technical-spec/7-phase-two-validation/3-fraud-proofs-involved.tex:9`). | `demo/midgard-validation/src/phase-a.ts:338`, `phase-b.ts:1072`                                                                     |

### Proof interaction classification

The governing rule is `GOAL_SPEC.md` §3: every violation that one prover can
establish from retained public authenticated evidence uses a single-party
proof. "Single-party" describes who must participate, not how many Cardano
transactions the proof consumes. L1 size or execution limits may require an
ordered multi-step, multi-transaction computation-thread chain without making
the proof interactive.

Challenge/response is permitted only where sound resolution intrinsically
requires competing authenticated execution traces, an adversarial response, or
a withholding deadline. Convenience, transaction size, execution cost, or
implementation reuse are not sufficient reasons. Each interactive proof family
must carry executable evidence demonstrating why a single-party construction is
insufficient.

## 3. End-to-end proof lifecycle

```
 detect fault          build evidence            L1 dispute                    state correction
┌─────────────┐   ┌─────────────────────┐   ┌─────────────────────────┐   ┌──────────────────────────┐
│ (manual /   │   │ prepare-* CLI or    │   │ submit-init: mint thread │   │ remove-fraudulent-block:     │
│ library     │──▶│ transition-trace    │──▶│ NFT (catalogue member-   │──▶│ per descendant link, one │
│ detect.ts;  │   │ reconstruct+witness │   │ ship proof) → step-01..N │   │ RemoveFaultyBlocksLink tx│
│ no watcher) │   │ from node API / DA  │   │ → final step finalize()  │   │ … then RemoveLastFaulty- │
└─────────────┘   │ payload / fixtures  │   │ burns thread, mints      │   │ Block + SlashOperator in │
                  └─────────────────────┘   │ permanent fault_proof    │   │ the same tx              │
                                            └─────────────────────────┘   └──────────────────────────┘
```

1. **Detection** — today entirely manual or library-level. `demo/midgard-watcher/` contains
   two design docs and zero code; `demo/midgard-node` never imports
   `@al-ft/midgard-fault-proofs`. The closest challenger logic is
   `demo/midgard-fault-proofs/src/transition-trace/detect.ts`, a pure function library with
   no polling loop. Mempool rejections (`RejectCodes`) are deliberately **not** mapped to
   fault categories: admission handles rejected transactions, while fault proofs target
   committed blocks. The resulting committed-block classifier gap is tracked in
   [`coverage-matrix.md`](coverage-matrix.md) §11.
2. **Evidence construction** — MPF tries and proofs built off-chain with
   `@aiken-lang/merkle-patricia-forestry` (`prepare-*.ts`, `ne-proofs.ts`,
   `transition-trace/phas.ts`). Data sources: live midgard-node REST (`GET /block`, `/tx`),
   local JSON/CBOR fixture files, or (transition-trace only) libp2p retrieval of the
   committee-retained `DaPayloadV1` (`transition-trace/fetch.ts`).
3. **Init** — `computation-thread.ak` mints the thread NFT after verifying catalogue
   membership of the category and prover signature
   (`onchain/aiken/validators/computation-thread.ak:42-121`).
4. **Steps** — each category's `step-NN.ak` spend validators consume evidence (redeemer
   data, reference-witness UTxOs, MPF proofs via `phas`/`pexcludes` withdrawals) and pass
   the NFT forward (`lib/midgard/fraud-proofs/common.ak:67,165,317`). The generic layer
   does **not** validate step sequencing — correctness is delegated wholly to each
   category's own step chain (`validators/computation-thread.ak:130-139`).
5. **Conclusion** — the final step's `finalize` (`common.ak:391-482`) burns the thread
   token via `Success` and mints the permanent `fault_proof` token
   (`validators/fraud-proof.ak:17-63`; its spend validator always fails, so the token is
   permanent). Uniqueness relies on deterministic naming + 1:1 burn-on-mint, not a
   one-shot UTxO — a duplicate `Init` for the same header can mint a second unit of the
   same asset name (benign for removal, relevant for reward accounting).
6. **State correction** — `state-queue.ak` `RemoveFaultyBlockHeader` (`:524-712`)
   references (not spends) the fault-proof token whose last 28 bytes match the faulty
   header hash, then either splices out one descendant (`RemoveFaultyBlocksLink`) or
   removes the now-tail faulty block (`RemoveLastFaultyBlock`). Every removal tx must
   co-execute a `slashing_approach` (`SlashActiveOperator` / `SlashRetiredOperator` /
   `OperatorAlreadySlashed`) cross-validated against the operator directory
   (`lib/midgard/operator-directory.ak:220-356`).
   Off-chain, `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts:2373-2422` loops
   successor removals until the faulty block is the tail, then removes it; non-tail
   removal requires the node's `/stateQueueMutationLease` HTTP coordinator (`:2200-2223`).
7. **Restoration of canonical state** — after removal, the next committed block's event
   interval must cover the removed blocks' intervals, forcing re-inclusion of the affected
   L1 events (`technical-spec/2-user-event-protocol/1-deposit.tex:36-38`).

### Known architectural seams (see coverage matrix for severity)

- **Cross-operator descendant removal deadlock.** `RemoveFaultyBlocksLink` checks the
  _descendant's_ `operator_vkey == faulty_operator`
  (`onchain/aiken/validators/state-queue.ak:661`), while the adjacent comment (`:633-636`)
  says a descendant needs no fault proof of its own. With scheduler shift rotation, a
  descendant committed by a different operator fails this check and the cascade cannot
  proceed on-chain.
- **Slashing economics are inert.** `slashing_penalty`, `fraud_prover_reward`,
  `required_bond`, `inactivity_slashing_penalty` are all `0`
  (`onchain/aiken/env/default.ak:21-35`, `env/testnet.ak:20-26`); the penalty is enforced
  only as `fee >= env.slashing_penalty`, and nothing on-chain routes the bond remainder to
  the prover.
- **Catalogue registration ≠ compiled validators.** The deployment layer registers only 5
  categories (`demo/midgard-sdk/src/fraud-proof/catalogue.ts:23-29`,
  `common.ts:162-168`) of the 12 compiled proof-type families — the other 7 cannot `Init`
  a thread against a deployed instance. The `FaultProofs` type also carries a TODO that
  multi-step registration needs "a more elaborate design" (`common.ts:160-161`).
- **`Success` trusts the terminal step.** `computation-thread.ak`'s `Success` branch only
  checks its own burn (`validators/computation-thread.ak:130-139`); thread/step-sequence
  integrity is each category's responsibility.

## 4. Evidence and data formats

### Committed on L1 (per block header)

Fixed-size header, Blake2b-224 hashed (`technical-spec/1-ledger-state/1-block.tex:130-181`;
Aiken type `onchain/aiken/lib/midgard/ledger-state.ak:57-77`):

- **8 roots**: `prev_utxos_root`, `utxos_root`, `withdrawals_root`,
  `forced_transactions_root`, `transactions_root`, `deposits_root`,
  `transition_trace_root`, `event_to_step_root` — all counted, domain-separated MPF
  commitments (`1-block.tex:70`).
- **6 counts** with equational constraints (`1-block.tex:167-176`).
- `start_time`/`end_time`, `prev_header_hash`, `operator_vkey`, `protocol_version`.

### Transaction commitment format

`MidgardTxCompactV1 = (version, body_hash, wits_hash, validity_code)`; the compact body
holds fixed-size roots/scalars only; the full form pairs each root with its preimage "for
offchain checks and proof generation"
(`technical-spec/1-ledger-state/6-transaction.tex:222-289`). On-chain decoding is a real
byte-offset CBOR codec (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/` — codec,
compact, transaction, preimages, components; ~2.6k lines, hash-checked round-trips). The
native-tx model supports **verification-key and timelock (native-script) witnesses only** —
there is no Plutus-script concept in the L1-disputable format
(`technical-spec/7-phase-two-validation/3-fraud-proofs-involved.tex:14-16`).

### Retained off-chain (evidence sources)

- **DA committee payload**: `DaPayloadV1` (header + utxos + four event member arrays +
  trace + event-to-step + counts) keyed by header hash
  (`technical-spec/6-offchain-data-architecture/1-da-layer.tex:20-26,39-53`). Committee
  members verify 7 roots + 6 counts against the L1 header before signing
  (`demo/da-committee-node/src/da/payload.ts:173-260,807-861`).
- **Proof artifacts served by the DA node** (`demo/da-committee-node/src/da/proof-artifacts.ts:136-354`):
  proof bundle (roots+counts CBOR), transition-trace step + MPF membership proof,
  event-to-step (non-)membership proof — re-verified against the stored L1 header at
  request time (`:356-457`).
- **Wire contract** (`demo/midgard-core/src/da-transport.ts:62-72`): libp2p protocols
  `payload-submit`, `payload-by-header`, `payload-chunk`, `metadata-by-header`,
  `proof-bundle-by-header`, `trace-step-by-index`, `event-to-step-by-event`,
  `attestations-by-header` (plus a `capabilities` handshake); 64 MiB payload / 1 MiB chunk limits, zstd envelope with
  sha256 verification (`da-payload-envelope.ts:237-313`).
- **Node database**: raw tx CBOR in `immutable` (never pruned), full DA payload bytes +
  roots in `da_payloads` (prunable), `blocks` mapping deleted at merge
  (`demo/midgard-node/TX_VALIDATION_TABLE_ROLES.md:91-95,150-151`).

### Retention vs the challenge window (disagreement)

Spec expects a 3–7 day maturity window; the DA architecture doc promises ≥14 days
retention with a 15-day config floor (`demo/da-committee-node/docs/da-committee-node-architecture.md:72,591-593`;
`DA_TRANSPORT_LIMITS_V1.minimumRetentionDays = 15`, `demo/midgard-core/src/da-transport.ts:34`);
the node enforces an 8-day floor but pruning is **disabled by default**
(`demo/midgard-node/src/database/retention-policy.ts:3,11`); the DA committee store has
**no deletion capability at all** (`demo/da-committee-node/src/store.ts:43-85`). No code
ties any retention deadline to the actual on-chain maturity deadline.

## 5. Trust assumptions (current implementation)

1. **DA committee honesty** — attestation is signature-only over the header hash; the
   validator never checks payload content, and no on-chain remedy exists if attested data
   is withheld (see [`coverage-matrix.md`](coverage-matrix.md) §DA). The committee
   pre-signing checklist (`technical-spec/5-ledger-rules/3-da-rules.tex:13-28`) is part of
   the soundness argument but is off-chain and unslashable.
2. **An active challenger exists** — no autonomous watcher exists; detection and the
   multi-step submission chains are manual.
3. **Economics parameters will be set** — with zeroed bond/penalty/reward and a 30 ms
   maturity window, a successful proof currently neither deters nor compensates.
4. **Operator monotonicity for cascades** — descendant removal currently assumes the
   descendant shares the faulty block's operator (see §3 seams).

# Midgard Plutarch Merkle Helpers

Status: Legacy supporting package. Aiken under `onchain/aiken` is the primary
on-chain implementation; this package supplies Plutarch Merkle Patricia
Forestry helpers and generated membership/non-membership scripts used by proof
work.

Last reviewed: 2026-08-23

## Off-chain blueprint

The node consumes an Aiken-blueprint-shaped `validators` array even when the
script compiler is Plutarch. Build the deterministic compatibility artifact
from the individual files in `generated/` with:

```bash
node scripts/build-offchain-blueprint.mjs
```

This writes the ignored `plutus.json` artifact. For the demo node, add
`docker-compose.plutarch.yaml` after the base Compose file so both the runtime
and migration images copy that artifact instead of the Aiken blueprint. Do not
mix images or deployment manifests produced from different contract sources.

The current Plutarch build targets the experimental `plutus-core 1.65` WSC/Van
Rossem implementation. The official Cardano node 11.0.1 release uses
`plutus-core 1.63`, so these artifacts cannot initialize on that node even when
the network reports protocol version 11. Run the real-contract UPLC evaluation
test against the target node stack before publishing reference scripts.

## Code map

- `src/MerkleTree/`: the MPF membership and non-membership validators and their
  redeemer types. These correspond to Aiken's `validators/phas.ak` and
  `validators/pexcludes.ak`.
- `src/Midgard/`: the port of the Aiken contracts, plus the pre-existing Midgard
  domain types (blocks, state commitments, transaction model).
- `src/Types/Classes.hs`, `src/Profile.hs`: toolchain-generic helpers belonging
  to neither namespace.

## Port status

Ported from `../aiken`, with behavioural tests in `tests/Testing/`:

| Aiken source | Plutarch module |
| --- | --- |
| `validators/phas.ak`, `validators/pexcludes.ak` | `MerkleTree.Validators.Membership` |
| `validators/hub-oracle.ak` | `Midgard.Validators.HubOracle` |
| `lib/midgard/hub-oracle.ak` | `Midgard.HubOracle` |
| `validators/fraud-proof-catalogue.ak` | `Midgard.Validators.FraudProofCatalogue` |
| `lib/midgard/fraud-proof-catalogue.ak` | `Midgard.FraudProofCatalogue` |
| `validators/fraud-proof.ak` | `Midgard.Validators.FraudProof` |
| `lib/midgard/fraud-proof.ak` | `Midgard.FraudProof` |
| `validators/reserve.ak` | `Midgard.Validators.Reserve` |
| `lib/midgard/reserve.ak` | `Midgard.Reserve` |
| `lib/midgard/computation-thread.ak` | `Midgard.ComputationThread` |
| `validators/computation-thread.ak` | `Midgard.Validators.ComputationThread` |
| `lib/midgard/payout.ak` | `Midgard.Payout` |
| `validators/payout.ak` | `Midgard.Validators.Payout` |
| `lib/midgard/ledger-state.ak` (hash aliases only) | `Midgard.LedgerState` |
| `lib/midgard/operator-directory.ak` | `Midgard.OperatorDirectory` |
| `validators/operator-directory/retired-operators.ak` | `Midgard.Validators.RetiredOperators` |
| `lib/midgard/operator-directory/retired-operators.ak` | `Midgard.OperatorDirectory.RetiredOperators` |
| `validators/operator-directory/registered-operators.ak` | `Midgard.Validators.RegisteredOperators` |
| `lib/midgard/operator-directory/registered-operators.ak` | `Midgard.OperatorDirectory.RegisteredOperators` |
| `validators/operator-directory/active-operators.ak` | `Midgard.Validators.ActiveOperators` |
| `lib/midgard/operator-directory/active-operators.ak` | `Midgard.OperatorDirectory.ActiveOperators` |
| `lib/midgard/scheduler.ak` | `Midgard.Scheduler` |
| `validators/scheduler.ak` | `Midgard.Validators.Scheduler` |
| `validators/user-events/deposit.ak` | `Midgard.Validators.Deposit` |
| `validators/user-events/withdrawal.ak` | `Midgard.Validators.Withdrawal` |
| `lib/midgard/user-events.ak` (mint side) | `Midgard.UserEvents` |
| `lib/midgard/user-events/witness.ak` | `Midgard.UserEvents.Witness` |
| `validators/user-events/witness.ak` | `Midgard.Validators.Witness` |
| `lib/midgard/user-events/deposit.ak` | `Midgard.UserEvents.Deposit` |
| `lib/midgard/user-events/withdrawal.ak` | `Midgard.UserEvents.Withdrawal` |
| `lib/midgard/ledger-state.ak` (event types, `HeaderV1`, `ConfirmedState`, header validity) | `Midgard.LedgerState` |
| `lib/midgard/state-queue.ak` | `Midgard.StateQueue` |
| `validators/state-queue.ak` | `Midgard.Validators.StateQueue` |
| `lib/midgard/da-attestation-types.ak` (mint redeemer, params datum) | `Midgard.DaAttestation` |
| `validators/da-attestation.ak` (signature and bitmap layer) | `Midgard.DaAttestation.Signatures` |
| `validators/da-attestation.ak` (authenticated-read layer) | `Midgard.DaAttestation.Readers` |
| `validators/da-attestation.ak` (lifecycle helpers, `validate_add_signatures`) | `Midgard.DaAttestation.Operations` |
| `validators/da-attestation.ak` (mint and spend handlers) | `Midgard.Validators.DaAttestation` |
| `validators/da-params-governor.ak` | `Midgard.Validators.DaParamsGovernor` |
| `validators/settlement.ak` | `Midgard.Validators.Settlement` |
| `lib/midgard/settlement.ak` | `Midgard.Settlement` |
| `validators/user-events/tx-order-v1.ak` | `Midgard.Validators.TxOrder` |
| `lib/midgard/user-events/tx-order-v1.ak` | `Midgard.UserEvents.TxOrder` |
| `validators/user-events/tx-field-preimage-v1.ak`, `validators/user-events/tx-field-receipt-spend-v1.ak` | `Midgard.Validators.TxOrderFields` |
| `validators/user-events/cek-program-material-v1.ak` | `Midgard.Validators.CekProgramMaterial` |
| `lib/midgard/user-events/tx-field-receipt-v1.ak` | `Midgard.UserEvents.TxFieldReceipt` |
| `validators/user-events/tx-field-receipt-v1.ak` | `Midgard.Validators.TxFieldReceipt` |
| `lib/midgard/ledger-state.ak` (field preimage, receipt and program-material datums) | `Midgard.LedgerState` |
| `lib/midgard/bounded-collection-v1.ak` | `Midgard.LedgerState` (shape), `Midgard.BoundedCollection` (logic) |
| `lib/midgard/bounded-item-v1.ak` | `Midgard.BoundedItem` |
| `lib/midgard/bounded-blob-v1.ak` | `Midgard.BoundedBlob` |
| `lib/midgard/ledger-output-v1.ak` | `Midgard.LedgerOutput` |
| `lib/midgard/ledger-output-commitment-v1.ak` | `Midgard.LedgerOutputCommitment` |
| `lib/midgard/ledger-output-value-v1.ak` | `Midgard.LedgerOutputValue` |
| `lib/midgard/ledger-output-scan-v1.ak` | `Midgard.LedgerOutputScan` |
| `lib/midgard/ledger-output-proof-v1.ak` | `Midgard.LedgerOutputProof` |
| `lib/midgard/native-script-scan-v1.ak` | `Midgard.NativeScriptScan` |
| `lib/midgard/native-script-v1.ak` | `Midgard.NativeScript` |
| `lib/midgard/native-tx-script-pushdown-v1.ak` | `Midgard.NativeTxScriptPushdown` |
| `lib/midgard/redeemer-item-proof-v1.ak` | `Midgard.RedeemerItemProof` |
| `lib/midgard/script-sources-redeemer-normalization-v1.ak` | `Midgard.ScriptSourcesRedeemerNormalization` |
| `lib/midgard/native-tx-field-access-v1.ak` | `Midgard.NativeTxFieldAccess` |
| `lib/midgard/native-tx-carriage-v1.ak` | `Midgard.NativeTxCarriage` |
| `validators/field-preimage-certificate.ak` | `Midgard.Validators.FieldPreimageCertificate` |
| `lib/midgard/native-tx-machine-walk-v1.ak` | `Midgard.NativeTxMachineWalk` |
| `lib/midgard/native-tx-intra-item-v1.ak` | `Midgard.NativeTxIntraItem` |
| `lib/midgard/native-tx-fault-statement-v1.ak` | `Midgard.NativeTxFaultStatement` |
| `aiken/cbor.deserialise` (stdlib v3.1.0) | `Aiken.Cbor` |
| `lib/midgard/cek-cost-v1.ak` | `Midgard.CekCost` |
| `lib/midgard/cek-data-v1.ak` | `Midgard.CekData` |
| `lib/midgard/cek-proof-v1.ak` | `Midgard.CekProof` |
| `lib/midgard/cek-constant-v1.ak` | `Midgard.CekConstant` |
| `lib/midgard/cek-blob-frontier-v1.ak` | `Midgard.CekBlobFrontier` |
| `lib/midgard/cek-data-frame-v1.ak` | `Midgard.CekDataFrame` |
| `lib/midgard/blake2b-256-trace-v1.ak` | `Midgard.Blake2b256Trace` |
| `lib/midgard/blake2b-224-trace-v1.ak` | `Midgard.Blake2b224Trace` |
| `lib/midgard/cek-source-blob-v1.ak` | `Midgard.CekSourceBlob` |
| `lib/midgard/cek-data-integer-v1.ak` | `Midgard.CekDataInteger` |
| `lib/midgard/cek-data-bytes-v1.ak` | `Midgard.CekDataBytes` |
| `lib/midgard/cek-data-scan-v1.ak` | `Midgard.CekDataScan` |
| `lib/midgard/cek-data-traverse-v1.ak` | `Midgard.CekDataTraverse` |
| `lib/midgard/cek-builtin-v1.ak` | `Midgard.CekBuiltin` |
| `lib/midgard/mpf-proof-v1.ak` | `Midgard.MpfProof` |
| `lib/midgard/mpf-proof-fold-v1.ak` | `Midgard.MpfProofFold` |
| `lib/midgard/mpf-chunked-proof-v1.ak` | `Midgard.MpfChunkedProof` |
| `lib/midgard/fraud-proofs/chunked-inclusion-v1.ak` | `Midgard.FraudProofs.ChunkedInclusion` |
| `lib/midgard/fraud-proofs/common.ak` | `Midgard.FraudProofs.Common` |
| `lib/midgard/fraud-proofs/field-opening-v1.ak` | `Midgard.FraudProofs.FieldOpening` |
| `validators/mpf-chunked-verify.ak` | `Midgard.Validators.MpfChunkedVerify` |
| `lib/midgard/fraud-proofs/double-spend/step-0{1..4}.ak` | `Midgard.FraudProofs.DoubleSpend` |
| `validators/fraud-proofs/double-spend/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.DoubleSpend` |
| `lib/midgard/fraud-proofs/zero-input/step-0{1,2}.ak` | `Midgard.FraudProofs.ZeroInput` |
| `validators/fraud-proofs/zero-input/step-0{1,2}.ak` | `Midgard.Validators.FraudProofs.ZeroInput` |
| `lib/midgard/fraud-proofs/no-input/step-0{1..4}.ak` | `Midgard.FraudProofs.NoInput` |
| `validators/fraud-proofs/no-input/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.NoInput` |
| `lib/midgard/fraud-proofs/input-no-idx/step-0{1..4}.ak` | `Midgard.FraudProofs.InputNoIdx` |
| `validators/fraud-proofs/input-no-idx/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.InputNoIdx` |
| `lib/midgard/fraud-proofs/reference-input-no-idx/step-0{1..4}.ak` | `Midgard.FraudProofs.ReferenceInputNoIdx` |
| `validators/fraud-proofs/reference-input-no-idx/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.ReferenceInputNoIdx` |
| `lib/midgard/fraud-proofs/no-reference-input/step-0{1..4}.ak` | `Midgard.FraudProofs.NoReferenceInput` |
| `validators/fraud-proofs/no-reference-input/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.NoReferenceInput` |
| `lib/midgard/fraud-proofs/missing-signature/step-0{1..4}.ak` | `Midgard.FraudProofs.MissingSignature` |
| `validators/fraud-proofs/missing-signature/step-0{1..4}.ak` | `Midgard.Validators.FraudProofs.MissingSignature` |
| `lib/midgard/fraud-proofs/invalid-signature/step-0{1,2}.ak` | `Midgard.FraudProofs.InvalidSignature` |
| `validators/fraud-proofs/invalid-signature/step-0{1,2}.ak` | `Midgard.Validators.FraudProofs.InvalidSignature` |
| `lib/midgard/fraud-proofs/missing-native-script-tx/step-0{1..6}.ak` | `Midgard.FraudProofs.MissingNativeScriptTx` |
| `validators/fraud-proofs/missing-native-script-tx/step-0{1..6}.ak` | `Midgard.Validators.FraudProofs.MissingNativeScriptTx` |
| `lib/midgard/fraud-proofs/invalid-range/step-0{1,2}.ak` | `Midgard.FraudProofs.InvalidRange` |
| `validators/fraud-proofs/invalid-range/step-0{1,2}.ak` | `Midgard.Validators.FraudProofs.InvalidRange` |
| `lib/midgard/fraud-proofs/min-fee/step-0{1,2}.ak` | `Midgard.FraudProofs.MinFee` |
| `validators/fraud-proofs/min-fee/step-0{1,2}.ak` | `Midgard.Validators.FraudProofs.MinFee` |
| `lib/midgard/fraud-proofs/da-hash-preimage/{rule,step-01,step-02}.ak` | `Midgard.FraudProofs.DaHashPreimage` |
| `validators/fraud-proofs/da-hash-preimage/step-0{1,2}.ak` | `Midgard.Validators.FraudProofs.DaHashPreimage` |
| `lib/midgard/fraud-proofs/withdrawn-reference-input/step-0{1,2,3}.ak` | `Midgard.FraudProofs.WithdrawnReferenceInput` |
| `validators/fraud-proofs/withdrawn-reference-input/step-0{1,2,3}.ak` | `Midgard.Validators.FraudProofs.WithdrawnReferenceInput` |
| `lib/midgard/script-proof-v1.ak` | `Midgard.ScriptProof` |
| `lib/midgard/script-context-v1.ak` | `Midgard.ScriptContext` |
| `lib/midgard/script-language-views-v1.ak` | `Midgard.ScriptLanguageViews` |
| `lib/midgard/validation-merkle-v1.ak` | `Midgard.ValidationMerkle` |
| `lib/midgard/fraud-proofs/native-tx/codec.ak` (all but `expect_deserialise`) | `Midgard.FraudProofs.NativeTx.Codec` |
| `lib/midgard/canonical-plutus-data-v1.ak` | `Midgard.CanonicalPlutusData` |
| `lib/midgard/canonical-cbor-scan-v1.ak` | `Midgard.CanonicalCborScan` |
| `lib/midgard/intra-item-bytes-v1.ak` | `Midgard.IntraItemBytes` |
| `lib/midgard/fraud-proofs/native-tx/types.ak` | `Midgard.FraudProofs.NativeTx.Types` |
| `lib/midgard/fraud-proofs/native-tx/compact.ak` | `Midgard.FraudProofs.NativeTx.Compact` |
| `lib/midgard/fraud-proofs/native-tx/components.ak` | `Midgard.FraudProofs.NativeTx.Components` |
| `lib/midgard/fraud-proofs/native-tx/preimages.ak` | `Midgard.FraudProofs.NativeTx.Preimages` |
| `lib/midgard/fraud-proofs/native-tx/transaction.ak` | `Midgard.FraudProofs.NativeTx.Transaction` |
| `lib/midgard/transition-trace.ak` (counted-root proofs) | `Midgard.TransitionTrace` |
| `lib/midgard/validation-trace-v1.ak` | `Midgard.ValidationTrace` |
| `lib/midgard/validation-claim-v1.ak` | `Midgard.ValidationClaim` |
| `lib/midgard/validation-dispute-v1.ak` | `Midgard.ValidationDispute` |
| `lib/midgard/fraud-proofs/transition-trace/proof.ak` | `Midgard.FraudProofs.TransitionTrace.Proof` |
| `lib/midgard/fraud-proofs/transition-trace/final-v1.ak` | `Midgard.FraudProofs.TransitionTrace.FinalV1` |
| `validators/fraud-proofs/transition-trace/{control,source,withdrawal,forced,accepted-transaction,deposit,l1-event,duplicate,route}-v1.ak` | `Midgard.Validators.FraudProofs.TransitionTrace` |
| `lib/midgard/validation-machine-v1.ak` (`encode_transaction_field_scan_witness`) | `Midgard.ValidationMachine` |
| `lib/midgard/cek-machine-v1.ak` | `Midgard.CekMachine` |
| `lib/midgard/canonical-decode-item-staging-v1.ak` | `Midgard.CanonicalDecodeItemStaging` |
| `lib/midgard/validation-{game,award,resolution,semantic,resolver}-v1.ak` (foundation and resolver selection) | `Midgard.ValidationGame`, `Midgard.ValidationAward`, `Midgard.ValidationResolution`, `Midgard.ValidationSemantic`, `Midgard.ValidationResolver` |
| `env/default.ak` (constants used) | `Midgard.Env` |
| `lib/midgard/common/types.ak` | `Midgard.Common.Types` |
| `lib/midgard/common/utils.ak` (partial) | `Midgard.Common.Utils` |

Where an Aiken `test` block exists for a ported validator, the Plutarch test
carries the same name and the same fixtures, so a divergence between the two
implementations fails the same case on each side. That covers the four
catalogue tests, the seven reserve tests, the four deposit-mint tests, and the
six `q49_l295_*` state-queue tests, and the eleven `ct_*` computation-thread
tests.

### What is left

The known implementation gaps are closed, including the retained transaction-
order counted path. Remaining work is a named-test parity audit: Aiken and
Plutarch often cover the same predicate under different test names, so the audit
must distinguish missing behavior from naming-only differences before adding
more tests.

**Correction.** An earlier version of this paragraph put
`validators/field-preimage-certificate.ak` (224) behind the same retired layer.
It is not: neither it nor `lib/midgard/native-tx-carriage-v1.ak` mentions
`bounded_collection_v1` at all. The carriage module *is* the §8.6 permissionless
chunk-certificate design named below as the replacement for the retired surface,
and the validator is the thin transaction-shape wrapper around it. Both are now
ported — see "The certificate at the top of the ladder" below.

The transaction-order mint handler, both field-carriage spend validators, the
program-material address, and both arms of the field-receipt policy are now
ported. `PublishField` and `verify_order_receipts` retain the legacy counted
checks literally even though honest flat commitments fail closed at that gate.

**The fraud-proof machine**, now started from the bottom.
`lib/midgard/validation-merkle-v1.ak` — the Merkle frontier every bounded proof
in the machine is built on — and `lib/midgard/bounded-item-v1.ak` on top of it
are ported, each tested against an independent reference implementation.

The path from here does **not** continue through `bounded-collection-v1.ak` —
that module is retired surface the Aiken tree explicitly says not to bind to
(again, see below). It continues into `fraud-proofs/native-tx/` (4,141 across
eight files), which is live: it is what `native_tx_field_access_v1`, the current
field-access door, is built on. **`fraud-proofs/native-tx/` is now complete** —
`codec.ak` (440), `types.ak` (209), `compact.ak` (514), `components.ak` (891),
`preimages.ak` (683) and `transaction.ak` (1,114) — together with the §5.1
envelope from `native-tx-field-access-v1.ak` that `preimages.ak` sits on,
including the retained `verify_midgard_transaction_field_chunk_v1` surface.

`native-tx-field-access-v1.ak` (922) is now complete: the carriage types, the
stride table, the three tiers and `authenticated_field_view` itself. See "The
field-access door" below.

The **proof-carriage layer under the fraud-proof families** is also done:
`mpf-proof-v1.ak` (406), `mpf-chunked-proof-v1.ak` (354) and
`fraud-proofs/chunked-inclusion-v1.ak` (226). Porting the first of those turned
up two disagreements with `plutarch-onchain-lib` — one arithmetic, one an ABI
break — both written up below and **both needing an owner decision before the
fraud-proof families are ported**, since every family opens a root through this
layer.

`lib/midgard/fraud-proofs/common.ak` (925) — the scaffolding the dispatch
validators share — is now ported too, along with the two `env` hashes and the
`plutarch_pexcludes_raw` helper it needed. See "Fraud-proof scaffolding" below.

`native-tx-machine-walk-v1.ak` (631) — the §10 resumable walk built on the door
— is ported too; see "The resumable walk" below.

`fraud-proofs/field-opening-v1.ak` (623) — the bridge a downstream family step
reaches the door through, binding both of the modules above — is ported; see
"The anchored handle" below.

`validators/mpf-chunked-verify.ak` (203) — the merkelized proof walk every
family shares for chunked proof carriage, a sibling of `phas.ak` and
`pexcludes.ak` — is ported; see "The merkelized chunked verifier" below.

**`lib/midgard/fraud-proofs/native-binding-fixture-v1.ak` (1,086) is not on the
path.** Its own header says it is referenced exclusively by the `test` blocks
embedded in `validators/fraud-proofs/**` and is not reachable from any on-chain
entry point. The Plutarch tree replaces those embedded tests with its own tasty
fixtures, so porting it into `src/` would put test scaffolding in the library.
What it is worth is later, as a shared `tests/Testing/` fixture, once the family
step modules that its block builders exist for are ported.

The next slice is therefore the family step modules and the dispatch validators
above them, taken one family at a time. **All thirteen are done** —
`double-spend`, `zero-input`, `no-input`, `input-no-idx`,
`reference-input-no-idx`, `no-reference-input`, `missing-signature`,
`invalid-signature`, `missing-native-script-tx`, `withdrawn-reference-input`,
`da-hash-preimage`, `invalid-range` and `min-fee`. See "The fraud-proof families"
below.

Two of them were previously recorded here as gated, and both gates turned out to
be smaller than they looked. `invalid-range` needed `aiken_design_patterns`'
`NormalizedTimeRange` pinned — it is a plain enum, so pinning it was moving the
already-ported `DesignPatterns.ValidityRangeNormalization` from Scott to
`Constr 0..4` and fixing its three internal readers. `min-fee` needed a
`NativeTxCompact` in a datum; rather than Data-encode a twelve-field record that
every native family produces and consumes internally, the port keeps it Scott and
gives step-01 a bespoke `Data` encoder. See "`min-fee`" below for why, and for
what would have to change if the family ever gains a real fee model.

With the families done, the fraud-proof machine surface above them is ported too.
`transition-trace/proof.ak` (2,123) includes its structural faults, its ledger-trie layer, all four one-step
transitions — withdrawal, forced, L2 and deposit — with the deposit projection
chain and the five-arm dispatcher over them, both L1-event faults (omitted-due
and out-of-window), the duplicate-event fault, all five count faults, the proof
envelope, all nine entry points, and the accepted-transaction transition mismatch.
The nine dispatch
validators above it — `validators/fraud-proofs/transition-trace/` (608 lines)
and the `final-v1.ak` (15) they share a datum with — are ported and covered too,
so the whole transition-trace family now stands. The validation-trace validator
surface is likewise represented by the consolidated Plutarch modules.
The two *libraries* those validators speak through are ported and covered:
`lib/midgard/validation-trace-v1.ak` (345 lines, the trace format) and
`lib/midgard/validation-claim-v1.ak` (446, what ties a trace to a block). What is
outstanding above them is the dispatch layer, not the formats. Beyond that, about
55,000 lines of library —
of which `lib/midgard/validation-machine-v1.ak` alone is 18,283 and
`lib/midgard/cek-*` another 12,000. This is a CEK interpreter and
its proof scaffolding, and it is far larger than everything above combined. It
is sequenced last deliberately: nothing else depends on it, and porting it
before the protocol validators would mean maintaining two copies of the largest
and least stable part of the codebase.

That family now has six modules in it. `lib/midgard/cek-cost-v1.ak` (610 lines) is
the pinned builtin budget, and it is the one `cek-*` module with no dependency on
the rest — it takes a tag and a size vector and answers two integers. Porting it
first was worth doing on its own terms, because checking it against
`plutus-core`'s cost model turned up three places where the pinned table is not
the model it quotes. See "The pinned cost model" below.

The family's other leaf is `cek-data-v1.ak` (1,351 lines), whose only Midgard
dependency is the already-ported `NativeTx.Codec` — and whose `inspect_*`
functions need `cbor.deserialise`. That was the tree-wide blocker, called by
**29** live library modules and unportable by delegation because Plutus has no
deserialise builtin. It is ported now as `Aiken.Cbor` (see "The CBOR decoder"
below), and `cek-data-v1.ak` with it, as `Midgard.CekData` — see "The
authenticated `Data` node" below. `cek-proof-v1.ak` (1,649 lines) followed, as
`Midgard.CekProof`; see "The content-addressed program" below, and
`cek-constant-v1.ak` after it, as `Midgard.CekConstant` — see "One constant, one
root".

`cek-blob-frontier-v1.ak` and `cek-data-frame-v1.ak` are ported too, as
`Midgard.CekBlobFrontier` and `Midgard.CekDataFrame` — see "Committing a blob
you cannot hold" and "One frame of a traversal".

`blake2b-256-trace-v1.ak` — which `cek-source-blob` needs — is ported as
`Midgard.Blake2b256Trace`; see "BLAKE2b, one round at a time".

`cek-source-blob`, `cek-data-integer`, `cek-data-bytes`, the scan and traversal
state machines, `cek-builtin-v1`, and `cek-machine-v1.ak` are now ported too.
The builtin port covers direct execution, BLS expression proofs, authenticated
failures, and bounded semantic constants; the machine port covers every core
transition and all 23 original machine test scenarios. What remains at this
layer is the validation machine.

### Linked list (`src/LinkedList/`)

Port of `aiken-design-patterns/linked-list` v1.7.0 — a third-party dependency
with no Plutarch equivalent, and the prerequisite for the operator-directory,
state-queue and settlement validators.

Midgard uses 12 functions from `linked_list`; all 12 are ported.

| Aiken | Plutarch |
| --- | --- |
| `linked-list.ak` types (`Element`, `ElementData`, `Link`, the `*Eval` readers) | `LinkedList.Types` |
| `linked-list/internal.ak` (value/mint/key helpers) | `LinkedList.Internal` |
| the reading layer: `authenticate_element_utxo_and_get_info`, `get_element_info`, `get_root_element_info`, `get_node_element_info` | `LinkedList` |
| the operations: `init`, `deinit`, `insert_ascending`, `insert_descending`, `append_unordered`, `remove`, `fold_from_root` | `LinkedList` |
| the spend gates: `spend_for_adding_or_removing_an_element`, `spend_for_updating_elements_data` | `LinkedList` |

**Not ported, because Midgard never calls them:** `prepend_unordered`, and the
`linked-list/advanced.ak` (1,402 lines) and `linked-list/nested.ak` (1,426)
variant modules.

Two rejection modes, mirroring Aiken. Operations that scan the inputs run their
checks inside a callback the scanner `expect`s, so a failed check *errors*;
`init` takes its output directly and returns `False`. Both reject, but tests
have to assert the right one.

Two notes for whoever continues it. `pfix` in Plutarch 1.14 takes a *Haskell*
function, `(Term s (a :--> b) -> Term s (a :--> b))`, not a term — unlike
`pfixHoisted` in the MPF library. And the Aiken original walks the value's `Data`
encoding with `builtin.un_map_data` rather than using the `Value` API; that
relies on two ledger guarantees (the Ada entry sorts first, and a list element
holds nothing but Ada and its NFT), so the checks it skips only make sense
against them. The port keeps that shape.

### Other design patterns (`src/DesignPatterns/`)

Midgard imports four more `aiken-design-patterns` modules. Every function it
calls from them is ported:

| Aiken | Plutarch | Calls |
| --- | --- | --- |
| `singular-utxo-indexer.ak` → `one_to_one` | `DesignPatterns.SingularUtxoIndexer` | 9 |
| `parameter-validation.ak` → `apply_prehashed_param` | `DesignPatterns.ParameterValidation` | 4 |
| `validity-range-normalization.ak` → `normalize_time_range` | `DesignPatterns.ValidityRangeNormalization` | 3 |
| `merkelized-validator.ak` → `delegated_compute` | `DesignPatterns.MerkelizedValidator` | 1 |

One porting note: Aiken's `merkelized_validator` matches
`Withdraw(Script(hash))`, which in Plutus V3 is `Rewarding` with a script
credential. The port checks the V3 purpose.

### External dependency status

| Package | Status |
| --- | --- |
| `aiken-design-patterns` | **Complete** for Midgard's usage — linked list plus the four modules above |
| `aiken-lang/merkle-patricia-forestry` | Covered by `plutarch-onchain-lib` |
| `aiken-lang/stdlib` | Covered by Plutarch's prelude and `plutarch-ledger-api`; gaps ported demand-driven into `Midgard.Common.Utils` |
| `aiken-lang/fuzz` | Not needed — property-test generators; the Haskell side uses `tasty-quickcheck` |
| `keyan-m/aiken-scott-utils` | **Not needed** — a transitive dependency of `aiken-design-patterns`, but Midgard has zero direct imports and the ported code never reached it |

So no external-dependency work remains.

`Midgard.OperatorDirectory` is now ported in full, including
`cross_validate_slashing_reason`. That last one is worth reading before the
settlement validator: a script that wants to know *why* an operator is being
slashed does not decide it — it reads the reason out of the redeemer of the set
actually removing the operator, and enforces only that the two agree on *who*.
That is what stops a settlement and an operator set accepting the same
transaction while disagreeing about the grounds.

All three operator sets are now ported. The active set is the only one whose
nodes are mutated in place — its spend path updates a node's bond unlock time
and inactivity strike count without touching the list structure — and the only
one that must stay synchronised with the scheduler. That is why porting it
pulled in `Midgard.Scheduler`; only what the active set reads is ported there,
and the scheduler's own validator is still a separate slice.

### User events (`src/Midgard/UserEvents*`)

Deposits, withdrawals and transaction orders share one minting abstraction. Its
mechanism is worth stating once: an event's identity is a *nonce*, the
blake2b-256 of a spent UTxO's serialised output reference. That nonce is the
event NFT's token name, and it also parameterises a witness staking script whose
registration the same transaction must carry. Uniqueness needs no registry — the
UTxO naming the nonce is consumed, and the ledger itself refuses to register an
already-registered credential.

`deposit` and `withdrawal` now have **both** sides. Withdrawal's spend path
splits on the operator's verdict: `InitializePayout` opens a payout accumulator
for a valid request, `Refund` returns an invalid one to the user. The refund
branch substitutes the redeemer's claimed verdict into the withdrawal info
*before* the membership check, which is what stops a user refunding on a verdict
the operator never gave.

All three user-event **spend** sides are ported. `tx-order`'s is the simplest of
them: an order has a single outcome — it is released once the block that
included it has settled — so the only thing that varies is the verdict, which
the settlement's `forced_transactions_root` has to corroborate.

`tx-order`'s **mint** side is the one remaining gap in this family; it needs
`verify_order_receipts` and the field-preimage receipt scripts.

 Its spend path moves the funds to the reserve
and is gated on the referenced settlement having already absorbed the deposit
into its L2 ledger, proved by a counted membership proof against the deposits
root — funds may only leave once the ledger has accounted for them. The
withdrawal and tx-order spend paths are still outstanding; both need the payout
model on top of what is now in place.

One rearrangement, applied in both validators. Aiken checks
`output == own - NFT` (and, for `InitializePayout`,
`output == own - withdrawalNFT + payoutNFT`). The port moves each NFT to the
side it is absent from — `output + NFT == own`, and
`output + withdrawalNFT == own + payoutNFT`. Plutarch's value union does not
drop the zero entry that subtraction leaves behind, while Aiken's `assets.add`
does, so the literal form compares unequal on a perfectly valid transaction.
This cost a debugging cycle twice; if a value equality is failing on input that
looks right, check for a subtraction that left a zero entry.
Two of the three event validators are ported. Their mint sides differ only in
the per-event predicate over the produced UTxO: a deposit may carry a bounded
basket of assets (`max_tokens_allowed_in_deposits`, counting Ada), while a
withdrawal must hold nothing but Ada and its NFT and must be marked
`WithdrawalIsValid` — only valid withdrawals may initialise a payout
accumulator, so an invalid one must not be mintable at all.

`tx-order` is the exception and is **not** cheap: its mint branch calls
`verify_order_receipts` over a 497-line lib plus the field-preimage receipt
scripts, so it belongs with that machinery rather than here.

`Midgard.Env.puserEventsWitnessScriptPrefix` is a 687-byte opaque constant
copied from `env/default.ak`. This package does not compile the witness script,
so a change on the Aiken side must be copied across or every derived hash
diverges. `Testing.DepositValidator` carries its own copy of that constant and
recomputes the witness hash in Haskell, so a one-sided edit fails a test rather
than silently agreeing with itself.

### The Merkle frontier (`src/Midgard/ValidationMerkle.hs`)

The first slice of the fraud-proof machine, and the foundation under every
bounded proof in it. A Merkle mountain range in compact form: rather than a
whole tree, a commitment carries only the *occupied binary peaks* of the leaves
appended so far, so appending a leaf is a binary increment — a carry merging
equal-height peaks upward — and at most 32 hashes cover the full `2^32 - 1` leaf
envelope. A transaction-local list of a few items reveals only a few hashes on
L1.

Two orderings are in play and they are opposites. The peak list is stored
**ascending** in height, low bit first, because a frontier is the leaf count
written in binary. The leaves are laid out with the **tallest** peak covering
the earliest of them, so `locate_peak` walks heights downward. Get either
backwards and every power-of-two tree still passes while every other size fails
— which is exactly how the test reference first went wrong here, and why the
tests cover sizes one through eight rather than a couple of convenient ones.

Because this layer is pure — no ledger types appear in it at all —
`Testing.ValidationMerkle` carries a second implementation of the scheme in
Haskell and checks the port against it leaf by leaf and proof by proof, over
every tree size from one to eight and every index inside each.

### Native-tx CBOR (`src/Midgard/FraudProofs/NativeTx/Codec.hs`)

Hand-written CBOR in both directions. It is hand-written for a reason: Midgard
commits to *canonical* CBOR, which admits exactly one encoding per value, and a
generic encoder is free to pick any admissible form while a generic decoder is
free to accept any of them. Either would break the one-encoding-per-value
property every hash commitment here rests on.

The part worth reading twice is that there are **two decoders for each integer**.
`decode_uint_at` and `decode_canonical_uint_at` read the same grammar; the
canonical one additionally rejects a value written in a wider form than it needs.
Both exist because they are used in different places — the compact-transaction
array and the machine's work-witness array pin their own header widths at the
call site and read permissively, while anything whose bytes fall under a hash
must go through the canonical one. Using the permissive decoder where the
canonical one belongs would let a single logical transaction have several valid
byte encodings, and therefore several different commitments.

`Testing.NativeTxCodec` tests every non-minimal encoding **twice** — accepted by
the permissive decoder, rejected by the canonical one — because a port that made
both permissive, or both strict, would still pass a one-sided test.

The array-header reader here is deliberately the permissive one and is *not* the
§5.1 field-preimage decoder; that grammar is interpreted only behind
`native_tx_field_access_v1.decode_field_array_header_at`.

`expect_deserialise` in the native transaction codec remains absent on purpose. It wraps Aiken's
`cbor.deserialise` — a complete CBOR reader written in Aiken, since Plutus has no
deserialising builtin — and has no callers in the live native-transaction tree.
The general decoder now lives in `Aiken.Cbor`; the transition-trace accepted
transaction mismatch uses it because that rule intentionally accepts the same
non-canonical CBOR forms as Aiken. `MidgardTxValidity` is
declared twice in the Aiken tree, here and in `ledger-state.ak`; the port keeps
both rather than collapsing them, since the two files would otherwise stop
corresponding. They share a `Data` encoding and are interchangeable across it.

One testing limit worth knowing: the four-byte length branch of
`encode_definite_bytes` needs a payload over 65,535 bytes, and building one
exceeds the evaluator's `appendByteString` ceiling. That single branch is
untested; the identical ladder in the array and map header encoders is covered,
since those emit five bytes and carry no payload.

### Canonical Plutus data (`src/Midgard/CanonicalPlutusData.hs`)

The datum/redeemer canonicity predicate, §6.2. Canonicity here is **membership in
the image of the `serialiseData` builtin** — exactly the byte forms it emits and
cardano-ledger's `decodeData` accepts — and the module decides that membership by
walking the bytes.

It replaced a round-trip pin through Aiken stdlib v3.1.0's decoder, and the
reason is worth carrying forward: that decoder is *narrower* than the builtin's
image in two places, and the round-trip inherited both gaps. Canonical tag-2/3
bignums fell through its major-6 arm into the constructor branch and produced a
negative alternative; tag-102 constructors, for alternatives at or above 128,
were declined outright. Both are forms L1 accepts, so rejecting them cost L1
parity — a datum Cardano would take could not be spent on Midgard.

**Every function is total, which is why it does not reuse the codec.**
`Midgard.FraudProofs.NativeTx.Codec` has the same head-reading arithmetic with
the opposite failure mode: it aborts. A predicate that aborted on the input it
exists to reject would be useless, so this module carries its own `pbyteAtM`,
`pscanHead` and `preadBigEndian`, each returning `PNothing` where the codec's
would `perror`. The bounds check inside `preadBigEndian` is load-bearing for the
same reason: `psliceBS` clamps a short read rather than failing, so without it a
truncated head would decode to a shorter number instead of to nothing.

**Two predicates, and they must keep disagreeing.**
`pisCanonicalPlutusDataV1` answers §6.2. `pisMaterialisablePlutusDataV1` answers
the narrower §11.2 question — whether the stdlib `deserialise` path could turn
the bytes back into `Data` — and is False for exactly the bignums and the tag-102
constructors. Declining to materialise is not declaring non-canonical. The flag
is threaded through the whole scan rather than read off the head byte, because
the head-byte screen this replaces missed a bignum nested one level down, and a
caller has to screen *before* asking the decoder: a bignum makes it abort rather
than decline.

**The tests use the definition as their reference.** `serialiseData` has a
Haskell implementation, so the first group runs the builtin over a spread of
`Data` values and requires every byte string it emits to be accepted. That
reference is the builtin itself rather than a second copy of the grammar, which
is the strongest independence available here. The negative direction cannot come
from the builtin — it emits nothing outside its own image — so those are tables
written from §6.2, one byte string per rule.

One property worth knowing before using `pcanonicalDataEndAtV1` as an
interior-access primitive: it is **not** a boundary check. It scans from wherever
it is pointed, and payload bytes can spell a canonical item of their own, so a
mid-item offset may answer with a plausible-looking end. The guarantee that an
offset is a boundary comes from having walked to it from zero. Two tests pin both
directions of that.

### One head at a time (`src/Midgard/CanonicalCborScan.hs`)

Two total readers — one canonical CBOR head, one definite byte string — for a
caller walking a structure it already knows the shape of.

This is the port's **third** copy of the same head arithmetic, and the
duplication is the Aiken tree's rather than the port's. The three differ in ways
that matter: `Midgard.FraudProofs.NativeTx.Codec` aborts, because its callers
have already established the bytes are well formed; `Midgard.CanonicalPlutusData`
is total and walks a whole grammar; this one is total and reads one head at a
time, taking the major type its caller expects as an argument and returning
nothing if the bytes disagree. That last point is what makes it a scanner rather
than a parser — a caller states what should come next instead of inspecting a
returned tag.

`PCborHeadV1` and `PCborBytesV1` are the port's second and third `DeriveAsScottRec`
types. The rule has not moved — Aiken has no Scott encoding, so `Constr` is what
it emits — but these are function results that never reach a datum or a redeemer
in either tree, so nothing observes their encoding. If a consumer ever puts one
on the wire, they have to become `DeriveAsDataRec`.

The tests take their positive direction from a minimal-head encoder written from
RFC 8949 §3, deliberately building its arguments by division rather than sharing
the port's slice-and-convert, so the two agree only if both are right about byte
order. The negative direction is a table, because a minimal encoder cannot
produce a non-minimal head.

### Compact transactions (`src/Midgard/FraudProofs/NativeTx/Compact.hs`)

The layer directly above the codec, and the one that makes fault proofs a
bounded size. A `NativeTxCompact` is a transaction body with each of its six
variable-length fields replaced by a 32-byte hash, plus the witness set's hash
and the operator's verdict — so a proof can carry a whole transaction's identity
without carrying the transaction.

Three commitments come out of it, each over a domain-separated preimage:
`MidgardNativeTxBodyV1` for the transaction id, `MidgardNativeTxFullV1` for the
full-transaction hash, and `MidgardNativeTxProofSourceV1` for the proof source.
The version number is inside the id's preimage, so a transaction cannot be
reinterpreted under a future version — and every encoder and decoder pins the
version independently besides.

`VerifiedMidgardNativeTxCompact` is evidence-as-a-type: nothing constructs one
except the three verifiers, so a downstream function taking that argument can
rely on the id having been checked against the bytes.

**One trap found in porting.** `NativeTxFieldPreimageLengthsV1` declares
`address_witnesses` before `script_witnesses`, but the encoder and decoder both
write and read *script before address*. They agree with each other, so the wire
format round-trips fine — but a port that followed the record's field order
positionally would transpose the two and still pass a round-trip test. The test
fixtures give the two lengths different values for exactly this reason, and
`the decoded field lengths carry each value in its own slot` pins all nine
values to distinct numbers so any permutation fails.

`Testing.NativeTxCompact` carries an independent Haskell encoder — body,
compact, witness set, lengths, and all three hashes — and checks the port
against it rather than against itself, so a shared mistake in the round trip
cannot hide. On top of that, `the canonical size tracks each length across its
header boundary` walks each of the nine lengths across the CBOR width
boundaries, where an off-by-one in the size computation would show up.

### Transaction components (`src/Midgard/FraudProofs/NativeTx/Components.hs`)

The per-item codecs — one input, one address, one value, one output, one witness
— and the layer `preimages.ak` and `transaction.ak` are built by running over a
list. Four things in it are worth carrying forward.

**The output index is deliberately non-minimal.** It is always the three-byte
`19 XXXX` form, even for `0`, which canonical CBOR spells in one byte. This is
the only such departure in the format (spec §5.3) and it buys arithmetic item
access: every input item is exactly 38 bytes, so with the `58 26` wrapper the
stride is 40 and the *n*th input sits at a computed offset instead of at the end
of a walk. Uniqueness is not given up — the minimal form, the `18 XX` form and
every wider form all reject, so each index still has exactly one encoding, just
not the shortest one. Three tests pin each rejected spelling separately.

**Script language tags are 0, 3 and 128 — not the constructor indices 0, 1, 2.**
A port that wrote the index would round-trip against itself and disagree with
every other implementation, which is exactly the failure a round-trip test cannot
see. The tags are asserted directly.

**Address type and payload length cross-check in both directions.** A 29-byte
payload must carry a type of 6 or 7 and a 57-byte one a type of 3 or less. Drop
either half and the same bytes decode as two different addresses; both halves
have their own negative test.

**Assets are keyed by a flat unit and grouped by adjacency.** An entry's key is
the policy id with the asset name concatenated onto it, while the wire format is
nested, so the encoder rediscovers the policy boundaries by scanning for a change
of prefix. The grouping is by adjacency and not by identity: `[A, B, A]` encodes
as *three* groups, with policy `A` appearing twice in the CBOR map. That is
admitted and round-trips — the value's commitment is over the ordered list, not
over the multiset, so two orderings of the same assets are two different values.
Worth knowing before writing anything that normalises an L2 value.

`Testing.NativeTxComponents` carries an independent Haskell encoder for all six
item kinds, and independent `Data` fixtures for the three `*_data` decoders, so
the `Data` wire shape is pinned as well as the byte one.

### Field preimages (`src/Midgard/FraudProofs/NativeTx/Preimages.hs`)

The nine field preimages, plus the §5.1 envelope they all share — which lives in
`Midgard.NativeTxFieldAccess`, ported alongside because `preimages.ak` sits
directly on it.

**§5.1 is deliberately narrower than CBOR.** Minimal width only, capped at the
`99 NNNN` form. The four-byte `9a` head is perfectly well-formed CBOR and
rejects here. This is why the header reader lives in the field-access module and
not in `Codec`: `pdecodeDefiniteArrayHeaderAt` is the *general* reader and admits
both, for structures outside this grammar (the compact-transaction array, the
machine's work-witness array), which pin their own widths at the call site. One
grammar, one verdict — the Aiken tree names the consequence of getting this
wrong, and it is the next point.

**Mint is the one field that never re-encodes.** Every other field is checked by
decoding and re-encoding, so a lenient decoder shows up in the byte comparison.
Field 5 is walked in place instead, deliberately, so a large mint does not have
to be materialised as `Data` inside the L1 execution-memory envelope. That makes
the canonical rules — ascending keys, no duplicates, minimal byte-string and
map-header widths, non-empty policy, non-zero quantity — the *only* thing keeping
one mint from having two encodings. Each is tested three times: on the encoder,
on the decoder, and again on `verify_canonical_mint_preimage_cbor`, which walks
the bytes on a third code path.

Two details worth carrying: an empty mint is the `Data` **list** `[]`, not an
empty map, and it envelopes to `80` like every other empty field (the retired
raw-map form `a0` is prohibited); and a **negative quantity is admitted**,
because a burn is a mint — only zero rejects.

**The empty-field commitment is a pinned literal.** §4 hashes the preimage bytes
with no domain tag and no field index, so all nine empty fields share one
commitment and recomputing it re-ran `blake2b_256` over the same single byte.
The Plutarch suite proves the literal against the producer, as the Aiken test
file does, so the pin cannot drift.

### Whole transactions (`src/Midgard/FraudProofs/NativeTx/Transaction.hs`)

The top of the carriage layer, and the last file in `fraud-proofs/native-tx/`.
It holds a whole Midgard transaction, the compact form derived from it, and the
verifiers a dispute uses to open one field at a time.

**Every field check is decode, re-encode, compare bytes.** A hash match alone
would authenticate a *non-canonical spelling* of the right value, and the whole
format rests on one value having one spelling. Field 5 is the single exception:
materialising a mint map as `Data` just to compare bytes does not fit the L1
execution-memory envelope, so it is walked in place instead.

**Field identity is positional, and that is testable.** §4 hashes the preimage
bytes with no field index mixed in — a field is the field it is because of the
slot its hash lands in. So every field's preimage is also offered to a
neighbouring slot and must be rejected there.

**Fields 6 and 7 are script-then-address.** The consensus index order puts script
witnesses at 6 and address witnesses at 7, the opposite of their order in the
witness set record — the same transposition the lengths record carries. A
transposed port round-trips happily and fails only on a test that pins the
indices.

**A partial view's witness set is checked before it is read.** The compact
transaction commits to the witness set by hash only, so `partial_view_from_compact_and_preimages`
re-hashes the supplied witness set against `witness_set_hash` first. Without
that, a caller could pair a valid compact transaction with a different witness
set and open *its* fields.

One asymmetry worth knowing, and it is Aiken's: the mint commitment verifier
**returns `False`** when the preimage length disagrees with the declared one, but
**fails the script** when the hash disagrees — the length check short-circuits
the conjunction before the erroring hash check is reached. The port keeps it by
using `#&&` rather than `pand'List`; see the note on strictness under "The Merkle
frontier".

### The MPF proof wire format matches Aiken

`plutarch-onchain-lib`'s `PNeighbor` derives `DeriveAsDataRec`, which encodes a
record as a **bare CBOR list**. Aiken's `mpf.Neighbor` is an ordinary
single-constructor record and therefore encodes as **`Constr 0`**. So a `Fork`
step serialises differently on the two sides:

```
Aiken     d87a9f 01 d8799f 02 41bb 41cc ff ff     Fork{skip:1, Neighbor{2,bb,cc}}
Plutarch  d87a9f 01     9f 02 41bb 41cc ff ff
```

The three *step* tags are right on both sides — `Constr 0` Branch, `1` Fork, `2`
Leaf. Only the nested neighbour differs, and only `Fork` steps carry one.

This is pinned from both directions in `Testing.MpfProof`'s "the proof wire
encoding" group, and it is not guesswork about intent: the Aiken tree has a test
named `mpf_proof_v1_matches_the_canonical_typescript_abi_vector` that fixes the
`Constr 0` form as the ABI, and a second, `mpf_proof_v1_rejects_the_obsolete_double_wrapped_fork_neighbor`,
that rejects a doubly-wrapped variant. The `Constr 0` form is deliberate and is
shared with the TypeScript tooling.

The port therefore owns `PProof`, `PProofStep` and `PNeighbor` in
`Midgard.MpfProof.Types`; `PNeighbor` uses `DeriveAsDataStruct`. Every Midgard
proof carrier, the chunk and fold formats, and the deployed `phas` and
`pexcludes` scripts use those types. `Testing.MpfProof` pins the exact canonical
vector, and `Testing.MembershipValidator` accepts the canonical Fork while
rejecting the obsolete bare-list form.

### Folding a proof one frame at a time (`src/Midgard/MpfProofFold.hs`)

`Midgard.MpfProof` walks a whole MPF proof in one script. That is fine while the
proof fits in one redeemer and one budget; when it does not, the walk has to be
suspended and resumed, and `mpf-proof-fold-v1.ak` is the format for doing so.
Each step becomes a *frame*, the frames are committed to as leaves of a
`Midgard.ValidationMerkle` frontier under a *descriptor*, and a *control* record
carries the partial roots between transactions.

**The fold runs backwards.** It starts at `frame_count - 1` with the cursor at the
descriptor's terminal cursor and walks down to `-1` and cursor `0`. That is the
same direction `pincluding` walks in as it returns from its recursion, turned
inside out so the intermediate state is nameable.

**Two roots, folded together.** Every frame advances an including and an excluding
root from the same bytes. They differ only at the terminal frame, where a fork or
a leaf contributes its own subtree instead of recursing — the same `do_excluding`
divergence `Midgard.MpfProof` documents, expressed here as an `is_terminal_frame`
flag rather than as a second walk. That is why the two roots have to be folded
together rather than derived from one another.

**Not `plutarch-onchain-lib`'s `pdo_fork`.** That one *aborts* when the branch
nibble equals the neighbour's. Here that check belongs to `pframeIsWellFormed`,
which refuses, so importing the abort would turn a refusable frame into a failed
script and strand the thread. `pdoBranch` and `pdoFork` are therefore written
from `pmerkle_16` and `psparse_merkle_16` directly.

**A hazard site, read positionally.** `frame_is_well_formed` opens with a
three-arm `when` whose arms are `skip`, `skip` and `skip` — the branch-selection
hazard's exact shape on a data-encoded type. `skip` is field zero of all three
`ProofStep` constructors, so `pstepSkip` reads field zero through `pconstrOf`
instead.

**The key is not bound by a fold step**, and the tests say so rather than
pretending otherwise. `path` enters only the nibble checks and the root
arithmetic, so folding the same frame under another key succeeds and lands on a
different root. What ties a proof to its key is the caller comparing the
*completed* root against the trie's.

**The oracle is the TypeScript implementation.** The frame preimage gets a Haskell
encoder written from the Aiken module, because a wrong field order would otherwise
hash consistently wrongly and go unnoticed. The fold itself gets the whole golden
fixture from `mpf-proof-fold-v1.test.ak`: four frames of a proof over a trie the
TypeScript implementation built, its four leaf hashes, and the two roots the
completed fold must reach. None of it was computed in this repository.

The frame preimage is built field by field, while the redeemer uses the same
canonical `PNeighbor` as every other proof carrier.

### The two MPF libraries disagree — Aiken is authoritative

**This is the most consequential thing found in the port so far, and it is not a
port bug.** `plutarch-onchain-lib`'s `pexcluding` and the Aiken
`merkle-patricia-forestry` library's `do_excluding` compute *different roots* for
the same non-membership proof. There are exactly two places, both reachable only
when a proof step carries `skip > 0`:

1. **Terminal `Fork`.** Aiken reconstructs `combine(nibble : prefix, root)`,
   dropping the skipped path nibbles entirely. The Plutarch library prepends
   `nibbles(path, cursor, cursor + skip)` first.
2. **Non-terminal `Leaf`.** Aiken reads the neighbour's nibble at `cursor`; the
   Plutarch library reads it at `next_cursor - 1`, which is `cursor + skip`.

The Aiken library is also internally inconsistent about the second: its own
`do_including` uses `next_cursor - 1` where its `do_excluding` uses `cursor`, and
`mpf-proof-v1.ak` copies both faithfully. On that reading the Plutarch library
looks like the *corrected* one — a `Fork` step's reconstructed prefix should
include the nibbles it skipped — but this README does not assert that, because
what matters here is that the two disagree.

`Testing.MpfProof`'s last group demonstrates it rather than describing it: the
two walks agree on every proof whose steps all have `skip == 0`, differ on a
terminal fork with `skip = 1` and on a non-terminal leaf with `skip = 1`, and
**both divergent proofs pass the well-formedness gate** — so they are reachable
from an untrusted prover, not artefacts the structural check would have thrown
out.

`MerkleTree.Validators.Membership` now uses Midgard's local including and
excluding walks, so the generated `phas` and `pexcludes` scripts follow Aiken's
arithmetic as well as its proof ABI.

What the port does meanwhile: `Midgard.MpfProof.pdoExcluding` reproduces
**Aiken's** arithmetic exactly, so `pdoesNotHave`, `pinsertRoot` and
`pdeleteRoot` are drop-in replacements. Routing them through the library
function would silently change which absence proofs are accepted, which is a
consensus change and not a translation. `pinsertRoot` also builds its result
directly rather than calling the library's `pinsert`, whose own `excluding`
precondition would re-check under the *other* arithmetic and could abort where
Aiken succeeds.

The upstream arithmetic remains useful only as a documented comparison. It is
not used by a proof-carrying Midgard production surface.

### The field-access door (`src/Midgard/NativeTxFieldAccess.hs`)

The single consumer-visible way to read one of a transaction's nine committed
fields. A caller hands over the committed compact structures and a field index,
gets a `FieldViewV1` back, and reads items out of it; nothing else at this layer
opens a field.

Four properties it owns, and the tests are organised around them.

**Positional identity** (§2.5, §4). Callers hand over the compact structures,
never a free-standing field hash, so `field_commitment_at` stays private and the
positional invariant cannot be side-stepped at a dispute entry point. This
matters more than it sounds: §4 removed field-index domain separation, so a
field-0 preimage and a field-1 preimage with the *same items* hash identically.
The index is the only thing that tells a reference-input opening from a
spend-input one. Three tests state that directly — one preimage opens at both
indices when the body commits it in both slots, and, with the stride and the
bytes held fixed, the same opening is accepted at the committed slot and refused
at its sibling.

For fields 6–8 the door also re-derives the witness set and checks it against
the compact structure's `witness_set_hash`. **That is half of the binding, not
all of it.** §3's transaction-id preimage is the body alone, so the trailing
`witness_set_hash` is not covered by the id: a caller that re-derived the
structure from redeemer bytes has authenticated nothing about it. The other half
belongs to the caller, and for a downstream family step it comes from thread
state. `field-opening-v1.ak`'s `WitnessAnchor` is where that lives, which is why
that module is the *next* consumer of this one and not the other way round.

**Abort, never clamp** (§7.3). The slice builtin clamps, and two clamped
out-of-range reads are byte-equal — which would fabricate equality evidence out
of a perfectly valid block. Every read goes through `slice_exact`.

**Lazy chunk verify** (§8.4, §8.8). Under tier-3 carriage a chunk is hashed the
first time a read reaches it and never otherwise, and an item's byte range may
straddle a chunk boundary. The test for this corrupts the *second* chunk's
digest and then shows that reading an item inside the first chunk still succeeds
while reading one inside the second aborts — a port that verified eagerly fails
the first half, one that never verified fails the second. A third case reads the
item that actually crosses the boundary.

**One grammar, one verdict** (§5.1, §6.1). The item head decoder admits the
minimal width only, and fixed-stride access reads the item's own wrapper rather
than inferring it from the stride. Skipping that would let `81 ‖ 00 00 ‖ …` open
beside the canonical `81 ‖ 58 1c ‖ …` and hand back the same payload — three
admissible byte forms for one logical field, which would leave a
non-canonically-committed preimage unfaultable. The test corrupts one wrapper
byte in a preimage whose hash and whose §7.4 arithmetic both still pass, and the
read is refused while its neighbours are still served.

Two constants are worth knowing about before relying on them.

**`chunk_bytes_k` reads 15900 and that is not the value of K.** The Phase-4
measurement refuted it — a real signed publication of a 15,900-byte chunk
measures 16,648 bytes against a 16,384-byte `maxTxSize` — and §8.3 erratum E1
re-pins K to 15,148. The literal is carried over unchanged because it is
compiled into an acceptance predicate and into every chunk boundary in the
system, so re-cutting it is a serialized surface patch rather than a local edit.
While the two disagree, preimages in (15148, 15900] have no admissible carriage
at all, and no tier-3 preimage of any length can be published, since every
tier-3 plan's first chunk is 264 bytes over `maxTxSize`. The port pins the
literal in a test so that changing it is deliberate.

**`field_item_count` aborts** for a variable-width field under tier 3, and that
is the design rather than a gap. The count exists only in the §5.1 header there
and nothing affordable authenticates it: tiers 1–2 authenticate theirs by
walking the whole preimage at construction, and tier 3 cannot, because a chunked
read re-verifies the chunk it lands in on every read, so an N-item walk costs N
hashes over a whole chunk. Reads still work — the walk behind them fails closed
the moment it leaves the committed bytes — so the port keeps a private
`declared_item_count` as a range guard and refuses to hand that number out as an
answer.

### The resumable walk (`src/Midgard/NativeTxMachineWalk.hs`)

What the dispute machine does with a view when one transaction is not enough: it
turns the view into a *position* that survives into the next transaction.

**Positions, not bytes** (§7.6). A checkpoint is five scalars and a 32-byte
transaction id, and its wire form is exactly 53 bytes *whatever the field
holds*. That constant is what makes "carries no preimage content" checkable
rather than merely asserted, so the tests check it as a property: 53 bytes
across four fields, two carriage tiers and several positions, and two walks over
*different* preimages of the same shape serialising byte-identically.

**Authenticate-once is structural** (§7.1). A walk opens a view once per
transaction and then carries a position; every further item costs a wrapper
decode and a slice, never another hash over the preimage. A fold with a budget
stops where the budget runs out, and resuming from the checkpoint it returns
finishes the field — the test adds the two halves' accumulators and checks the
total against visiting each item exactly once.

**Opacity is the mechanism, and it ports.** Aiken makes `FieldWalkCheckpointV1`
an `opaque type` and keeps both the decoder and the checkpoint-taking resume
private. The Haskell spelling is the same thing: the type is exported *without*
its constructor, and `pdecodeFieldWalkCheckpoint` and `presumeFieldWalk` are not
exported at all. This is load-bearing rather than tidy. A variable-width
position costs a full re-walk to recompute, so §10.2's binding check
deliberately does *not* recompute it — its integrity is inductive, from a chain
whose base case is a position derived from an authenticated view. If a caller
could write a checkpoint literal at an offset of its choosing, that induction
would have no base. With the constructor withheld there are exactly three ways
to hold a position: derive it, advance it, or put 53 bytes through
`resume_field_walk_from_commitment`, which hashes them against a digest the
*previous* step committed.

The tests plant positions the only way a caller can — write the wire bytes, hash
them, resume — and each negative case moves one scalar of an otherwise honest
checkpoint. Four carry the Aiken tree's own case names, and one fixture is its
vector verbatim: `8244aa41ccdd43eeff99`, the ten-byte field-6 preimage that
isolates §5.1's no-trailing-bytes rule at the advance. That guard is worth
naming, because it is the one refusal that *cannot* be reached from an honestly
opened walk. Reaching it takes a position planted through the commitment at an
offset whose item ends inside the field — which §10.2's own check passes,
honestly, because that is all it claims to check. The residual gap is real and
the advance is what closes it one step later.

**One thing the library cannot hold for its callers.** Where the `committed`
digest came from is the caller's business. A validator that read it from a
redeemer rather than from thread state would be back to trusting the prover's
arithmetic on a variable-width position. §10.6 is normative on dispute entry
points about this, and the port carries the warning rather than the enforcement,
exactly as Aiken does.

### Fraud-proof scaffolding (`src/Midgard/FraudProofs/Common.hs`)

`fraud-proofs/common.ak` is what the roughly 160 dispatch validators are built
on, so its failure modes are their failure modes. Three things live in it.

**Step transitions.** `pcontinue` carries a computation thread one step forward,
`pfinalize` ends it in a conviction, `pcancel` abandons it. What they enforce is
the thread's identity: exactly one thread token and no other tokens on the input
*and* on the output, and an unchanged fraud prover. The token being singular on
both sides is what prevents double satisfaction — two threads cannot be advanced
by one output — and it is easy to read as redundant when only one side is
checked. The tests walk each side separately.

**Evidence.** `pverifyNativeTxInStateQueueNodeWith` is the seam through which
every family reaches a transaction inside a committed block, and everything that
makes the evidence trustworthy is there and nowhere else: the canonical codec
precondition, hub identity, the queue node's key matching the thread token's
asset name with the catalogue id dropped, and the counted-root authentication
that turns the prover's raw MPF root into the header's own commitment. Only then
does the opening run. `pverifyCommittedTransactionsLeafInStateQueueNode` is the
codec-free twin: `da-hash-preimage` (Q44) proves a committed leaf whose key is
*not* the hash-preimage commitment of its value, so for that one family the
codec check must not be a precondition. No other family may use it.

**Carriage.** `PNativeTxInclusionCarriage` and `PNonMembershipCarriage` are the
prover's choice between putting a proof in this transaction's redeemers and
naming chunk UTxOs published beforehand (#545). The opening is delegated to a
`membership_check` callback, which is the point of the `_with` split: a carriage
chooses *where the proof's bytes travelled*, never *what they prove*. The tests
drive both arms through one fixture and assert they reach the same `validation`
call with the same authenticated evidence.

Four things are worth recording about the port itself.

**Aiken evaluates the codec check strictly; Plutarch would not.** The guard in
`pverifyNativeTxInStateQueueNodeWith` opens with `pverified'txId #== nativeTxId`,
which is a tautology given `pverifyNativeTxCompactCborV1` — it either returns a
record whose id is the argument or aborts. It is there to force the call, so a
caller that never looks at the decoded view still aborts on bad CBOR.

**Rejection modes are mixed, deliberately.** Aiken's `expect` aborts; a couple of
predicates here return `False` instead. `pcontinue` and `pfinalize` hand the
family's verdict straight back (so a family may reject without erroring), the
`pass_*` helpers wrap it in `expect` (so a family's `False` becomes an abort),
`pverifyNonMembershipCarried` returns its verdict, and the membership check
inside the evidence helpers sits under an `expect` and aborts. Each is what the
Aiken original does, and the tests assert the mode and not merely the failure.

**`fraud_proof.Datum` was encoded wrongly and is fixed here.** It derived
`DeriveAsDataRec`, which is a bare CBOR list; Aiken's record is `Constr 0`. It
had no consumer until now, and `pfinalize` rebuilds the datum and compares it to
the produced output's byte-for-byte, so the old encoding would have rejected
every genuine finalisation. `Testing.FraudProofsCommon` pins both the encoding
and the rejection of the bare-list form.

**One deliberate weakening.** Aiken's `expect ... : ct.StepDatum<Data>`
type-checks the produced datum before reading it; the port coerces, so a
malformed datum fails when a field is read. Every field is read on every path
here, so the two agree on which transactions pass and can differ only in which
error is reported. This matches how the rest of the port reads datums.

### The anchored handle (`src/Midgard/FraudProofs/FieldOpening.hs`)

The bridge every family step reaches the door through. It is small, and all of
it is guard.

**The anchor is paid once, and it is an anchor.** `panchoredNativeTx` re-derives
the transaction id from the redeemer's compact bytes and hands back an opaque
handle; every field the step then opens re-runs the per-field guards against that
one handle. The saving is the id derivation — a `blake2b_256` over the body,
measured on the Q21 vector at roughly 211k memory units — which a step opening
two fields through the single-field entry points paid twice.

**The second check is the one §3 cannot make for itself.** The id preimage is
the body CBOR alone, so re-deriving it pins the version and the body and says
nothing about the trailing `witness_set_hash`. A prover may therefore hand over
the *genuine* body — which re-derives to the anchored id — followed by a
`witness_set_hash` of its own choosing, and then "authenticate" a witness set
against it. The useful forgery is the empty witness set, because it makes every
§2.5 absence rule true of every transaction, which is a slashing proof against an
honest operator. The witness arm therefore checks the re-derived hash against the
one *thread state* named. The test reproduces the forgery end to end and then
shows the same bytes are accepted when the anchor is the one that named them, so
the case turns on the anchor rather than on anything about the witness set.

**The §2.5 half stays enforced per field, not per handle.** A handle checked out
under a body opening is refused at fields 6–8 and one checked out under a witness
opening is refused at 0–5. That is the guard the single-field form got for free
from the opening's own constructor and the multi-field form has to assert; both
directions are tested, because neither is reachable from an honest family and
that is exactly why neither may be assumed.

**Tier 3 does not reach the anchor for a witness-set field.** The §8.6
certificate's authority is a token named `(tx_id, field_index)`, and the minter
takes the `witness_set_hash` off the tail of *its own* redeemer's bytes — the
tail §3's id does not cover. So a certifier may present the genuine body and
certify a field-6, -7 or -8 preimage the transaction never committed, and the
door discards its own expected hash on the certified arm. The refusal here is a
**limit, not a repair** (spec §8.3 erratum E2, limit 3; the repair is #579), and
it is an abort — nothing falls back to a lower tier on the prover's behalf.

**`NativeTxWitnessSetCompact` moved from Scott to `Constr 0` for this.** Its
sibling compact structures are produced by the codec and consumed by the
accessors inside one script and never cross a data boundary, so Scott costs less
and the port used it. This one does cross: it is a field of `FieldOpeningV1`,
which is a redeemer field on the Q1x step validators, so its `Constr 0` shape is
wire format. `Testing.FieldOpening` pins that shape and the two opening
constructors' tags alongside it.

**`AnchoredNativeTxV1` is opaque, and the opacity is the mechanism.** Aiken makes
it an `opaque type`; the Haskell spelling exports the type without its
constructor, so `panchoredNativeTx` is the only way to obtain one and "these
structures were checked against the thread's anchor" is a property of the value
rather than a claim its holder makes. There is deliberately no accessor for
`witness_set_hash` — handing it out would put the one ingredient the forgery
above needs one door call away from a value the type labels anchored — and the
one accessor that returns an unanchored value says so in its name,
`punanchoredValidityCodeOf`.

### The merkelized chunked verifier (`src/Midgard/Validators/MpfChunkedVerify.hs`)

One withdraw script, shared by every fault-proof family, holding the MPF walk
for published-chunk proof carriage.

**Why it is not step-local code.** The walk is the expensive part of the
carriage, in script bytes as much as in execution units. Compiled into every
step that can open a trie it added about 3.7 kB to each step's spending script —
which the step transaction carries whichever route the prover takes, so it made
the *redeemer-carried* route markedly more exhaustible: the measured ceiling fell
from branch level 21–23 to 8–9. Remediating one route by degrading the other is
not a remediation, so the walk lives here, exactly as `phas.ak` and
`pexcludes.ak` already host the single-transaction walks.

**It authenticates nothing about the claim.** The redeemer names a root, a key, a
terminal and the chunk order; this script reassembles the proof from those UTxOs'
inline datums and runs the walk. Binding the root to a challenged header and the
key to the step's own evidence is the *delegating step's* job, which it does by
requiring this exact claim in this exact redeemer. That division is what lets one
script serve every family without knowing any of them.

The tests reconstruct a 22-level all-`Branch` root by folding the MPF primitives
directly — `combine`, `suffix`, `nibbles`, `merkle_16`, written out from the
library rather than called — and then hand that root to the validator. The
acceptance case is therefore a real cross-check of the fold and not a
round-trip: at 16 steps a chunk the ladder publishes as two UTxOs, and the depth
is past where the redeemer-carried route fits inside the preserved 16,384-byte
envelope, which is the whole reason the script exists. Each case carries the name
of the Aiken `test` block it reproduces.

### The receipt policy's retired branch (`src/Midgard/Validators/TxFieldReceipt.hs`)

`PublishField` is ported literally, including every indexed UTxO guard,
receipt-NFT check, `verify_midgard_transaction_field_chunk_v1`, and
`verify_receipt_chain_link`. Honest §4 flat field commitments fail at the
legacy counted-root binding on both sides. Standalone counted-root fixtures can
reach the pure verifiers, which is why preserving the full path matters even
though production publication remains fail-closed.

The redeemer type is declared **in full** even so, both arms, with `PublishField`
at `Constr 0` and `BurnReceipts` at `Constr 1`. Dropping the dead arm would shift
`BurnReceipts` to tag 0 and every burn an SDK built would decode as something
else — a worse failure than the honest refusal the branch actually is. Declaring
it is what moved `NativeTxProofSourceCborV1` from Scott to `Constr 0`: it is a
field of that redeemer, so its shape is wire format.

### The fraud-proof families

**All thirteen are ported.** Each is a `Midgard.FraudProofs.<Family>` of
thread-state and redeemer types and a `Midgard.Validators.FraudProofs.<Family>`
of the spending validators above them, with the scaffolding every step shares
factored into `Midgard.Validators.FraudProofs.Step`.

Read in the order below they build on each other, and two facts do most of the
work across all of them:

* **§4 removed field-index domain separation.** A field-0 preimage and a field-1
  preimage over the same items commit *identically*, and all nine empty fields
  share one commitment. The slot is positional, derived inside the door from the
  compact structures the verified id authenticates — never asserted by the
  preimage about itself.
* **§3's transaction id preimage is the body alone.** The compact structure's
  trailing `witness_set_hash` is outside it, so genuine body bytes with an
  invented tail re-derive to the same id. Any family opening a witness-set field
  needs the second half of the §2.5 anchor, read by step-01 and carried.

The sections below record what is specific to each family rather than repeating
those two.

#### `double-spend`

The first family ported whole: four step type modules
(`Midgard.FraudProofs.DoubleSpend`) and the four spending validators above them
(`Midgard.Validators.FraudProofs.DoubleSpend`). Everything the earlier slices
built — the counted-root evidence seam, the field-access door, the resumable
walk's spend-input shortcut, the anchored handle — is load-bearing here for the
first time.

**Soundness lives in two places, and neither is the obvious one.**

*Distinctness is step-02's.* Two identical transactions are one transaction — a
block committing the same canonical bytes twice commits one leaf — so without
`tx1_id != tx2_id` a prover could bind the same transaction twice and convict an
honest operator of double-spending against itself. The check is on canonical
transaction ids, because that is the only place the two are comparable: the field
openings happen two steps later, against different anchors. The test that
reproduces it is one line away from the honest case, which is the point — a port
that dropped the check would pass every other test in the module.

*The disputed input is carried, not re-derived.* Step-03 reads it out of tx1's
authenticated field 0 and puts it in thread state; step-04 reads tx2's field 0 at
its own index and compares. A challenge against a *valid* block dies at that
comparison, because distinct transactions of a valid block spend disjoint inputs.

**The slot is positional, never named.** Steps 03 and 04 pass
`spend_inputs_field_index` to the door and the door derives the commitment from
the compact structures the *verified id* authenticates. Nothing in the family
ever holds a free-standing field hash, because under §4's plain hashing there is
no such thing as one that names a slot — a field-0 and a field-1 preimage over
the same items hash identically.

**The cost defect this shape closed.** Step-04 is where issue #551 (finding
Q1X-F6) was measured. The retired idiom reproduced tx2's whole spend-input
collection in order to re-hash it, which put the proof past the ledger's memory
cap at the admissible 296-input cardinality. The door hashes the preimage once
and §5.3's fixed 38-byte item makes `spend_input_at` one multiplication and one
slice (§10.5), so the cost no longer scales with how many inputs tx2 spends.

**What the four validators share is factored, once.** `pstep` is Aiken's
`spend(datum, redeemer, own_out_ref, tx)` plus `else(_) { fail }`; `pdispatch` is
the `Cancel`/`Continue` split, with cancelling identical in every step of every
family. Each step then contributes only its own guards. That factoring is what
makes the remaining eleven families a matter of their own logic rather than of
their scaffolding.

#### `zero-input`

Two steps, and the whole family turns on one number being read positionally.

The obvious shape — forward the disputed transaction's spend-inputs commitment
and compare it against the pinned commitment of the empty field — is wrong in a
way that reads as correct. §4 removed field-index domain separation, so the empty
field has **one** commitment shared by all nine slots: that equality proves "some
field of this transaction is empty", and would convict an honest operator whose
transaction merely has no required signers. What travels is the transaction id,
and step-02 opens field 0 through the door and reads its authenticated item
count. The tests pin the difference directly: a transaction that spends nothing
is convicted, and one that spends an input — but whose other fields are empty —
is refused.

#### `no-input`

Four steps, because non-existence is **two** absences. An output either predates
the block or was produced inside it, so step-03 proves absence from
`prev_utxos_root` and step-04 from `transactions_root`; either alone proves
nothing, since an output produced mid-block is legitimately absent from the
initial ledger.

The two keys differ and neither is the obvious one. The ledger MPF is keyed by
the node's CBOR encoding of a transaction input — a definite two-element array,
`encode_midgard_tx_input`, **not** a serialised Plutus constructor — and the
transactions MPF by the raw 32-byte transaction id. Each step's tests include a
case that hands the delegated `pexcludes` walk *the other step's key*, which is
what keeps the two from being swapped.

Both roots are thread state because neither can be re-derived later:
`prev_utxos_root` comes off the challenged header, and the raw transactions root
comes off the carriage *after* step-01 checked it against the header's counted
`transactions_root`.

#### `input-no-idx` and `reference-input-no-idx`

Four steps each, and the same proof one §2.5 slot apart: a committed transaction
spends (or references) output `n` of a transaction with fewer than `n+1` outputs.

**The two bindings are separate on purpose.** Steps 01 and 03 both run the full
inclusion check, against the same thread and so the same block — the computation
thread token's asset name is what ties them together. Two are needed because the
proof is about a relationship between two transactions and neither is reachable
from the other's bytes. Step-03's guard, `producing_tx_id == bad_input_tx_id`, is
where a challenge against a valid block dies: in a valid block every input names
its true producing transaction.

**The verdict rests on a count**, so step-04 reads it from the door rather than
from a list it reproduced. `field_item_count` only answers where the count is
authenticated — tiers 1–2 walk the whole preimage at view construction, and it
refuses outright for a variable-width field under tier 3. Field 2 *is*
variable-width, so a tier-3 carriage of it is refused rather than worked around:
a non-existence verdict resting on an unauthenticated count is exactly the
fabricated evidence §7.4 exists to prevent.

**Why the two families are tested in one module.** Nothing in either family's
types or redeemers names its slot — the index is a compiled-in literal passed to
the door — and under §4's plain hashing a field-0 and a field-1 preimage over the
same items commit identically. A port that passed the wrong constant would prove
the other family's fault and no test of the types would notice. The fixture's
`tx3` therefore has *different* spend inputs and reference inputs, and each
family is handed both of that one committed transaction's preimages: its own slot
accepts, the sibling's is refused.

That case was worth getting right twice. The first version used a transaction
built by modifying `tx3`'s reference inputs — which changes the compact body,
hence the id, hence the commitment — so the refusals passed for the wrong reason:
the preimage failed the hash check, not the slot check. Making `tx3` itself
asymmetric is what turns them into slot tests.

#### `no-reference-input`

`no-input` one slot over, with two differences the port keeps rather than levels.
The absence proofs are **redeemer-carried only** — `no-input` takes a carriage at
both absences so a prover may publish the proof beforehand, this family takes a
bare proof — because the two redeemers are wire format and an SDK built against a
levelled one would produce bytes neither validator decodes. And the withdrawal
index in those redeemers is **vestigial on both sides**: Aiken's
`plutarch_pexcludes_raw` binds it and then finds the redeemer by script hash,
requiring uniqueness. It stays in the type because the type is the interface.

#### `missing-signature` and `invalid-signature`

The first two families whose evidence is a **witness**, and the pair that forced
the anchor to carry two halves rather than one.

Every family above anchors its field opening on the transaction id alone, and
that is sound for them because fields 0–5 all live in the body, and §3's id
preimage *is* the body CBOR. Field 7 does not. The compact structure's trailing
`witness_set_hash` sits **outside** the id preimage, so genuine body bytes with an
invented 32-byte tail re-derive to the same id. A step that let its redeemer name
the witness-set hash would let a prover open a witness set the committed
transaction never had.

Both directions of the §2.5 absence rules break on that, in mirror image.
`missing-signature` claims a required signer has no witness; an invented empty
witness set makes that true of every transaction. `invalid-signature` claims a
named witness's signature does not verify; a fabricated witness set holding a key
and a signature that genuinely do not match makes that true of every transaction
too. One value fixes both: `verified_witness_set_hash` must be **step-01's
reading** off the compact structure the block's counted `transactions_root`
committed, carried in thread state, never a downstream redeemer field. That is
why `Step02State` here is a pair — `bad_tx_id` and `bad_tx_witness_set_hash` —
where every earlier family's is a single id.

`invalid-signature` is also the one family with no fold. §5.3 fixes the
address-witness item at 101 bytes, so reaching witness *n* is a multiplication and
a slice; the claim is about one named item, and naming it is precisely what buys
not having to see the others. `missing-signature` does walk, over field 4's
28-byte required-signer hashes, to find the signer whose key hash has no witness.

Two fixture facts worth stating because the plausible guess is wrong in both
cases: a field-7 item is `82 ‖ 58 20 vkey ‖ 58 40 sig` = 101 bytes and a field-4
item is a bare 28-byte hash — and 101 and 28 both exceed 23, so §5.1 wraps each in
the `58 xx` one-byte-length form, not the packed header. A one-witness field-7
preimage is 1 + (2 + 101) = 104 bytes; a one-signer field-4 preimage is
1 + (2 + 28) = 31.

#### `missing-native-script-tx`

Six steps, the longest chain in the machine, and the only family that binds
**two** transactions: the one that spends a script-locked output, and the one
that produced it.

The argument, in order: step-01 binds the bad transaction and writes both halves
of its anchor; step-02 opens field 0 and names the spent input; step-03 binds the
*producing* transaction and checks its verified id is the one that input names;
step-04 opens the producing transaction's field 2, reads the spent output, and
requires its payment credential to be a script; step-05 exhibits script bytes
hashing to that credential under the native language tag; step-06 opens the bad
transaction's field 6 and convicts when those bytes are absent.

**The subject changes at step-03**, which is the structural fact the other
families do not have. From step-04 on the openings are against `producing_tx_id`,
and both ids travel because step-06 goes back. The one equality holding it
together is step-03's `producing_tx_id == input_with_missing_script.tx_id`;
without it a prover binds any committed transaction and step-04 reads *its*
slot 0.

**Step-05 changes nothing, and that is its job.** It reads a state and writes a
byte-identical one. What it establishes is that the 28-byte credential step-04
found is the hash of a *native* script — the credential itself says nothing about
which language produced it, and a family that skipped this would convict a
transaction for not witnessing a Plutus script, a different fault under a
different rule. The tag hashed is the language tag (0, 3, 128), not the
constructor index (0, 1, 2); for native scripts the two coincide, which is
exactly the sort of agreement that makes a port look right until the second case.
`Midgard.ScriptProof` uses those language tags across every source, purpose,
redeemer, execution, output, signer, and context-item commitment in Aiken's
`script-proof-v1.ak`.

**The absence is last, and it walks.** Steps 02–05 are positive claims, each
checked against something the block committed. Only step-06 asserts a negative,
and a negative is worth exactly as much as the set it is asserted over — which is
why the witness-set half of the anchor has to survive four intermediate states to
reach it. Field 6 is variable-width, so the fold walks it once: indexing item *n*
re-walks from item 0, so a scan by index would be quadratic in the field. Each
item is re-encoded and compared to the committed bytes, because the decoder reads
a *prefix* and `decode_definite_bytes_at` accepts a non-minimal length header —
either would let two distinct committed items decode to the same script, making
the hash a statement about something the field never committed.

Two carriage limits apply and both abort rather than clamp (§7.3): a
variable-width field's item count is authenticated only under tiers 1–2, and a
witness-set field is refused tier-3 carriage outright because a §8.6 certificate
cannot be bound to a transaction id that does not commit the witness set. A
prover whose field-6 preimage does not fit tier-2 carriage cannot finalize this
family. Both are recorded as limits 2 and 3 of `docs/spec/midgard-tx.md` §8.3
erratum E2.

##### The fixture pair that makes the anchor's necessity visible

`txScriptSpend` and its witnessed variant differ only in `tScripts`, which
reaches the compact structure through `witness_set_hash` alone. Their bodies are
byte-identical, so **§3 gives them the same transaction id** — the forged opening
in the test suite is literally the honest twin's own compact structure. A door
checking only the id would accept either transaction's witness set against the
other, and the family's conviction would mean nothing at all. That pair is not a
contrivance for the test; it is precisely the substitution §3's body-only id
preimage permits, and the reason `bad_tx_witness_set_hash` is thread state read
by step-01 rather than a step-06 redeemer field.

#### `withdrawn-reference-input`

The one family whose conviction is a **presence**. Every other absence family has
to rule out two trees — the output either predates the block or was produced
inside it — because an absence from one proves nothing. Here the conclusion is
that a withdrawal event *is* in the block's withdrawals tree and names exactly
the referenced output, and a withdrawal is by itself disqualifying. That is why
three steps suffice.

The weight moves onto the counted-root machinery instead. Step-01 reads
`withdrawals_root` and `withdrawal_count` off the header and both travel to
step-03, which could not re-read them soundly: it holds no inclusion argument, so
it has nothing that says *which* block's header it is looking at. Both halves are
needed because a Midgard root is a commitment to `(domain, phas_root, count)` and
cannot be unwrapped without the count — a count a redeemer chose would let a
prover present a tree of the wrong size, which is the substitution the counted
scheme exists to stop.

Step-03's conviction is three independent refusals, and the port keeps them
independent rather than folding them into one conjunction: the event's validity
must be `WithdrawalIsValid` (an event the operator itself rejected never removed
anything); its `l2_outref` must be the named reference input, both halves; and it
must be in the tree the header committed. The key and value bytes handed to the
membership check are **serialised at the call site**, not taken from the
witness's own `key` and `value` fields, because a witness that supplied its own
encoding could present one tree entry under two different keys.

Porting this needed two pieces of groundwork.
`Midgard.TransitionTrace.pverifyRootMembershipWithBytes` and the raw-root walk
under it, which no earlier call site reached; and `PRootMembershipProof`'s `key`
and `value` moving from `PByteString` to `PData`. Aiken parameterises that type,
and the ported call sites genuinely disagree about the instantiation — the
settlement and user-event proofs carry `ByteArray` on both sides, this one
carries a `WithdrawalId` and a whole `WithdrawalInfo`. The wire shape is the same
either way, so `PData` is the honest encoding of "polymorphic" and the call site
keeps the interpretation.

#### `da-hash-preimage`

The odd one out, twice over.

Its subject is not a ledger fault but a **provability** one: a committed
`transactions_root` leaf whose key is not the canonical native-V1 transaction id
of its own value. Such a leaf breaks hash/preimage correspondence, and no other
family can ever open it — every native family runs
`verify_native_tx_compact_cbor_v1`, which requires `derived_id == key`. The block
hides a transaction nothing can dispute, and this is the family that says so.

**Step-01 therefore must not run the codec precondition**, because that
precondition is precisely what is in dispute. Running it would make a violating
leaf *abort* the step rather than be convicted by it. What step-01 binds is
strictly weaker on purpose: the leaf is a genuine member of the block's counted
`transactions_root`, and nothing more. The Plutarch suite drives exactly the
leaves no other family's step-01 could bind — a genuine transaction committed
under a foreign key, arbitrary bytes, a leaf too short to frame — and expects all
three to bind.

**The derivation is arithmetic, not decoding.** The canonical compact encoding is
fixed-framed at both ends — `0x84 ‖ version` in front, `0x58 0x20 ‖ wsh32 ‖
validity` behind, two bytes and thirty-five — so an honest leaf's body preimage is
exactly `slice(value, 2, len - 37)`. The tail is a constant rather than a parse
because `expect_validity_code` bounds the code to `0..=5`, so it never widens past
one byte. Soundness runs in both directions without the leaf being decodable: for
an honest leaf the slice *is* the encoder's `body_cbor`, so the derived id always
equals the key and the challenge can never finalize; for a faulty one the same
total computation convicts whatever the bytes are.

Both framing constants are pinned in the test suite against the fixture's own
encoder, across every fixture transaction rather than one — a tail constant that
happened to fit a single body length would be worth nothing, and getting it wrong
would convict every honest operator in the network.

The underframed case is the family's one clamp, and it is not a §7.3 violation:
`committed_leaf_body_cbor_v1` returns the empty slice for a leaf shorter than the
frame, but that value is never acted on, because `is_da_hash_preimage_violation_v1`
convicts on underframing *before* comparing the ids. The suite pins that
short-circuit directly, with a case where the two ids agree and the leaf is
convicted anyway.

#### `invalid-range`

Two steps, and almost all of it is the normalisation between them. A native body
carries its interval as two integers with `env.posix_time_none` (`-1`) meaning
unbounded, which is four combinations — and the bounded-bounded case splits again
on whether the lower exceeds the upper, giving five shapes.

**Exclusive on the wire, inclusive in the type.** `validity_interval_end` is an
*exclusive* upper bound and every bounded constructor of `NormalizedTimeRange`
holds an *inclusive* one, so step-01 subtracts one. That is also why step-02's
upper test is `>=` against the block's `end_time` while its lower test is `<`
against `start_time` — the block's range is read as inclusive-lower,
exclusive-upper, which the Aiken source itself flags as an assumption pending a
spec clarification. The port keeps the asymmetry rather than tidying it, because
tidying it would change which transactions are convictable, and the test suite
sits its cases exactly on both boundaries.

**An unbounded end is not a fault**, so only the end that exists is compared, and
`Always` makes step-02 **abort** rather than refuse — faithful to Aiken's `fail`.
A thread that reached step-02 with an unbounded range was built on a premise the
family cannot be about, and an abort says that where a refusal would look like an
ordinary failed proof.

`InvalidRange` is the one arm that convicts unconditionally, and it is not "no
range": it is a range nothing can satisfy, so no block covers it.

The gate this family sat behind was `NormalizedTimeRange`'s encoding. It is a
plain Aiken enum, so pinning it meant moving the already-ported
`DesignPatterns.ValidityRangeNormalization` from `DeriveAsSOPStruct` to
`DeriveAsDataStruct` — the same Scott-to-`Constr` move `FieldOpeningV1` and
`NativeTxWitnessSetCompact` needed, for the same reason: it is now a datum field.
Its three internal readers in `Midgard.Common.Utils` gained a `pfromData` each.

#### `min-fee`

**This family cannot finalize, in either tree.** Aiken's
`get_min_transaction_fee` is a stub returning `0` for every transaction —
`TODO: This will need execution traces to calculate it` — so step-02's conclusion
is `fee < 0`, which no honest fee satisfies.

The port reproduces the stub rather than inventing a fee model. The two trees then
agree on which transactions pass; a Plutarch validator that convicted where
Aiken's does not would be the divergence, not the fix. Compare the receipt
policy's `PublishField` arm, whose literal counted-root gate cannot authenticate
an honest flat §4 commitment. Here the missing piece is instead a calculation
nobody has written yet, so reproducing it leaves exactly one function to change
when the traces land. The test suite pins the consequence
from both sides: every honest fee is refused, and the only thing that convicts is
a fee below zero.

Step-01's half is real and is ported unchanged. What it needed was a decision
about `NativeTxCompact`, which Aiken puts in the step's *datum*.

**The port keeps that type Scott-encoded and gives step-01 a bespoke `Data`
encoder instead.** Data-encoding a twelve-field body would cost execution units in
every native family — the codec produces it and the accessors consume it inside a
single script everywhere else — to serve one call site in the one family that
cannot conclude. `pnativeTxCompactToData` writes the exact `Constr 0` layout an
SDK builds, so the datum bytes are identical to Aiken's; what is given up is a
decode step-02 never performs, and the state field is typed `PData` to say so.
The test suite rebuilds that encoding independently from §2.5's field layout,
including a case pinning that the body is nested rather than spliced flat, because
a hand-written encoder is exactly the thing that drifts silently. **If
`get_min_transaction_fee` ever becomes real, this is the decision to revisit** —
and the type move would have to carry `NativeTxBodyCompact` with it.

#### One coincidence worth recording

`MidgardTxInput` is `Constr 0 [B tx_id, I output_index]` and `input-no-idx`'s
step-03 state is `Constr 0 [B bad_input_tx_id, I bad_input_output_index]` — the
two are **byte-identical**. An SDK that forwarded the decoded input whole instead
of splitting it would produce the right bytes by accident. That is harmless,
since they mean the same thing, but it means the split is a readability choice
rather than an enforced one, and a test asserting otherwise would be asserting
nothing.

#### The shared scaffolding

`pstep` is Aiken's `spend(datum, redeemer, own_out_ref, tx)` plus
`else(_) { fail }`; `pdispatch` is the `Cancel`/`Continue` split, with cancelling
identical in every step of every family; `pexpectStateAs` is the typed `expect`
on thread state. The Aiken tree repeats that opening in each of its roughly 160
step files because Aiken has no way not to. Factoring it means a step module
contains its own guards and nothing else, which is what makes the remaining
families a matter of their own logic rather than their own boilerplate.

On the test side the same move produced `Testing.FraudProofsFixture` — the
Haskell counterpart of `native-binding-fixture-v1.ak`, which the README above
explains does not belong in the library. It builds one committed block and the L1
transactions that drive a step against it, all written from the spec rather than
from the port: canonical compact CBOR and the §3 transaction id; every §5.1/§5.3
field preimage, including real script-locked outputs and real Ed25519 witnesses;
a header whose `transactions_root` and `withdrawals_root` are counted commitments
over raw MPF roots; the block's one withdrawal event; and the hub and state-queue
reference inputs a step reads its evidence from.

Every real fixture bug this port turned up was found by a *positive* case
failing, not by a negative one passing — which is the argument for driving each
family's honest path end to end rather than only its refusals.

### Retired counted surface

Worth knowing before planning any more of this port, because it changes what
"remaining" means for the whole transaction-order family.

`lib/midgard/bounded-collection-v1.ak` opens with a header declaring itself
**retired**. Midgard's nine field commitments used to be counted
bounded-collection Merkle roots; they are now flat blake2b-256 hashes over the
enveloped field-preimage bytes, produced and opened only through
`native_tx_field_access_v1`. A flat hash has no leaves, so `verify_item` — which
compares a counted Merkle root against one — cannot be satisfied. The module is
kept only so the dispute machine and the Phase-5 rebind lanes keep compiling, and
its header says in terms: *do not bind new surface to it.*

Two live call sites still do, and both are therefore dead:

  * `verify_midgard_transaction_field_chunk_v1` in
    `fraud-proofs/native-tx/transaction.ak`, which gates
    `tx-field-receipt-v1`'s `PublishField` branch. The Aiken tree documents this
    at length and pins it with a test named `publish_field_gate_is_unsatisfiable`.
  * `receipt_descriptor_is_valid` in `user-events/tx-order-v1.ak`, reached from
    `verify_order_receipts` — the tx-order **mint** side. This one is *not*
    documented as dead, and its tests are not marked `fail`. Running
    `aiken check -m first_receipt_and_terminal_receipt_authenticate_one_item_material`
    fails on exactly `expect receipt_descriptor_is_valid(receipt, commitments)`,
    so it is red in the Aiken suite for the same retired-surface reason.

Both fail *closed* — no receipt can be minted for a field that was never opened —
so this is a liveness break, not a soundness one. The replacement is the §8.6
permissionless chunk-certificate validator and its tooling (#573, #574), after
the machine walk core (#570).

For the original scoped port this meant leaving the transaction-order mint side,
the field-receipt `PublishField` branch, and collection verification out. The
current 1:1 migration has a stricter target: every Aiken script and test must be
represented even when the retained Aiken surface is retired and fails closed.
`Midgard.BoundedCollection` therefore now ports the complete legacy verifier and
its three tests. This does not make it a supported integration point; new code
must still use the replacement field-access path.

`field-preimage-certificate.ak` was on that list and should not have been: it is
the replacement, not the retired surface, and it is now ported along with the
carriage library it wraps.

The 1:1 migration now carries both
`verify_midgard_transaction_field_chunk_v1` and its private positional helper,
plus `verify_receipt_chain_link`, `verify_order_receipts`, the tx-order mint
handler, and the complete `PublishField` branch. Standalone counted-root
fixtures can satisfy these pure predicates, matching Aiken's conformance tests;
honestly constructed §4 flat field commitments cannot, matching
`publish_field_gate_is_unsatisfiable`.

### Bounded items (`src/Midgard/BoundedItem.hs`)

One item of a transaction field, committed to in 4,095-byte chunks folded into a
frontier. The problem it solves: an item can be far larger than any single L1
datum or redeemer, but a fault proof still has to authenticate a slice of it. So
the item's commitment is one 32-byte hash covering its length and the frontier
over its chunk hashes, and a proof for one chunk is the chunk, its Merkle path,
and the frontier.

Positions live *inside* the hashes rather than beside them. A chunk hash covers
the field index, the item index and the chunk index; an item's commitment covers
the field index, the item index and the total length. So a chunk cannot be
replayed at another position, in another item, or in another field, and
`Testing.BoundedItem` moves each coordinate in turn to show it.

Two details that would be easy to get wrong and are pinned by tests. A
zero-length item is **one empty chunk**, not none — with no chunks its frontier
would be empty, and every zero-length item everywhere would share a commitment.
And the total length is committed separately from the frontier even though the
leaf count already implies a chunk count, because the chunk count only bounds the
length to within a chunk; the tests check that two items differing by one byte
inside the same chunk commit differently.

`verify_chunk` is the second place the short-circuit hazard below bites, and
harder than the first: **three** of its conjuncts are partial and each is guarded
by an earlier one — `expected_chunk_length` errors on an out-of-range index,
`hash_chunk` errors on an over-long chunk, and `commitment` errors on a malformed
frontier. All three have a test asserting `False` rather than failure.

#### A strictness hazard worth knowing about

Aiken's `and { .. }` short-circuits. Plutarch's `pand'List` does **not**:
`pand'` is strict and UPLC application is call-by-value, so every conjunct is
evaluated. `#&&` is the lazy one.

Most of the time this costs nothing. On the accepting path every conjunct is
evaluated under either semantics, and on a rejecting path inside a validator the
transaction fails whichever way the conjunction gets there. It becomes a real
divergence in one situation: **a conjunct that can error or diverge, in a `Bool`
that is consumed by a branch rather than by rejection.** Then Aiken returns
`False` and the port fails the script.

Both instances live in this module and both are now `#&&`:

  * `frontier_is_well_formed` checks `count >= 0` before a recursion that halves
    the count toward zero — with a negative count that recursion never reaches
    its base case and burns the budget instead of returning `False`.
  * `verify_membership` checks the sibling count before calling `peak_hash_at`,
    which *errors* when no peak sits at the height.

Each has a test named for it. The general criterion to carry forward: a
`pand'List` over total comparisons — which is what almost every use in this port
is — is fine, and the ones to look at are library functions returning a `Bool`
that some caller branches on.

**The audit is now done** across all ~180 uses in `src/`. The method: find every
`pand'List` with a partial term in a non-first conjunct, then check whether the
result is immediately `pif`'d to `perror` — if it is, both semantics reject and
nothing is observable. Thirty-one blocks had a partial conjunct; twenty-six went
straight to a rejection. Of the remaining five, three read constructor fields
that Aiken's `expect` would have errored on too, so there is no divergence.

Two were real, and both were the same function: `phasNftStrict`, duplicated in
`Midgard.DaAttestation.Operations` and `Midgard.DaAttestation.Readers`. It was
written as

```haskell
pand'List [ pnot # (pnull # entries), pnull # (ptail # entries), ... ]
```

so on an empty token map the guard says `False` while `ptail` and `phead` error
— and Aiken's `assets.has_nft_strict` is a total `match` against a one-entry
value that simply returns `False`. Note this divergence was *introduced by the
port's own restructuring*, not transcribed: the original has no `and { }` here at
all. Every current caller rejects on `False`, so nothing observable changed, but
the function's contract is "returns a `Bool`" and the first caller to branch on
it would have got a script failure. Both copies now use `#&&`.

### Blobs beside items (`src/Midgard/BoundedBlob.hs`)

`bounded-blob-v1.ak` commits to a whole transaction *field* the way
`bounded-item-v1.ak` commits to one item inside a field: 4,095-byte chunks, a
chunk hash per position, folded into a `Midgard.ValidationMerkle` frontier under
one 32-byte commitment. The port mirrors `Midgard.BoundedItem` closely and the
two should be read together.

**They differ in exactly one place, and it propagates.** `chunk_count(0)` is
**zero** for a blob and **one** for an item. The item module needs the phantom
chunk: without it every zero-length item in every field would share the empty
frontier and so share a commitment. A blob does not, because its commitment
already covers the field index and the total length directly. The visible
consequence is `pverifyChunk`'s outer guard, which demands a *positive* total
length — an empty blob has a commitment that no chunk proof can ever be offered
against. Both halves of that are pinned, because a port that reused the item
module's count would pass every non-empty case.

The two domain strings differ as well, so the schemes cannot collide where their
arithmetic coincides; one test hashes the same chunk both ways to say so.

The test module imports the frontier and CBOR halves of its reference from
`Testing.BoundedItem` rather than transcribing them again — they are shared
machinery, already pinned there against a different port. Everything blob-specific
is written fresh from the Aiken module. On top of that, `bounded-blob-v1.test.ak`
pins two chunk hashes and one commitment on exact bytes; those three are asserted
against the port *and* against this module's reference separately, which is the
strongest oracle available here — a shared misreading of the preimage would have
to be shared with Aiken too.

### Field carriage (`src/Midgard/Validators/TxOrderFields.hs`)

A forced transaction is too big to publish in one UTxO, so its material goes up
piecewise: each of the nine fields in chunks, each chunk as a *preimage* UTxO,
each preimage acknowledged by a *receipt* UTxO. Two three-line spend validators
address those, and a third address — program material — has no spend path at all.

The name a receipt NFT is minted under is the whole scheme in one function.
`field_receipt_asset_name` hashes every coordinate that identifies a chunk: the
order's policy and output reference, the transaction commitment, and the field,
item and chunk indices. Two consequences follow. A receipt cannot be moved to a
different chunk, order or transaction, because its name would not match. And a
chunk cannot be receipted twice, because the same coordinates always produce the
same name and the ledger will not mint a live NFT again.

Note that the material is filed under the *transaction commitment* rather than
the transaction id. It has to be: the id is not known until every field has been
published.

Both spend validators reduce to the same demand — the transaction must burn both
the order's event NFT and this chunk's receipt NFT — which is to say the material
is releasable only while the order it belongs to is being dismantled. Were it
releasable on its own, an operator could retire the evidence for a forced
transaction that was still awaiting classification or challenge.

One asymmetry between the two datums is worth knowing when reading them: a
preimage carries a full `ChunkProofV1` and takes all three indices off it, while
a receipt carries no chunk proof and takes the field and item indices off its
*collection* proof and the chunk index off its own field.
`Testing.TxOrderFields` gives every number in the receipt fixture a distinct
value so a positional slip between them fails the positive case.

`field_receipt_asset_name`'s bounds are `expect`s and therefore error. The one
that is not obvious is `field_index < 9`: a Midgard transaction has exactly nine
fields and the index is encoded in a single byte, so a tenth would alias one of
them.

The proof types themselves (`ItemProofV1`, `ChunkProofV1`, `FrontierPeak`) live
in `Midgard.LedgerState`. Their verification is ported in
`Midgard.BoundedCollection` and `Midgard.BoundedItem` for validation-machine
parity.

The receipt *policy* is half ported. `PublishField` — where a receipt is created
— verifies the chunk against the transaction's compact CBOR, so it waits for the
native-tx layer. `BurnReceipts` needs none of that and is ported in full
(`Midgard.UserEvents.TxFieldReceipt`). It walks the policy's entries in
`tx.mint` alongside the input indices the redeemer supplies, in lockstep, and
both lists must run out together: tokens left over would be a name burnt with no
receipt behind it, indices left over would be unrelated inputs riding along. Each
step checks the burnt name twice over — the datum's coordinates must *hash* to
it, and the input's own value must *hold* it — which are two different failures
(retiring a receipt under another chunk's name, and pointing at a receipt UTxO
the burnt token did not come from).

One trap for whoever writes tests against a bare mint field. `PMintValue`'s
`PLiftable` instance is `DeriveDataPLiftable (PAsData PMintValue)`, so
`pconstant` yields a term that is still a `Data` `Map` while its type says
otherwise, and the first read of it dies on a `case` over `Data`. The real
transaction field arrives through `pfromData`, which unwraps it. In a test,
build it as `punsafeCoerce (pasMap # pconstant @PData ...)`; `Testing.TxFieldReceipt`
carries the note. This one cost a debugging cycle, and it presents as *both*
positives failing while every negative passes — the vacuous-negative signature.

### The witness script (`src/Midgard/Validators/Witness.hs`)

The other side of the pairing above: `Midgard.UserEvents.Witness` is the check
an *event policy* runs to confirm the witness was published, and this module is
the witness script itself — what it demands of the transaction publishing its
certificate.

`MintOrBurn` is the lifetime tie. Registering the credential must accompany the
event's mint and unregistering it must accompany the burn, with the quantity
read under the nonce the script was parameterised with. Nothing here constrains
which policy the redeemer names; that binding lives on the event side, in
`validate_witness_redeemer`, which requires the witness's redeemer to be
`MintOrBurn` naming the event policy itself.

The other two redeemers are a *proof of prior absence*. There is no ledger query
for "this credential is unregistered", but registering one is only possible
while it is not registered, so a transaction that registers and immediately
unregisters it proves absence and leaves the ledger as it found it. Both halves
are witnessed by this script, and each names the other by index.

**A finding: the two halves are not symmetric.** `RegisterToProveNotRegistered`
requires the unregistration to *immediately follow* the registration it is given
— adjacency is what keeps the pair inert, since an intervening certificate could
observe the credential as registered. `UnregisterToProveNotRegistered` requires
only that a registration of the same credential exist at the named index: not
adjacent, and not even *before* the unregistration. Certificates apply in list
order, so `[UnregisterCredential(c), RegisterCredential(c)]` with the index
pointing forward at the registration satisfies that half — and a transaction
shaped that way needs `c` to have been registered beforehand, which is the exact
opposite of what the proof claims. The pair's inertness therefore rests entirely
on the registration half also being witnessed *and* carrying the register
redeemer, and the ledger does not guarantee that. `getScriptWitnessConwayTxCert`
returns `Nothing` for `ConwayRegCert cred SNothing` — the legacy no-deposit
registration — and only demands the script witness for the deposit-bearing form,
so a transaction can register this credential without running its script at all.
(The ledger notes that form is transitional and future eras will drop it, which
puts a shelf life on the gap but does not close it now.) Nothing consumes either redeemer yet — `grep` finds no other
reference to them in the Aiken tree — so there is no live exploit, and the cheap
fix is to give the unregister half the same adjacency check as the register
half. Ported as written; `Testing.WitnessValidator` pins the asymmetry with a
test named for it.

Two smaller notes. Aiken's `list.drop` returns the list untouched for a
non-positive count rather than failing, so a negative certificate index names
the head rather than erroring; `pdropList` reproduces that, and a test pins it.
And the two rejection modes are preserved — `MintOrBurn` returns `False` for a
certificate that neither registers nor unregisters, while the other two branches
are `expect` chains and error — though at the validator boundary both reject, so
only a future caller composing these branches would be able to tell.

### The block header

`HeaderV1` is the Midgard block header: nine roots, seven counts, and the
metadata binding a block to its predecessor and its operator. Its field order is
the on-chain encoding *and* is read positionally by the state queue's own merge
redeemer — `Midgard.Validators.Settlement` reaches into fields 0 and 4..7 by
index — so it must not be reordered.

Two conditions in `Midgard.StateQueue` are worth knowing:

`decode_header_view` is a protocol-version gate, not a conversion. It refuses a
header whose version is not v1, which is what stops the genesis sentinel
(version **zero**, and not an ordinary header version) or a future version being
read as a v1 block.

`commit_bound_header_time_is_valid` ties a block's event interval to the
transaction committing it: the interval must be non-empty and end exactly at the
commit's inclusive upper bound. The start is deliberately not checked here — it
is the preceding header's end, checked when the block is linked in. Combined
with `max_validity_range_length` (eight minutes), this bounds how much interval
an operator can claim for one block.

### Reading the state queue, and a strictness trap

The four readers — `get_confirmed_state`, `get_state_queue_node`,
`get_block_datum_v1`, `get_prev_header_hash_of_node_v1` — are how every other
script learns what the queue says. Two things about them are load-bearing.

The **root/node split** is a safety property, not bookkeeping. The root's
payload is a `ConfirmedState` and a node's is a `StateQueueNode`; they are
different types occupying the same field positions, so a reader handed the
wrong element would decode one as the other rather than fail. `get_confirmed_state`
requires the element key to be absent and `get_state_queue_node` requires it to
be present, and that is the whole of what keeps them apart.

The **node key is minted, not stated**. It is the node NFT's asset name with the
`MBLC` prefix stripped, so a caller that compares it against an expected header
hash has authenticated *which* block it read. That is why
`get_prev_header_hash_of_node_v1` checks it: without that line the caller learns
the predecessor of whichever block sat at that reference-input index, not of the
block it meant to ask about.

The trap: `get_block_datum_v1` differs from `get_state_queue_node` only by
applying the version gate, and in Plutarch it is easy to write that gate so it
never runs. Aiken's `let header = decode_header_view(...)` is strict, so the
check happens whether or not the continuation reads the header. Passing the
continuation an unforced term instead makes the gate vanish for any caller that
ignores its header — and such callers exist. The port binds it with `plet`,
which compiles to a lambda application, and UPLC application is call-by-value. A
test asserts a node at protocol version 2 is rejected *by a continuation that
discards the header*; without the `plet` that test passes the block.

### Header validity (`src/Midgard/LedgerState.hs`)

`header_v1_is_valid` is what a block header must satisfy *on its own*, before
anything is checked about what it is appended to. The state queue runs it on
commit and again on merge, and fraud proofs are written assuming it held.

Note what is deliberately **not** in it: nothing about `start_time`, `end_time`,
`prev_header_hash` or `prev_utxos_root`. Those are relational — they only mean
something against the preceding block or the confirmed state — so the state
queue checks them separately. A test pins this, by accepting a header whose end
time precedes its start.

Three of its conditions are accounting identities rather than bounds, and they
are what make the counts non-negotiable:

- `total_event_count` is the sum of the four per-kind counts, so a block cannot
  inflate its event total beyond what it itemised;
- `transition_step_count == total_event_count` — one step per event, exactly;
- `validation_trace_count` counts exactly the script-running events, the forced
  transactions and the L2 transactions, and so excludes withdrawals and deposits.

`root_matches_count_v1` then ties every root to its count in both directions: a
count of zero forces the empty-tree root, and any other count forces a 32-byte
root that is *not* the empty one. That is what stops a block committing to
events it does not admit to, or claiming events under a root committing to none.

The confirmed-state pair is worth reading together.
`confirmed_state_next_header_protocol_version_v1` authenticates a confirmed
state and answers with the version the *next* header must carry — not the
state's own. That indirection is the whole point: the genesis sentinel's version
is zero, and answering `v1` for it is how that zero is kept out of every
committed header. The genesis arm is an exact-match test on all six fields,
because the sentinel is one specific value rather than a shape; the ordinary arm
instead requires `header_hash != genesis_header_hash`, which is what stops a
forged state passing itself off as genesis-adjacent.

### The payout policy (`src/Midgard/Validators/Payout.hs`)

A payout is an L2 withdrawal being paid out on L1. Only the **minting policy** is
ported; the spend handler (`AddFunds` and `ConcludeWithdrawal`, ~330 lines of
value arithmetic over the reserve) is a separate slice.

Both branches are hinges between other scripts rather than decisions of their
own, and both re-derive the payout policy id from the hub oracle and require it
to equal the policy actually running. That is what ties the script to one
protocol instance: a payout token minted under a look-alike deployment cannot be
spent against this one's reserve.

`MintPayout` is a **conversion**. The payout token comes into existence in the
same transaction, and under the same asset name, as the withdrawal token going
out of existence — the shared name is what carries the withdrawal's identity
across. Requiring the mint field to be *exactly* those two entries is what stops
a second payout riding along on one withdrawal's authority. It also checks the
withdrawal is being spent for `InitializePayout` rather than `Refund`: refunding
a user and paying them out are not the same thing, and only one of them licenses
a payout token.

`BurnPayout` defers to the payout's own spend handler for whether the funds
reached the user, and enforces only that the two scripts mean the same payout —
same input index, same hub reference — and that the burn brings nothing into
existence alongside it.

### DA attestation handlers (`src/Midgard/Validators/DaAttestation.hs`)

An attestation's life: `Init` creates it empty against a committed block,
`AddSignatures` accumulates committee signatures, and it ends either applied to
the state queue or — if it can no longer be applied — rescued so its Ada is not
locked forever.

**Applied and rescuable are exact complements, and that is deliberate.** An
attestation freezes *both* governed values at `Init`. Applying requires both to
still match; rescuing requires *either* to have moved.

The Aiken source is emphatic that the rescue must invert the whole conjunction
rather than just the committee hash, and the reasoning is worth keeping:
governance may change `da_threshold` over an unchanged committee — the governor
explicitly permits it — and such an attestation could then never apply (the
threshold no longer matches) and never be rescued (the committee hash still
does), while `AddSignatures` went on accepting signatures that could never
amount to anything. Its Ada would be locked for good.

The complement also means the wider rescue cannot strip an attestation still in
flight: whenever the rescue condition holds, the apply gate is unsatisfiable
however many further signatures arrive.

`Init` additionally requires the block to carry *no* attestation yet, so one
block cannot accumulate two.

`validate_burn_binding` is what keeps the two burn spends apart: each names the
mint constructor that may authorise it, so a `BurnForStateQueue` cannot be
satisfied by a rescue authorisation nor the reverse — and either way the mint
redeemer must name *this very input*.

The handlers' dispatch wiring is covered so far only for
`validate_burn_binding` — the cross-authorisation refusal, where a
`BurnForStateQueue` must reject a rescue authorisation and vice versa. The three
mint branches and `AddSignatures` are ported but have no transaction-level tests
yet; the layers they call are covered individually.

### DA attestation accumulation (`src/Midgard/DaAttestation/Operations.hs`)

`validate_add_signatures` is the seam Aiken exposes, and everything it pins is
about accumulation being **monotonic and attributable**.

The output may differ from the input in exactly two fields, and neither is
supplied by the caller: the bitmap is whatever verifying the signatures produces
from the old one, and the count is that bitmap's population. Address, whole
value, header hash, threshold and committee hash all carry over untouched.

The count must **strictly increase**. Without that, resubmitting signatures
already recorded would be accepted — harmless in itself, but it would also mean
the count could move without new signers, which is the thing a threshold counts.

The committee is checked twice, and both matter. The datum's frozen
`committee_signers_hash` must still equal the *current* governed one, so a
rotation retires an in-progress attestation rather than letting it continue
under keys the protocol no longer trusts; and the signatures verify against
`params.committee`, the live packed keys, not against anything the attestation
carries.

`expect_sole_burn` requires the policy's *whole token map* to be one pair at
`-1`. That is what stops one authorisation retiring two attestations: two burns
of one name collapse to `-2`, two names produce a second pair.

`validate_rescue_refund` refuses a refund back to this script. Every spend path
needs the UTxO to carry its attestation token, and the token is being burnt — so
such an output could never be spent again, and the rescue would re-strand
exactly what it set out to free.

### DA attestation reads (`src/Midgard/DaAttestation/Readers.hs`)

How the attestation validator learns three things it cannot take on trust.

`get_da_params` carries the property worth isolating this layer for: it
**re-derives** `committee_signers_hash` from `committee` rather than believing
the field. The Aiken source records this as a real gap that was closed — until
it was added, every consumer compared its own frozen hash against a number
nothing had checked, so a params datum publishing a **rotated** committee under
the **pre-rotation** hash would satisfy every such comparison while signatures
verified against the new keys. This is the sole re-derivation site, paid once
per read, and it is what makes the two fields a single fact for every caller.

`get_authenticated_state_queue_policy_id` reads the queue's policy id out of a
reference input's *attached reference script* rather than from a redeemer. The
ledger computes a reference script's hash, so a caller cannot claim one policy
and supply another; the authenticating NFT is what says that particular script
is the deployment's own state-queue minter.

`validate_init_output` pins a fresh attestation five ways, and two of them are
what make it start from nothing: an empty bitmap and a zero count, so a creator
cannot mint an attestation that already claims signatures. The threshold and
committee hash are copied from the *current* params — that is the freezing the
apply path later reconciles against. The asset name is derived from the datum's
own header hash, so the token cannot name a different block than the datum does.

### DA attestation signatures (`src/Midgard/DaAttestation/Signatures.hs`)

How a data-availability attestation counts its signers. The validator's own
branches are a separate slice; this is the part the threshold rests on.

The property the layer exists to guarantee is that a threshold counts
**distinct** committee members, and **two independent mechanisms** enforce it:

- `set_attested_signer` fails on a bit that is already set;
- `verify_indexed_signatures` carries a `min_signer_index` of the previous index
  plus one, so indices must strictly ascend.

Either alone would suffice today. They are tested separately anyway, because
that is what stops a future simplification from removing whichever one was
actually load-bearing. Between them, one member cannot be counted twice by any
route — not by repeating an index, not by reordering, not by supplying two
different signatures under one index, and not across transactions, since the
incoming bitmap is checked too.

The signed message carries a `MidgardDAAttestationV1` prefix. Without it a
signature over a bare header hash could be replayed anywhere else in the
protocol that asks a committee member to sign a hash.

The witness format is one index byte plus a 64-byte Ed25519 signature, packed
end to end. The index selects a verification key positionally out of the packed
committee, which is why the committee's fixed 32-byte stride matters — and why
the governor proves that committee sorted and unique before any of this runs.

Tests use real Ed25519 keys from `cardano-crypto-class` rather than stubbing
verification, and recompute the expected bitmap independently in Haskell.

### The DA params governor (`src/Midgard/Validators/DaParamsGovernor.hs`)

Governance over the data-availability parameters: which committee may attest,
how many of them a valid attestation needs, and which owners may change either.

Almost all of it is one predicate, `valid_datum`, and the two handlers are thin
shells around it. That shape is the point — the same invariant constrains the
datum being minted, the datum being spent, and the datum being produced, so a
parameter set that could not have been created cannot be reached by update
either. The spend re-validates its *input* datum as well as its output, which
means a set that somehow became invalid is frozen rather than serving as a base
to step out from.

**The floor is what makes single-key capture unrepresentable.** Both thresholds
must be at least `max(2, ceil(2 * set_len / 3))`. The clamp at two rules out a
one-signature quorum; the two-thirds term stops a quorum voting itself down to a
minority slice of its own set. Neither threshold may exceed its set, so the
parameters stay satisfiable. Two of these are tested as properties over set
sizes 2..64 rather than at single points: the floor never exceeds its set, and
always exceeds half of it.

**Sets are proved sorted and unique as they are measured.** Strict ascent is
both the ordering and the uniqueness proof, since equal adjacent keys are not
strictly increasing. A duplicate would let one key count twice towards a
threshold — one signer satisfying a threshold of two.

`init_ref` is what makes minting one-shot: there is no `Deinit` and no authority
check at mint, so without a specific UTxO being consumed anyone could mint a
second parameter set and the protocol would have two answers to "who may
attest".

One fidelity note. Aiken's `valid_datum` is an `expect` chain: it returns `True`
or fails, never `False`. The port matches that, and it matters even though both
handlers call it under an `expect` anyway — an exported predicate that returned
`False` would hand a future caller a different contract from the original's.

### The computation thread (`src/Midgard/Validators/ComputationThread.hs`)

A computation thread is a fraud proof in progress. Proving fraud against a block
is too large for one transaction, so it runs as a chain of steps, each a UTxO
holding a thread NFT and a working datum. This policy mints that NFT and burns it
when the thread ends.

Only `Init` has substance, and what it pins is the whole point:

- the **category** is a member of the fraud-proof catalogue's Merkle tree, proved
  by delegating to `phas`;
- the **destination** is that category's own script, so a proof cannot be parked
  somewhere it will never be checked;
- the **token name** is `fraud_category_id ++ fraudulent_header_hash`, so a thread
  names both what is being proved and which block it is proved against, and
  neither can be swapped afterwards;
- the **block** is a real state-queue entry, read through the authenticated
  reader; and
- the **prover** signed, so a thread cannot be opened to pay someone else.

`Success` and `BurnForCancellation` are deliberately asymmetric, and the
asymmetry is load-bearing. `Success` checks only that the thread token burns,
because whether the thread *earned* its success is the fraud category's last step
to decide — and other tokens must be permitted alongside, since the fraud-proof
token is minted in the same transaction. `BurnForCancellation` instead requires
the mint field to be *exactly* the one burn: a cancellation earns nothing, so
nothing may ride along with it, least of all a fraud-proof token.

### The state queue (`src/Midgard/Validators/StateQueue.hs`)

Midgard's chain of blocks: a linked list whose root is the confirmed state and
whose nodes are committed headers. Every other L1 script reads it; this is the
only one that writes it. Its five mint branches are a block's whole life —
`InitV1`/`Deinit` create and destroy the queue alongside the hub oracle,
`CommitBlockHeader` appends, `RemoveFraudulentBlockHeader` tears one out once
fraud is proved, `MergeToConfirmedStateV1` retires the oldest into settled
history.

**A block's key is its own hash.** The node's NFT asset name is
`blake2b_224(serialise(header))`, minted by this policy, so the key cannot
disagree with the header it names. Everything downstream that identifies a block
by hash — fraud proofs, settlements, the merge — depends on that.

**Appending has two routes and they are not symmetric.** After another block, the
new header copies that block's fields directly. After the *confirmed state*, the
protocol version is not copied but obtained from
`confirmed_state_next_header_protocol_version_v1`, which authenticates the state
and answers with the version the next header must carry. That indirection is what
keeps the genesis sentinel's version zero out of every block. An Aiken test pins
it from the other side too: a state carrying the sentinel's fields but claiming
version 1 is rejected, so a forged state cannot stand in for the chain head.

**Removal walks in from the tail.** A fraudulent block's successors inherit its
fraud, so `RemoveFraudulentBlocksLink` strips a successor without needing its own
fraud proof, and only `RemoveLastFraudulentBlock` removes the proved block. Both
require the operator to be losing its bond *for that reason* —
`SlashOperatorForBadState`, read back out of whichever operator set holds it.
Being slashed for something else does not license removing a block.

**Merging is where a block stops being disputable.** Maturity is seven days,
measured from the block's `end_time` to the transaction's inclusive *lower*
bound; using the lower bound is what makes it a real wait rather than something
a wide validity range can fake. The new confirmed state is constructed here and
compared whole rather than checked field by field, so no field goes unchecked by
omission — it keeps the old state's `start_time`, since the confirmed state
covers everything since genesis, and takes the block's hash, utxo root, end time
and version.

The settlement binding is two-directional and both directions matter: a block
carrying L2 material must spawn a settlement keyed by its own hash, and a block
carrying none must spawn none *and* must not even name a redeemer index.
Otherwise a settlement could be spawned against a block that moved nothing,
tying up a bond or supporting a payout claim with no L2 activity behind it.

Aiken exposes six of these predicates as `pub fn` and tests them directly; the
port mirrors those six `q49_l295_*` tests with the same fixtures and mutations,
and adds its own for the three merge-side seams, which Aiken does not test.

### The scheduler (`src/Midgard/Validators/Scheduler.hs`)

One UTxO, one datum: whose shift it is and when that shift began. Every block
committed to the state queue is checked against it.

Its seven advancing branches are seven reasons the turn may pass — the shift
ended, the operator went inactive, the operator is being removed, or nobody was
scheduled and a first operator is being appointed. The first three come in
`GoToNext`/`Rewind` pairs because the active-operator set is a linked list: an
ordinary step moves to a successor, but from the front of the list the turn
wraps to the last node, which is a structurally different transaction.

Two properties are worth knowing before reading it.

**The scheduler decides nothing on its own authority.** Whether an operator was
inactive is decided by the active set's `StrikeForInactivity` redeemer; whether
it is being removed, and what survives, is read out of the active set's *mint*
redeemer. This validator only checks the two agree — and in particular that they
agree about *this* scheduler UTxO. Every cross-script check compares
`scheduler_input_index` on both sides, which is what stops one script's
accusation being reused against a different scheduler.

**Advancing is permissionless.** No branch requires a signature. What constrains
it is that each branch must exhibit reference inputs whose linked-list structure
admits only one successor: a node keyed by the incoming operator *and* linking to
the outgoing one is necessarily the outgoing operator's predecessor, and there is
exactly one of those.

The two time anchors are deliberately different, and getting them backwards
would be a real bug rather than a cosmetic one. End-of-shift advancement starts
the new shift at the validity range's **lower** bound, so a schedule left stale
for many shifts catches up in a single transaction instead of replaying every
missed one. Every unscheduled branch — strike, removal, appointment — starts it
at the **upper** bound, so the incoming operator gets a whole shift rather than
one already partly elapsed. Both directions are tested.

A note on the inactivity threshold: it is the *later* of the operator's shift
start plus the grace period and the event-driven deadline. Taking the later of
the two is what makes the grace period a floor a newly appointed operator can
rely on, rather than something an accuser can route around by pointing at an old
block.

#### The two skipped-operator branches are unreachable under `env/default.ak`

`validate_operator_inactivity_and_get_its_link` requires

```
inactivity_threshold < shift_end_time(inactive_operators_shift_start_time)
```

`inactivity_threshold` is a `max` whose first argument is
`shift_start + new_shift_inactivity_grace_period`, and `shift_end_time(t)` is
`t + shift_duration`. With the default environment's values that reduces to
`start + 300_000 < start + 30`, which is false for every `start`: the grace
period is five minutes and a shift lasts thirty milliseconds, so the threshold
always lands past the end of the shift it is supposed to fall inside.

So `GoToNextDueToSkippedOperator` and `RewindDueToSkippedOperator` cannot be
reached at all — no operator can be struck for inactivity — in the default
environment.

This is a property of the Aiken source, not of the port, and it is deliberate on
Aiken's side. `env/default.ak` and `env/testnet.ak` are byte-identical except
for this one constant: `shift_duration` is 30 in the first and `60 * 60 * 1000`
in the second. Raising it to an hour is the *entire* reason the second
environment exists, and an hour is comfortably above the five-minute grace
period, so a testnet build reaches both branches normally.

The rule to preserve when adding an environment: **`shift_duration` must exceed
`new_shift_inactivity_grace_period`, or operators cannot be struck for
inactivity at all.** Nothing in either codebase checks this.

`Midgard.Env` mirrors `env/default.ak`, so this package carries the development
value; `Midgard.Env.pshiftDuration` documents the divergence at the constant.

The consequence for this package is a real coverage gap, stated here rather than
hidden behind green tests. The rest of the branch *was* verified: temporarily
replacing that single conjunct with `True` and running the full fixture set
passed eighteen cases covering the strike-redeemer agreement, the state-queue
tip requirement, both the confirmed-state and block-header end-time paths, the
grace period, all three neglected-event types and the negligence timeout. Those
tests are not kept, because with the conjunct restored every one of them would
assert rejection and so hold no matter what the branch did. What remains is one
test per branch pinning the unreachability itself, so that a change to either
constant shows up as a failure rather than silently switching the branches on.

### Settlement mint, and a coverage note

`validators/settlement.ak` is ported on both sides.

Its spend path covers a settlement's whole life after spawning.
`AttachResolutionClaim` has one detail worth calling out: the claim's resolution
time is **not** chosen here. It is read out of the active operators set's own
`UpdateBondHoldNewSettlement` redeemer, which ties the claim's deadline to the
bond hold the operator just accepted — an operator cannot claim a settlement
resolved on a deadline it did not also lock its bond against. There is a test
for a claim time other than the bond hold's.

`DisproveResolutionClaim` is the mirror: it tears a claim down by exhibiting a
user event the settlement's own trees say was never resolved, requires the
dispute to arrive inside the claim's deadline, and requires the operator to be
getting slashed in the same transaction for *that* reason — read out of
whichever operator set holds it rather than asserted here. It is covered at
transaction level, both operator-set paths included, because its components
passing individually is not evidence the branch composes: two bugs in this port
were found only in composition, with every part green in isolation.

That function is worth reading for how each arm derives the value it proves
membership of. A deposit's info is read straight from its datum. A withdrawal's
is read and then has the claimed verdict *substituted* before the check. A
transaction order has no stored value at all, so the whole `ForcedInclusionTxV1`
is reassembled around the claimed verdict. In every case the claimant names a
verdict and the operator's committed root must corroborate it — which is what
stops a disputant inventing one. The three `EventInclusion` verdict tests are
the ones that pin this.

Two details of the original are preserved deliberately. The state-queue merge
redeemer is decoded *positionally* with an explicit constructor-and-arity check
— it arrives across a script boundary, so its ABI is authenticated before use
rather than trusted — and the presence of the optional settlement-redeemer index
inside it is required, because that is what shows at least one of the merged
block's trees is non-empty. Without it the endpoint would accept a merge of an
entirely empty block. Both have tests.

Worth recording: the Aiken file's own comments state that its remaining tests
"execute no settlement code at all" and that "the settlement `mint`/`spend`
handler coverage gap is reported separately and is NOT closed by these tests".
The fifteen Spawn/Remove cases in `Testing.SettlementValidator` are the first
behavioural coverage of that handler on either side of the port.

### The L2 value model (`src/Midgard/Common/Value.hs`)

Midgard's L2 value is a `ValuePairs` — a bare list of policy/token/quantity
pairs — and `assets.from_asset_list` turns one into a ledger `Value`. That is a
**validating** conversion, not a cast, and its checks are load-bearing: the
result is the *target* a payout accumulator must not exceed, so a malformed list
quietly collapsing to a smaller value would weaken that bound.

Three failure modes, all reproduced: a policy with an empty token list; token
names not strictly ascending or any zero quantity; and the same policy twice.

One thing it deliberately does **not** require: policy entries need not be
ascending. Aiken inserts them into a dict one at a time, so any order is
accepted and the result comes out sorted. Requiring sorted policies would reject
transactions Aiken accepts — there is a test pinning that.

Aiken subtracts values as `merge(a, negate(b))`. Writing that as
`punionWith (-)` is wrong and the mistake is silent: entries present only in `b`
pass through non-colliding and so keep the *wrong sign*, which would turn a
deficit into a surplus. `pnegateValue` then `pmergeValues` is the faithful
composition, and the "an asset absent from the target is a deficit" test is the
one that catches the difference.

A note on representations, since it cost a debugging cycle: `PAssocMap k v`
wraps the builtin pair list directly, while `PSortedMap` and `PUnsortedMap` each
wrap a `PAssocMap`. Reaching the list costs one `pto` from a `PAssocMap` and two
from either of the others — so `PSortedValue` needs three.

### The counted-root scheme (`src/Midgard/TransitionTrace.hs`)

A Midgard root is not a bare Merkle root. It commits to a
`(domain, phas_root, count)` triple, and the count is the load-bearing part:
without it, a tree with the same entries but a different size would verify
against the same root. The domain is committed too, so a proof for one of a
block's seven trees cannot be replayed against another.

Membership itself is **not** verified in Haskell. `plutarch_phas_raw` delegates
to the `phas` staking validator — the very script this package generates as
`membership-stake.plutus.json` — via the merkelized-validator pattern, and only
checks that the arguments in that withdrawal's redeemer are the ones being
claimed. The security argument is split across two scripts: `phas` establishes
the proof is valid for *its* arguments, and this establishes those arguments are
*ours*. Hence the uniqueness requirement: two withdrawals by `phas` in one
transaction would make "the arguments" ambiguous, and
`get_unique_withdraw_redeemer` rejects that.

`Midgard.Env.pplutarchPhasValidatorHash` must track the generated script. If
`membership-stake.plutus.json` changes without that constant being updated,
every membership proof silently checks the wrong script's redeemer.

One fidelity note worth keeping. Aiken's `and { .. }` short-circuits, and the
`expect`s inside `plutarch_phas_raw` *error* rather than returning False. The
port had to match both: `pvalidCountedMembership` chains `#&&` (lazy) rather
than `pand'List` (strict), so a malformed claim is rejected by an earlier check
before the delegation is reached. Writing it with `pand'List` type-checks and
passes the positive test, but turns four rejections into errors and evaluates
the delegation on claims that should never reach it.

### The validation trace (`src/Midgard/ValidationTrace.hs`)

Port of `lib/midgard/validation-trace-v1.ak`. This is the format the
validation-machine fraud proofs argue over: a run of the machine is a sequence of
`ValidationMachineStateV1` snapshots, the sequence is committed as a balanced
Merkle tree, and a `ValidationTraceDescriptorV1` publishes that tree's root
alongside the step count, the verdict and the machine version. A proof is one
state, its index, and a path.

Three things in it are worth reading before touching it.

**Seven domains, and the pair that carries the weight.** Every hash is
domain-separated — states, trace leaves, trace branches, work witnesses,
rejection codes, validation contexts, ledger deltas. Six of those separations are
ordinary hygiene. The seventh, *leaf versus branch*, is what stops a
second-preimage attack: in a tree where leaves and branches hash alike, an
internal node can be re-presented as a leaf and a shorter path forged for it.
The test module pins that the seven are pairwise distinct, and pins directly that
a leaf and a branch over the same bytes hash differently.

**The verdict binds the rejection code, in both directions.**
`pverdictRejectionBindingIsValid` requires a rejection to carry a non-zero code
hash *and* an acceptance to carry exactly the zero code hash. Only checking the
first half would let an accepting descriptor smuggle a rejection reason, which is
the state a dispatch validator would then read.

**Depth follows the state count, not the step count.** A trace of *n* steps has
*n+1* states, so `ptraceDepth` climbs to `n+1`, not `n` — a zero-step trace is a
depth-0 tree holding a single state, and a one-step trace is depth 1. Off by one
here and every proof at a power-of-two boundary fails. Both the depth function
and `pverifyTraceProof` are tested against real depth-0, depth-1 and depth-2
trees built by an independent reference implementation, including the failures
that matter: a path folded at the wrong index, a path shorter or longer than the
tree's depth, and an index past the trace's last state.

`ptraceDepth` and the well-formedness predicates *error* rather than returning
False on out-of-range input, matching Aiken's `expect`. `pverifyTraceProof`
returns False for a proof that simply does not hold, and errors only on
structurally malformed input (a sibling of the wrong width). The tests
distinguish the two — `pfails` for an abort, a negated assertion for a refusal —
because collapsing them is how a rejection quietly becomes a script failure.

### The committed claim (`src/Midgard/ValidationClaim.hs`)

Port of `lib/midgard/validation-claim-v1.ak`. A claim is what a
validation-machine fraud proof has to open *before* it can argue about a step: it
establishes that the run under dispute is the one the block committed, applied to
the transaction the block committed, at the transition step the block committed —
and it does that without running the machine.

Four block-committed trees are opened and cross-tied: `validation_traces_root`
for the descriptor, `transition_trace_root` for the step, `event_to_step_root`
for the location, and one of `forced_transactions_root` / `transactions_root` for
the source. Each opening on its own proves an isolated fact; the security is in
the conjunction, and the tests below exist mostly to break individual ties.

Four things are worth reading before touching it.

**`cbor.serialise` chunks, `encode_definite_bytes` does not.** Both appear in the
same expression when a work witness is hashed: the scan witness is *built* with
the hand-written definite encoder, and then *hashed* through `cbor.serialise`.
For anything longer than 64 bytes those disagree — `serialiseData` emits an
indefinite-length sequence of 64-byte chunks, the definite encoder emits one
header — and a scan witness is always longer than 64 bytes. This is the one thing
in the slice the port got right and the test's independent reference got wrong,
which is exactly the direction that check exists for.

**The validation context is checked by construction, not by decoding.** Aiken
deserialises the context CBOR, destructures it as a seven-element list, checks
each element against the header, and separately requires re-serialising to
reproduce the supplied bytes. There is no CBOR *decoder* builtin, so that
`cbor.deserialise` is a hand-written byte walk — and the canonicality check after
it collapses the whole thing: those clauses hold together exactly when the bytes
equal the canonical serialisation of the list the header determines. The port
builds that list and compares once. Same accepted set, same rejected set, no
decoder. The range checks on network id, the two fee coefficients and the block
slot survive unchanged, and still apply to the header rather than being made
vacuous — Aiken only ever applies them to values it has already required to equal
the header's.

**The three-way split is load-bearing.** `committed_claim_structure_is_valid`
authenticates roots and endpoints without asserting the endpoints are normatively
correct; the normative checks live in
`committed_claim_endpoints_and_source_are_valid`. Fusing them would make a block
that commits a malformed endpoint *unchallengeable* rather than convictable,
because an honest challenger could no longer open the claim to point at it.

**The source membership is the hazard's shape again.**
`ValidationSourceMembershipV1`'s two constructors carry the same payload, and in
the port they are structurally identical, because the ported
`RootMembershipProof` carries its key and value as `Data` and Aiken's
distinguishing type parameters erase on-chain. `initial_work_root_is_exact` reads
the same field out of both arms — two arms, one body, the exact shape described
below. The port reads the tag with `pconstrOf` everywhere and, in that function,
never reads the tag at all.

One asymmetry the tests pin explicitly: a forced transaction is keyed in its tree
by a *structured* order id, so its key bytes are that value's CBOR, while an L2
transaction is keyed by its raw id and goes into the tree as itself. Serialising
the latter would prepend a two-byte header and address a different slot.

`Midgard.ValidationMachine` holds the one function out of
`validation-machine-v1.ak` that a claim needs — the opening phase's work-witness
encoder — so the claim does not have to wait on the machine, and so the call site
does not move when the machine lands.

### The structural transition faults (`src/Midgard/FraudProofs/TransitionTrace/Proof.hs`)

Most of `lib/midgard/fraud-proofs/transition-trace/proof.ak`. First the faults
that can be stated about a block's commitments alone, without replaying any event
— a trace that does not start where the block says the ledger was, two steps that
do not meet, a step and the event-to-step map that disagree, an event in the trace
but in no source (or the reverse, or in the wrong phase) — and then the four
one-step transitions, which replay one event each against the ledger trie and
convict the step that published a different root.

These are *fault* predicates — True means guilty — so every test carries both
directions. A fault predicate that always returned True would sail through a suite
made only of guilty blocks, which is why the fixture is a well-formed two-step
trace and each case is the smallest perturbation that convicts it.

Three notes.

**Phase is derived three ways and all three are checked.** Off the step, off the
step's index against the header's four counts, and off the event key's own
constructor. `trace_has_bad_phase` is the fault for any disagreement. The index
derivation is what ties a step to the header's counts; the key derivation is what
stops a withdrawal being filed as a deposit. `phase_for_step_index` **aborts** on
an index outside the block, so it sits behind a lazy `#||`.

**An empty tree has exactly one shape.** `counted_root_is_consistent` requires an
empty phas root to carry a count of zero *and* the published root to be the empty
sentinel itself rather than a commitment over it. So "this tree is empty" is the
only shape in which a non-membership proof with an empty path exists, and any
fixture proving an absence has to publish the matching zero count. Getting that
wrong is silent: the proof simply refuses, and reads as "the fault does not hold".

**Two entries, not two trees.** The trace fixture is a real two-entry
Merkle-Patricia trie with real inclusion proofs, built by `Testing.MpfTrie` from
the walk's own definition. A fixture giving each step its own one-entry tree would
let every proof verify against its own root — precisely the forgery the
same-tree check in `verify_adjacent_trace_proof` exists to stop — and would pass
while proving nothing.

**The ledger trie is where the two empty-root conventions meet.** `ledger_trie`
translates Midgard's `blake2b_256("")` sentinel into the MPF library's 32 zero
bytes on the way *in*, and neither `insert_root` nor `delete_root` translates
back on the way *out* — so a trace step that empties the ledger publishes the
zero root, not the sentinel. That asymmetry is Aiken's, and the port reproduces
it rather than smoothing it over; a test pins the two apart so nobody
"fixes" it.

**A UTxO's ledger key is the native-transaction encoding of its output
reference**, not a serialised Plutus constructor — which is what lets a spend
witness and a ledger deletion be about the same UTxO without either re-deriving
the other's key. Inside it, §5.3's fixed three-byte `19 XXXX` output index
applies: writing the minimal CBOR form instead yields a key addressing a
different slot. The test module's independent reference got that wrong first,
and the positive ledger-key case caught it.

**The L2 transition is where the port supplies a decoding instead of computing
one.** Aiken deserialises the `transactions_root` leaf, casts it to an
`L2TransactionSourceV1`, and then requires `cbor.serialise(source)` to reproduce
the leaf bytes. Those three steps hold together exactly when the leaf is the
canonical serialisation of a well-formed source value — so the port has the
prover supply the proof-source triple and rebuilds the leaf from it, checking
`serialiseData(Constr 0 [B key, triple])` against the leaf bytes. That single
equality is Aiken's canonicality clause *and* its `source.tx_id == key` clause,
serialisation is injective so it admits exactly one triple per leaf, and a
malformed shape has no preimage at all. Accepted set identical; cost, one extra
redeemer field. This is the same collapse the validation context uses, and it is
why the L2 transition did not have to wait for a decoder the way
`terminal_acceptance_post_root` does.

The two field indices in that rule are literals, never witness values. §4 removed
field-index domain separation, so a field-0 and a field-2 preimage over the same
items commit identically — a prover who could choose the index could steer an
outputs read onto the reference-inputs commitment. Two tests pin that: opening
field 0 with field 2's preimage aborts, and so does a preimage the transaction
does not commit to at all.

**The duplicate-event fault reads the default fixture the other way round.** Its
two steps both apply the same withdrawal, which makes that block innocent of
every other fault here and guilty of this one. Both readings are correct: a
duplicate is a fault about the trace as a whole, not about either step, and the
fixture is the cheapest place to make that visible.

**The deposit transition is the one rule that reads L1 state.** Every other
transition here replays something the block itself committed; a deposit's funds
live in an L1 UTxO, so the rule takes the reference-input list and reads the
UTxO the deposit NFT authenticates. That read is deliberately *not*
`get_authentic_input_with_nft_at`, the usual way to authenticate a state UTxO:
that helper requires the input to hold exactly one non-Ada asset, and a deposit
UTxO holds the user's funds alongside its NFT. Authenticity rests on the NFT
appearing exactly once, and the rest of the value is the thing being projected.

The projection is where the L1 value model meets the L2 one, and three details
carry it:

* **The NFT is removed by deletion, not subtraction.** `assets.add(policy, name,
  -1)` on an entry of one drops the entry and drops the policy if that emptied
  it; Plutarch's value arithmetic would leave `{policy: {name: 0}}` behind, and
  the projected output is hashed into a ledger key, so a stray zero is a
  different UTxO. `pvalueWithoutAsset` is the port of it. It is *not*
  `pvalueWithoutNft`, which drops the whole policy entry when the name is the
  only one under it and otherwise leaves the entry — including the name it was
  asked to remove — untouched. That is correct where the caller has already
  established the policy holds nothing else, which is not the case here: a
  deposit policy may hold other names. A test pins exactly that difference.
* **Ada is dropped from the asset list**, because an L2 value carries its
  lovelace in its own field and an Ada entry left in the list would be counted
  twice. Aiken's `flatten` then convert is fused into one walk, which is sound
  because `flatten` is a `foldr` over the sorted maps and emits exactly the
  sequence the walk visits.
* **A pointer stake credential aborts.** Midgard's address encoding has no
  pointer form, so there is nothing to project — an operator who accepted a
  deposit to such an address cannot be convicted through this rule. That is
  Aiken's behaviour, not an omission.

The witness is checked and then *rebuilt*: the prover's `LedgerInsertWitness`
must carry the derived key and value, and the insertion is applied to a witness
built from those derived values keeping only the prover's two proofs. The
equalities and the rebuild say the same thing, which is the point — the root
being compared is not one a prover could steer by supplying a key the equality
happened not to cover.

**The two L1-event faults are the only rules here that read L1 state directly**
— besides the deposit transition, which reads it for the same reason. An event
that reached L1 inside a block's window is a debt: `omitted_due_l1_event` is the
fault that the block never paid it, and `out_of_window_source_event` is the fault
that it paid one it did not owe. Three details are worth carrying:

* **The inclusion window is half-open, `(start, end]`.** An event at exactly
  `start_time` belongs to the block that *ended* there, so two adjacent blocks
  cannot both owe it. Tests pin all four boundaries.
* **A forced transaction owes two windows, and `-1` is an open end, not a large
  number.** `forced_tx_is_due` is three cases rather than one pair of
  comparisons, and its overlap comparisons are closed where the inclusion window
  is half-open. The validity interval is re-derived from the order's own compact
  bytes under its own transaction id, never taken from the datum — otherwise the
  window an operator is judged against would be the prover's to invent.
* **The two non-deposit out-of-window arms rebuild the committed leaf.** A source
  tree stores the operator's verdict beside the event and the L1 datum carries
  none, so the prover supplies the verdict and the arm rebuilds the leaf the
  block would have committed under it. That costs nothing: any verdict at all
  still convicts a block that took an event out of its window, and a wrong guess
  simply fails to match.

**The five-arm one-step dispatcher carries one extra redeemer field.** The L2 arm
appends the proof-source triple, which is the redeemer cost of the decoder
collapse described above; every field before it is positionally Aiken's. Its ten
tests are the slowest in the suite at about twelve seconds each — the arm under
test is one branch of a term that compiles all five — and they are worth it: a
routing test that only ever fed guilty blocks in would pass whether or not the
arms reached their own rules, so each arm is checked in both directions.

**The nine entry points are where the wildcard is the point.** Aiken writes each
as `and { envelope, when fault is { … _ -> False } }`, and that wildcard is a
security property, not a formality: a control-fault thread pointed at a deposit
fault must be *refused*, not answered. Four of the nine reach past the fault's
own tag into the one-step witness's, because a withdrawal validator answers for
withdrawal transitions and nothing else. Every selection here is written as a tag
comparison rather than a `pmatch`, since several arms share the body `False` and
that is exactly the shape the branch-selection hazard below eats.

The envelope's three clauses are likewise all load-bearing: the hash ties the
supplied header to the challenged hash, the length and prefix tie the thread
token to this fault category, and the suffix ties that token back to the same
hash. Without the last, a prover could convict block A on a thread opened against
block B — one test does exactly that and is refused.

**The accepted-transaction mismatch is decoded permissively.**
`validate_accepted_transaction_transition_mismatch` uses `Aiken.Cbor.pdeserialise`
without a canonicality comparison, matching Aiken's rule exactly. Its four
claim-binding and post-root vectors, the category entry point, and the final
validator path are covered.

`Testing.MpfTrie` was extracted from `Testing.TraceProofs` for this slice. It is
the independent reference for the MPF walk — branch hashes, path nibbling, suffix
sentinels, and the roots and proofs of one- and two-entry tries — and more than
one test module needs a real tree.

### The transition-trace dispatch layer (`src/Midgard/Validators/FraudProofs/TransitionTrace.hs`)

Nine scripts sit on top of that library: eight final validators, one per entry
point, and `route-v1` above them. `final-v1.ak`'s datum and redeemer are shared
by all eight, so they are ported once as
`Midgard.FraudProofs.TransitionTrace.FinalV1`.

**Why the fault is routed before it is judged.** Every entry point answers a
different subset of the same ten-armed fault type, and one script answering all
ten would compile every rule — the deposit projection chain, the L2 transition's
two preimage walks, the L1-event readers — into a single budget. Splitting them
means a challenger pays only for the rule they invoke. The price is `route-v1`,
which reads the fault's constructor, nothing else, and requires the thread to
continue at the script that constructor names.

Routing decides nothing about guilt, which is why it is safe for it to be a
challenger-only step: sending a fault to the wrong script gets it refused there,
and sending it to the right one proves nothing on its own. What `route-v1` *does*
have to prevent is a proof being routed and a different one adjudicated, which is
why the whole proof travels in the redeemer and the output's state must equal it
verbatim.

**The routing table is the branch-selection hazard at maximum density.**
`route_index` is a ten-arm `when` in which four arms answer `0`, two answer `2`,
two answer `4` and two answer `6` — five pairs of identical bodies. Both halves
of the read go through `pconstrOf` for that reason: the fault's own tag, and then
the nested one-step witness's. All fourteen distinct shapes are pinned
individually against a table rewritten from `route-v1.ak`.

Aiken's `when` is total over the ten constructors and so has no failure branch; a
tag outside `0..9` cannot reach it, because the redeemer is structurally decoded
first. The port reads positionally, so that impossibility is written down as an
abort — reachable only on input Aiken would have rejected at the boundary.

**The eight final validators are one function with a rule hole.** All eight Aiken
files are the same forty lines with one name changed, so the port factors the
shape into `ptransitionTraceFinal` and each validator is the line that differs.
The rule is handed the hub reference-input index and the transaction's reference
inputs even though six of the eight ignore them, mirroring the shared `Args`:
one redeemer shape for all eight, so the router does not have to choose between
two.

**The convictions in the tests are not built by the tests.** A final validator
convicts only if its entry point answers, so a proof assembled in the dispatch
suite would either be an accident or a second copy of fixtures that already
exist. `Testing.TransitionTraceProof` now exports the eight cases its own
entry-point group asserts, and both groups consume them — a failure in the
dispatch suite is therefore a dispatch failure and never a fault-rule one. The
routing table is the deliberate exception, rewritten from `route-v1.ak`, because
it is the thing under test.

One wrinkle worth knowing before reusing those fixtures elsewhere: the rule tests
coerce raw `Data` straight into `PTxInInfo` and never read an output reference,
so theirs is encoded as V1 and V2 encode it — a `TxId` inside a constructor. A
real V3 `TxOutRef` will not decode that, so the dispatch tests take the *resolved
output* and pair it with a reference of their own. That is `depositReferenceOutput`
rather than `depositReferenceInput`, and the distinction is not cosmetic: putting
the latter into a `ScriptContext` fails every case in the suite, including the
cancel arm, with no trace at all.

The `source-v1` and `l1-event-v1` cases are the slowest here at roughly 28s each,
for the same reason the one-step dispatcher tests are slow: the arm under test is
one branch of a term that compiles all of them.

### The batch of four small libraries

Four reachable libraries, ported together in one build cycle rather than four.
None needs a CBOR decoder and none depends on the validation machine, which is
what made them reachable at all.

**`Midgard.IntraItemBytes`** — the byte primitives §11's interior grammars share.
It is the port's *third* copy of the same CBOR head arithmetic and the
duplication is the Aiken tree's, not the port's. The three differ in failure
mode, which is the whole point: `NativeTx.Codec` aborts because its callers have
already established the bytes are well formed, `CanonicalPlutusData` is total and
walks a whole grammar, and this one aborts while reading one head at a time. The
§7.3 rule it exists to enforce is **abort, never clamp** — `sliceByteString`
clamps, and two clamped reads of different ranges can be byte-equal, so evidence
fabricated that way is indistinguishable from the real thing. One test pins
exactly that pair of ranges.

**`Midgard.ScriptLanguageViews`** — the script integrity hash over the one
cost-model view Midgard admits. Four cases on a two-bit bitmap, and the empty one
is the trap: a bitmap of `0` hashes CBOR `null` (`0xf6`), not the empty string and
not an empty map. All three are 32-byte hashes of *something*, so a port that
picked the wrong one would look healthy until a script-free transaction's
integrity hash disagreed with L1's. All three collisions are ruled out explicitly.

**`Midgard.ValidationDispute`** — the interactive bisection game. Almost nothing
returns a `Bool`: a malformed move is not a losing move, it is a transaction that
does not exist. The thing worth getting wrong is the bisection *direction* — when
the challenger's midpoint agrees with the operator's the disagreement is in the
upper half, and when it disagrees it is in the lower. A port that swapped them
would still terminate, still respect the round cap, and still converge on a
one-step interval: the wrong one. The fixture carries **two** challenger traces
with real Merkle roots, one agreeing at the midpoint and one not, because a single
trace could only ever exercise one direction.

**`Midgard.NativeTxCarriage`** — the producer half of the §8 carriage ladder, and
the §8.6 replacement for the retired counted surface (see the correction under
"Port status"). §8.4's split rule is what makes independent publishers
byte-compatible, so it is tested against a reference implementation at every
boundary around `K` and the aggregate cap rather than by example.

Two things in it are worth knowing. §2.4 transposes wire positions 6 and 7 —
6 is *script* witnesses, 7 is *address* — so each preimage is committed at one
witness slot and then offered under both indices; a table that transposed one of
the pair fails one test and a table that transposed both fails the other.
And the rejection modes are mixed on purpose: the final field-commitment
comparison answers `False`, while every guard above it is an `expect` and
therefore aborts. The first draft of the suite asserted `False` for all of them
and six tests failed — the port was right and the tests were wrong, which is the
direction that failure should come from.

### The certificate at the top of the ladder

`Midgard.Validators.FieldPreimageCertificate` is the transaction-shape wrapper
around `Midgard.NativeTxCarriage` — §8.4 to §8.7, and the surface that replaces
the retired counted layer. The content rules were already ported and already
tested; what this adds is everything that is only true of a *transaction*: which
output the certificate lands in, which token exists afterwards, and who may make
it stop existing.

**Nothing about it is privileged, and that is a design property with a test.**
The policy takes no parameter at all — no operator, no hub oracle, no
deployment-time argument — so its id is a function of the compiled code alone.
That is what makes a failed or malicious publication healable: because §8.4's
split is a pure function of the preimage bytes, an unrelated party who obtains
the same preimage republishes byte-identical chunks and certifies from them, and
the certificate they get carries the same content-addressed name the yanked one
had. The suite asserts that as a positive — a different key, different UTxOs, a
different min-Ada reclaim owner, and a decoy output ahead of the certificate so
the positional index is doing real work — rather than merely asserting that the
policy never reads an identity.

**One script, two handlers, two terms.** Aiken's `mint` and `spend` are handlers
of one validator, which is what makes the policy id and the spend credential one
hash: the mint sends the token to its own address and the spend burns its own
policy, with no reference-script bootstrap and no cycle between a policy and the
address it pays. Plutarch has no handler syntax, so the two arrive here as two
terms, and each has to refuse the other's purpose itself — Aiken's
`else(_) { fail }` applied twice. Both refusals are tested, because a spend that
could be satisfied by whatever the mint branch happened to check would be a hole
the single-script shape hides rather than closes.

**Two guards are stated in the shape that cannot be double-satisfied**, and both
are worth reading as a pattern rather than as a rule about certificates. The mint
compares the *whole* per-policy pair list of the certificate output against a
one-element list, instead of asking the quantity of the name it derived: a
quantity says nothing about the names it was not asked about, so a second name of
this policy arriving from an input could ride into the very output whose datum was
proved, and be read there by anything that trusts "a token of the certificate
policy is present". The spend requires that **no output carries** the name,
instead of counting the burn: a burn count is a sum over the whole transaction, so
two certificates of the same name spent together could each point at the same
single `-1` and let one token survive — and a surviving token is a certificate
whose datum the minting policy never saw.

The one divergence from the original is the datum read. Aiken's
`expect certificate: FieldPreimageCertificateV1 = certificate_data` is a
structural check of the datum against the type; the port coerces, as every datum
read in this tree does, and lets the fields refuse — the owner's length is
checked, the tx-id is hashed as bytes, the field index is bounded by the
asset-name derivation, and the digest vector is compared against digests computed
from the chunks. A datum that is not a certificate fails at the first of those
rather than at the coercion: the same refusal, one step later.

### The CBOR decoder (`src/Aiken/Cbor.hs`)

`aiken/cbor.deserialise`, and the single largest thing that stood between this
port and the rest of the Aiken tree: **twenty-nine** live library modules call
it, and none of them could be ported without it. Plutus has `serialiseData` as a
builtin and no inverse, and Aiken's decoder is not a builtin either — it is an
interpreter written in Aiken — so there was nothing to delegate to. It is
transcribed structure for structure from
`build/packages/aiken-lang-stdlib/lib/aiken/cbor.ak`.

**The cursor runs backwards.** The original counts *remaining* bytes: the cursor
starts at the input's length, every read subtracts, and a decode succeeded only
if the cursor is exactly zero at the end — which is what rejects trailing bytes.
That convention is kept verbatim. So is `take`'s clamp: `slice_bytearray` returns
a short slice rather than failing, and the original does not check for it,
because a clamped read drives the cursor negative and negative is not zero. (This
is the opposite of §7.3's abort-never-clamp rule elsewhere in the tree, for a
different reason, and both are documented at their sites.)

**A Plutarch hazard worth its own line.** The original is a set of mutually
recursive functions, and writing the port that way took the machine down — see "A
cycle among top-level Plutarch terms is an infinite value" below. The recursion is
tied once, with `pfix` inside `pdecodeData`, and every helper that needs to decode
an item takes that decoder as an argument.

**The test uses the builtin as its oracle.** `serialiseData` is a builtin with no
inverse, so it is a reference the decoder cannot have been fitted to: for any
`Data`, the bytes it emits must decode back to that `Data`. That covers the paths
easiest to get wrong for free — the builtin emits *indefinite* lists, *chunked*
byte strings above 64 bytes, and the 121/1280 constructor tags. The forms it
never emits — definite arrays, definite byte strings, indefinite maps, the four
multi-byte length headers — are written out by hand from RFC 8949, because a
decoder that only ever saw its own encoder's output would accept a strictly
smaller language than it claims to.

**Round-tripping is not an identity.** The stdlib documents
`deserialise(serialise(x)) == Some(x)`. Three families of value break it, all of
them the original's behaviour:

- an **empty byte string that ends the input**, because `take` refuses at an
  exhausted cursor before it looks at the width. Move it anywhere that leaves a
  byte behind and it decodes.
- a **constructor index above 127**, which Plutus encodes as tag 102 — the one
  tag the decoder refuses by name. The boundary is exact: 127 round-trips, 128
  does not.
- an **integer past 64 bits**, which Plutus encodes as a tagged bignum. This one
  does not decline — it **aborts**. The decoder reads every tag as a constructor
  index, so tag 2 becomes `-119`, and `constrData` rejects a negative index by
  terminating the machine. It generalises past bignums: every tag below 121
  except 102 produces a negative index, so four bytes an adversary writes by hand
  abort any script that calls `deserialise` on them.

All four claims were confirmed against the Aiken toolchain directly, as `aiken
check` probes over the stdlib. The two that decline pass there. The two that
abort make `aiken check` itself **panic** — `TryFromBigIntError` at
`crates/uplc/src/machine/runtime.rs:879` — because its evaluator mishandles a
negative `constrData` index where the ledger raises a clean script error. The
port produces the ledger's behaviour.

### The pinned cost model (`src/Midgard/CekCost.hs`)

A fault proof that replays a CEK step has to agree with the ledger about what
that step *cost*, to the unit, or the replay proves nothing. `cek-cost-v1.ak` is
the pinned Plutus V3 builtin budget — one `(cpu, memory)` pair per builtin tag,
under the same size arguments the machine would have measured — and the port is a
transcription of it, tag by tag and in its order.

**The test does not reimplement the model. It uses the model.** The reference is
`plutus-core`'s own `cekCostModelForVariant DefaultFunSemanticsVariantC`,
evaluated through `plutus-core`'s own costing-function runners at the same sizes
the port is given, so the coefficients, the model *shapes* and the arithmetic all
come from the library the ledger runs. The glue is one `ExMemoryUsage` instance
whose measurement is the number handed to it, which lets both sides be driven at
identical sizes without constructing values of the right shape.

One thing in that suite comes from neither library: the tag table. Midgard's
builtin tags are the UPLC **flat** tags, which are not `DefaultFun`'s declaration
order — `serialiseData` is 51 and the two secp256k1 verifications are 52 and 53,
where the Haskell datatype puts them at 53, 22 and 23. It is checked twice over,
by the arity it implies and by the coefficients it selects.

**Eighty-four of the eighty-seven builtins agree with Plutus exactly**, in both
dimensions, at every size vector. Three do not, and all three are the Aiken
source's rather than the port's:

- **the division family** (tags 3–6). Plutus's `quadratic_in_x_and_y` reads `c02`
  as the coefficient of `y²`; the Aiken source multiplies it by `y`. They agree
  for `y ∈ {0, 1}` — which is exactly where the Aiken suite's own golden vectors
  sit, so its tests never separated them — and differ by `900·(y² − y)` elsewhere
  above the diagonal. `divideInteger` at `(3,2)` words: Plutus 143,168, the table
  144,968.
- **`multiplyInteger`** (tag 2). Plutus charges `slope·(x·y)`, because
  multiplying an `x`-word integer by a `y`-word one is quadratic work; the table
  uses its `linear2_add` helper and charges `slope·(x + y)`. Same two
  coefficients, different function. At 32×64 words Plutus charges 1,153,346 and
  the table charges 140,258 — an order of magnitude under.
- **`verifyEd25519Signature`** (tag 21). Plutus charges the **message**, the only
  argument whose length varies. The table charges the **signature**, which is
  always 64 bytes, so a megabyte message is priced as 64 bytes.

The port reproduces all three as written. A port that silently corrected them
would disagree with the tree it is a port of, and the disagreement would show up
as a fault proof that the two implementations score differently — which is worse
than a known-wrong number in both. They are recorded here, in the module header,
and in three pairs of tests that pin *both* facts: that the port matches Aiken,
and that Aiken does not match Plutus. Correcting them is a change to the Aiken
tree, and it is consensus-affecting.

### The authenticated `Data` node (`src/Midgard/CekData.hs`)

A Merkle-shaped commitment to a Plutus `Data` value in which every node carries
not only its children's roots but the two numbers a CEK replay needs about it —
its **CBOR length** and its **ExMemory**. A step that pushes a `Data` onto the
stack is charged by the value's memory and one that serialises it by its length,
so a proof that had to reveal the value to establish either would be bounded by
the value's size rather than by the step's. Here both travel with the root, and a
step is proved against three 32-byte hashes and a handful of integers however
large the datum is.

Nodes, item links and entry links hash under three separate domain strings, so a
preimage of one shape can never be read as another. The empty list and the empty
map are the hashes of a bare `0x80` under their own domains, which is what lets a
length-zero sequence have a root at all.

**Every inspector re-encodes.** `Aiken.Cbor` reads indefinite-length arrays,
non-minimal integer headers and reordered map keys — deliberately, because
canonicity is a separate question from decodability. The inspectors decode a
preimage, range-check every field, rebuild the node, and then re-encode it and
compare against the bytes they were handed. That comparison is the only thing
rejecting those forms, and the test suite feeds each of them in as a preimage
that decodes to the right fields, so nothing else can be what turns them away.

**The two numbers are checked against `serialiseData`, not against a second
transcription.** The length arithmetic branches on CBOR header widths — a
definite map header, the indefinite `0x9f…0xff` list form, and the 64-byte
chunking Plutus uses for byte strings above 64 bytes — and each branch is pinned
against the actual length of `serialiseData` applied to a real value of that
shape, at every size where a header grows.

**One node shape cannot be inspected at all.** `BytesData` encodes as kind,
*root*, length — the only shape putting its root before its count — while
`inspect_data_node_preimage_v1`'s five-item branch reads kind, *count*, root and
demands an integer where the byte node has 32 bytes. No valid byte node survives
its own inspector. This is the Aiken's behaviour, confirmed by running a
hand-written round-trip through `aiken check`, not an artefact of the port; the
port reproduces it and the suite pins it as a refusal, so a later fix to the
field order has to be a deliberate one on both sides.

### The content-addressed program (`src/Midgard/CekProof.hs`)

Every term, value, sequence link, environment link, continuation frame and blob
chunk of a UPLC program is a 32-byte root, and a fault proof names roots rather
than carrying structures. Nine domain strings keep the shapes apart, and they
have to: a sequence link and an environment link have byte-identical preimages,
and a delay term and a lambda term differ by one tag byte. The test suite
asserts each of those pairs produces different roots, because a port that
dropped or duplicated a domain would pass every round-trip test and still let a
proof read an environment as a term list.

**The sidecar is walked, not trusted.** `verify_complete_program_material_v1`
takes a flat sorted list of (root, preimage) entries and an envelope naming a
term root, a node count and a total byte length. It walks the program from the
term root, resolves each child root against the list, and accepts only when its
own count and byte total match the envelope *and* it reached every entry. That
last clause is the completeness one: without it a sidecar could carry
unreachable material and still verify. Two rules do the rest of the work — a
task's kind and its entry's kind must agree, so a term root resolved to a value
entry is a refusal rather than a coercion; and a zero-count sequence must name
the *canonical* empty root rather than any root at all, so a program cannot
hang unwalked material off a count nobody reads.

The tests build a real eight-node program — `(λx. x) c` over a `Data` constant,
with the constant's value node, its type blob, its semantic `Data` node and that
node's blob leaf — hash it, sort it and pack it into a sidecar. Verifying it is
one test; the rest are single-field mutations of it, each naming the rule it
breaks.

Two things worth knowing that the format inherits rather than states. An empty
blob chunk cannot be published: its preimage is the single byte `0x40`, and
`cbor.deserialise` declines a `B ""` whose header is the last byte of the input
(that refusal is pinned in the CBOR suite). And the program above commits its
constant as an *integer* rather than a byte string, because
`inspect_data_node_preimage_v1` cannot inspect a byte node at all — see the
field-order note above.

### One constant, one root (`src/Midgard/CekConstant.hs`)

A UPLC constant is a type, a payload, and the root both a whole-payload reveal
and a node-by-node semantic proof must agree on. `constant_root_v1` takes a
witness carrying the entire payload; `semantic_constant_root_v1` takes only a
`CekData` summary of it. Both build the same constant-value preimage, with the
semantic root standing in for the payload root *and* the semantic root — and
that identity is what lets a builtin proof touch three nodes of a large list
without the value's identity changing. The suite asserts the two agree across
every payload shape, which is not a tautology: they arrive at the memory number
by different routes.

**The Aiken suite pins five roots produced by a TypeScript implementation**, and
those are carried over verbatim. They are the strongest check in the CEK family
so far — a shared misunderstanding of the preimage would have to be shared with
a third implementation on a third runtime. Between them they cover an integer, a
boolean, a list, a 4,095-byte byte string and an 8,800-byte one, which is one,
two and three blob chunks; so the bounded-blob tree, the semantic `Data`
summary and the constant-value hashing are all pinned on absolute bytes at once.

Two details worth naming. The type is a **prefix expression** — `[5, 0]` is a
list of integers, `[6, 0, 1]` a pair of an integer and a byte string — and the
wire tags **skip 7**, so tags run 0–6 then 8–11 while the constructors run 0–10.
A port that read the wire tag as a constructor index would accept `[7]` and
misname everything above it, so every tag is round-tripped and 7 is pinned as a
refusal. And the two memory numbers must not be confused: `data_memory_size_v1`
is what Plutus charges a `Data` value (four words a node plus the leaf), while
`semantic_memory_size_v1` is what the machine charges a *typed* constant (the
payload alone). A list of two integers costs 3 under the second and 15 under the
first. Both are pinned, because a port that used one where the other belongs
would misprice every constant step and still pass a round-trip suite.

### Committing a blob you cannot hold (`src/Midgard/CekBlobFrontier.hs`)

A streaming commitment to a byte string larger than a proof can carry. Chunks
are appended one at a time and the tree is kept as a list of **peaks**, one per
complete power-of-two subtree, so the state is logarithmic in the chunk count
rather than linear. The peaks run right to left with strictly increasing
heights, which makes appending a fold: a new height-0 peak merges with the head
while the heights match, exactly as binary addition carries.

The clause that ties the peak list to the count is `remaining % (2 * leaves) >=
leaves` — the bit test asserting this subtree's height really is set in the
binary expansion of the leaf count. Without it a frontier could claim any set of
subtrees it liked. Only the rightmost peak may hold a partial final chunk, and
**nothing may be appended after one**: the append guard requires the accumulated
length to be exactly `count` full chunks, which makes the format
streaming-*once* rather than resumable.

**Two implementations of the same commitment already exist in the Aiken**, and
they must agree: `bounded_blob_root_v1` in `cek-proof-v1.ak` commits a blob of
at most three chunks by writing the two tree shapes out by hand, with no
frontier at all. The tests assert the streaming and non-streaming roots match at
every size either side of both chunk boundaries. Beyond three chunks the
reference is an explicitly written tree — `B(B(c1,c2), B(B(c5,c6), c7))` and so
on for one through eight chunks — rather than a second copy of the peak
algorithm, so a reversed ordering or a swapped branch argument would show up
immediately.

Two refusals are aborts rather than declines, and both were confirmed against
`aiken check` rather than assumed. A negative peak height aborts inside
`power_of_two`'s own `expect`, before the `height >= 0` clause that would have
refused it — Aiken's `let` is strict, so the guard never runs. And
`append_chunk_v1` aborts on an oversized chunk because it hashes before it
checks, while `append_chunk_root_v1` *declines* the same length, having nothing
to hash. Same fault, two rejection modes, depending on which door you come in;
both are pinned.

### One frame of a traversal (`src/Midgard/CekDataFrame.hs`)

The state a proof carries while walking a `Data` node one child at a time. A
frame is **filled and then folded, never both at once**: while `fold_cursor` is
zero, children are added to a Merkle frontier one leaf each; once folding
starts, no more may arrive and every child must already be present. The
well-formedness check enforces the separation directly — while the cursor is
zero the sequence summary must still be *exactly* the canonical empty one for
that kind.

Folding runs **right to left**, and that is not an implementation detail. The
sequence summaries in `CekData` are built by prepending, so folding from the
last child backwards is what makes the traversal's root equal the one a
whole-value commitment produces. The fold refuses any index other than
`expected_children - fold_cursor - 1`, so the order cannot be chosen by the
prover.

A map frame stores `2n` children — key, value, key, value — but folds `n` times,
proving *two* membership paths per step at `2i` and `2i + 1`, so a key cannot be
paired with a value from another entry. And a constructor index above 127 never
appears in the frame at all: it travels as its authenticated scalar summary, a
root and two numbers, which is what keeps the frame preimage bounded however
large the alternative is.

**The test that matters is a whole traversal.** For each of fourteen `Data`
values the suite builds the frame, appends every child, folds them all back with
a real membership path each, finalises, and asserts the result equals
`semantic_data_summary_v1` of the original. That one assertion spans `CekData`,
`ValidationMerkle` and this module: a reversed fold order, a child leaf that
omitted its index, a map pairing across entries, or summaries built by appending
would each break it.

One test needed a four-item fixture rather than a three-item one. With three
leaves the last sits alone in its own peak and its membership path is *empty*,
so a corrupted path is indistinguishable from the real one — the test would have
passed without testing anything.

### BLAKE2b, one round at a time (`src/Midgard/Blake2b256Trace.hs`)

BLAKE2b-256 as a **replayable trace**, so a fault proof can charge a single
mixing round rather than a whole digest, and can dispute the hash of a message
far larger than any proof could carry. Fourteen steps a block: one to absorb,
twelve to mix, one to fold.

Plutus has no machine words, so a 64-bit word is eight little-endian bytes and
every operation goes through them — addition is modular arithmetic, XOR is
`xorByteString` over two eight-byte strings, and rotation is a divide and a
multiply. Only BLAKE2b's four rotation distances (32, 24, 16, 63) are accepted;
anything else aborts.

**The oracle is the builtin.** The tests drive the trace to completion and
compare its digest against `blake2b_256` — the ledger's own implementation, in
C. There is nothing to get subtly right and hide: the IV, the parameter block,
the sigma permutation, the counter, the finalisation flag and the zero padding
either all agree or the digest is wrong. Messages sit either side of every block
boundary, since that is where the counter and the finalisation flag change.

Three things the format enforces that are easy to miss. The padding of a short
final block must really be zero — the prover supplies the block, and without the
check two traces could absorb the same message bytes and disagree. A cursor of
zero implies the *canonical* initial chaining value, without which a prover
could start anywhere and reach any digest. And the block argument belongs to
exactly one stage: `ready` demands one, every other stage refuses one, and
`terminal` has no successor.

One deliberate narrowing, pinned so a later port cannot quietly widen it: the
**empty message has no trace at all** (`total_length > 0`), even though
`blake2b_256("")` is perfectly well defined.

### A cycle among top-level Plutarch terms is an infinite value

A group of mutually recursive top-level `Term` definitions type-checks, compiles,
and then exhausts memory the moment the term is built — there is no `S`-indexed
laziness to stop the unfolding, so the "definition" is an infinite value rather
than a recursive function. Aiken writes such groups freely: `cek-data-v1.ak`'s
`semantic_data_summary_v1`, `commit_data_list` and `commit_data_pairs` are a
three-way cycle, and `cbor.deserialise`'s decoder is a larger one — the first
draft of that port exhausted the machine's memory rather than failing.

The fix is to tie the knot once with `pfix` and thread the recursive occurrence
through as an explicit parameter — `pcommitDataList` takes the summariser it
calls rather than naming it. It is worth knowing about because the failure is not
a type error or a hang at an obvious place: it is the machine running out of
memory while a test that has nothing to do with the module is compiling.

### A Plutarch branch-selection hazard

Read this before writing another `pmatch`. Plutarch mis-compiles a `pmatch` whose
arms have **identical bodies**: neither arm is selected, the wildcard is taken,
and a valid input is silently rejected — no error, no trace.

Aiken writes such checks as or-patterns over one shared body, so the shape that
triggers this is exactly the shape a faithful port reaches for. Two sites in
`Midgard.Validators.ActiveOperators` hit it:

- `StrikeForInactivity`, matching the two skipped-operator advancing approaches;
- the removal-driven advance check in
  `validate_scheduler_syncs_with_operator_removal`.

Both now read the constructor tag directly via `pconstrOf` instead of matching —
one obvious meaning, and it cannot regress into the broken shape. Where a field
is needed too, both constructors carry it at the same index, and the read sits
inside a lazy `pif` because `pand'List` is strict and the other constructors have
fewer fields.

How it was found: every component passed in isolation while the composition
failed, and the bisection ended at "reorder two commutative conjuncts and it
works". A follow-up audit of every `\case` in `src/` for identical arm bodies
turned up exactly one other candidate,
`Midgard.Common.Utils.pgetInclusiveUpperBoundOfInterval` and its lower-bound
twin.

**That audit's conclusion has since expired, and the way it expired is the point
worth recording.** It read: *not affected — `PNormalizedTimeRange` is
`DeriveAsSOPStruct`, and the fault appears to be specific to
`DeriveAsDataStruct`.* Porting `invalid-range` moved that type to
`DeriveAsDataStruct`, because its step-02 state carries one in a datum — and both
readers broke the same day, silently, exactly as described. Three validator
suites caught it, all of them indirectly: the failures surfaced as *"correct
input rejected"* in settlement removal, bond holds and claim disproof, none of
which mention time ranges.

The lesson is not about these two functions. It is that **"safe because of its
encoding" is a claim with an expiry date**, and the thing that expires it — a
type moving from Scott to `Constr` because it became a datum or redeemer field —
is a routine and recurring move in this port. Three types have made it so far
(`FieldOpeningV1`, `NativeTxWitnessSetCompact`, `NormalizedTimeRange`). When the
fourth does, audit its consumers for identical arm bodies before running the
suite, not after.

Both readers now go through `pnormalizedBoundAt`, which reads the constructor tag
and indexes the field list — the same workaround as `pconstrOf` above — and both
have direct tests covering every arm that can return a bound, as do all four arms
of the two sites above.

`validators/fraud-proofs/transition-trace/route-v1.ak`'s `route_index` was the
hazard's densest shape — a ten-arm `when` over a data-encoded fault type in which
*four arms return 0*, two return 2, two return 4 and two return 6 — and a literal
`pmatch` port of it would have mis-routed silently. It is ported as a tag read:
`pconstrOf` on the fault, and a second tag read on the nested witness. See the
dispatch-layer section above.

A re-run of the audit over all of `src/` turns up one remaining pair of identical
arm bodies: `Midgard.FraudProofs.NativeTx.Components.pmidgardCredentialHash`,
whose two arms are byte-for-byte the same read of field 0. It has **no wildcard**,
so there is no third branch to fall into, and both arms compute the same thing
from the same index — which is why it is safe rather than lucky. Both credential
kinds are covered by the address round-trip tests. Everything else the audit
flagged is either two arms of a `PMaybe` with different bodies or two separate
function equations.

If you hit an inexplicable "correct input rejected" in a data-encoded `pmatch`
with a wildcard, check whether two arms compile to the same term.

Where a consumer reads one field out of another script's redeemer, the fields it
does not read stay `PData` rather than being given real types — see
`Midgard.StateQueue` and `Midgard.Settlement`. Aiken's `expect X { f, .. } = d`
structurally validates the whole redeemer and a positional read does not, but
the script that owns that redeemer validates it properly in the same
transaction, so the lost check is redundant. Revisit if that stops holding.

`Midgard.Common.Utils` is filled in demand-driven — a helper is ported when the
first validator needing it is ported, so its Plutarch shape is chosen against a
real consumer. The port table above is the source of truth for completed
modules; remaining top-level proof work includes the unfinished portions of
`validation-machine-v1.ak`.

Two encoding rules have already cost a debugging cycle and are worth stating:

- An Aiken record with the default derived encoding is a `Constr 0` on the wire,
  so it ports to `DeriveAsDataStruct`. `DeriveAsDataRec` encodes a bare `List`,
  which is right only for Aiken types with a hand-written `ToData` — as
  `Neighbor` in the MPF library has.
- Aiken's `PosixTime`/`H28`/`H32` are transparent aliases over `Int`/`ByteArray`.
  They port to type synonyms, not newtypes; wrapping them in the ledger newtypes
  would silently change the encoding at every call site.
- `tests/Testing/`: MPF, validator, transaction-proof, crypto, and evaluation
  tests.
- `generated/`: generated Plutus JSON for the membership staking scripts.
- `app/Main.hs`: script-generation executable.

## Build and test

The package is built independently from the Aiken project. From this directory:

```sh
nix develop
cabal build all
cabal test all --test-show-details=direct
```

The Plutarch suite is not currently part of the primary Midgard node CI
workflow. A proof or release claim that depends on these helpers must record a
successful run and verify the generated script hashes against the deployment
manifest. Regenerating files under `generated/` requires review of the resulting
script bytes and hashes; do not treat generated changes as formatting output.

## Scope and safety

These helpers prove MPF membership properties. They do not establish complete
Midgard fraud-proof coverage, data availability, transition validity, or safe
challenge timing. See `../../docs/fault-proofs/` for the current coverage audit.

## License

See [LICENSE](LICENSE) for the MIT license.

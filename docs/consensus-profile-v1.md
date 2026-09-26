# Midgard canonical consensus profile V1

Status: normative implementation contract for the next validator deployment.
This is the sole pre-launch Midgard profile. Release activation remains closed
until the validator-hash-bound proof evidence is complete.

## 1. Safety rule

A transaction feature is supported by V1 only when an honest challenger can
make an incorrect operator verdict or state transition lose on L1 before the
challenged block matures. Off-chain admission, DA re-execution, and honest-node
agreement are defence in depth; none of them substitutes for the L1 path.

Unknown versions, unknown machine instructions, missing preimages, malformed
proofs, unavailable bisection nodes, non-canonical encodings, and witnesses
that exceed a compiled proof bound fail closed.

## 2. Exact version tuple

The canonical deployment uses one indivisible version tuple:

| Surface                             |                         V1 value |
| ----------------------------------- | -------------------------------: |
| consensus profile id                |           `midgard-consensus-v1` |
| `Header.protocol_version`           |                              `1` |
| native transaction version          |                              `1` |
| transition-step schema              |                              `1` |
| validation-machine version          |                              `1` |
| validation-trace descriptor version |                              `1` |
| validation-dispute version          |                              `1` |
| DA inner payload schema             |                              `1` |
| CEK program envelope                |                              `1` |
| CEK constant-value schema           |                              `1` |
| CEK program-material schema         |                              `1` |
| CEK material sidecar                |                              `1` |
| proof submission envelope           |                              `1` |
| deployment manifest schema          | `midgard-deployment-manifest-v1` |
| protocol-info API                   |                              `1` |

No component may negotiate individual members of the tuple. A deployment
either matches the complete tuple and profile digest or refuses to start,
submit, build, sign, attest, or challenge. Any non-V1 tuple is rejected.

## 3. Authenticated block data

The V1 header adds `validation_traces_root`, a counted MPF root keyed by the
same `EventKey` used by the transition trace. Each value is:

```text
ValidationTraceDescriptor {
  schema_version,
  machine_version,
  trace_root,
  step_count,
  initial_state_hash,
  terminal_state_hash,
  verdict,
  rejection_code_hash,
}
```

`trace_root` is the binary Merkle root of exactly `step_count + 1` machine
state hashes, padded by repeating the terminal state to the next power of two.
The descriptor commits both endpoints, the exact count, and the operator's
verdict. The counted map prevents empty-root/count ambiguity.

The DA payload contains canonical full transactions and all ledger/script
preimages needed to reconstruct the deterministic trace. It does not contain
the complete state-hash trace. A challenged operator must reveal requested
bisection children from the committed tree. Failure to answer before the
compiled response deadline proves the block faulty.

## 4. Canonical transaction and forced-order data

Native transaction V1 is the canonical off-chain/DA encoding. It treats each
dynamic field preimage as an independently bounded proof item instead of
requiring the complete transaction in one fault-proof transaction.

```text
MidgardTransaction {
  version = 1,
  validity,
  body,
  witness_set,
}

MidgardTransactionWitnessSet {
  addr_tx_wits,
  script_tx_wits,
  redeemer_tx_wits,
}
```

Each field preimage uses the canonical definite-array/byte-envelope grammar in
[MidgardTx §5](spec/midgard-tx.md#5-the-uniform-enveloped-preimage-grammar).
Nested datum/redeemer Plutus Data follows §6.2, including its required
indefinite list and long-byte-string forms. CEK program material is a separate
DA/material-publication concern, not a fourth witness-set field.
The compact transaction commits each field hash and the full witness-set hash.
The transaction id remains the domain-separated hash of the canonical compact
body and version.

A V1 transaction-root value is the exact Plutus-Data projection produced by the
production native encoder:

```text
L2TransactionSourceV1 {
  tx_id,
  source: NativeTxProofSourceV1 {
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  },
}
```

The map key is the 32-byte `tx_id`, and duplicate keys are rejected. A
V1 transaction-order payload commits a distinct validity-free forced source
and its forced-domain proof commitment. The operator adds a typed verdict in the forced-inclusion leaf.
The forced-transactions source root maps
the serialized L1 order output reference (not the L2 transaction ID) to:

```text
ForcedInclusionTxV1 {
  tx_id,
  submitted_source: ForcedTxProofSourceV1 {
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  },
  verdict: ForcedTxValid | ForcedTxInvalid { reason: RejectionReasonV1 },
}
```

The submitted full encoding is `[1, body, witness_set]`, and its compact has
`[1, compact_body, witness_set_hash]`. Neither contains validity. The verdict
is the only operator decision; changing it preserves the original source and
body-derived ID. The forced commitment uses `MidgardForcedTxProofSourceV1`.
Normal transactions retain their four-element encoding and native hash domain.
Forced ledger size is submitted byte length plus one; transport uses actual bytes.

The hashed profile requires
`forcedTransactionSourceEncoding: "midgard-forced-submission-v1"`.
This pre-launch replacement changes deployment identity even though the V1
profile name remains. Old profile, validator, DA and journal identities cannot
attach to the new deployment. Validators and dependent identities must be
regenerated together. Existing persistent deployments are not reset or migrated
implicitly; a fresh deployment requires its own explicit lifecycle decision.

The same L2 transaction may therefore occur under multiple order keys, and
each forced value preserves the exact operator verdict constructor. The
normal map is committed under `TransactionsV1RootDomain`; the forced map is
committed under `ForcedTransactionsV1RootDomain`. These domains are distinct
and are part of the counted-root preimage.
The canonical input and generated TypeScript/Aiken golden are maintained at
`demo/midgard-node/tests/fixtures/transaction-root-v1.canonical.json`; run
`pnpm --dir demo run fixtures:transaction-root-v1:check` to verify that the
derived JSON and Aiken projection are fresh. Set `MIDGARD_AIKEN_BIN` when a
specific Aiken formatter executable must be used.

Normal DA carries every canonical field preimage. Forced-order material follows
[MidgardTx §8.11](spec/midgard-tx.md#811-forced-order-material-carriage-normative):
the order mint authenticates each non-empty field through the shared field-access
door, using inline, raw-UTxO, or certified carriage. The ordered carriage vector
must be exhausted exactly; empty fields have the canonical one-byte encoding.
Authenticated lengths must match the compact source's declared lengths.

The order datum commits the transaction source, not carriage output references.
After minting, material is available from L1 history and addressed by digest;
order settlement does not consume the carriage UTxOs. Raw material remains in
the publisher's custody and can be reclaimed after minting. A later dispute
re-publishes identical bytes when needed. The burn carries an empty material
vector. The implementation is in
[`tx-order-v1.ak`](../onchain/aiken/validators/user-events/tx-order-v1.ak) and its
[material verification library](../onchain/aiken/lib/midgard/user-events/tx-order-v1.ak).

This flow replaces the staged field-receipt protocol. Its old receipt execution
measurements do not demonstrate fit for the current mint. The variable-width
field walk still has the execution-budget limitation documented in §8.11;
release acceptance must resolve that limitation for the required capability floor.

Non-native program material uses a separate permissionless, append-only L1
publication address:

```text
CekProgramMaterialDatumV1 {
  kind,
  root,
  preimage,
}
```

Each output carries one independently bounded content node. The compiled
validator has no successful spending path, so an operator cannot erase a
forced submitter's material before inclusion or challenge. The typed root is
recomputed from `(kind, preimage)`; a wrong root, kind, encoding, or oversized
preimage is ignored and cannot satisfy a program envelope. The compact
transaction already commits each program's term root, node count, and material
byte count, so the order datum does not contain an unbounded list of material
references. A node discovers the graph from those roots, persists exact
content-addressed nodes, and refuses to build a due proof-profile block while
a required graph is incomplete. Material may be published across multiple L1
transactions before the order; each node remains independently revealable.

## 5. Validation machine

`ValidationMachineV1` is deterministic and total over canonical bounded
inputs. It has these ordered phases:

1. field-by-field canonical decode, size, version, and domain checks;
2. transaction-id, compact, and field-preimage-hash binding;
3. static network, fee, and immutable-body rules;
4. spend/reference-input uniqueness, disjointness, and validity-bound shape;
5. required-signer and address-witness verification;
6. stateless Phase-A native-script validation;
7. stateless observer and script-bundle preconditions;
8. validity-at-slot and prior-state spend/reference resolution with MPF
   membership/non-membership witnesses, including input authorization;
9. script-source, reference-script, redeemer-purpose, protected-receive, mint,
   and observer discovery;
10. resolved native-script evaluation;
11. exact script-language-set and script-integrity binding;
12. PlutusV3/MidgardV1 context construction and CEK execution for spend, mint,
    receive/protected-output, and observe purposes;
13. multi-asset input/output/mint/fee accounting;
14. ledger-delta verification (`LedgerDelta`);
15. absorbing acceptance or rejection (`Terminal`).

The phase code order above is consensus data. The sole pre-launch machine
version is `1`; it includes authenticated source constants, a distinct
runtime-only script-context term, and work-witness hashing using Aiken's exact
`cbor.serialise` byte-string chunking. Retired internal version numbers are not
accepted deployment versions. See
[`consensus-profile.ts`](../demo/midgard-core/src/consensus-profile.ts).

Every state commits the phase, program counter, immutable transaction/source
commitment, prior ledger root, the operator's claimed ledger delta root,
work-stack roots, accumulated execution units, and current verdict. A terminal
state is absorbing, which makes Merkle padding unambiguous.

The claimed ledger delta root is part of the state's immutable context: it is
supplied once with the initial state and is carried unchanged by every
transition (`validation-machine/` `immutable_context_matches`) and across
the committed claim endpoints (`validation-claim-v1.ak`). No instruction writes
it; the accepting terminal reconstructs the operation frontier independently and
compares it against the claim.

The same initial-state constructor and machine apply to normal and forced
transactions. Source authentication differs; transaction semantics do not.

## 6. Script programs and CEK

V1 script witnesses use a canonical Merkleized UPLC program representation.
The credential commits `(language, program_version, term_root)`. Raw Flat/CBOR
bytes are an SDK input format, not the consensus identity: the SDK decodes
them and emits the canonical term DAG. This removes an otherwise separate
untrusted bytes-to-term interpretation from L1 consensus.

The authoring decoder must round-trip to the exact canonical Flat bytes before
the graph is accepted. UPLC 1.1.0 type-instantiation forces that the authoring
library represents implicitly around polymorphic builtins are restored as
explicit authenticated term nodes. Missing, duplicated, or otherwise
non-canonical builtin forces therefore reject instead of being normalized into
a different proof program.

Term, environment, value, and continuation nodes are domain-separated and
hash-addressed. A CEK one-step witness supplies only the preimages touched by
that transition. L1 checks every supplied node hash and computes the unique
next state. Builtin execution uses the corresponding Plutus V3 builtin with
the exact Plutus V3 cost-model digest. Unknown term tags, builtin tags, language
versions, or cost-model digests are rejecting terminal states.

PlutusV3 receives the canonical Cardano-compatible Plutus V3 context defined by the
technical specification. MidgardV1 receives the protected-address/receive and
observer-aware context. Receiving is not accepted for PlutusV3. Script success
means a halting CEK constant within the redeemer's declared and profile
execution-unit bounds; error, non-constant halt, or budget exhaustion rejects.

## 7. Interactive dispute

A challenger opens a dispute against one authenticated descriptor and posts a
bond plus its claimed terminal state. Direct endpoint mismatches resolve
without bisection. Otherwise:

1. operator and challenger bisect the committed interval;
2. each move is bound to the current interval and trace roots;
3. the interval shrinks until it contains one machine transition;
4. the L1 one-step verifier computes the successor from the agreed pre-state;
5. the party whose post-state differs loses.

An invalid move, an unavailable node, or a missed response deadline loses.
Timeout branches are explicit validator transitions, not watcher policy.
The stateful `validation-trace/dispute-v1` computation-thread validator binds
the opening claim to an authentic `Header`, the challenged header hash, and
the block operator key. Operator midpoint moves require that operator's
signature; challenger moves require the fraud prover's signature. Every move
reproduces the same computation-thread NFT and exact dispute datum. The final
resolver accepts only the dispute stored by that UTxO and mints a fraud-proof
result only when the L1 one-step verifier selects the challenger. If the
operator owes a move and withholds it, the timeout transition selects the
challenger.

Dispute version 1 uses a five-minute (`300,000` ms) response window and at most
32 bisection rounds. The exact V1 block maturity is seven days
(`604,800,000` ms). The derived minimum for opening, two full windows per
round, settlement, and a two-times reserve is `39,600,000` ms. The opening
validator also rejects a challenge begun too late to finish before maturity.
A differently versioned response schedule or non-exact maturity fails
manifest admission.

For a forced order, a terminal verdict different from the operator verdict is
a fault. Therefore a valid transaction classified invalid/no-op and an invalid
transaction classified valid/effectful are both challengeable. An invalid
forced transaction whose no-op transition matches its rejecting terminal
state remains supported.

## 8. Transition binding

An accepting terminal state derives the exact ordered delete/insert ledger
operations. The transition-trace one-step proof checks those operations
against the prior root. A rejecting terminal state derives no operations and
requires `pre_utxos_root == post_utxos_root`.

The rejecting terminal's no-op obligation is discharged **at this
transition-binding layer only**, and never by mutating a validation-machine
state field. Concretely it is enforced three ways: the rejection work witness
itself commits `post root = prior root` with an empty operation list
(`encode_terminal_rejection_witness`); the committed claim requires
`pre_utxos_root == post_utxos_root` for a `Rejected` descriptor
(`validation-claim-v1.ak`); and any actual ledger movement on an invalid
forced transaction is a unilateral fault
(`fraud-proofs/transition-trace/proof.ak`). The machine state's claimed ledger
delta root is immutable context (§5) and is carried forward unchanged by a
rejecting terminal.

V1 requires the same accepted-transaction transition witness for valid forced
transactions as for normal L2 transactions, with forced-source membership and
full-transaction binding. Canonical block construction and DA verification
retain the complete forced transaction preimage and apply its validated
delete/insert frontier. Release activation remains closed until the complete
validator-hash-bound normal and forced proof paths satisfy the release-evidence
gate.

## 9. Feature surface

After the L1 verifier and dispute game are deployed, V1 supports:

- mint and burn, including mint-policy authorization;
- spend and reference inputs;
- inline and reference native/PlutusV3/MidgardV1 scripts;
- script payment credentials and redeemers;
- protected public-key and script outputs, including receive purposes;
- required observers;
- valid effectful forced transactions;
- invalid forced transactions as proved no-ops.

Builders retain mint/burn and script APIs. Support is selected by the exact
connected V1 deployment tuple, never by an operator boolean.

## 10. Compiled bounds and proof-fit gate

No 8 KiB aggregate transaction ceiling exists in V1. The effective transaction
bound is derived by summing every bounded dynamic field in the canonical
encoding and then adding its fixed-size fields and CBOR framing.

[Appendix A](#appendix-a--exact-compiled-profile) is the generated inventory of
profile limits. Do not maintain a second prose table of those values. The limits
are defined in
[`consensus-profile.ts`](../demo/midgard-core/src/consensus-profile.ts); field
carriage is defined by [MidgardTx §8](spec/midgard-tx.md#8-field-preimage-carriage-three-tiers).
The profile's transaction-field chunk reservation and CEK blob chunk limit are
not the field-carriage chunk size `K`.

Aggregate fields and full ledger-output preimages can exceed one proof
transaction's available witness budget. Their bounds therefore require
incremental authentication and consumption, with real transaction framing
included in each step's measurement. CEK programs use a compact envelope and
content-addressed material nodes; raw Flat is an authoring format. Exact encoded
DA size, including tuple framing and all material, remains the aggregate gate.

Measured bytes and execution units are evidence tied to a compiler, blueprint,
parameters, and fixture; they are not additional consensus constants. Use the
[component-spec carriage measurements](spec/midgard-tx.md#810-cost-claims--the-carriage-exit-measurements)
and [fault-proof testing status](fault-proofs/testing-status.md) to locate
measurement commands and limitations. Historical receipt-protocol measurements
and unsigned CML framing estimates do not activate the current profile.
Release evidence must measure the actual applied publication, resolution, and
settlement transactions against the deployment's Cardano parameter snapshot.

These are upper bounds, not throughput targets. A release may lower a bound
without changing semantics only by deploying a distinct profile id and
digest. It must never raise one in place.

Before V1 is marked supported, generated worst-case witnesses for every
machine instruction and builtin must satisfy all of:

- serialized L1 proof transaction at or below the deployment's measured
  `maxTxSize`;
- execution memory and CPU at or below the deployment's measured protocol
  limits with a 20% reserve;
- challenger can complete 32 rounds plus settlement inside half the maturity
  window under the configured response deadlines — the maturity window for
  this threshold is the exact V1 production block maturity (seven days;
  half-maturity 302,400,000 ms), never a scaled acceptance or test window
  (owner clarification, 2026-08-06);
- canonical DA payload and decompressed payload stay within their compiled
  limits;
- mutation tests show unknown tags, omitted nodes, oversized preimages, wrong
  roots, and timeout paths all reject.

If any instruction has no fitting witness, that instruction and every script
language version that can reach it remain unsupported in a different profile.
V1 must not advertise partial builtin coverage as PlutusV3 or MidgardV1
support.

## 11. Release evidence

The support claim requires:

- Aiken positive/negative tests for every machine instruction and terminal
  rule;
- differential traces against the off-chain evaluator for all UPLC terms and
  Plutus V3 builtin tags supported by the version;
- adversarial disputes for wrong endpoints, wrong midpoint, wrong CEK step,
  withheld response, and both forced-verdict directions;
- property tests for value conservation, mint/burn, source resolution,
  redeemer cardinality, and protected/observer purposes;
- end-to-end normal and forced transactions for native, PlutusV3, and
  MidgardV1 scripts;
- generated proof-size/CPU/memory and DA-bound reports tied to the validator
  hashes and deployment profile digest.

No documentation or API may report a V1 feature as supported before all of
that evidence is present for the deployed validator hashes.

## Appendix A — Exact compiled profile

This block is generated from the compiled canonical profile. CI checks it
byte-for-byte so documented limits, features, schema identities, proof
families, and the profile digest cannot drift from source.

<!-- BEGIN MIDGARD_CONSENSUS_PROFILE_V1_GENERATED: do not edit -->

Profile digest: `6a872433afcfa01800204da702cabfc969bfc70da80fb22168779e3c0291fa01`

```json
{
  "cekProgramEnvelopeVersion": 1,
  "cekProgramMaterialSidecarVersion": 1,
  "cekProgramMaterialVersion": 1,
  "cekValueSchemaVersion": 1,
  "daEnvelopeVersion": 1,
  "daPayloadVersion": 1,
  "daRuntimeManifestSchemaVersion": "midgard-da-libp2p-runtime-manifest-v1",
  "daTransportProtocolVersion": 1,
  "deploymentManifestSchemaVersion": "midgard-deployment-manifest-v1",
  "features": [
    "mint_burn",
    "reference_inputs",
    "native_cardano_scripts",
    "plutus_v3_scripts",
    "midgard_v1_scripts",
    "script_witnesses",
    "redeemers",
    "reference_scripts",
    "l1_program_material_publication",
    "script_payment_credentials",
    "protected_outputs",
    "required_observers",
    "valid_forced_transactions",
    "invalid_forced_transactions"
  ],
  "forcedTransactionJournalVersion": 1,
  "forcedTransactionSourceEncoding": "midgard-forced-submission-v1",
  "headerSchemaVersion": 1,
  "ledgerOutputSchemaVersion": 1,
  "limits": {
    "blockMaturityMs": 604800000,
    "coinsPerUtxoByte": 4310,
    "maxAddressWitnessCount": 16384,
    "maxAddressWitnessesPreimageBytes": 32768,
    "maxCanonicalTransactionBytesPerBlock": 16777216,
    "maxCekBlobChunkBytes": 4095,
    "maxCekBuiltinTag": 86,
    "maxCekDirectBlsExpressionDepth": 10,
    "maxCekDirectBlsMillerLoopLeaves": 10,
    "maxCekProgramEnvelopeBytes": 50,
    "maxCekProgramMaterialBytes": 67108417,
    "maxCekProgramNodeCount": 1597819,
    "maxDaPayloadBytes": 67108864,
    "maxDepositCount": 10000,
    "maxDistinctAssetCount": 16384,
    "maxForcedTransactionCount": 10000,
    "maxL2TransactionCount": 10000,
    "maxLedgerMembershipProofOverheadBytes": 12288,
    "maxLedgerOperationCount": 40000,
    "maxLedgerOutputPreimageBytes": 16384,
    "maxMintPreimageBytes": 32768,
    "maxNativeScriptDepth": 16384,
    "maxNativeScriptNodeCount": 16384,
    "maxOutputCount": 16384,
    "maxOutputsPreimageBytes": 32768,
    "maxOutputValueCborBytes": 5000,
    "maxRedeemersPreimageBytes": 32768,
    "maxReferenceInputCount": 16384,
    "maxReferenceInputsPreimageBytes": 32768,
    "maxRequiredObserverCount": 16384,
    "maxRequiredObserversPreimageBytes": 32768,
    "maxRequiredSignerCount": 16384,
    "maxRequiredSignersPreimageBytes": 32768,
    "maxScriptExecutionCount": 16384,
    "maxScriptWitnessesPreimageBytes": 32768,
    "maxSinglePublicationCompleteItemBytes": 14396,
    "maxSpendInputCount": 16384,
    "maxSpendInputsPreimageBytes": 32768,
    "maxTotalEventCount": 40000,
    "maxTransactionAggregateFieldBytes": 32768,
    "maxTransactionFieldChunkBytes": 4095,
    "maxTransactionFieldProofOverheadBytes": 7168,
    "maxTransitionStepCount": 40000,
    "maxTxCanonicalCborBytes": 295041,
    "maxValidationBisectionRounds": 32,
    "maxValidationMachineStepCount": 4294967295,
    "maxValidationTraceCount": 20000,
    "maxWithdrawalCount": 10000,
    "minSupportedL1MaxTxBytes": 16384,
    "minSupportedL1MaxTxCpuUnits": 10000000000,
    "minSupportedL1MaxTxMemoryUnits": 16500000,
    "minSupportedTransactionExecutionCpuUnits": 10000000000,
    "minSupportedTransactionExecutionMemoryUnits": 16500000,
    "minValidationDisputeMaturityMs": 39600000,
    "validationDisputeResponseWindowMs": 300000
  },
  "mpfProofSchemaVersion": 1,
  "nativeTransactionProofSourceVersion": 1,
  "nativeTransactionVersion": 1,
  "profileId": "midgard-consensus-v1",
  "proofSubmissionEnvelopeVersion": 1,
  "protocolInfoApiVersion": 1,
  "protocolVersion": 1,
  "requiredProofFamilies": [
    "validation-trace-endpoint",
    "validation-trace-bisection",
    "validation-machine-one-step",
    "validation-dispute-timeout",
    "transition-trace-accepted-transaction",
    "transition-trace-rejected-no-op",
    "forced-transaction-verdict-mismatch",
    "forced-program-material-availability"
  ],
  "scriptProofSchemaVersion": 1,
  "stateQueueSchemaVersion": 1,
  "transactionFieldPublicationSchemaVersion": 1,
  "transactionOrderSchemaVersion": 1,
  "transitionStepSchemaVersion": 1,
  "validationDisputeVersion": 1,
  "validationMachineVersion": 1,
  "validationTraceDescriptorVersion": 1
}
```

<!-- END MIDGARD_CONSENSUS_PROFILE_V1_GENERATED -->

`requiredProofFamilies` names release obligations, not a coverage attestation.
Use the [coverage matrix](fault-proofs/coverage-matrix.md) and
[public-testnet checklist](public_testnet_readiness.md) to assess completion for
a particular revision and deployment.

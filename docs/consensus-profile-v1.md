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
MidgardTransactionV1 {
  version = 1,
  body,
  witness_set,
}

MidgardTransactionWitnessSetV1 {
  address_witnesses,
  script_witnesses,
  redeemers,
  script_programs,
}
```

Every variable-sized field has one canonical definite-length CBOR encoding.
The compact transaction commits each field hash and the full witness-set hash.
The transaction id remains the domain-separated hash of the canonical compact
body and version.

A V1 transaction-order commitment contains the compact transaction, the fixed
body fields, and the hash of every canonical dynamic field preimage. The
forced-transactions source root maps the order id to:

```text
ForcedInclusionTxV1 {
  tx_id,
  compact_tx,
  field_preimage_hashes,
  operator_verdict,
}
```

Normal DA carries every canonical field preimage. A forced submission uses a
staged L1 protocol for each field:

1. publish the field as a script-locked fragment bound to the order id,
   transaction commitment, field index, and receipt policy;
2. reference that existing fragment in a receipt-policy transaction;
3. let the receipt policy verify the compact source, canonical field,
   committed length/hash, and exact compiled field bound;
4. mint one deterministic receipt NFT to a compact receipt datum that binds
   the exact fragment output reference.

Publishing and receipting are separate transactions because a transaction ID
hashes its inline output datums; embedding the same transaction's ID in its own
receipt datum would be circular. The final order mint references exactly nine
compact receipt UTxOs. It does not place all field fragments in one validator
context. Its datum records both ordered fragment and receipt references.
Consuming the order must consume all eighteen staged UTxOs and burn both the
order NFT and all receipt NFTs. Fragment and receipt spending validators
independently enforce the exact burns. A sidecar or unreceipted fragment is
insufficient. Missing, duplicated, non-canonical, mismatched, oversized, or
wrong-policy material fails closed.

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
14. accepted ledger delta or rejected no-op terminal state.

The phase code order above is consensus data. Versions 1 and 2 never reached
the release gate and are rejected rather than reinterpreted. Version 3 added a
cursor for independently revealed field preimages. Version 4 additionally
binds every CEK constant to its semantic Data root and exact UPLC memory, and
admits typed Data-node material. It also binds the off-chain rejection
priority to the L1 instruction order, including transactions that violate more
than one rule. Version 8 made source-constant decoding an exact one-step rule
and used a distinct runtime-only term for the authenticated script context, so
a source program cannot substitute an unproved semantic-memory claim.
Version 9 additionally pins work-witness hashing to Aiken's exact
`cbor.serialise` byte-string chunking, closing the off-chain/on-chain encoding
ambiguity for witnesses longer than 64 bytes.

Every state commits the phase, program counter, immutable transaction/source
commitment, prior ledger root, the operator's claimed ledger delta root,
work-stack roots, accumulated execution units, and current verdict. A terminal
state is absorbing, which makes Merkle padding unambiguous.

The claimed ledger delta root is part of the state's immutable context: it is
supplied once with the initial state and is carried unchanged by every
transition (`validation-machine-v1.ak` `immutable_context_matches`) and across
the committed claim endpoints (`validation-claim-v1.ak`). No instruction writes
it; the accepting terminal reconstructs the operation frontier independently and
compares it against the claim. This enumeration previously omitted the field
even though `encode_machine_state` commits it; the omission is corrected here.
The correction was prompted by a production defect in which the *rejecting*
terminal rule required the successor to clear this field, contradicting its
immutability and making rejection one-steps unprovable from every pre-state
carrying a real (non-empty) claimed delta. See §8.

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
the opening claim to an authentic `HeaderV1`, the challenged header hash, and
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

This was previously implemented incorrectly: `rejected_successor_is_exact`
additionally required the rejecting successor to *write* the empty frontier
commitment into `ledger_delta_root`. Because the same transition must satisfy
`immutable_context_matches` (pre == post on that field), the two clauses were
jointly unsatisfiable for every pre-state with a non-empty claimed delta —
i.e. for every real transaction, and in particular for the governing
adversarial case in which an operator commits an `Accepted` descriptor over a
transaction that truly rejects. No challenger could construct a winning
rejection successor, so the dishonest operator won by default. The clearing
clause has been deleted; the obligation lives here, where it always belonged.

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

| Item                                                            |                                                                                            Maximum |
| --------------------------------------------------------------- | -------------------------------------------------------------------------------------------------: |
| supported L1 fault-proof transaction floor                      |                                                                                             16 KiB |
| supported L1 fault-proof execution floor                        |                                                             16,500,000 memory / 10,000,000,000 CPU |
| supported Midgard transaction execution floor                   |                        16,500,000 memory / 10,000,000,000 CPU; validation may span multiple proofs |
| transaction-field proof overhead reservation                    |                                                                                              7 KiB |
| each aggregate dynamic transaction field                        |                                                32,768 bytes, consumed through authenticated chunks |
| each independently revealed transaction-field chunk             |                                                                                        4,095 bytes |
| measured maximum field-publication datum                        |                                                                                        4,574 bytes |
| measured maximum unsigned field-publication transaction         |                                                                                        4,675 bytes |
| maximum CEK-material publication datum                          |                                                                                        4,268 bytes |
| measured one-node unsigned CEK-material publication transaction |                                                                                        4,369 bytes |
| measured maximum field-chunk receipt publication                |                                                               3,398,228 memory / 1,209,745,039 CPU |
| measured canonical receipt-order verification                   |                                                                 1,233,800 memory / 432,521,347 CPU |
| ledger-membership proof overhead reservation                    |                                                                                             12 KiB |
| each ledger output preimage                                     |                                             16,384 bytes, retained and authenticated incrementally |
| serialized Cardano output `Value`                               |                                                      5,000 bytes; no lower independent Midgard cap |
| consensus script envelope                                       |                                                                                           50 bytes |
| canonical CEK material nodes per program                        |                                                           at most 1,597,819 within the DA envelope |
| canonical CEK material per program                              |                           at most 67,108,418 structural bytes; exact encoded aggregate must fit DA |
| canonical CEK blob chunk                                        |                                                                                        4,095 bytes |
| pinned CEK builtin tags                                         |                                                                                       0 through 86 |
| derived canonical full transaction                              |                                                                                      295,041 bytes |
| canonical datum                                                 |                                         no independent cap; contained by its output/field preimage |
| reference script                                                | supported as a real output reference script with separately authenticated/chunked program material |
| spend-inputs aggregate field                                    |                                                                                       32,768 bytes |
| reference-inputs aggregate field                                |                                                                                       32,768 bytes |
| outputs aggregate field                                         |                                                                                       32,768 bytes |
| required-observers aggregate field                              |                                                                                       32,768 bytes |
| required-signers aggregate field                                |                                                                                       32,768 bytes |
| mint aggregate field                                            |                                                                                       32,768 bytes |
| address-witnesses aggregate field                               |                                                                                       32,768 bytes |
| script-witnesses aggregate field                                |                                                                                       32,768 bytes |
| redeemers aggregate field                                       |                                                                                       32,768 bytes |
| spend-input count guardrail                                     |                                                              16,384; aggregate bytes are effective |
| reference-input count guardrail                                 |                                                              16,384; aggregate bytes are effective |
| output count guardrail                                          |                                                              16,384; aggregate bytes are effective |
| address-witness count guardrail                                 |                                                              16,384; aggregate bytes are effective |
| required-signer count guardrail                                 |                                                              16,384; aggregate bytes are effective |
| script-execution/redeemer count guardrail                       |                                                    16,384; bytes and execution units are effective |
| required-observer count guardrail                               |                                                              16,384; aggregate bytes are effective |
| distinct non-ADA asset count guardrail                          |                                                                16,384; `Value` bytes are effective |
| native-script depth/node guardrail                              |                                                       16,384 each; transaction bytes are effective |
| each transaction-bearing source class per block                 |                                                                                             10,000 |
| total source events / transition steps per block                |                                                                                             40,000 |
| validation trace descriptors per block                          |                                                                                             20,000 |
| ledger operations per block                                     |                                                                                             40,000 |
| validation-machine steps                                        |                                                                                         `2^32 - 1` |
| bisection rounds                                                |                                                                                                 32 |
| dispute response window                                         |                                                                                         300,000 ms |
| derived minimum maturity for this dispute schedule              |                                                                                      39,600,000 ms |
| exact V1 block maturity                                         |                                                                            604,800,000 ms (7 days) |
| canonical transactions per block                                |                                                                                             16 MiB |
| DA payload                                                      |                                                                                             64 MiB |

The 32,768-byte aggregate-field reservation is not an independently revealed
preimage or an L1 transaction claim. It accommodates canonical Midgard wrapper
expansion while proofs consume the field in ordered, authenticated chunks of
at most 4,095 bytes. Finalization authenticates the exact aggregate count and
length. The one-byte-per-item count guardrail is derived from Cardano's 16 KiB
complete-transaction floor; real item encodings and aggregate byte limits are
always tighter, so the guardrail cannot exclude a collection cardinality that
could fit in a Cardano transaction.

The current publication measurements use a maximum field chunk: a 4,574-byte
datum and 4,675-byte unsigned publication transaction. Script execution
reveals one independently bounded CEK material node at a time; the compact
50-byte program envelope identifies the complete content-addressed graph and
its measured source-scan resolver payload is 7,546 bytes. A maximum
4,095-byte blob chunk has a 4,098-byte canonical typed preimage, a 4,268-byte
immutable publication datum, and a 4,369-byte one-input/one-output unsigned
publication transaction.

Diagnostic CML framing measurements do not activate the profile. Release
evidence must construct the actual applied/parameterized publication,
resolution, and settlement transactions and measure their complete serialized
bytes and execution units against the live Cardano parameter snapshot. Ledger
outputs are retained in full but represented in MPF leaves by small
authenticated descriptors, allowing their real bytes, Values, datums, and
reference scripts to be checked incrementally rather than imposing a smaller
whole-output proof limit. The release-evidence digest remains unset until
those paths and the capability-parity corpus pass.

CEK graph material is not capped by the former 6,911-byte raw-script limit.
Raw Flat/CBOR is an authoring input; consensus carries a 50-byte program
envelope plus independently content-addressed nodes. An otherwise-empty,
structurally valid canonical V1 payload is 445 bytes. Switching its material
list from empty to non-empty leaves 446 fixed bytes outside the tuples. The
smallest possible tuple is 42 bytes (tuple framing, a 32-byte content root,
and a versioned typed one-byte preimage). Therefore the 64 MiB DA envelope
admits no more than `floor((67,108,864 - 446) / 42) = 1,597,819` material
nodes. The corresponding structural preimage-byte upper bound is 67,108,418
bytes. Actual tuple framing and typed preimages consume additional bytes, so
the exact canonical V1 encoded-size gate is authoritative and generally
tighter. These bounds remove the arbitrary raw-script cap without allowing a
program to escape the finite DA/proof envelope.

A one-shot order mint that verified all nine maximum fields measured
45,154,331 memory and 14,905,078,582 CPU, so it is not a valid consensus path.
The staged receipt protocol is mandatory, not an optimization or feature
flag. Its generated near-maximum fixture is 51,080 bytes of canonical L2
transaction data. Every individual receipt proof remains below the compiled
16.5M-memory/10B-CPU L1 floor; the largest is the streaming canonical mint
field at the values shown above. The final order validates only the nine
compact receipts. These execution-unit measurements are pinned alongside the
byte-envelope tests.

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
  window under the configured response deadlines;
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

Profile digest: `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`

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
  "headerSchemaVersion": 1,
  "ledgerOutputSchemaVersion": 1,
  "limits": {
    "blockMaturityMs": 604800000,
    "maxAddressWitnessCount": 16384,
    "maxAddressWitnessesPreimageBytes": 32768,
    "maxCanonicalTransactionBytesPerBlock": 16777216,
    "maxCekBlobChunkBytes": 4095,
    "maxCekBuiltinTag": 86,
    "maxCekDirectBlsExpressionDepth": 10,
    "maxCekDirectBlsMillerLoopLeaves": 10,
    "maxCekProgramEnvelopeBytes": 50,
    "maxCekProgramMaterialBytes": 67108418,
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

Note on `requiredProofFamilies`. `validation-machine-one-step` and
`forced-transaction-verdict-mismatch` are listed as required and are, after the
`rejected_successor_is_exact` fix described in §8, provable in both directions.
Before that fix the rejecting half of `validation-machine-one-step` was
unprovable whenever the operator's claimed delta root was non-empty, and
`forced-transaction-verdict-mismatch` was provable only in the
operator-says-invalid direction — a direct contradiction of the specification's
"a fault in either direction". No profile *value* changed as a result of the
fix, so the digest above is unaffected; only the executable status of the
listed families did.

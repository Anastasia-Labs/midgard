# Canonical V1 format registry

- **Status:** Active implementation gate for
  `canonical-v1-consolidation.md`
- **Last reviewed:** 2026-07-24
- **Source:** current dirty worktree; implementation status is recorded in
  §14
- **Scope:** independently serialized, authenticated, hashed, persisted, or
  externally exchanged Midgard-owned formats

## 1. Classification rules

| Class              | Treatment                                                                                                                                                                                                                   |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `schema`           | Retain the newest semantics, rename the format to V1, encode/check exact version `1`, and remove every older implementation.                                                                                                |
| `nested-schema`    | The value is independently encoded or hashed inside another format. Give it one V1 encoding/domain, but do not add a redundant version field when the mandatory outer V1 envelope already authenticates the interpretation. |
| `semantic`         | Constructor, enum, phase, validity, purpose, or algorithm tag. Preserve its current meaning and numeric value.                                                                                                              |
| `sentinel`         | Preserve the intentional special value unless a separate protocol decision changes it.                                                                                                                                      |
| `external`         | Cardano, Plutus, dependency, operating-system, or tool version. Do not rename.                                                                                                                                              |
| `artifact`         | Local/E2E/benchmark document schema. Retain only its newest active shape, reset its schema identity to `-v1`, and regenerate it.                                                                                            |
| `post-launch seam` | Keep the mechanism at its V1 baseline; it is not compatibility with an abandoned pre-launch format.                                                                                                                         |

Rules:

- A source filename suffix does not determine the class. The encoded field and
  trust boundary do.
- A `V1` source symbol is not automatically retained: if it belongs only to
  launch-v1, it is deleted with the launch profile.
- Nested types share the outer envelope's deployment interpretation. They do
  not gain decorative version integers.
- Permanent tests prove the exact current V1 parser. Historical encoders and
  removal-only fixtures are temporary implementation evidence and are deleted.

## 2. Consensus, deployment, and public API

| ID  | Source owner and current identity                                                                                                                                          | Class    | Canonical V1 result                                         | Boundary, binding, and persistence                                         | Required evidence                                                        |
| --- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------- | ----------------------------------------------------------- | -------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| C01 | `midgard-core/src/consensus-profile.ts`: launch profile `midgard-launch-consensus-v1`                                                                                      | schema   | Delete                                                      | Compile-time/runtime profile selection                                     | No launch-profile export or runtime branch                               |
| C02 | `midgard-core/src/proof-consensus-profile.ts`: `midgard-proof-consensus-v3`                                                                                                | schema   | `MidgardConsensusProfileV1`; `midgard-consensus-v1`         | Compiled tuple; manifest; protocol-info; node/DA startup; pending journals | One canonical tuple/digest and exact startup equality                    |
| C03 | Proof protocol version `1`, native version `2`, transition schema `3`, validation machine `9`, descriptor `1`, dispute `2`, DA inner `4`, CEK `3`, submission `2`, API `8` | schema   | Every Midgard-owned tuple field becomes `1`                 | Canonical tuple digest and every consuming parser                          | Tuple golden vector and consumer equality tests                          |
| C04 | `deployment-manifest-v2.ts`: launch manifest v3 and proof manifest v9                                                                                                      | schema   | `DeploymentManifestV1`; `midgard-deployment-manifest-v1`    | Exact JSON parser; node/DA config; final deployment identity               | Exact-object positive test; missing/extra/tampered field rejection       |
| C05 | `commands/protocol-info.ts`: API 2 and proof API 8; Lucid fallback accepts 2/4                                                                                             | schema   | `ProtocolInfoV1`; API `1`; no fallback                      | HTTP `/protocol-info`; online SDK initialization                           | Node/SDK round trip; absent endpoint and non-1 rejection                 |
| C06 | Contract deployment marker and `manifestId` computation                                                                                                                    | schema   | `DeploymentMarkerV1` bound to canonical final manifest ID   | Postgres, MPF metadata, DA store, E2E run state                            | Canonical preimage golden; exact marker mismatch rejection               |
| C07 | Target-network Cardano parameter snapshot                                                                                                                                  | external | Preserve Cardano names/values; include snapshot in manifest | Trusted provider input and release evidence                                | Provider-derived snapshot and manifest binding                           |
| C08 | `GENESIS_PROTOCOL_VERSION = 0n` and Aiken `genesis_protocol_version = 0`                                                                                                   | sentinel | Preserve `0`                                                | Genesis state-queue datum and initialization checks                        | Genesis succeeds only with sentinel; ordinary header V1 remains distinct |
| C09 | Cardano `PlutusV1/V2/V3`, Aiken `plutus = "v3"`                                                                                                                            | external | Preserve                                                    | Cardano script language and compilation target                             | Existing language/hash tests                                             |
| C10 | `MidgardV1` script-language tag `128` and hash prefix `0x80`                                                                                                               | semantic | Preserve as the first Midgard script language               | Versioned-script CBOR and script hash                                      | Exact tag/hash vectors; unknown-language rejection                       |

## 3. Native transaction and witness formats

| ID  | Source owner and current identity                                                 | Class         | Canonical V1 result                                                                              | Boundary, binding, and persistence                       | Required evidence                                            |
| --- | --------------------------------------------------------------------------------- | ------------- | ------------------------------------------------------------------------------------------------ | -------------------------------------------------------- | ------------------------------------------------------------ |
| N01 | `codec/native.ts`: canonical/full native transaction, versions 1 and 2            | schema        | `MidgardNativeTxCanonicalV1` / `MidgardNativeTxFullV1`; version `1`; retain current V2 semantics | Submission API, admission payload, DA transaction member | Canonical CBOR round trip and exact version check            |
| N02 | Compact native transaction                                                        | nested-schema | `MidgardNativeTxCompactV1`                                                                       | Native tx ID and proof source                            | TS/Aiken compact bytes and tx-ID vector                      |
| N03 | Compact transaction body                                                          | nested-schema | `MidgardNativeTxBodyCompactV1`                                                                   | Native tx ID preimage                                    | Field-order golden vector                                    |
| N04 | Canonical transaction body                                                        | nested-schema | `MidgardNativeTxBodyCanonicalV1`                                                                 | Full transaction CBOR and field hashes                   | Canonical field-preimage/hash vectors                        |
| N05 | Compact witness set                                                               | nested-schema | `MidgardNativeTxWitnessSetCompactV1`                                                             | Compact transaction witness hash                         | Hash vector                                                  |
| N06 | Canonical witness preimages                                                       | nested-schema | `MidgardNativeTxWitnessSetCanonicalV1`                                                           | Full transaction CBOR                                    | Round trip and consistency proof                             |
| N07 | `MidgardNativeTxProofSourceV3`: compact CBOR, compact witness CBOR, field lengths | schema        | `MidgardNativeTxProofSourceV1`                                                                   | Forced transaction source, DA, proof commitments         | Canonical source/commitment vectors                          |
| N08 | Proof field-length nine-tuple V3                                                  | nested-schema | `MidgardNativeTxProofFieldLengthsV1`                                                             | Native proof source                                      | Exact length/count/canonical CBOR tests                      |
| N09 | `computeMidgardNativeTxFullHashV2`                                                | nested-schema | `computeMidgardNativeTxFullHashV1` with V1 domain                                                | Admission/full-source commitment                         | Hash vector and TS/Aiken agreement                           |
| N10 | `MidgardPartialWitnessBundle`, version `1`                                        | schema        | `MidgardPartialWitnessBundleV1`, version `1`                                                     | Lucid import/export and partial signing                  | Canonical bundle, tx/body binding, unknown-version rejection |
| N11 | Native transaction validity codes `0..5`                                          | semantic      | Preserve                                                                                         | Native tx compact/canonical validity field               | Existing code-to-meaning tests                               |
| N12 | `MIDGARD_POSIX_TIME_NONE = -1`, network-id-none `255`                             | sentinel      | Preserve                                                                                         | Native body validity/network fields                      | Existing sentinel tests                                      |
| N13 | Native Cardano script constructors and tags                                       | semantic      | Preserve                                                                                         | Native script CBOR and phase-A evaluation                | Cardano-compatible native-script vectors                     |
| N14 | Redeemer purpose/tag/index values                                                 | semantic      | Preserve                                                                                         | Witness preimage, script context, script proof           | Purpose/index binding tests                                  |

## 4. DA payload, envelope, transport, and evidence

| ID  | Source owner and current identity                                        | Class         | Canonical V1 result                                                                | Boundary, binding, and persistence                         | Required evidence                                          |
| --- | ------------------------------------------------------------------------ | ------------- | ---------------------------------------------------------------------------------- | ---------------------------------------------------------- | ---------------------------------------------------------- |
| D01 | SDK `DaPayloadV2`                                                        | schema        | Delete                                                                             | Historical raw payload path                                | No export/decoder/capability                               |
| D02 | SDK `DaPayloadV3`                                                        | schema        | Delete                                                                             | Historical envelope payload path                           | No export/decoder/capability                               |
| D03 | SDK `DaPayloadV4` and DA-side `VerifiedDaPayloadV4`                      | schema        | `DaPayloadV1` / `VerifiedDaPayloadV1`, version `1`                                 | Commit worker, DA committee, watcher, reconstruction       | Canonical CBOR, roots, counts, and size vectors            |
| D04 | `DaPayloadBodyV4`, retained `DaPayloadCountsV3`, and nested arrays/maps  | nested-schema | `DaPayloadBodyV1` / `DaPayloadCountsV1`                                            | Mandatory payload envelope                                 | Field order and root/count validation                      |
| D05 | `DaPayloadEnvelopeV3`, envelope `3`, inner `2/4`, raw inference          | schema        | `DaPayloadEnvelopeV1`, envelope `1`, inner `1`; mandatory `identity` or `zstd` tag | Stored bytes, libp2p submission/retrieval, DB payload rows | Identity/zstd round trips; raw/off/inferred input rejected |
| D06 | DA transport protocol version `1` and protocol IDs/topics                | schema        | Sole DA transport V1                                                               | libp2p negotiation                                         | Exact protocol/capability handshake                        |
| D07 | `DaPayloadAnnouncementV1`                                                | schema        | Retain V1                                                                          | Gossip frame                                               | Canonical CBOR/signature/context validation                |
| D08 | `DaPayloadSubmitRequestV1/ResponseV1`                                    | schema        | Retain V1                                                                          | Producer-to-committee stream                               | Request/response bounds and error codes                    |
| D09 | `DaCapabilitiesRequestV1/ResponseV1`                                     | schema        | Retain V1; advertise payload/envelope V1 only                                      | Capability negotiation                                     | No V2/V3/V4 payload advertisement                          |
| D10 | `DaPayloadByHeaderRequestV1/ResponseV1`                                  | schema        | Retain V1                                                                          | Retrieval stream                                           | Manifest/deployment/header binding                         |
| D11 | `DaPayloadChunkManifestV1`, chunk request/response V1                    | schema        | Retain V1                                                                          | Bounded retrieval chunks                                   | Chunk hash/length/order tests                              |
| D12 | Metadata-by-header V1                                                    | schema        | Retain V1                                                                          | Retrieval metadata                                         | Exact deployment/header binding                            |
| D13 | Proof-bundle-by-header V1                                                | schema        | Retain V1                                                                          | Auditor/fault-proof retrieval                              | Root/member/preimage binding tests                         |
| D14 | Trace-step-by-index V1                                                   | schema        | Retain V1                                                                          | Auditor/fault-proof retrieval                              | Index/root proof validation                                |
| D15 | Event-to-step-by-event V1                                                | schema        | Retain V1                                                                          | Auditor/fault-proof retrieval                              | Event/root proof validation                                |
| D16 | Attestation gossip/query V1 and `MidgardDAAttestationV1` domain          | schema        | Retain V1                                                                          | Committee signature and L1 attestation                     | Signature preimage and threshold tests                     |
| D17 | Conflict evidence V1                                                     | schema        | Retain V1                                                                          | Peer evidence and persistence                              | Conflicting signature/header tests                         |
| D18 | DA runtime manifest v2 plus v1 fixture readers                           | artifact      | `DaRuntimeManifestV1`; `midgard-da-libp2p-runtime-manifest-v1`                     | Producer/watcher startup                                   | Exact manifest parser and deployment identity              |
| D19 | DA persisted payload record with optional/missing schema defaulting to 2 | schema        | `DaStoredPayloadRecordV1`; explicit V1 required                                    | DA Postgres/file store                                     | Missing/non-1 rejected; no default                         |
| D20 | DA signature record `source: "legacy"`                                   | schema        | `DaSignatureRecordV1`; remove `legacy` source                                      | DA store                                                   | Only local/peer sources accepted                           |

## 5. Ledger, header, state queue, and user-event formats

| ID  | Source owner and current identity                                            | Class         | Canonical V1 result                                                          | Boundary, binding, and persistence                            | Required evidence                                |
| --- | ---------------------------------------------------------------------------- | ------------- | ---------------------------------------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------------ |
| L01 | SDK `HeaderV2`                                                               | schema        | `HeaderV1`                                                                   | State-queue datum, block hash, DA root checks                 | TS/Aiken datum and hash vector                   |
| L02 | `HeaderTransitionCommitmentsV2`                                              | nested-schema | `HeaderTransitionCommitmentsV1`                                              | Header datum                                                  | Complete commitment/count validation             |
| L03 | `StateQueueNodeV2`                                                           | schema        | `StateQueueNodeV1`                                                           | L1 state-queue datum                                          | Topology and datum round trip                    |
| L04 | `InitV2`, `MergeToConfirmedStateV2`, current state-queue redeemers           | schema        | `InitV1`, `MergeToConfirmedStateV1`                                          | State-queue validator redeemers                               | Initialization/merge emulator tests              |
| L05 | Launch transition schema 1                                                   | schema        | Delete with launch profile                                                   | Launch DA/trace path                                          | No runtime branch                                |
| L06 | Proof transition schema 3                                                    | schema        | `TransitionStepV1`, schema `1`                                               | DA trace member and on-chain membership                       | TS/Aiken encoding/hash vector                    |
| L07 | `TxOrderDatumV2`, event/payload/forced V2                                    | schema        | Delete                                                                       | Launch transaction-order path                                 | No validator/export/parser                       |
| L08 | `TxOrderDatumV3`, event/payload/source/forced V3                             | schema        | Corresponding `...V1` formats                                                | L1 tx-order UTxO, node ingestion, DA event                    | Datum/redeemer/forced-key vectors                |
| L09 | `TxFieldPreimageV3`, `TxOrderFieldFragmentV3`, and `TxOrderFragmentBundleV3` | schema        | `TxFieldPreimageV1`, `TxOrderFieldFragmentV1`, and `TxOrderFragmentBundleV1` | L1 publication datum, SDK publication plan, and proof opening | Field kind/hash/length and fragment-bundle tests |
| L10 | `TxFieldReceiptV3`, receipt publication V3, and mint/spend/burn redeemers V3 | schema        | Corresponding `...V1` formats                                                | L1 receipt assets, SDK publication plan, and validator spends | Mint/burn/consume authorization tests            |
| L11 | `CekProgramMaterialDatumV2`, publication V2, and publication config V2       | schema        | Corresponding `...V1` formats                                                | L1 material publication UTxO and SDK publication plan         | Datum/hash/material-kind tests                   |
| L12 | Forced transaction journal member V5                                         | schema        | `ForcedTransactionJournalMemberV1`, version `1`                              | Postgres pending/finalization journal                         | Canonical encode/decode; no alias/default        |
| L13 | Pending-block-finalization launch/proof metadata union                       | schema        | `PendingBlockFinalizationV1`                                                 | Postgres crash/recovery journal                               | Exact V1 metadata and replay tests               |
| L14 | Full finalized UTxO snapshot fallback                                        | schema        | Delete; retain canonical delta representation as V1                          | Finalization recovery                                         | Delta-chain recovery only                        |
| L15 | Foreign-tip reconciliation launch/proof evidence                             | schema        | `ForeignTipReconciliationV1`                                                 | Postgres and DA recovery                                      | Exact manifest/profile and evidence binding      |
| L16 | `ledger-output-v2.ak` output preimage                                        | nested-schema | `LedgerOutputV1`                                                             | Ledger MPF value and proof opening                            | TS/Aiken output decode/hash vector               |
| L17 | `mpf-proof-v2.ak` proof steps                                                | nested-schema | `MpfProofV1`                                                                 | Membership/non-membership/update/delete proofs                | Existing MPF differential/proof vectors          |
| L18 | Deposit, withdrawal, reserve, and operator event constructors                | semantic      | Preserve                                                                     | L1 event datums/redeemers and event roots                     | Existing event-flow tests                        |
| L19 | Scheduler shift/status/action constructors                                   | semantic      | Preserve                                                                     | Scheduler datum/redeemer                                      | Existing scheduler lifecycle tests               |

## 6. Script and script-context proof formats

| ID  | Source owner and current identity                                                  | Class             | Canonical V1 result                            | Boundary, binding, and persistence                            | Required evidence                           |
| --- | ---------------------------------------------------------------------------------- | ----------------- | ---------------------------------------------- | ------------------------------------------------------------- | ------------------------------------------- |
| S01 | `native-script-v2.ak`                                                              | nested-schema     | `native-script-v1.ak`; `NativeScriptProofV1`   | Phase-A native-script proof                                   | TS/Aiken valid/invalid script tests         |
| S02 | `script-context-v4.ak`                                                             | nested-schema     | `script-context-v1.ak`; `ScriptContextProofV1` | CEK context construction                                      | Cross-language context root/summary vectors |
| S03 | `script-language-views-v3.ak`                                                      | nested-schema     | `script-language-views-v1.ak`                  | Script-integrity hash                                         | Cardano language-view/hash vectors          |
| S04 | `script-proof-v3.ak` and TS proof-v3 helpers                                       | nested-schema     | `script-proof-v1.ak`; V1 helper names/domains  | Script/redeemer/signer/output/execution/context Merkle leaves | TS/Aiken leaf/root vectors                  |
| S05 | `MidgardVersionedScript` language tags Native `0`, Plutus V3 `3`, Midgard V1 `128` | semantic/external | Preserve tag meanings                          | Script witness preimage                                       | Exact tag/hash and unknown-tag rejection    |
| S06 | Script-source attached/reference variants                                          | semantic          | Preserve                                       | Resolution schedule and validation controls                   | Source-kind and reference-script tests      |
| S07 | Script purpose/redeemer constructors                                               | semantic          | Preserve                                       | Script context and proof leaves                               | Purpose binding tests                       |

## 7. CEK formats

| ID  | Source owner and current identity                                                     | Class         | Canonical V1 result                                                    | Boundary, binding, and persistence         | Required evidence                             |
| --- | ------------------------------------------------------------------------------------- | ------------- | ---------------------------------------------------------------------- | ------------------------------------------ | --------------------------------------------- |
| K01 | CEK program envelope version 3; Aiken file `cek-proof-v3.ak` with `ProgramEnvelopeV1` | schema        | `MidgardCekProgramEnvelopeV1`; envelope version `1`; `cek-proof-v1.ak` | Native script witness, DA, L1 proof        | TS/Aiken envelope inspect/decode/hash vector  |
| K02 | CEK machine state V1                                                                  | schema        | Retain `MidgardCekMachineStateV1`, version `1`                         | Validation trace leaf                      | TS/Aiken state hash vector                    |
| K03 | CEK term/value/BLS/sequence/environment/continuation node V1 families                 | nested-schema | Retain V1 names; move implementation modules to V1                     | Program/material Merkle DAG                | Per-node TS/Aiken hash vectors                |
| K04 | CEK blob chunk/branch/commitment V1                                                   | nested-schema | Retain V1                                                              | Bounded program/data blobs                 | Chunk/branch/root vectors                     |
| K05 | CEK Data node/list/pair/summary V1 in `cek-data-v3.ak`                                | nested-schema | Retain V1; module becomes `cek-data-v1.ak`                             | Semantic Data commitment                   | TS/Aiken Data summary/hash vectors            |
| K06 | CEK data-scan frame/control V1                                                        | nested-schema | Retain V1                                                              | One-step validation work witness           | Control/frame encoding and step tests         |
| K07 | CEK constant type/witness V1                                                          | nested-schema | Retain V1                                                              | Program constants and direct witnesses     | Type/payload/root/memory tests                |
| K08 | CEK builtin witness/budget V1                                                         | nested-schema | Retain V1                                                              | One-step builtin execution                 | Success/failure/budget tests                  |
| K09 | CEK program-material entry/value version 3                                            | schema        | `MidgardCekProgramMaterialValueV1`, version `1`                        | DA material set and L1 publication         | Canonical entry/value/material verification   |
| K10 | CEK program-material sidecar version 3                                                | schema        | `MidgardCekProgramMaterialSidecarV1`, version `1`                      | Admission payload and pending finalization | Merge/deduplicate/hash tests                  |
| K11 | Proof submission version 2                                                            | schema        | `MidgardProofSubmissionV1`, version `1`                                | Fault-proof CLI/L1 proof submission        | Canonical decode and exact version            |
| K12 | CEK machine modes `0..8`, errors `0..7`, builtin tags, constant tags                  | semantic      | Preserve                                                               | CEK interpreter/one-step proof             | Existing exhaustive semantic transition tests |
| K13 | UPLC program language tuple `[1,1,0]`                                                 | external      | Preserve                                                               | Flat/UPLC program semantics                | Existing program decode/evaluation tests      |

## 8. Validation trace, claim, dispute, and one-step formats

| ID  | Source owner and current identity                                    | Class         | Canonical V1 result                                                     | Boundary, binding, and persistence       | Required evidence                          |
| --- | -------------------------------------------------------------------- | ------------- | ----------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------ |
| V01 | Validation machine state version 9; TS/Aiken type currently named V1 | schema        | `ValidationMachineStateV1`, machine version `1`                         | Trace leaf and dispute state             | TS/Aiken encode/hash/well-formed vectors   |
| V02 | Validation trace descriptor schema 1 plus machine 9                  | schema        | `ValidationTraceDescriptorV1`, both exact `1`                           | DA trace descriptor and dispute datum    | Descriptor/root/count vectors              |
| V03 | Validation trace proof V2                                            | nested-schema | `ValidationTraceProofV1`                                                | Merkle trace membership                  | Valid/invalid membership proof tests       |
| V04 | Validation Merkle frontier/membership in `validation-merkle-v3`      | nested-schema | V1 module/types                                                         | Trace commitment and proof               | TS/Aiken frontier/membership vectors       |
| V05 | Validation claim witness version 1 in `validation-claim-v2.ak`       | schema        | `ValidationClaimWitnessV1`, version `1`; module V1                      | L1 claim/dispute opening                 | Claim/header/source membership tests       |
| V06 | Validation dispute version 2 and validator `dispute-v3.ak`           | schema        | `ValidationDisputeV1`, version `1`; validator/module V1                 | L1 datum/redeemer and timeout            | Open/bisect/one-step/timeout tests         |
| V07 | Dispute turn/action/winner/redeemer constructors                     | semantic      | Preserve                                                                | L1 dispute state machine                 | Constructor/authorization tests            |
| V08 | Validation phase/verdict/source-kind constructors                    | semantic      | Preserve                                                                | Trace state and validation engine        | Existing phase/verdict transition tests    |
| V09 | Validation one-step witness V1 and evidence V2                       | nested-schema | `ValidationOneStepWitnessV1`, `ValidationOneStepEvidenceV1`             | Dispute one-step resolution              | Cross-language one-step fixtures           |
| V10 | Validation auxiliary witness V2                                      | nested-schema | `ValidationAuxiliaryWitnessV1`                                          | One-step work witness                    | Canonical encode/hash tests                |
| V11 | Resolve-inputs control V2                                            | nested-schema | `ResolveInputsControlV1`                                                | Validation work witness                  | Input resolution step tests                |
| V12 | Script-sources control V3 and discovery control V1                   | nested-schema | `ScriptSourcesControlV1`, `ScriptDiscoveryControlV1`                    | Validation work witness                  | Attached/reference/discovery tests         |
| V13 | Native-scripts control/witness V3                                    | nested-schema | Corresponding V1 formats                                                | Validation work witness                  | Native-script fold tests                   |
| V14 | CEK context/redeemer/final/parts/tx-info controls V1                 | nested-schema | Retain V1                                                               | Validation work witness                  | Context assembly step tests                |
| V15 | Value accumulator/update/asset mutation/value-and-mint control V3    | nested-schema | Corresponding V1 formats                                                | Validation work witness and ledger delta | Value conservation and mint/burn tests     |
| V16 | Ledger-delta control/witness/operation V3; TS operation V2           | nested-schema | Corresponding V1 formats                                                | Accepted transition work witness         | Operation leaf/frontier/delta tests        |
| V17 | Terminal acceptance/rejection witness V3/current                     | nested-schema | Corresponding V1 formats                                                | Final validation step                    | Valid/misclassified outcome tests          |
| V18 | Validation source-membership record/enum V3                          | nested-schema | `ValidationSourceMembershipV1`; preserve forced/normal constructor tags | Claim witness                            | Membership-kind and membership-proof tests |

## 9. Persistence, MPF, and worker protocols

| ID  | Source owner and current identity                                          | Class                | Canonical V1 result                                                                                  | Boundary, binding, and persistence  | Required evidence                          |
| --- | -------------------------------------------------------------------------- | -------------------- | ---------------------------------------------------------------------------------------------------- | ----------------------------------- | ------------------------------------------ |
| P01 | Main Postgres `0001_initial_schema` plus migration ledger                  | post-launch seam     | Fresh V1 baseline remains migration `0001`                                                           | Node startup/database               | Checksum and exact baseline tests          |
| P02 | DA Postgres startup column rename and old-column detection                 | schema               | Delete; fresh V1 schema only                                                                         | DA startup                          | Fresh schema test; no runtime ALTER/rename |
| P03 | Forced transaction alias column/encoder                                    | schema               | Delete                                                                                               | Node database API                   | Canonical column/encoder only              |
| P04 | `ParkedMpfOverlayV1`                                                       | schema               | Retain V1 while engine remains authorized                                                            | Pending/speculative worker transfer | Park/resume/promote/crash tests            |
| P05 | Event-flat parked/resumed overlay V2                                       | schema               | Corresponding V1 format                                                                              | Worker transfer                     | Park/resume/promote tests                  |
| P06 | Architecture G owner/RPC/generation/full-index/replay domains V1           | schema               | Retain V1; engine selection remains out of scope                                                     | Native owner process/LevelDB        | Existing protocol and recovery tests       |
| P07 | Worker `WirePhaseACandidate` structured-clone shape                        | nested-schema        | `WirePhaseACandidateV1` if independently persisted/exchanged; otherwise registry marks process-local | Validation worker boundary          | Exhaustiveness and round-trip worker tests |
| P08 | MPF engine identifiers `legacy`, `overlay`, `event_flat`, `architecture_g` | semantic/operational | Preserve until existing engine gate authorizes consolidation                                         | Runtime configuration               | Existing differential/config tests         |

## 10. Active operational artifact schemas

Every row is `artifact`: keep the newest active shape, rename its schema string
to `-v1`, delete earlier readers, and regenerate evidence. Already-V1 rows keep
their existing identity unless their shape changes as part of this plan.

| ID  | Current artifact family                                                  | Canonical result and owner                                                     |
| --- | ------------------------------------------------------------------------ | ------------------------------------------------------------------------------ |
| A01 | DA libp2p runtime manifest v2                                            | `midgard-da-libp2p-runtime-manifest-v1`; `midgard-core/da-transport`           |
| A02 | Contract/deployment manifest v2/v3/v9                                    | `midgard-deployment-manifest-v1`; node deployment manifest                     |
| A03 | Deployment run state v1                                                  | Retain `midgard-deployment-run-state-v1`; E2E run-state parser                 |
| A04 | E2E step v1                                                              | Retain `midgard-e2e-step-v1`                                                   |
| A05 | E2E summary v2                                                           | `midgard-e2e-summary-v1`                                                       |
| A06 | E2E DA gate v2                                                           | `midgard-e2e-da-gate-v1`                                                       |
| A07 | E2E L2 stress v3                                                         | `midgard-e2e-l2-stress-v1`                                                     |
| A08 | E2E managed service/service supervisor/process ownership v1              | Retain their distinct `-v1` identities                                         |
| A09 | E2E reconciliation v1                                                    | Retain `midgard-e2e-reconciliation-v1`                                         |
| A10 | Phase-4 local reset attestation v3                                       | `midgard-phase4-local-devnet-reset-attestation-v1`                             |
| A11 | Phase-4 matched snapshot identity v2                                     | `midgard-phase4-matched-snapshot-identity-v1`                                  |
| A12 | Phase-4 environment, genesis, PHAS, T1, pipelined-acceptance families v1 | Retain their distinct `-v1` identities                                         |
| A13 | Stress wallet consolidate v1/v2 and upgrade/verification artifacts       | Retain only current consolidate V1; delete upgrade/legacy verification formats |
| A14 | Stress wallet, prepare, fanout, terminal-drain, readiness v1             | Retain distinct `-v1` identities                                               |
| A15 | Stress corpus manifest/generation/verification/prefix evidence v1        | Retain distinct `-v1` identities                                               |
| A16 | Historical stress-corpus extension and historical binding formats        | Delete active readers/tooling; historical documents may describe them          |
| A17 | Phase-1 live corpus binding v2                                           | `midgard-phase1-live-corpus-binding-v1` if still an active benchmark input     |
| A18 | Phase-3 load-generator isolation v3                                      | `midgard-phase3-load-generator-isolation-v1`                                   |
| A19 | Phase-3 clean live E2E v2                                                | `midgard-phase3-architecture-g-clean-live-e2e-v1`                              |
| A20 | Phase-3 live soak v4                                                     | `midgard-phase3-architecture-g-live-soak-v1`                                   |
| A21 | Architecture G candidate/runtime/gate/corpus/root/probe artifacts v1     | Retain their distinct `-v1` identities                                         |
| A22 | Phase-5 DA measurement/distribution/fixture/strict-chain artifacts v1    | Invalidate/remove; regenerate only after §2.5 follow-up can produce complete V1 bytes within bounded publication |
| A23 | Secret-scan and throughput-watchdog artifacts v1                         | Retain their distinct `-v1` identities                                         |

## 11. Explicit exclusions

The following are classified and MUST NOT be changed by the V1 schema reset:

- Cardano protocol version and protocol-parameter field names;
- Plutus language versions and Cardano script-envelope labels;
- Aiken/compiler, package, Node, pnpm, PostgreSQL, libp2p, cgroup, Docker, and
  dependency versions;
- native-script constructors, validation phases/verdicts, CEK modes/errors,
  dispute actions/turns/winners, redeemer purposes, event constructors, and
  validity codes;
- genesis protocol sentinel `0`, unbounded-time sentinel `-1`, and absent
  network-id sentinel `255`;
- database migration version `0001` and migration checksum enforcement;
- monotonic journal/cache/revision counters such as mempool-ledger-delta
  `version`, which identify sequence position rather than a schema;
- semantic algorithm identities such as
  `earliest_commit_scheduler_v1` and active MPF replay-corpus V1;
- MPF engine names and operational recovery modes retained by their separate
  approved plans.

## 12. Candidate-name disposition

The WB0 source scan groups every current exported `V2+` candidate as follows.
This table is a navigation aid for the rows above; it does not create aliases
or broaden a row.

| Candidate family found by the source scan                                                   | Registry disposition                                                                 |
| ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| `DaPayload*V2/V3/V4`, verified payloads, encoders, decoders, sizing, roots, and envelope V3 | D01-D05                                                                              |
| `DeploymentManifestV2`, proof-manifest V2, builders, parsers, and readers                   | C04                                                                                  |
| Header/commitment/state-queue V2 types and helpers                                          | L01-L04                                                                              |
| Transaction-order V2/V3 datums, UTxOs, configs, builders, and fetch helpers                 | L07-L08                                                                              |
| Native proof source/length/commitment V3 and full-hash V2 helpers                           | N07-N09                                                                              |
| Field fragment/receipt/publication V3 and CEK-material publication V2 helpers               | L09-L11                                                                              |
| Forced-value V3, journal-member V5, and deprecated forced-value V2 alias                    | L12 and P03                                                                          |
| Validation machine/trace/claim/dispute/source-membership V2/V3 types and helpers            | V01-V18                                                                              |
| Event-flat parked/resumed overlay V2                                                        | P05                                                                                  |
| `collect/decode/hashMidgardProofV3*` script helpers                                         | S04                                                                                  |
| Plutus V3 script/context/cost-model names                                                   | External; §11                                                                        |
| `WirePhaseACandidate`                                                                       | P07; independently exchanged structured-clone record, target `WirePhaseACandidateV1` |

The same scan maps every Aiken `-v2`, `-v3`, and `-v4` module to L06-L11,
L16-L17, S01-S04, K01-K08, or V01-V18. The obsolete transaction-order V2
library and validator are deleted by L07 rather than renamed.

## 13. Registry completion checks

Before WB1 begins:

1. Every exported TypeScript identifier ending in `V2` or above is mapped to a
   row above or classified as external.
2. Every Aiken source/module filename ending in `-v2`, `-v3`, or `-v4` is
   mapped to a row above.
3. Every active schema string ending in `-v2` or above is mapped in §10.
4. Every numeric `version`, `schema_version`, `machine_version`, protocol
   version, and named version constant in active source is classified.
5. Review confirms that nested types do not receive redundant version fields.
6. The final implementation may refine a row, but no unregistered format may
   be renamed or deleted.

## 14. Implementation disposition

The canonical source, Aiken modules, generated blueprint, manifests, database
baseline, public APIs, and active operational artifact schemas have landed as
their sole V1 forms. Final evidence commands are recorded in the consolidation
plan.

A22 could not be regenerated honestly under the mechanically retained V1
limits. A one-off converter used the retired fixture only as temporary input,
decoded and canonically re-encoded every one of its 50,000 native
transactions, recomputed V1 transaction IDs and proof-source records, and
constructed the complete newest V1 payload shape. Before writing any file it
measured 71,049,618 inner bytes, above the 67,108,864-byte DA limit, and
failed. Peak RSS was 1,540,196 KiB, with no swaps and no file output. The
converter was deleted; the obsolete envelope, measurement, active runner,
runner-only fixtures, Docker target, and package commands were invalidated.
Historical documents retain a prominent non-runnable banner. A replacement
50k gate belongs to the mandatory capability/chunking follow-up in §2.5.

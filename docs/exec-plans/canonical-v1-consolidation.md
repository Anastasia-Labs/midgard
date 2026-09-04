# Canonical V1 protocol and schema consolidation

- **Status:** Active; approved for implementation on 2026-07-24
- **Last reviewed:** 2026-07-24
- **Implementation boundary:** Midgard-owned consensus profiles, wire formats,
  persisted formats, public APIs, generated artifacts, validators, SDKs, node,
  DA committee, fault-proof tooling, active documentation, and fixtures
- **Explicit non-goals:** changing Cardano `PlutusV1`/`PlutusV2`/`PlutusV3`
  language identities; weakening validation; adding migration or compatibility
  modes; selecting a new MPF engine before its existing acceptance gates pass;
  completing the Cardano capability-floor and remaining fault-proof work in the
  same change;
  deleting whole legacy packages such as `onchain/plutarch`,
  `demo/midgard-manager`, or `offchain`
- **Dependencies:** approval of this plan and a fresh validator deployment and
  development-state reset for acceptance
- **Mandatory follow-up before activation:** implement the accepted Cardano
  capability-floor decision and close #459, D-S5, D-S9, and every remaining
  proof-coverage gap

## 1. Objective

Replace Midgard's accumulated pre-launch generations with one coherent
protocol epoch named **V1**.

For every retained Midgard-owned versioned format:

1. Keep only the newest intended semantics.
2. Rename its public schema/type to `...V1`.
3. Encode the exact version or schema discriminator as `1`.
4. Delete every older decoder, encoder, union, alias, fallback, migration shim,
   capability advertisement, fixture, and branch.
5. Reject missing or unsupported schema-version discriminators at the first
   hostile boundary.

For example, `DaPayloadV2`, `DaPayloadV3`, and `DaPayloadV4` are replaced by
one `DaPayloadV1`. `DaPayloadV1` has the current V4 semantics and the wire
discriminator `1`; it is not the historical payload that may previously have
used the number `1`.

This is a coordinated pre-launch reset, not a migration. It changes
serialized bytes, commitments, hashes, validator hashes, manifests, fixtures,
and persisted records. Abandoned decoders and old persisted state are deleted;
the implementation does not carry code that tries to recognize development
history.

There is no requirement to distinguish an abandoned historical byte string
from a newly valid V1 byte string when they are structurally identical. With no
shipped deployment or retained state, bytes accepted by the sole V1 decoder
have V1 semantics. Removal may use temporary historical fixtures to verify
that old APIs and explicit V2+ formats are gone, but those fixtures and
removal-only tests MUST be deleted before finalization.

## 2. Naming and versioning policy

### 2.1 Canonical naming

- Public Midgard-owned serialized types MUST use a `V1` suffix:
  `DaPayloadV1`, `DeploymentManifestV1`, `MidgardNativeTxV1`,
  `ValidationMachineStateV1`, and so on.
- A family MUST NOT expose unsuffixed compatibility aliases or multiple
  numbered exports.
- Internal helpers that do not define a format MAY use semantic unsuffixed
  names, such as `decodeDaPayload` internally, but the public format it returns
  remains `DaPayloadV1`.
- Constants MUST use the form `<FAMILY>_V1_VERSION = 1` or
  `<FAMILY>_V1_SCHEMA_VERSION = 1`. There MUST NOT be a generic “current
  version” constant alongside it.
- String schema identities MUST end in `-v1`.
- A missing discriminator MUST fail. Code MUST NOT use `?? 1`, infer V1 from a
  record shape, or treat raw bytes as a historical implicit format.

### 2.2 Version taxonomy

Only fields that identify a Midgard-owned schema or protocol epoch are reset:

- wire, persistence, manifest, API, proof, and artifact schema versions become
  `1`;
- semantic sum-type constructor tags, enum values, state-machine phases,
  validity codes, and algorithm choices retain their meanings and numeric
  values;
- the intentional genesis protocol sentinel remains `0` unless a separate
  protocol decision redesigns genesis as a different authenticated variant;
- Cardano protocol parameters, Plutus language versions, dependency versions,
  Node/cgroup versions, and similar external identities are not renamed.

The format registry in §3 classifies every candidate before code changes. A
numeric value MUST NOT be changed merely because it looks like a version.

### 2.3 What V1 means before and after launch

- Before the first production deployment, an approved semantic change replaces
  V1 in place and development state is destroyed and recreated.
- The deployment manifest binds the complete V1 tuple and generated validator
  hashes. A binary whose compiled tuple differs MUST refuse startup.
- After V1 has actually shipped, a consensus- or format-changing upgrade is V2
  and requires a separate upgrade plan. V1 MUST never silently acquire a V2
  interpretation after launch.
- Source package versions, dependency versions, Cardano protocol versions, and
  Cardano Plutus language generations are outside this naming reset.

### 2.4 Capability and safety invariants

The consolidation MUST preserve the newest proof-oriented behavior, not the
restrictive launch profile:

- mint and burn remain supported;
- scripts, redeemers, reference inputs, reference scripts, script credentials,
  protected outputs, and observers remain supported;
- valid effectful forced transactions remain supported;
- invalid and no-op forced transactions remain supported with their exact
  specified outcomes;
- the consolidation MUST NOT introduce any new transaction, field, Value,
  cardinality, script, or execution limit;
- unsupported, malformed, stale, or unproven behavior fails closed.

This plan deliberately does not claim that the selected behavior already meets
the accepted Cardano L1 capability floor or has complete L1 proofs. The sole V1
profile retains a compiled release-evidence gate. The mandatory follow-up in
§2.5 completes that work before activation; the system MUST NOT fall back to
launch-v1 or disable individual Cardano capabilities in order to activate
early.

### 2.5 Deferred capability-floor and proof-completion work

Immediately after consolidation, a separate approved plan updates the
pre-launch V1 implementation in place to:

- implement the accepted Cardano L1 capability floor;
- resolve #459 and reverse D-S5 and D-S9 by completing their proof coverage;
- support oversized independently authenticated fields through bounded
  chunking, incremental folds, or multi-transaction proof continuations;
- cover outputs, Values, reference scripts, scripts, redeemers, inputs,
  witnesses, asset sets, mint/burn, and native-script complexity without
  arbitrary Midgard-only restrictions;
- prove valid normal and forced transaction execution, invalid/no-op forced
  outcomes, and normal/forced transaction misclassification; and
- regenerate the validator-hash-bound release evidence.

Until that plan passes, `MIDGARD_V1_RELEASE_EVIDENCE_DIGEST` remains unset and
the canonical profile is not release-activatable. Consolidation may still be
implemented and tested locally because it removes obsolete shapes without
claiming release readiness.

## 3. Canonical selection map

The implementation MUST use this map. “Source semantics” identifies the
current implementation to retain; the source number itself does not survive.

| Family                  | Source semantics to retain                           | Canonical result                                                      |
| ----------------------- | ---------------------------------------------------- | --------------------------------------------------------------------- |
| Consensus profile       | current proof-v3 profile semantics                   | `MidgardConsensusProfileV1`, `midgard-consensus-v1`                   |
| Protocol version        | current proof protocol                               | `MIDGARD_PROTOCOL_V1_VERSION = 1`                                     |
| Native transaction      | native transaction V2/full proof source V3           | `MidgardNativeTxV1`, `NativeTxProofSourceV1`, wire version `1`        |
| Transition step         | proof transition schema 3                            | `TransitionStepV1`, schema version `1`                                |
| DA inner payload        | DA payload V4                                        | `DaPayloadV1`, payload version `1`                                    |
| DA stored envelope      | DA envelope V3                                       | `DaPayloadEnvelopeV1`, envelope version `1`                           |
| DA transport            | current libp2p transport V1                          | retain as the sole `...V1` protocol                                   |
| DA runtime manifest     | runtime manifest V2                                  | `DaRuntimeManifestV1`, schema `midgard-da-libp2p-runtime-manifest-v1` |
| Deployment manifest     | proof deployment manifest v9                         | `DeploymentManifestV1`, schema `midgard-deployment-manifest-v1`       |
| Protocol-info API       | proof API v8                                         | `ProtocolInfoV1`, API version `1`                                     |
| Submission envelope     | proof submission envelope V2                         | `ProofSubmissionEnvelopeV1`, version `1`                              |
| Transaction order       | transaction-order/forced-inclusion V3                | `TxOrderDatumV1`, `TxOrderEventV1`, `ForcedInclusionTxV1`             |
| Field publications      | field preimage/receipt V3                            | retired by #587; §8 carriage replaces it (see note below)             |
| Forced journal member   | forced journal member V5                             | `ForcedTransactionJournalMemberV1`, exact version `1`                 |
| CEK program envelope    | CEK envelope V3                                      | `MidgardCekProgramEnvelopeV1`                                         |
| CEK Value/material      | CEK Value/material/sidecar V3                        | corresponding sole `...V1` formats                                    |
| CEK machine state       | current CEK machine state                            | `MidgardCekMachineStateV1`, version `1`                               |
| Validation machine      | machine version 9 and current one-step semantics     | `ValidationMachineV1`, machine version `1`                            |
| Validation trace        | current trace/descriptor semantics                   | `ValidationTraceV1`, descriptor version `1`                           |
| Validation dispute      | dispute V2/current validator V3                      | `ValidationDisputeV1`, version `1`                                    |
| Script proof formats    | current native-script V2, context V4, views/proof V3 | corresponding sole `...V1` formats                                    |
| Ledger output/proof     | ledger-output V2 and MPF-proof V2                    | `LedgerOutputV1`, `MpfProofV1`                                        |
| Header and commitments  | `HeaderV2` and transition commitments V2             | `HeaderV1`, `HeaderTransitionCommitmentsV1`                           |
| State queue             | current `StateQueueNodeV2`, `InitV2`, merge V2       | corresponding sole `...V1` formats                                    |
| Parked MPF state        | parked overlay V1 and event-flat overlay V2          | one V1 name per still-distinct engine-owned format                    |
| E2E/benchmark artifacts | newest schema for each active artifact family        | reset each retained family to its sole `-v1` schema                   |
| CDDL                    | newest canonical native transaction shape            | `midgard_tx_*_v1`, version `1`                                        |

The map does not authorize combining semantically different records merely
because both become V1. Each family retains its own type, domain separator, and
boundary validation.

**Field publications, retired (#587).** The `TxFieldPreimageV1` /
`TxFieldReceiptV1` pair the row above once named is gone: it expressed the
counted per-item publication receipt chain, and `docs/spec/midgard-tx.md` §4's
flat field commitment made the receipt mint's own gate
(`verify_midgard_transaction_field_chunk_v1`) unsatisfiable for any payload whose
commitments are the §4 flat hashes of real material, so no receipt could be
minted for a field carrying an item under the format §4 actually defines. The
gate is not unsatisfiable outright — it checks the opening against the
commitment the payload's own compact *declares*, so a payload declaring counted
roots could satisfy it — which is why the replacement below closes the gap by
construction rather than relying on that arithmetic. #587 deleted both validators, both
libraries, and both SDK twins. The role the family was supposed to serve — L1
availability of a forced order's material — is now §8 carriage: a §8.6
prover-chosen `FieldCarriageV1` per non-empty field, supplied in the tx-order
mint redeemer and read through the §8.8 field-access door. #594's owner ruling
wired it: `verify_order_material` now authenticates every field whose committed
hash is not `empty_field_commitment` against that carriage, at all three tiers.
The certificate validator's *deployment* role remains outside the frozen
blueprint and lands with #579's single regeneration event, which is the only
part of the tier-3 path still pending. #589 closed as superseded by #594.

### 3.1 Exhaustive format registry gate

The family map above states direction but is not the implementation inventory.
Before changing source, WB0 produces
`docs/exec-plans/canonical-v1-format-registry.md` with one row for every
independently serialized or authenticated Midgard-owned format. Each row
records:

- source symbol and module;
- retained semantics and target V1 symbol;
- whether the numeric field is a schema version, semantic constructor,
  sentinel, algorithm version, or external version;
- mandatory outer envelope, if any;
- exact discriminator and canonical encoding;
- trust boundary and parser owner;
- hash/domain-separation owner and whether renumbering changes it;
- deployment-manifest binding;
- persistence location;
- positive V1 test and generic unsupported-version rejection test.

The registry explicitly includes validation claims, auxiliary and one-step
records, membership records, validation ledger deltas and controls, CEK
material-publication datums, native proof field-length records, partial witness
bundles, proof bundles, scheduler evidence identities, database deployment
markers, and the genesis sentinel. Nested implementation types that are never
independently encoded are marked as such and do not gain redundant version
fields.

No renaming work begins until the registry has no unclassified candidate.

## 4. Compatibility surface to delete

### 4.1 Protocol and codec branches

- Delete the launch consensus profile, proof/launch unions, launch feature
  matrix, and every runtime branch selecting launch versus proof behavior.
- Delete DA payload V2/V3 types, readers, writers, root layouts, stored-record
  fallbacks, and peer capability advertisements.
- Delete native transaction V1 historical semantics and V2/V3 source names.
- Delete transaction-order V2 and all dual V2/V3 routing.
- Delete old CEK envelope inspection and old validation-machine interpretation.
- Delete old header, state-queue, transition, script-proof, and dispute schema
  branches.
- Delete canonical-decoder fallbacks to permissive generic Plutus-data decoding.
- Delete any `Any*Version`, `Legacy*`, or old/new format union whose only
  purpose is pre-launch compatibility.

### 4.2 API, configuration, and CLI compatibility

- Delete the Lucid `/protocol-info` fallback. Online providers require the
  exact V1 endpoint; explicitly offline builders use a separate non-submitting
  context.
- Delete DA manifest camelCase/snake_case aliases and the legacy root `da`
  section. One canonical manifest spelling is required.
- Delete deprecated environment names after updating every active deployment
  manifest and runbook.
- Delete command aliases such as `submit-tx` and E2E old-step-ID mappings.
- Delete fault-proof `allowIncompatibleOutput`, legacy category aliases, and
  old submit ABI builders.

### 4.3 Persistence and generated artifacts

- Delete runtime `ALTER TABLE` repair, legacy column renames, alias columns,
  deprecated encoder aliases, nullable-old-record handling, and missing-version
  defaults.
- Delete the stress-wallet journal V1-to-V2 verifier/upgrader and historical
  stress-corpus readers.
- Delete full-snapshot pending-finalization fallback; V1 requires the canonical
  ledger delta/journal representation.
- Squash each database into a fresh V1 baseline. Keep the migration ledger and
  checksum mechanism as the post-launch seam, starting at migration `0001`.
- Invalidate and regenerate DA stores, Postgres databases, LevelDB MPF state,
  pending journals, manifests, E2E run state, benchmark corpora, fixtures,
  reference-script outputs, and deployment evidence.
- Abandoned development artifacts are destroyed rather than upgraded,
  relabeled, or carried into V1. Current startup checks remain responsible for
  matching the exact active deployment manifest.

## 5. Implementation work breakdown

The cutover is one atomic integration change. Intermediate commits MAY be used
for review, but no mixed-version commit is deployable.

### WB0 — Complete the format registry and temporary removal audit

1. Record every public version constant, exported numbered type, schema string,
   file/module suffix, decoder branch, database fallback, CLI alias, and
   capability advertisement in the §3.1 registry.
2. Classify schema versions separately from constructors, sentinels,
   algorithms, and external versions.
3. During implementation only, add a temporary stale-surface checker and
   frozen historical fixtures for explicit retired formats.
4. Use them to prove that old exports, decoders, aliases, manifest shapes,
   capability advertisements, and persistence fallbacks have been removed.
5. Delete the checker, frozen encoders, historical fixtures, and removal-only
   tests before finalization. Retain only ordinary V1 positive tests and
   generic exact-version rejection tests that protect current boundaries.

### WB1 — Define the sole consensus tuple

1. Replace launch and proof profile modules with one
   `consensus-profile.ts`.
2. Define the entire tuple in one frozen `MidgardConsensusProfileV1` object:
   protocol, native transaction, transition, DA, CEK, validation, dispute,
   manifest, API, size bounds, enabled features, and proof families.
3. Set every Midgard-owned tuple version to `1`.
4. Preserve the release-evidence digest gate and make startup fail if it is
   absent, malformed, or does not bind the generated V1 validators.
5. Remove all profile selection flags and launch/proof conditional types.
6. Add a canonical serialization and digest for the complete V1 tuple so node,
   DA committee, SDK diagnostics, manifests, and evidence all compare the same
   bytes.
7. Preserve current limits mechanically. Record every known deviation from the
   accepted Cardano capability floor as an input to the mandatory §2.5
   follow-up; do not claim the consolidation resolves it.

### WB2 — Reset Aiken schemas and validators to V1

1. Rename current retained Aiken modules and validators from `*-v2`, `*-v3`,
   and `*-v4` to `*-v1`.
2. Delete superseded transaction-order V2 modules and validators.
3. Reset only the schema/epoch fields classified by the registry. Do not
   renumber datum/redeemer constructors, semantic enum tags, state-machine
   phases, algorithm choices, or the genesis `0` sentinel.
4. Update imports, blueprint titles, generated validator names, tests, and
   cross-language fixture generators.
5. Keep Plutus language V3 unchanged in `aiken.toml`, script envelopes, and
   Cardano APIs.
6. Rebuild `plutus.json` and treat every validator hash change as expected
   deployment-breaking evidence requiring fresh reference scripts and a new
   manifest.

### WB3 — Consolidate core codecs and domain separation

1. Implement exactly one encoder and one strict decoder for each V1 format.
2. Remove structural inference and permissive decode fallback.
3. Reset encoded discriminators to `1`.
4. Rename Midgard-owned cryptographic domain separators to V1 where they encode
   the retired schema generation. Do not collapse separators belonging to
   different data families.
5. Regenerate canonical CBOR golden vectors and all expected hashes.
6. Permanently test missing and representative unsupported schema versions at
   each parser class. Use exhaustive abandoned-format vectors only during the
   temporary removal audit in WB0.

### WB4 — Consolidate SDK, Lucid, validation, and fault-proof APIs

1. Replace all public numbered unions with their single V1 type.
2. Update builders to emit only V1 and remove profile-dependent construction.
3. Make online Lucid initialization require and validate `ProtocolInfoV1`.
4. Preserve an explicitly offline, non-submitting builder path without a
   protocol-info compatibility fallback.
5. Update local validation, CEK execution, script contexts, dispute evidence,
   and proof preparation to consume the same V1 types.
6. Remove fault-proof compatibility reports and incompatible-output modes.
7. Update package exports so importing any removed V2+ symbol is a compile
   error; do not provide deprecated aliases.

### WB5 — Consolidate node admission, execution, DA, and finalization

1. Remove launch/proof branches from admission, MPF transition construction,
   forced-transaction handling, block building, submission, state queue, and
   reconciliation.
2. Store, transmit, and accept payloads only through
   `DaPayloadEnvelopeV1`. The envelope carries an explicit V1 encoding choice
   such as `identity` or `zstd`; raw/off storage and structural inference are
   removed.
3. Advertise only the V1 DA capability and exact active deployment identity.
   A peer that does not present them cannot participate in quorum.
4. Require explicit V1 on every DA record and journal member; remove `??`
   version defaults.
5. Keep effectful forced transactions, mint/burn, scripts, redeemers, reference
   inputs/scripts, script credentials, protected outputs, and observers on the
   canonical path.
6. Remove full-snapshot finalization recovery and require the V1 delta chain.
7. Preserve crash consistency, idempotency, L1 chain-point binding, DA
   threshold verification, and sticky halt behavior.

### WB6 — Reset manifests, persistence, configuration, and operational artifacts

1. Produce one exactly parsed `DeploymentManifestV1`. Missing and unexpected
   fields fail. Its canonical manifest-ID preimage contains:
   - the complete V1 tuple and tuple digest;
   - network and target-network Cardano parameter snapshot;
   - genesis and hub-oracle one-shot identity;
   - every validator and policy hash;
   - every reference-script role, script hash, and outref;
   - DA committee members, threshold, transport/profile configuration, and
     retention identity; and
   - proof-evidence digest and blueprint hash.
2. Compute the V1 tuple digest before validator compilation. Validators may
   authenticate that tuple digest and the fresh one-shot identity. Compute the
   final manifest ID only after validator hashes and reference-script outrefs
   exist, over the canonical manifest with the `manifestId` field omitted.
   Validators MUST NOT depend on the final manifest ID, avoiding a circular
   hash dependency.
3. Bind runtime node, DA, SDK diagnostics, database deployment markers, and E2E
   run state to the final manifest ID.
4. Replace the current node and DA database definitions with fresh V1 baseline
   schemas. Remove runtime repair and old-record reads.
5. Add startup checks that reject a database, LevelDB marker, DA store,
   manifest, or run-state identity not created for the exact V1 deployment.
6. Do not create a second reset sequence in this plan. Use
   `docs/agents/state-reset.md` and the `midgard-e2e-acceptance` skill's
   validated `fresh` mode. That workflow preserves Cardano provider state,
   creates the fresh one-shot identity, resets only matching Midgard state,
   publishes fresh reference scripts, initializes the protocol, generates DA
   manifests, and produces final evidence.
7. Canonicalize DA manifest property names and environment variables.
8. Reset active E2E, phase, stress, benchmark, snapshot, and recovery artifact
   schema identities to V1 and regenerate their fixtures.

### WB7 — Update active specifications and documentation

1. Replace `consensus-launch-profile.md` and
   `consensus-proof-profile-v3.md` with one active `consensus-profile-v1.md`.
2. Rename proof-v3 commands, Make targets, fixtures, and operator instructions
   to canonical V1 terminology.
3. Update the technical specification, CDDL, docs site, fault-proof matrix,
   readiness document, and deployment runbooks.
4. Mark superseded execution plans Historical or Superseded. Historical text
   may retain old terms only when its banner prevents it from being interpreted
   as current implementation or operator guidance.
5. Document that the next shipped incompatible change is V2, but do not
   implement dual-version negotiation in V1.
6. Link the accepted capability-floor decision and the mandatory §2.5
   follow-up without claiming that this consolidation completed either.

### WB8 — Remove obsolete support and prove absence

1. Delete old tests and fixtures after equivalent V1 success and rejection
   coverage exists.
2. Run the temporary V1-surface checker over source, active docs, schemas,
   blueprint names, generated files, and database SQL, retain its final report,
   then delete the checker.
3. Search for `legacy`, `compat`, `fallback`, `alias`, and numbered schema
   identities; manually classify every remaining hit.
4. Retain only operational recovery fallbacks, Cardano language identities,
   post-launch migration infrastructure, and gated MPF differential machinery.
5. Record the final classified results in the plan evidence; do not retain a
   compatibility allowlist or historical fixture suite in production source.

## 6. Verification plan

Commands are run from the stated working directory.

### 6.1 Static and build checks

From repository root:

```sh
make spec
make validation-one-step-cross-language
node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
```

During WB0/WB8, also run the temporary removal-audit command recorded in the
format registry. It is not part of the final repository.

From `onchain/aiken`:

```sh
aiken check
aiken build --env testnet
```

From `demo`:

```sh
pnpm run lint
pnpm run typecheck
pnpm run build
pnpm run test
```

### 6.2 Required focused suites

From `demo`, run package suites that own the renamed formats:

```sh
pnpm --filter @al-ft/midgard-core test
pnpm --filter @al-ft/midgard-sdk test
pnpm --filter @al-ft/lucid-midgard test
pnpm --filter @al-ft/midgard-validation test
pnpm --filter @al-ft/midgard-fault-proofs test
pnpm --filter midgard-watcher test
```

From `demo/midgard-node`, run the transaction-preparation feedback ladder:

```sh
cd ..
pnpm run test:tx-prep:sdk
pnpm run test:tx-prep:node
pnpm run test:tx-prep:emulator
pnpm run test:tx-prep:local
```

Then run focused node files, renamed to V1 as part of the implementation:

```sh
cd midgard-node
pnpm exec vitest run \
  tests/contract-deployment-info.test.ts \
  tests/da-libp2p-runtime-manifest.test.ts \
  tests/da-payload.test.ts \
  tests/database.test.ts \
  tests/deployment-run-state.test.ts \
  tests/forced-transactions-root.test.ts \
  tests/midgard-contracts.test.ts \
  tests/midgard-native-tx-codec.test.ts \
  tests/native-transaction-integration.test.ts \
  tests/reference-scripts.test.ts \
  tests/state-queue-topology.test.ts \
  tests/transition-trace-builder.test.ts
pnpm run test:mpf:differential
```

The permanent tests cover V1 canonical round trips, exact version validation,
manifest/profile/store mismatches, DA envelope encoding, forced valid/invalid/
no-op behavior, enabled feature preservation, finalization recovery, and MPF
root equivalence. Exhaustive historical-format fixtures are temporary WB0/WB8
evidence and are removed.

### 6.3 Cross-language and deployment evidence

1. Generate TS V1 fixtures and validate them in Aiken.
2. Generate Aiken V1 fixtures and validate them in TypeScript.
3. Recompute and review every validator hash and reference-script artifact.
4. Bind the complete V1 tuple, blueprint hash, validator hashes, proof evidence,
   and target-network Cardano parameters into `DeploymentManifestV1`.
5. Validate the current E2E runbook from repository root:

   ```sh
   node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs
   ```

6. Use the `midgard-e2e-acceptance` skill in `fresh` mode with
   `aiken build --env testnet`. Do not reproduce or alter its reset sequence in
   this plan.
7. Require the final `summary.json` and `summary.md` to report
   `functionalVerdict`, `cleanRunVerdict`, and `verdict` as `success`, with
   `nextSafeAction = none_run_complete`.
8. Retain the format registry, final temporary-removal report, V1 canonical
   vectors, validator and policy hashes, blueprint hash, V1 tuple digest,
   canonical manifest and manifest ID, target-network Cardano parameter
   snapshot, and E2E summaries under the run's durable evidence directory.

## 7. Acceptance criteria

- [ ] Exactly one Midgard consensus profile exists and it is named V1.
- [ ] The format registry classifies every independently serialized or
      authenticated format and every version-like numeric field.
- [ ] Every retained Midgard-owned serialized/public format has exactly one V1
      type and an exact discriminator equal to `1`.
- [ ] No V2+ encoder, decoder, union, deprecated alias, runtime migration shim,
      old capability advertisement, or missing-version default remains in
      active code.
- [ ] `DaPayloadV1` has the newest former DA payload semantics and no other DA
      payload generation is readable.
- [ ] All stored and transmitted DA payloads use `DaPayloadEnvelopeV1` with an
      explicit encoding tag; raw/off formats and structural inference are gone.
- [ ] Protocol-info, deployment manifest, native transaction, transition, DA,
      CEK, validation, dispute, forced transaction, header, and state-queue
      components all bind the same V1 tuple.
- [ ] Mint/burn, scripts/redeemers, reference inputs/scripts, script
      credentials, protected outputs, observers, and effectful valid forced
      transactions remain represented and tested on the V1 path.
- [ ] The consolidation introduces no new transaction restriction. Existing
      deviations from the accepted Cardano capability floor are enumerated as
      inputs to the mandatory §2.5 follow-up.
- [ ] Unsupported, malformed, or absent schema versions fail closed at every
      current external and persistence boundary.
- [ ] No compatibility decoder, legacy-state migration, removal-only fixture,
      or temporary stale-surface checker remains.
- [ ] The existing state-reset rules and E2E `fresh` mode complete a fresh V1
      deployment without deleting Cardano provider state.
- [ ] A fresh V1 deployment passes Aiken checks, all demo builds/typechecks,
      focused proof/DA/forced-transaction suites, the full test suite, and the
      named E2E acceptance workflow.
- [ ] The V1 release remains inactive until validator-hash-bound evidence proves
      every enabled transition family.

## 8. Risks and mitigations

### Incomplete consolidation

A partial implementation could leave node, DA, SDK, or validator components
using different names or encodings. Mitigation: no mixed-version build is
deployable; the exact manifest and tuple checks remain; acceptance uses the
existing E2E `fresh` mode with newly generated validators, reference scripts,
stores, and manifests.

### Historical bytes

The consolidation does not promise to classify or reject an abandoned
historical byte string that is also structurally valid under the new V1
decoder. With no shipped compatibility contract or retained deployment state,
accepted bytes have current V1 semantics. Mitigation is deletion, fresh state,
and a fresh deployment—not permanent epoch wrappers, historical decoders, or
collision fixtures.

### Commitment and validator-hash drift

Changing encoded versions and domain strings changes transaction IDs, roots,
proof commitments, validator hashes, and reference scripts. Mitigation:
regenerate all vectors and deployments, cross-check TS/Aiken bytes, and prohibit
attach/resume against an existing deployment.

### Capability regression hidden as cleanup

Deleting launch/proof branches could accidentally retain the restrictive
launch behavior or add a new restriction. Mitigation: select the newest
proof-oriented semantics before renaming, compare the before/after limit and
feature matrices, and run explicit feature tests. Existing capability-floor
deviations remain visible follow-up work rather than being misreported as fixed.

### Incomplete proof coverage

Making V1 the only profile could tempt activation before its proof system is
complete. Mitigation: preserve a single global compiled release-evidence gate;
do not retain launch-v1 as a fallback and do not disable required Cardano
features to make the gate pass.

### Over-broad removal of recovery behavior

Searches for “fallback” can match legitimate crash, provider, lease, or
rollback recovery. Mitigation: the removal inventory is format-specific;
operational recovery remains unless a separate correctness proof authorizes its
removal.

## 9. Deliberately retained surfaces

- Cardano Plutus language generations and dependency/API language adapters.
- Exact V1 validation at every hostile boundary.
- Generic rejection tests for missing or unsupported current schema versions.
- The database migration ledger/checksum framework, initialized with one V1
  baseline for post-launch use.
- Operational provider failover, transaction-finalization recovery, lease
  expiry, idempotent replay, rollback handling, and write-behind recovery.
- MPF `legacy`, overlay, event-flat, and Architecture G differential/rollback
  machinery until the existing engine-selection plan authorizes consolidation.
- Cryptographic separation between different record families.

## 10. Approval boundary

Approval of this plan authorizes implementation of the coordinated V1
consolidation described above. It does not authorize:

- deleting the whole `onchain/plutarch`, `demo/midgard-manager`, or `offchain`
  trees;
- changing the selected MPF production engine;
- implementing or declaring complete the deferred Cardano capability-floor and
  proof-coverage follow-up;
- weakening a proof, DA, finalization, rollback, or persistence invariant;
- activating V1 without the required proof and deployment evidence;
- automatically deleting developer data without an explicit, target-resolved
  operator action.

Any implementation discovery that requires one of those actions returns to
plan review.

## 11. Independent-review disposition

The 2026-07-24 independent review was resolved as follows:

- **Historical byte collisions:** no permanent epoch namespace or collision
  suite is added. There is no shipped legacy contract. Temporary removal
  fixtures are deleted when consolidation is complete.
- **Capability floor and proof completion:** explicitly deferred to the
  mandatory §2.5 follow-up before V1 activation.
- **Version taxonomy:** accepted and added in §2.2, including the genesis `0`
  sentinel.
- **Exhaustive format inventory:** accepted as the §3.1 registry gate.
- **Manifest identity:** accepted; WB6 now defines the exact preimage and
  non-circular tuple/manifest digest sequence.
- **Fresh redeploy sequencing:** owned by the existing state-reset rules and
  `midgard-e2e-acceptance` `fresh` mode instead of being duplicated.
- **DA framing:** accepted; `DaPayloadEnvelopeV1` is mandatory.
- **Runnable evidence:** accepted; §6 names the feedback ladder, package
  suites, focused node tests, runbook validator, and final E2E verdict fields.
- **Surface checker:** retained only as temporary implementation evidence and
  deleted before finalization.

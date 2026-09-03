#!/usr/bin/env node

import { execFileSync } from "node:child_process";
import { readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, extname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { assertHeaderV1AbiContract } from "./verify-canonical-v1-header-v1-abi.mjs";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
// Git provenance is the authority: bind one immutable source path/revision and
// compare those source bytes directly, without duplicating Git tree/file hashes.
const canonicalSourcePath = "docs/exec-plans/canonical-v1-format-registry.md";
const canonicalSourceRevision = "7a87a5402ea79d814d6a7a822d93b56dd4f92214";
const canonicalAuthority = {
  sourcePath: canonicalSourcePath,
  sourceRevision: canonicalSourceRevision,
};
const markdownPath = resolve(repoRoot, canonicalSourcePath);
// `--registry-under-test=` lets the behavioral self-test drive this exact gate
// against a mutated copy of the registry. Only the artifact is redirected: the
// canonical Markdown, the Aiken corpus and every repository source the gate
// checks against stay the real ones, so a seeded excuse is judged against
// reality rather than against a fixture of its own choosing.
const registryUnderTestArgument = process.argv
  .slice(2)
  .find((argument) => argument.startsWith("--registry-under-test="));
const registryPath =
  registryUnderTestArgument === undefined
    ? resolve(
        repoRoot,
        "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json",
      )
    : resolve(registryUnderTestArgument.slice("--registry-under-test=".length));
const allowIncomplete = process.argv.includes("--allow-incomplete");
const printBootstrap = process.argv.includes("--print-bootstrap");

const markdownBytes = readFileSync(markdownPath);
const markdown = markdownBytes.toString("utf8");
const rowPattern = /^\|\s*((?:C|N|D|L|S|K|V|P|A)\d{2})\s*\|(.+)$/gm;

const normalizeCell = (value) =>
  value.trim().replace(/\s+/g, " ").replace(/^`|`$/g, "");

const markdownRows = [...markdown.matchAll(rowPattern)].map((match) => {
  const cells = match[2]
    .split("|")
    .map(normalizeCell)
    .filter((cell, index, all) => index < all.length - 1 || cell.length > 0);
  const classification =
    cells.find((cell) =>
      [
        "schema",
        "nested-schema",
        "semantic",
        "sentinel",
        "external",
        "artifact",
        "post-launch seam",
        "semantic/external",
        "semantic/operational",
      ].includes(cell),
    ) ?? (match[1].startsWith("A") ? "artifact" : "UNVERIFIED");

  return {
    id: match[1],
    family: match[1][0],
    sourceOwnerAndCurrentIdentity: cells[0] ?? "UNVERIFIED",
    classification,
    canonicalResult: cells[2] ?? cells[1] ?? "UNVERIFIED",
    boundaryBindingAndPersistence: cells[3] ?? "UNVERIFIED",
    requiredEvidence: cells[4] ?? "UNVERIFIED",
    disposition: "UNVERIFIED",
    auditStatus: "UNVERIFIED",
    sourceEvidence: [],
    canonicalForms: [],
    positiveEvidence: [],
    rejectionEvidence: [],
    crossLanguageEvidence: {
      status: "UNVERIFIED",
      tests: [],
      notApplicableReason: null,
    },
    obsoleteBranchEvidence: [],
    notes: [],
  };
});

const expectedIds = markdownRows.map(({ id }) => id);
const canonicalAbsenceScans = [
  {
    id: "A01-retired-da-runtime-manifest-identities",
    paths: [
      "demo/da-committee-node/src",
      "demo/midgard-core/src",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
    ],
    patterns: [
      "midgard-da-libp2p-runtime-manifest-v[2-9][0-9]*",
      "DA_RUNTIME_MANIFEST_V[2-9][0-9]*",
    ],
  },
  {
    id: "A02-retired-deployment-manifest-identities",
    paths: [
      "demo/da-committee-node/src",
      "demo/lucid-midgard/src",
      "demo/midgard-core/src",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-sdk/src",
    ],
    patterns: [
      "midgard-deployment-manifest-v[2-9][0-9]*",
      "DeploymentManifestV[2-9][0-9]*",
    ],
  },
  {
    id: "P07-retired-unversioned-worker-candidate",
    paths: [
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-validation/src",
    ],
    patterns: ["\\bWirePhaseACandidate\\b"],
  },
  {
    id: "P05-retired-event-flat-overlay-identities",
    paths: [
      "demo/midgard-node/native",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
    ],
    patterns: [
      "ParkedEventFlat(?:Overlay|Shard)V[2-9][0-9]*",
      "ResumedEventFlatOverlayV[2-9][0-9]*",
      "(?:park|resumeParked)EventFlatOverlayV[2-9][0-9]*",
    ],
  },
  {
    id: "L01-L06-retired-ledger-transition-identities",
    paths: [
      "demo/da-committee-node/src",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-sdk/src",
      "onchain/aiken/lib",
      "onchain/aiken/validators",
    ],
    patterns: [
      "HeaderV[2-9][0-9]*",
      "HeaderTransitionCommitmentsV[2-9][0-9]*",
      "StateQueueNodeV[2-9][0-9]*",
      "InitV[2-9][0-9]*",
      "MergeToConfirmedStateV[2-9][0-9]*",
      "TransitionStepV[2-9][0-9]*",
      "transitionStepSchemaVersion",
    ],
  },
  {
    // L09/L10's claim is that the retired publication-receipt identities are not
    // *live surface*. Live surface means declared, so this scans for declarations
    // rather than for the names: #587's own prose — the Aiken gravestones on
    // `verify_order_material`, `reconstructTxOrderMaterialV1`'s note, the SDK's
    // `TxOrderMaterialV1` docstring — names every one of them on purpose, and a
    // bare-name scan would either forbid documenting the retirement or pass only
    // because the corpus was pruned to files that never mention it. Declaration
    // anchors are the discriminating form: they cannot appear in a comment, and
    // they are exactly what a resurrection would have to add.
    id: "L09-L10-retired-publication-receipt-declarations",
    paths: [
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
      "demo/midgard-watcher/src",
      "demo/midgard-watcher/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
      "onchain/aiken/lib/midgard",
      "onchain/aiken/validators",
    ],
    patterns: [
      "export (?:const|type) (?:TxFieldPreimageV1|TxFieldReceiptV1)\\b",
      "export const (?:TxFieldPreimageV1|TxFieldReceiptV1|TxOrderFieldFragmentV1|TxOrderFragmentBundleV1)Schema\\b",
      "export const (?:decodeTxFieldReceiptV1Cbor|deriveTxOrderFragmentBundleV1)\\b",
      "export const (?:deriveMidgardTxFieldReceiptAssetNameV1|MIDGARD_TX_FIELD_RECEIPT_V1_DOMAIN)\\b",
      "pub (?:fn|type|const) (?:verify_order_receipts|TxFieldPreimageV1|TxFieldReceiptV1)\\b",
      "terminal_receipt_reference:",
    ],
  },
  {
    id: "L07-L11-retired-transaction-order-material-identities",
    paths: [
      "demo/midgard-sdk/src/ledger-state.ts",
      "demo/midgard-sdk/src/user-events/tx-order.ts",
      "demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.ts",
      "onchain/aiken/lib/midgard/ledger-state.ak",
      "onchain/aiken/lib/midgard/user-events/tx-order-v1.ak",
      // The counted publication receipt chain's three files —
      // `lib/midgard/user-events/tx-field-receipt-v1.ak` and the
      // `tx-field-preimage-v1` / `tx-field-receipt-v1` validators — were scanned here
      // until #587 deleted them. A path that no longer exists cannot be scanned; the
      // `TxField(?:Preimage|Receipt)V[2-9]` pattern below still runs over every
      // surviving path, which is where a resurrected identity would have to appear.
      "onchain/aiken/validators/user-events/tx-order-v1.ak",
      "onchain/aiken/validators/user-events/cek-program-material-v1.ak",
    ],
    patterns: [
      "TxOrder(?:Id|Payload|Event|Datum|SpendRedeemer)V[2-9][0-9]*",
      "ForcedInclusionTxV[2-9][0-9]*",
      "TxField(?:Preimage|Receipt)V[2-9][0-9]*",
      "TxOrder(?:FieldFragment|FragmentBundle)V[2-9][0-9]*",
      "CekProgramMaterial(?:Datum|Publication|Config)V[2-9][0-9]*",
      "(?:tx-order|tx-field|cek-program-material)-v[2-9][0-9]*",
    ],
  },
  {
    id: "L18-retired-event-operator-identities",
    paths: [
      "demo/midgard-sdk/src/user-events/internals.ts",
      "demo/midgard-sdk/src/user-events/deposit.ts",
      "demo/midgard-sdk/src/user-events/withdrawal.ts",
      "demo/midgard-sdk/src/reserve.ts",
      "demo/midgard-sdk/src/active-operators.ts",
      "demo/midgard-sdk/src/registered-operators.ts",
      "demo/midgard-sdk/src/retired-operators.ts",
      "demo/midgard-sdk/src/payout.ts",
      "demo/midgard-sdk/src/settlement.ts",
      "onchain/aiken/lib/midgard/user-events.ak",
      "onchain/aiken/lib/midgard/reserve.ak",
      "onchain/aiken/lib/midgard/operator-directory.ak",
      "onchain/aiken/lib/midgard/operator-directory/active-operators.ak",
      "onchain/aiken/lib/midgard/operator-directory/registered-operators.ak",
      "onchain/aiken/lib/midgard/operator-directory/retired-operators.ak",
      "onchain/aiken/lib/midgard/payout.ak",
      "onchain/aiken/lib/midgard/settlement.ak",
    ],
    patterns: [
      "(?:Deposit|Withdrawal|Reserve|Payout|Settlement)(?:Datum|MintRedeemer|SpendRedeemer|EventType)V[2-9][0-9]*",
      "(?:Active|Registered|Retired)Operator(?:Datum|MintRedeemer|SpendRedeemer)V[2-9][0-9]*",
      "(?:SlashingReason|SlashingArguments|SchedulerSync)V[2-9][0-9]*",
    ],
  },
  {
    id: "L16-retired-ledger-output-identities",
    paths: [
      "demo/midgard-core/src",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-validation/src",
      "onchain/aiken/lib",
      "onchain/aiken/validators",
    ],
    patterns: [
      "LedgerOutputCommitmentV[2-9][0-9]*",
      "ledger_output_commitment_v[2-9][0-9]*",
    ],
  },
  {
    id: "L17-retired-mpf-proof-identities",
    paths: [
      "demo/midgard-fault-proofs/src",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-sdk/src",
      "demo/midgard-validation/src",
      "onchain/aiken/lib",
      "onchain/aiken/validators",
    ],
    patterns: [
      "MpfProofV[2-9][0-9]*",
      "ProofStepV[2-9][0-9]*",
      "mpf[-_]proof[-_]v[2-9][0-9]*",
    ],
  },
  {
    id: "L19-retired-scheduler-identities",
    paths: [
      "demo/midgard-sdk/src/scheduler.ts",
      "onchain/aiken/lib/midgard/scheduler.ak",
      "onchain/aiken/validators/scheduler.ak",
    ],
    patterns: [
      "Scheduler(?:Datum|MintRedeemer|SpendRedeemer)V[2-9][0-9]*",
      "(?:AdvancingApproach|NeglectedUserEvent|OperatorRemovalReason)V[2-9][0-9]*",
      "scheduler[-_]v[2-9][0-9]*",
    ],
  },
  {
    id: "V01-V04-retired-validation-trace-identities",
    paths: [
      "demo/midgard-core/src",
      "demo/midgard-sdk/src",
      "demo/midgard-validation/src",
      "onchain/aiken/lib/midgard",
      "onchain/aiken/validators",
    ],
    patterns: [
      "ValidationMachineStateV[2-9][0-9]*",
      "ValidationTraceDescriptorV[2-9][0-9]*",
      "ValidationTraceProofV[2-9][0-9]*",
      "validation[-_](?:trace|merkle)[-_]v[2-9][0-9]*",
    ],
  },
  {
    id: "V05-V18-retired-validation-identities",
    paths: [
      "demo/midgard-sdk/src/fraud-proof/validation-dispute.ts",
      "demo/midgard-sdk/src/fraud-proof/validation-auxiliary-witness-v1.ts",
      "demo/midgard-validation/src/validation-machine-data.ts",
      "demo/midgard-validation/src/validation-machine.ts",
      "onchain/aiken/lib/midgard/validation-claim-v1.ak",
      "onchain/aiken/lib/midgard/validation-dispute-v1.ak",
      "onchain/aiken/lib/midgard/validation-game-v1.ak",
      "onchain/aiken/lib/midgard/validation-machine-v1.ak",
    ],
    patterns: [
      "Validation(?:ClaimWitness|Dispute|OneStepWitness|OneStepEvidence|AuxiliaryWitness|SourceMembership)V[2-9][0-9]*",
      "(?:ResolveInputs|ScriptSources|ScriptDiscovery|NativeScripts|CekContext|ValueAndMint|LedgerDelta)ControlV[2-9][0-9]*",
      "(?:ValueAccumulator|LedgerDeltaPendingMutation|TerminalAcceptance|TerminalRejection)V[2-9][0-9]*",
      "validation[-_](?:claim|dispute|machine|controls|source-membership)[-_]v[2-9][0-9]*",
    ],
  },
  {
    id: "L12-retired-forced-journal-identities",
    paths: [
      "demo/midgard-node/src/database",
      "demo/midgard-node/src/fibers",
      "demo/midgard-node/src/workers",
    ],
    patterns: [
      "ForcedTransactionJournalMemberV[2-9][0-9]*",
      "FORCED_TRANSACTION_JOURNAL_MEMBER_V[2-9][0-9]*",
      "forced_transaction_journal_member_v[2-9][0-9]*",
    ],
  },
  {
    id: "L13-L15-retired-finalization-recovery-identities",
    paths: [
      "demo/midgard-node/src/commands",
      "demo/midgard-node-tools/src/commands",
      "demo/midgard-node/src/database",
      "demo/midgard-node/src/fibers",
      "demo/midgard-node/src/transactions",
      "demo/midgard-node/src/workers",
    ],
    patterns: [
      "PendingBlockFinalizationV[2-9][0-9]*",
      "ForeignTipReconciliationV[2-9][0-9]*",
      "ledger_delta_v[2-9][0-9]*",
      "ledger_delta_native_mpf_v[2-9][0-9]*",
      "verified_(?:empty|da)_v[2-9][0-9]*",
      "replaceConfirmedLedgerWithEntries(?:Transaction)?",
    ],
  },
  {
    id: "A21-obsolete-architecture-g-artifact-identities",
    paths: [
      "demo/midgard-node/scripts",
      "demo/midgard-node-tools/scripts",
      "demo/midgard-node/src/workers",
    ],
    patterns: [
      "midgard-architecture-g-(?:phase1-formal-binding-identity|runtime-identity|corpus-preparation|corpus-funding|production-root-gate|root-diagnostic-smoke|commit-candidate-seed|commit-candidate-seed-result|commit-candidate-input|commit-candidate-probe|commit-candidate-gate|commit-candidate-smoke)-v[2-9][0-9]*",
    ],
  },
  {
    id: "A23-retired-secret-watchdog-artifacts",
    paths: [
      "demo/midgard-node/scripts/phase3-architecture-g-closure-lib.mjs",
      "demo/midgard-node/scripts/phase3-architecture-g-live-e2e.mjs",
      "demo/midgard-node/scripts/throughput-load-watchdog.mjs",
      "demo/midgard-node/scripts/verify-phase3-architecture-g-live-e2e-report.mjs",
    ],
    patterns: [
      "midgard-secret-scanned-log-v[2-9][0-9]*",
      "midgard-throughput-watchdog-v[2-9][0-9]*",
    ],
  },
  {
    id: "C01-obsolete-consensus-profiles",
    paths: [
      "demo/da-committee-node/src",
      "demo/lucid-midgard/src",
      "demo/lucid-midgard/tests",
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-validation/src",
      "demo/midgard-validation/tests",
    ],
    patterns: ["midgard-launch-consensus-v1", "midgard-proof-consensus-v3"],
  },
  {
    id: "A22-retired-phase5-da-artifacts",
    paths: [
      "demo/midgard-node/package.json",
      "demo/midgard-node-tools/package.json",
      "demo/midgard-node/scripts",
      "demo/midgard-node-tools/scripts",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
    ],
    patterns: [
      "MIDGARD_DA_LARGE_OPERATIONAL_ENVELOPE",
      "MIDGARD_DA_LARGE_OPERATIONAL_REPORT",
      "midgard-phase-5-live-da-50k-v1",
      "midgard-phase-5-live-da-v1",
      "test:da-phase5-e2e",
    ],
  },
  {
    id: "A13-A16-retired-stress-artifacts",
    paths: [
      "demo/midgard-node-tools/src/commands/stress-wallets.ts",
      "demo/midgard-node-tools/src/commands/stress-corpus-generate.ts",
      "demo/midgard-node-tools/src/commands/stress-corpus",
      "demo/midgard-node-tools/src/commands/stress-open-loop.ts",
      "demo/midgard-node/src/open-loop-corpus-format.ts",
      "demo/midgard-node/scripts/throughput-valid-stress-corpus.mjs",
    ],
    patterns: [
      "midgard-stress-wallet-consolidate-v1",
      "historicalExtension",
      "historicalBinding",
    ],
  },
  {
    id: "N10-obsolete-partial-witness-bundle",
    paths: [
      "demo/lucid-midgard/src/builder/witness-bundle.ts",
      "demo/lucid-midgard/src/builder.ts",
      "demo/lucid-midgard/tests/partial-signing.test.ts",
      "demo/lucid-midgard/tests/api-export-snapshot.test.ts",
    ],
    patterns: [
      "MidgardPartialWitnessBundle(?!V1)",
      "PARTIAL_WITNESS_BUNDLE_VERSION\\s*=\\s*[2-9]",
    ],
  },
  {
    id: "N01-N09-obsolete-native-transaction-identities",
    paths: [
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
      "demo/midgard-validation/src",
      "demo/midgard-validation/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx",
      "onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak",
    ],
    patterns: [
      "MidgardNativeTx(?:Canonical|Full|Compact|BodyCompact|BodyCanonical|WitnessSetCompact|WitnessSetCanonical|ProofSource|ProofFieldLengths)V[2-9]",
      "computeMidgardNativeTxFullHashV[2-9]",
      "MIDGARD_NATIVE_TX_V[2-9]",
    ],
  },
  {
    id: "D01-D05-retired-da-payload-identities",
    paths: [
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
      "demo/da-committee-node/src",
      "demo/da-committee-node/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
    ],
    patterns: [
      "(?:DaPayload|VerifiedDaPayload|DaPayloadBody|DaPayloadCounts|DaPayloadEnvelope)V[234]",
      "DA_PAYLOAD_(?:ENVELOPE_)?V[234]",
      "(?:encode|decode)DaPayloadV[234]",
    ],
  },
  {
    id: "D17-retired-da-conflict-evidence-identities",
    paths: [
      "demo/midgard-core/src/da-transport.ts",
      "demo/da-committee-node/src/domain.ts",
      "demo/da-committee-node/src/store.ts",
      "demo/da-committee-node/src/store/postgres.ts",
      "demo/da-committee-node/src/da/libp2p",
      "demo/da-committee-node/src/watcher.ts",
      "demo/da-committee-node/src/index.ts",
    ],
    patterns: [
      "DaConflictEvidenceV[2-9]",
      "DaConflictingSignatureHeaderEvidenceV[2-9]",
      "DaStoredConflictEvidenceRecordV[2-9]",
    ],
  },
  {
    id: "P02-retired-da-postgres-column-migration",
    paths: ["demo/da-committee-node/src/store/postgres.ts"],
    patterns: [
      "migratePeerIdColumnNames",
      "renameColumnIfOnlyLegacyExists",
      "information_schema\\.columns",
      "ALTER\\s+TABLE[\\s\\S]*RENAME\\s+COLUMN",
      "peer_base_url",
    ],
  },
  {
    id: "P03-retired-forced-transaction-aliases",
    paths: [
      "demo/midgard-node/src/database/forcedTransactions.ts",
      "demo/midgard-node/src/database/migrations/sql/0001_initial_schema.sql",
      "demo/midgard-node/src/fibers/fetch-and-insert-tx-order-utxos.ts",
      "demo/midgard-node/src/workers",
      "demo/midgard-sdk/src/ledger-state.ts",
    ],
    patterns: [
      "encodeForcedInclusionValue(?!V1)",
      "ForcedInclusionValueInput(?!V1)",
      "ForcedInclusionTxSchema(?!V1)",
      "ForcedInclusionTx(?!V1)",
    ],
  },
  {
    id: "A17-A20-obsolete-phase-artifacts",
    paths: [
      "demo/midgard-node/scripts/phase1-formal-identity.mjs",
      "demo/midgard-node/scripts/create-phase1-formal-binding.mjs",
      "demo/midgard-node/scripts/phase3-architecture-g-load-generator-isolation.mjs",
      "demo/midgard-node/scripts/phase3-architecture-g-live-e2e.mjs",
      "demo/midgard-node/scripts/verify-phase3-architecture-g-live-e2e-report.mjs",
      "demo/midgard-node/scripts/phase3-architecture-g-soak.mjs",
      "demo/midgard-node/scripts/verify-phase3-architecture-g-soak-report.mjs",
    ],
    patterns: [
      "midgard-phase1-live-corpus-binding-v[2-9]",
      "midgard-phase3-load-generator-isolation-v[2-9]",
      "midgard-phase3-architecture-g-clean-live-e2e-v[2-9]",
      "midgard-phase3-architecture-g-live-soak-v[2-9]",
    ],
  },
  {
    id: "A10-A12-obsolete-phase4-artifacts",
    paths: [
      "demo/midgard-node-tools/src/commands/e2e-pipelined-commit-process-acceptance.ts",
      "demo/midgard-node-tools/src/commands/phase4-genesis-ledger.ts",
      "demo/midgard-node-tools/src/commands/phase4-t1-recovery.ts",
      "demo/midgard-node/src/transactions/phas-membership-registration.ts",
      "demo/midgard-node/scripts/phase4-environment-fingerprint-lib.mjs",
      "demo/midgard-node-tools/scripts/verify-phase4-pipelined-process-summary.mjs",
      "demo/midgard-node/scripts/verify-phase4-pipelined-report.mjs",
    ],
    patterns: [
      "midgard-phase4-local-devnet-reset-attestation-v[2-9]",
      "midgard-phase4-matched-snapshot-identity-v[2-9]",
      "midgard-phase4-environment-artifact-v[2-9]",
      "midgard-phase4-local-genesis-ledger-v[2-9]",
      "midgard-phase4-t1-recovery-attestation-v[2-9]",
      "midgard-phase4-pipelined-commit-process-acceptance-v[2-9]",
    ],
  },
  {
    id: "A03-A09-obsolete-e2e-artifacts",
    paths: [
      "demo/midgard-node/src/e2e/run-state.ts",
      "demo/midgard-node-tools/src/e2e/runner.ts",
      "demo/midgard-node-tools/src/e2e/summary.ts",
      "demo/midgard-node-tools/src/e2e/da-gates.ts",
      "demo/midgard-node-tools/src/e2e/service-supervisor.ts",
      "demo/midgard-node-tools/src/e2e/process-ownership.ts",
      "demo/midgard-node-tools/src/commands/e2e-service.ts",
      "demo/midgard-node-tools/src/commands/e2e-stress-l2-throughput.ts",
      "demo/midgard-node/src/commands/reconcile.ts",
    ],
    patterns: [
      "midgard-deployment-run-state-v[2-9]",
      "midgard-e2e-step-v[2-9]",
      "midgard-e2e-summary-v[2-9]",
      "midgard-e2e-da-gate-v[2-9]",
      "midgard-e2e-l2-stress-(?:config|summary)-v[2-9]",
      "midgard-e2e-(?:managed-service|service-supervisor|owned-process-group)-v[2-9]",
      "midgard-e2e-reconciliation-v[2-9]",
    ],
  },
  {
    id: "K01-K13-retired-cek-identities",
    paths: [
      "demo/midgard-core/src",
      "demo/midgard-core/tests",
      "demo/midgard-validation/src",
      "demo/midgard-validation/tests",
      "demo/lucid-midgard/src",
      "demo/lucid-midgard/tests",
      "demo/midgard-node/src",
      "demo/midgard-node-tools/src",
      "demo/midgard-node/tests",
      "demo/midgard-node-tools/tests",
      "demo/midgard-sdk/src",
      "demo/midgard-sdk/tests",
      "demo/midgard-fault-proofs/src",
      "demo/midgard-fault-proofs/tests",
      "onchain/aiken/lib/midgard/cek-proof-v1.ak",
      "onchain/aiken/lib/midgard/cek-proof-v1.test.ak",
      "onchain/aiken/lib/midgard/cek-machine-v1.ak",
      "onchain/aiken/lib/midgard/cek-machine-v1.test.ak",
      "onchain/aiken/lib/midgard/cek-data-v1.ak",
      "onchain/aiken/lib/midgard/cek-data-v1.test.ak",
      "onchain/aiken/lib/midgard/cek-data-scan-v1.ak",
      "onchain/aiken/lib/midgard/cek-data-scan-v1.test.ak",
      "onchain/aiken/lib/midgard/cek-constant-v1.ak",
      "onchain/aiken/lib/midgard/cek-constant-v1.test.ak",
      "onchain/aiken/lib/midgard/cek-builtin-v1.ak",
      "onchain/aiken/lib/midgard/cek-builtin-v1.test.ak",
    ],
    patterns: [
      "MidgardCek(?:ProgramEnvelope|MachineState|ProgramMaterial(?:Value|Entry|Sidecar))V[2-9][0-9]*",
      "MidgardProofSubmissionV[2-9][0-9]*",
      "MIDGARD_(?:CEK_PROGRAM_ENVELOPE|CEK_MACHINE_STATE|CEK_PROGRAM_MATERIAL(?:_SIDECAR)?|PROOF_SUBMISSION(?:_ENVELOPE)?)_V[2-9][0-9]*",
      "cek[-_](?:proof|machine|data|data[-_]scan|constant|builtin)[-_]v[2-9][0-9]*",
    ],
  },
];

if (printBootstrap) {
  await new Promise((resolveWrite, rejectWrite) =>
    process.stdout.write(
      `${JSON.stringify(
        {
          schema: "midgard-canonical-v1-format-registry-v1",
          version: 1,
          source: canonicalSourcePath,
          authority: canonicalAuthority,
          unknownFormatPolicy: "reject",
          forbiddenActivePatterns: canonicalAbsenceScans,
          formats: markdownRows,
        },
        null,
        2,
      )}\n`,
      (error) => (error == null ? resolveWrite() : rejectWrite(error)),
    ),
  );
  process.exit(0);
}

const errors = [];
const fail = (message) => errors.push(message);
const isNonEmptyString = (value) =>
  typeof value === "string" && value.trim().length > 0;
const requireNonEmptyString = (value, label) => {
  if (!isNonEmptyString(value)) {
    fail(`${label} must be a non-empty string`);
  }
};
const verifyGitAuthority = (authority, label) => {
  if (
    authority === null ||
    typeof authority !== "object" ||
    Array.isArray(authority)
  ) {
    fail(`${label} must be an object`);
    return;
  }
  if (authority.sourcePath !== canonicalSourcePath) {
    fail(`${label}.sourcePath must be ${canonicalSourcePath}`);
  }
  if (
    typeof authority.sourceRevision !== "string" ||
    !/^[0-9a-f]{40}$/iu.test(authority.sourceRevision)
  ) {
    fail(`${label}.sourceRevision must be a full 40-character Git commit`);
    return;
  }

  try {
    execFileSync(
      "git",
      ["merge-base", "--is-ancestor", authority.sourceRevision, "HEAD"],
      { cwd: repoRoot, stdio: ["ignore", "ignore", "ignore"] },
    );
  } catch {
    fail(`${label}.sourceRevision must be an ancestor of HEAD`);
  }

  try {
    const historicalSourceBytes = execFileSync(
      "git",
      ["show", `${authority.sourceRevision}:${canonicalSourcePath}`],
      { cwd: repoRoot, stdio: ["ignore", "pipe", "ignore"] },
    );
    if (!historicalSourceBytes.equals(markdownBytes)) {
      fail(
        `${label}.sourceRevision does not contain the current canonical Markdown source bytes`,
      );
    }
  } catch {
    fail(`${label}.sourceRevision does not contain ${canonicalSourcePath}`);
  }
};
const loadRepoText = (relativePath, label) => {
  if (
    !isNonEmptyString(relativePath) ||
    relativePath.startsWith("/") ||
    relativePath.includes("..")
  ) {
    fail(`${label} must be a repository-relative path without '..'`);
    return null;
  }
  try {
    return readFileSync(resolve(repoRoot, relativePath), "utf8");
  } catch {
    fail(`${label} does not exist: ${relativePath}`);
    return null;
  }
};
// #519 V-8: `text.includes(name)` accepted a row that cited, as a test name,
// the literal argument of a `process.stdout.write` in the very script it named
// — the cross-language claim was satisfied by a verifier's own success message
// in its own source. Textual containment cannot tell a declared test from a
// printed sentence, so the printed sentences are located explicitly and a
// citation that only ever matches inside one is rejected.
const OUTPUT_CALL =
  /(?:process\.(?:stdout|stderr)\.write|console\.(?:log|error|warn|info|debug))\s*\(/gu;
const outputLiteralExtensions = new Set([".cjs", ".js", ".mjs", ".ts", ".tsx"]);
/** Byte ranges of every argument list passed to an output-writing call. */
const outputCallRanges = (text) => {
  const ranges = [];
  OUTPUT_CALL.lastIndex = 0;
  let match;
  while ((match = OUTPUT_CALL.exec(text)) !== null) {
    const start = match.index + match[0].length;
    let depth = 1;
    let index = start;
    while (index < text.length && depth > 0) {
      const character = text[index];
      if (character === "(") depth += 1;
      else if (character === ")") depth -= 1;
      index += 1;
    }
    ranges.push([start, index]);
    OUTPUT_CALL.lastIndex = index;
  }
  return ranges;
};
const citesOnlyOutputLiterals = (text, path, name) => {
  if (!outputLiteralExtensions.has(extname(path))) return false;
  const ranges = outputCallRanges(text);
  if (ranges.length === 0) return false;
  let occurrences = 0;
  let inside = 0;
  for (
    let index = text.indexOf(name);
    index !== -1;
    index = text.indexOf(name, index + 1)
  ) {
    occurrences += 1;
    if (ranges.some(([start, end]) => index >= start && index < end)) {
      inside += 1;
    }
  }
  return occurrences > 0 && occurrences === inside;
};
const verifyReferences = (references, label, contentField) => {
  if (!Array.isArray(references)) {
    fail(`${label} must be an array`);
    return;
  }
  for (const [index, reference] of references.entries()) {
    if (reference === null || typeof reference !== "object") {
      fail(`${label}[${index}] must be an object`);
      continue;
    }
    const text = loadRepoText(reference.path, `${label}[${index}].path`);
    const names = reference[contentField];
    if (!Array.isArray(names) || names.length === 0) {
      fail(`${label}[${index}].${contentField} must be a non-empty array`);
      continue;
    }
    for (const [nameIndex, name] of names.entries()) {
      requireNonEmptyString(
        name,
        `${label}[${index}].${contentField}[${nameIndex}]`,
      );
      if (text !== null && isNonEmptyString(name) && !text.includes(name)) {
        fail(
          `${label}[${index}] does not contain ${contentField} entry ${JSON.stringify(name)}: ${reference.path}`,
        );
      } else if (
        text !== null &&
        contentField === "testNames" &&
        isNonEmptyString(name) &&
        citesOnlyOutputLiterals(text, reference.path, name)
      ) {
        fail(
          `${label}[${index}] cites ${JSON.stringify(name)} as a test name, but ${reference.path} only ever writes it as output: a verifier's own success message is not a test`,
        );
      }
    }
  }
};
// `.rs` was missing, so the P05 absence scan over `demo/midgard-node/native`
// collected none of that crate's four Rust sources. `target` was missing from
// the ignore list, so the same scan descended into the cargo build directory
// and read 117 `.json` fingerprint artifacts instead — 117 files scanned, 0 of
// them the files P05 is about. On a clean checkout (no `target/`) the same leg
// collected 0 files and still exited 0. (#532, residual of #519.)
const activeSourceExtensions = new Set([
  ".ak",
  ".cjs",
  ".js",
  ".json",
  ".mjs",
  ".rs",
  ".sql",
  ".ts",
  ".tsx",
]);
const ignoredDirectoryNames = new Set([
  ".turbo",
  "coverage",
  "dist",
  "logs",
  "node_modules",
  "target",
]);
const collectActiveFiles = (relativePath, label) => {
  if (
    !isNonEmptyString(relativePath) ||
    relativePath.startsWith("/") ||
    relativePath.includes("..")
  ) {
    fail(`${label} must be a repository-relative path without '..'`);
    return [];
  }
  const absolutePath = resolve(repoRoot, relativePath);
  let stat;
  try {
    stat = statSync(absolutePath);
  } catch {
    fail(`${label} does not exist: ${relativePath}`);
    return [];
  }
  if (stat.isFile()) {
    return [absolutePath];
  }
  if (!stat.isDirectory()) {
    fail(`${label} is neither a file nor a directory: ${relativePath}`);
    return [];
  }

  const files = [];
  const visit = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      if (entry.isDirectory() && ignoredDirectoryNames.has(entry.name)) {
        continue;
      }
      const child = resolve(directory, entry.name);
      if (entry.isDirectory()) {
        visit(child);
      } else if (
        entry.isFile() &&
        activeSourceExtensions.has(
          entry.name.slice(entry.name.lastIndexOf(".")),
        )
      ) {
        files.push(child);
      }
    }
  };
  visit(absolutePath);
  return files;
};
const passedAbsenceScans = new Set();
const verifyAbsenceScans = (scans) => {
  if (!Array.isArray(scans)) {
    fail("canonical absence scan table must be an array");
    return;
  }
  const seenScanIds = new Set();
  for (const [scanIndex, scan] of scans.entries()) {
    const label = `forbiddenActivePatterns[${scanIndex}]`;
    requireNonEmptyString(scan?.id, `${label}.id`);
    if (seenScanIds.has(scan?.id)) {
      fail(`${label}.id is duplicated: ${scan.id}`);
      continue;
    }
    seenScanIds.add(scan?.id);
    if (!Array.isArray(scan?.paths) || scan.paths.length === 0) {
      fail(`${label}.paths must be a non-empty array`);
      continue;
    }
    if (!Array.isArray(scan?.patterns) || scan.patterns.length === 0) {
      fail(`${label}.patterns must be a non-empty array`);
      continue;
    }
    const files = scan.paths.flatMap((path, pathIndex) =>
      collectActiveFiles(path, `${label}.paths[${pathIndex}]`),
    );
    // Fail closed on an empty corpus: an absence scan that reads no files
    // reports success without having looked at anything. (#532)
    if (files.length === 0) {
      fail(
        `${scan.id} collected 0 scannable files from ${scan.paths.join(", ")}, so it proves nothing`,
      );
      continue;
    }
    let scanPassed = true;
    for (const [patternIndex, pattern] of scan.patterns.entries()) {
      requireNonEmptyString(pattern, `${label}.patterns[${patternIndex}]`);
      if (!isNonEmptyString(pattern)) {
        scanPassed = false;
        continue;
      }
      let expression;
      try {
        expression = new RegExp(pattern, "u");
      } catch (error) {
        fail(
          `${label}.patterns[${patternIndex}] is not a valid regular expression: ${error.message}`,
        );
        scanPassed = false;
        continue;
      }
      for (const file of files) {
        const text = readFileSync(file, "utf8");
        const match = expression.exec(text);
        if (match !== null) {
          const line = text.slice(0, match.index).split("\n").length;
          fail(
            `${scan.id} found /${pattern}/ at ${relative(repoRoot, file)}:${line}`,
          );
          scanPassed = false;
        }
      }
    }
    if (scanPassed && isNonEmptyString(scan?.id)) {
      passedAbsenceScans.add(scan.id);
    }
  }
};
const verifyAbsenceEvidence = (references, label) => {
  if (!Array.isArray(references)) {
    fail(`${label} must be an array`);
    return;
  }
  for (const [index, reference] of references.entries()) {
    requireNonEmptyString(reference?.scanId, `${label}[${index}].scanId`);
    if (
      isNonEmptyString(reference?.scanId) &&
      !passedAbsenceScans.has(reference.scanId)
    ) {
      fail(`${label}[${index}] references an absent or failed scan`);
    }
  }
};
// ---------------------------------------------------------------------------
// Bounded cross-language "not applicable" excuses (V-6, issue #530)
//
// A row used to discharge its cross-language obligation with any non-empty
// prose in `notApplicableReason`, and 66 of 132 rows did. Free text is not
// checkable, so the excuse could not be wrong: a row with a real Aiken
// counterpart could publish "Aiken never decodes this" and pass.
//
// Every N/A row now declares a category from a closed set, and the gate checks
// that category against reality rather than reading the prose:
//
//   deleted-identity        the format is gone, so there is nothing to keep in
//                           agreement. Checked against the row's own
//                           disposition and its passing absence scans.
//   no-onchain-counterpart  no Aiken code names this format. Checked by
//                           deriving the row's identity symbols from its own
//                           sourceEvidence — the artifact does not get to
//                           choose which symbols are examined — and requiring
//                           every one of them to be absent from the Aiken
//                           corpus in both its literal and snake_case forms.
//
// The prose is retained as the human explanation but is no longer the evidence.
// ---------------------------------------------------------------------------

const aikenCorpusRoots = [
  "onchain/aiken/lib",
  "onchain/aiken/validators",
  "onchain/aiken/env",
];
const readAikenCorpus = () => {
  const sources = [];
  const walk = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      const full = resolve(directory, entry.name);
      if (entry.isDirectory()) {
        if (!ignoredDirectoryNames.has(entry.name)) walk(full);
      } else if (entry.name.endsWith(".ak")) {
        sources.push(readFileSync(full, "utf8"));
      }
    }
  };
  for (const root of aikenCorpusRoots) walk(resolve(repoRoot, root));
  return sources.join("\n");
};
const aikenCorpus = readAikenCorpus();
const toSnakeCase = (symbol) =>
  symbol
    .replace(/([a-z0-9])([A-Z])/gu, "$1_$2")
    .replace(/([A-Z]+)([A-Z][a-z])/gu, "$1_$2")
    .toLowerCase();
// Aiken spells types in PascalCase and functions/constants in snake_case, so a
// TypeScript identity that crossed the boundary appears in one of those two
// forms. Both are searched before an absence claim is accepted.
const namedInAiken = (symbol) =>
  aikenCorpus.includes(symbol) || aikenCorpus.includes(toSnakeCase(symbol));
// An identity symbol is an exported name, not an English word: it must be at
// least four characters and carry a capital or an underscore. This keeps bare
// enum strings such as `legacy` out of the derived absence set while every
// `MidgardNativeTxV1`-shaped identity stays in it.
const isIdentitySymbol = (symbol) =>
  typeof symbol === "string" &&
  /^[A-Za-z_][A-Za-z0-9_]*$/u.test(symbol) &&
  symbol.length >= 4 &&
  /[A-Z_]/u.test(symbol);
const deriveIdentitySymbols = (row) => [
  ...new Set(
    (Array.isArray(row.sourceEvidence) ? row.sourceEvidence : []).flatMap(
      (reference) =>
        (Array.isArray(reference?.symbols) ? reference.symbols : []).filter(
          isIdentitySymbol,
        ),
    ),
  ),
];
const crossLanguageNotApplicableCategories = new Set([
  "deleted-identity",
  "no-onchain-counterpart",
]);
const crossLanguageTestedIds = new Set();
const crossLanguageNotApplicableIds = new Set();
const crossLanguageNotApplicableByCategory = new Map();
const observedExactFieldPinDeficits = new Set();
const verifyCrossLanguageNotApplicable = (row, label) => {
  const crossLanguage = row.crossLanguageEvidence;
  const category = crossLanguage.notApplicableCategory;
  if (!isNonEmptyString(crossLanguage.notApplicableReason)) {
    fail(
      `${label}.crossLanguageEvidence needs tests or a non-empty notApplicableReason`,
    );
    return;
  }
  if (!isNonEmptyString(category)) {
    fail(
      `${label}.crossLanguageEvidence: free-text notApplicableReason is not evidence; a notApplicableCategory from {${[
        ...crossLanguageNotApplicableCategories,
      ].join(", ")}} is required`,
    );
    return;
  }
  if (!crossLanguageNotApplicableCategories.has(category)) {
    fail(
      `${label}.crossLanguageEvidence.notApplicableCategory ${JSON.stringify(
        category,
      )} is not one of {${[...crossLanguageNotApplicableCategories].join(", ")}}`,
    );
    return;
  }
  crossLanguageNotApplicableByCategory.set(
    category,
    (crossLanguageNotApplicableByCategory.get(category) ?? 0) + 1,
  );
  if (category === "deleted-identity") {
    if (row.disposition !== "delete") {
      fail(
        `${label}.crossLanguageEvidence claims deleted-identity but the row disposition is ${JSON.stringify(
          row.disposition,
        )}`,
      );
    }
    if (
      !Array.isArray(row.obsoleteBranchEvidence) ||
      row.obsoleteBranchEvidence.length === 0
    ) {
      fail(
        `${label}.crossLanguageEvidence claims deleted-identity without a passing absence scan proving the identity is gone`,
      );
    }
    return;
  }
  if (row.disposition === "delete") {
    fail(
      `${label}.crossLanguageEvidence claims no-onchain-counterpart for a deleted format; use deleted-identity`,
    );
    return;
  }
  const identitySymbols = deriveIdentitySymbols(row);
  if (identitySymbols.length === 0) {
    fail(
      `${label}.crossLanguageEvidence claims no-onchain-counterpart but the row publishes no identity symbol the claim could be checked against`,
    );
    return;
  }
  const counterparts = identitySymbols.filter(namedInAiken).sort();
  if (counterparts.length > 0) {
    fail(
      `${label}.crossLanguageEvidence claims no-onchain-counterpart, but the Aiken corpus names ${String(
        counterparts.length,
      )} of this row's ${String(identitySymbols.length)} identity symbols: ${counterparts.join(
        ", ",
      )}`,
    );
  }
};

// ---------------------------------------------------------------------------
// Exact-field pins (V-6, issue #530)
//
// `exactFields` used to be unvalidated prose everywhere except N05 and N08,
// and even those two pins were guarded by `isPass(id)` — so demoting a row's
// own `auditStatus` switched off the check that its wire layout had not moved.
// The guards are gone, and two unconditional structural rules now apply to
// every row:
//
//   1. Positional pins must be exact. A field list that indexes its entries
//      (`0:`, `1:`, `3..5:`) must cover a dense range from 0 with no gap,
//      overlap or reordering, and must total the declared `arrayArity`.
//   2. A row with a cross-language counterpart that declares an integer
//      `arrayArity` must pin its fields positionally, so the two languages are
//      compared against an ordered layout rather than a bag of labels.
//
// Rule 2 is not yet satisfiable for seven forms whose published field lists
// are prose summaries rather than per-index pins. Those forms are named in the
// verifier-owned register below, and the register is checked for equality
// against the derived set: a new prose form fails the gate, and a form that is
// fixed but left in the register fails it too.
// ---------------------------------------------------------------------------

const exactFieldPinDeficits = new Map([
  [
    "D16/DaAttestationsByHeaderRequestV1 / DaAttestationsByHeaderResponseV1",
    "one form carries the request and response layouts together, so its eight labels cannot index a single four-member arity",
  ],
  [
    "K13/MidgardCekProgramUplcVersionV1",
    "the published list carries a fourth encoding-rule label beyond the three version members",
  ],
  [
    "V12/ScriptSourcesControlV1 family",
    "the thirty ordered control fields are published as a two-line prose summary",
  ],
  [
    "V13/NativeScriptsControlV1",
    "the twenty-six ordered fold/control fields are published as a three-line prose summary",
  ],
  [
    "V14/CEK context control family V1",
    "the twenty-four ordered context fields are published as a two-line prose summary",
  ],
  [
    "V15/Value-and-mint controls V1",
    "the twelve ordered value-and-mint fields are published as a three-line prose summary",
  ],
  [
    "V16/Ledger delta controls and operations V1",
    "eleven labels are published for fourteen ordered control fields",
  ],
]);
// Pure: expands `0:`/`3..5:` prefixes into the index span each entry pins, or
// null when the entry is not a positional pin at all.
const parsePositionalPin = (entry) => {
  const match = /^(\d+)(?:\s*\.\.\s*(\d+))?\s*:/u.exec(entry);
  if (match === null) return null;
  const start = Number(match[1]);
  const end = match[2] === undefined ? start : Number(match[2]);
  return end < start ? null : { start, end };
};
const verifyExactFieldPins = (row, form, formLabel, hasCrossLanguageTests) => {
  const fields = Array.isArray(form.exactFields) ? form.exactFields : [];
  const pins = fields.map(parsePositionalPin);
  const positional = fields.length > 0 && pins.some((pin) => pin !== null);
  const deficitKey = `${row.id}/${form.name}`;
  if (positional) {
    let next = 0;
    let dense = true;
    for (const [index, pin] of pins.entries()) {
      if (pin === null) {
        fail(
          `${formLabel}.exactFields mixes positional pins with an unpinned entry at ${String(
            index,
          )}: ${JSON.stringify(fields[index])}`,
        );
        dense = false;
        break;
      }
      if (pin.start !== next) {
        fail(
          `${formLabel}.exactFields pins index ${String(pin.start)} where index ${String(
            next,
          )} was required, so the layout has a gap, overlap, or reordering`,
        );
        dense = false;
        break;
      }
      next = pin.end + 1;
    }
    if (
      dense &&
      Number.isInteger(form.arrayArity) &&
      next !== form.arrayArity
    ) {
      fail(
        `${formLabel}.exactFields pins ${String(next)} members for declared arrayArity ${String(
          form.arrayArity,
        )}`,
      );
    }
    return { deficitKey, deficient: false };
  }
  if (
    hasCrossLanguageTests &&
    Number.isInteger(form.arrayArity) &&
    form.arrayArity > 0
  ) {
    if (!exactFieldPinDeficits.has(deficitKey)) {
      fail(
        `${formLabel} has a cross-language counterpart and a declared arrayArity ${String(
          form.arrayArity,
        )}, so its exactFields must pin each member by index`,
      );
    }
    return { deficitKey, deficient: true };
  }
  return { deficitKey, deficient: false };
};

const verifyUnverifiedRow = (row, label) => {
  if (row.auditStatus !== "UNVERIFIED") {
    fail(`${label}.auditStatus must be UNVERIFIED for an incomplete row`);
  }
  if (row.disposition !== "UNVERIFIED") {
    fail(`${label}.disposition must be UNVERIFIED for an incomplete row`);
  }
  for (const field of [
    "sourceEvidence",
    "canonicalForms",
    "positiveEvidence",
    "rejectionEvidence",
    "obsoleteBranchEvidence",
    "notes",
  ]) {
    if (!Array.isArray(row[field]) || row[field].length !== 0) {
      fail(`${label}.${field} must be an empty array for an incomplete row`);
    }
  }
  const crossLanguage = row.crossLanguageEvidence;
  if (
    crossLanguage === null ||
    typeof crossLanguage !== "object" ||
    Array.isArray(crossLanguage)
  ) {
    fail(
      `${label}.crossLanguageEvidence must be an object for an incomplete row`,
    );
    return;
  }
  if (crossLanguage.status !== "UNVERIFIED") {
    fail(
      `${label}.crossLanguageEvidence.status must be UNVERIFIED for an incomplete row`,
    );
  }
  if (!Array.isArray(crossLanguage.tests) || crossLanguage.tests.length !== 0) {
    fail(
      `${label}.crossLanguageEvidence.tests must be empty for an incomplete row`,
    );
  }
  if (crossLanguage.notApplicableReason !== null) {
    fail(
      `${label}.crossLanguageEvidence.notApplicableReason must be null for an incomplete row`,
    );
  }
  if (crossLanguage.notApplicableCategory !== undefined) {
    fail(
      `${label}.crossLanguageEvidence.notApplicableCategory must be absent for an incomplete row`,
    );
  }
};

let registry;
try {
  registry = JSON.parse(readFileSync(registryPath, "utf8"));
} catch (error) {
  fail(`registry is missing or invalid JSON: ${error.message}`);
}

if (registry !== undefined) {
  if (registry.schema !== "midgard-canonical-v1-format-registry-v1") {
    fail("registry.schema must be midgard-canonical-v1-format-registry-v1");
  }
  if (registry.version !== 1) {
    fail("registry.version must be the number 1");
  }
  if (registry.source !== canonicalSourcePath) {
    fail(`registry.source must be ${canonicalSourcePath}`);
  }
  if (registry.unknownFormatPolicy !== "reject") {
    fail("registry.unknownFormatPolicy must be reject");
  }
  const authorityKeys =
    registry.authority !== null &&
    typeof registry.authority === "object" &&
    !Array.isArray(registry.authority)
      ? Object.keys(registry.authority).sort()
      : [];
  if (
    JSON.stringify(authorityKeys) !==
      JSON.stringify(["sourcePath", "sourceRevision"]) ||
    registry.authority?.sourcePath !== canonicalAuthority.sourcePath ||
    registry.authority?.sourceRevision !== canonicalAuthority.sourceRevision
  ) {
    fail("registry.authority must equal the canonical Git authority");
  }
  verifyGitAuthority(registry.authority, "registry.authority");
  verifyAbsenceScans(canonicalAbsenceScans);
  if (
    JSON.stringify(registry.forbiddenActivePatterns) !==
    JSON.stringify(canonicalAbsenceScans)
  ) {
    fail(
      "registry.forbiddenActivePatterns must exactly equal the canonical absence scan table",
    );
  }
  if (!Array.isArray(registry.formats)) {
    fail("registry.formats must be an array");
  } else {
    const actualIds = registry.formats.map((row) => row?.id);
    const duplicateIds = actualIds.filter(
      (id, index) => actualIds.indexOf(id) !== index,
    );
    if (duplicateIds.length > 0) {
      fail(
        `registry has duplicate IDs: ${[...new Set(duplicateIds)].join(", ")}`,
      );
    }
    if (JSON.stringify(actualIds) !== JSON.stringify(expectedIds)) {
      const missing = expectedIds.filter((id) => !actualIds.includes(id));
      const extra = actualIds.filter((id) => !expectedIds.includes(id));
      fail(
        `registry IDs/order differ from Markdown (missing: ${missing.join(", ") || "none"}; extra: ${extra.join(", ") || "none"})`,
      );
    }

    for (const [rowIndex, row] of registry.formats.entries()) {
      const label = row?.id ?? `<row-${rowIndex}>`;
      const hasCrossLanguageTests =
        Array.isArray(row?.crossLanguageEvidence?.tests) &&
        row.crossLanguageEvidence.tests.length > 0;
      if (row === null || typeof row !== "object" || Array.isArray(row)) {
        fail(`${label} must be an object`);
        continue;
      }
      if (row.id !== expectedIds[rowIndex]) {
        fail(
          `${label}.id must be ${expectedIds[rowIndex] ?? "absent"} at row ${rowIndex + 1}`,
        );
      }
      if (row.family !== expectedIds[rowIndex]?.[0]) {
        fail(`${label}.family must match its canonical ID`);
      }
      requireNonEmptyString(
        row.sourceOwnerAndCurrentIdentity,
        `${label}.source`,
      );
      requireNonEmptyString(row.classification, `${label}.classification`);
      requireNonEmptyString(row.canonicalResult, `${label}.canonicalResult`);
      requireNonEmptyString(
        row.boundaryBindingAndPersistence,
        `${label}.boundaryBindingAndPersistence`,
      );
      requireNonEmptyString(row.requiredEvidence, `${label}.requiredEvidence`);

      if (row.auditStatus === "UNVERIFIED") {
        verifyUnverifiedRow(row, label);
        if (!allowIncomplete) {
          fail(`${label}.auditStatus must be PASS`);
        }
        continue;
      }
      if (row.auditStatus !== "PASS") {
        fail(`${label}.auditStatus must be PASS`);
        continue;
      }
      if (JSON.stringify(row).includes("UNVERIFIED")) {
        fail(`${label} is PASS but still contains UNVERIFIED`);
      }
      if (
        ![
          "retain",
          "reset-to-v1",
          "delete",
          "external",
          "semantic",
          "sentinel",
        ].includes(row.disposition)
      ) {
        fail(`${label}.disposition is not a final disposition`);
      }

      verifyReferences(
        row.sourceEvidence,
        `${label}.sourceEvidence`,
        "symbols",
      );
      verifyReferences(
        row.positiveEvidence,
        `${label}.positiveEvidence`,
        "testNames",
      );
      verifyReferences(
        row.rejectionEvidence,
        `${label}.rejectionEvidence`,
        "testNames",
      );
      verifyAbsenceEvidence(
        row.obsoleteBranchEvidence,
        `${label}.obsoleteBranchEvidence`,
      );

      if (row.disposition === "delete") {
        if (
          !Array.isArray(row.canonicalForms) ||
          row.canonicalForms.length !== 0
        ) {
          fail(`${label}.canonicalForms must be empty for a deleted format`);
        }
        if (
          !Array.isArray(row.obsoleteBranchEvidence) ||
          row.obsoleteBranchEvidence.length === 0
        ) {
          fail(`${label} deletion requires obsoleteBranchEvidence`);
        }
      } else {
        if (
          !Array.isArray(row.sourceEvidence) ||
          row.sourceEvidence.length === 0
        ) {
          fail(`${label} requires sourceEvidence`);
        }
        if (
          !Array.isArray(row.canonicalForms) ||
          row.canonicalForms.length === 0
        ) {
          fail(`${label} requires at least one canonical form`);
        } else {
          for (const [index, form] of row.canonicalForms.entries()) {
            const formLabel = `${label}.canonicalForms[${index}]`;
            for (const field of [
              "name",
              "kind",
              "wireRepresentation",
              "versionPolicy",
              "boundary",
              "binding",
              "persistence",
            ]) {
              requireNonEmptyString(form[field], `${formLabel}.${field}`);
            }
            if (
              (!Array.isArray(form.exactFields) ||
                form.exactFields.length === 0) &&
              !isNonEmptyString(form.exactFieldsNotApplicable)
            ) {
              fail(
                `${formLabel} requires exactFields or exactFieldsNotApplicable`,
              );
            }
            if (
              (!Array.isArray(form.constructorTags) ||
                form.constructorTags.length === 0) &&
              !isNonEmptyString(form.constructorTagsNotApplicable)
            ) {
              fail(
                `${formLabel} requires constructorTags or constructorTagsNotApplicable`,
              );
            }
            if (
              !Number.isInteger(form.arrayArity) &&
              !isNonEmptyString(form.arrayArityNotApplicable)
            ) {
              fail(
                `${formLabel} requires integer arrayArity or arrayArityNotApplicable`,
              );
            }
            const pinOutcome = verifyExactFieldPins(
              row,
              form,
              formLabel,
              hasCrossLanguageTests,
            );
            if (pinOutcome.deficient)
              observedExactFieldPinDeficits.add(pinOutcome.deficitKey);
            verifyReferences(form.encoders, `${formLabel}.encoders`, "symbols");
            verifyReferences(form.parsers, `${formLabel}.parsers`, "symbols");
          }
        }
        if (
          !Array.isArray(row.positiveEvidence) ||
          row.positiveEvidence.length === 0
        ) {
          fail(`${label} requires positiveEvidence`);
        }
        if (
          row.classification !== "external" &&
          (!Array.isArray(row.rejectionEvidence) ||
            row.rejectionEvidence.length === 0)
        ) {
          fail(`${label} requires rejectionEvidence`);
        }
      }

      const crossLanguage = row.crossLanguageEvidence;
      if (crossLanguage?.status !== "PASS") {
        fail(`${label}.crossLanguageEvidence.status must be PASS`);
      } else if (
        Array.isArray(crossLanguage.tests) &&
        crossLanguage.tests.length > 0
      ) {
        crossLanguageTestedIds.add(row.id);
        verifyReferences(
          crossLanguage.tests,
          `${label}.crossLanguageEvidence.tests`,
          "testNames",
        );
        if (crossLanguage.notApplicableCategory !== undefined) {
          fail(
            `${label}.crossLanguageEvidence carries cross-language tests and must not also claim a not-applicable category`,
          );
        }
      } else {
        crossLanguageNotApplicableIds.add(row.id);
        verifyCrossLanguageNotApplicable(row, label);
      }
    }

    const l01Contract = registry.formats.find((row) => row?.id === "L01")
      ?.canonicalForms?.[0];
    if (l01Contract === undefined) {
      fail("L01 requires the machine-readable HeaderV1 ABI contract");
    } else {
      try {
        assertHeaderV1AbiContract(l01Contract);
      } catch (error) {
        fail(`L01 HeaderV1 ABI contract is invalid: ${error.message}`);
      }
    }

    const canonicalFieldsFor = (id) =>
      registry.formats.find((row) => row.id === id)?.canonicalForms?.[0]
        ?.exactFields;
    const expectedN05Fields = [
      "0: canonical address-witness collection commitment hash32",
      "1: canonical script-witness collection commitment hash32",
      "2: canonical redeemer collection commitment hash32",
    ];
    const expectedN08Fields = [
      "0: spend-inputs preimage byte length",
      "1: reference-inputs preimage byte length",
      "2: outputs preimage byte length",
      "3: required-observers preimage byte length",
      "4: required-signers preimage byte length",
      "5: mint preimage byte length",
      "6: script-witnesses preimage byte length",
      "7: vkey-witnesses preimage byte length",
      "8: redeemers preimage byte length",
    ];
    // Unconditional: a row may not switch off its own layout pin by demoting
    // its auditStatus, which the previous `isPass(id) && ...` guards allowed.
    if (
      JSON.stringify(canonicalFieldsFor("N05")) !==
      JSON.stringify(expectedN05Fields)
    ) {
      fail(
        "N05 must preserve the stable compact witness tuple [address, script, redeemer]",
      );
    }
    if (
      JSON.stringify(canonicalFieldsFor("N08")) !==
      JSON.stringify(expectedN08Fields)
    ) {
      fail(
        "N08 must preserve transaction field order [spend, reference, outputs, observers, signers, mint, script, vkey, redeemer]",
      );
    }

    // The exact-field pin register is verifier-owned and must equal the set the
    // gate just derived: a newly prose-only form fails, and a form that has
    // been pinned but left in the register fails too.
    const staleDeficits = [...exactFieldPinDeficits.keys()]
      .filter((key) => !observedExactFieldPinDeficits.has(key))
      .sort();
    if (staleDeficits.length > 0) {
      fail(
        `exact-field pin register lists forms that now pin their members and must be removed from it: ${staleDeficits.join(
          "; ",
        )}`,
      );
    }

    // The cross-language split is re-derived here rather than trusted, and the
    // artifact's published summary must equal the derivation.
    const derivedCrossLanguageSummary = {
      rows: registry.formats.length,
      withCounterpartTests: crossLanguageTestedIds.size,
      notApplicable: crossLanguageNotApplicableIds.size,
      notApplicableByCategory: Object.fromEntries(
        [...crossLanguageNotApplicableByCategory.entries()].sort(([a], [b]) =>
          a < b ? -1 : 1,
        ),
      ),
      exactFieldPinDeficits: [...observedExactFieldPinDeficits].sort(),
    };
    if (
      JSON.stringify(registry.crossLanguageSummary) !==
      JSON.stringify(derivedCrossLanguageSummary)
    ) {
      fail(
        `registry.crossLanguageSummary does not equal the derived split; derived: ${JSON.stringify(
          derivedCrossLanguageSummary,
        )}`,
      );
    }
  }
}

if (errors.length > 0) {
  process.stderr.write(
    `Canonical V1 format registry verification failed (${errors.length}):\n`,
  );
  for (const error of errors) {
    process.stderr.write(`- ${error}\n`);
  }
  process.exit(1);
}

process.stdout.write(
  `Canonical V1 format registry verified: ${expectedIds.length} rows${allowIncomplete ? " (incomplete rows allowed)" : ""}.\n`,
);

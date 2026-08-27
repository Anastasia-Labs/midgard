import { createHash } from "node:crypto";

import {
  DA_ATTESTATION_ASSET_NAME_PREFIX,
  DaAttestationDatum,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
} from "@al-ft/midgard-sdk";
import { CML, Data, valueToAssets } from "@lucid-evolution/lucid";

import {
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestV1FraudProofCatalogueCategory,
  type DeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  type VerifiedWatcherDeploymentIdentityV1,
  verifyWatcherDeploymentIdentityV1,
  type WatcherDeploymentIdentityPolicyV1,
  type WatcherDeploymentTrustRootV1,
} from "./deployment-identity.js";
import {
  encodeWatcherDurableStoreV1,
  journalWatcherProtocolUtxoTransitionV1,
  parseWatcherDurableStoreV1,
  type WatcherConfirmationV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
  type WatcherProtocolUtxoV1,
} from "./durable-store.js";
import {
  evaluateWatcherFinalityV1,
  parseWatcherFinalityPolicyV1,
  watcherFinalityConfiguredSourceV1,
  type WatcherFinalityResultV1,
} from "./finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  normalizeWatcherL1BlockV1,
  type WatcherL1RedeemerV1,
  type WatcherL1TransactionV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
  type WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistencyV1,
  type WatcherMultiProviderConsistencyV1,
} from "./multi-provider-consistency.js";
import {
  parseWatcherPostFinalityRecoveryResultV1,
  parseWatcherRollbackResultV1,
  type WatcherPostFinalityRecoveryInputV1,
  type WatcherPostFinalityRecoveryResultV1,
  type WatcherRollbackResultV1,
  type WatcherRollbackVerificationContextV1,
} from "./rollback-engine.js";

export const WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-policy-v1" as const;
export const WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-journal-v1" as const;
export const WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-layout-v1" as const;
export const WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-observation-v1" as const;
export const WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-public-context-v1" as const;
export const WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-state-v1" as const;
export const WATCHER_PROOF_THREAD_RESULT_V1_SCHEMA_VERSION =
  "midgard-watcher-proof-thread-result-v1" as const;

export const WATCHER_PROOF_THREAD_V1_BOUNDS = Object.freeze({
  families: 128,
  stepsPerFamily: 512,
  historyEntries: 256,
  auditEntries: 256,
  transitionEntries: 512,
  evidenceContainerEntries: 16_384,
  finalityLineageSteps: 2_160,
  observationsPerFinalityStep: 16,
  cumulativeEvidenceBytes: 134_217_728,
  cumulativeEvidenceNodes: 2_000_000,
  uint64Maximum: 18_446_744_073_709_551_615n,
});

/**
 * The signed proof-thread deployment authority must name every registered
 * catalogue family and every validator that can own its computation thread.
 * These counts describe deployed validator topology only; they do not claim
 * that an autonomous watcher detector exists for the corresponding fault.
 */
export const WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1 = Object.freeze({
  doubleSpend: {
    familyId: "double-spend",
    stepCount: 4,
    deployedStepContractNames: ["fraudProofDoubleSpend"],
  },
  nonExistentInput: {
    familyId: "non-existent-input",
    stepCount: 4,
    deployedStepContractNames: ["fraudProofNonExistentInput"],
  },
  nonExistentInputNoIndex: {
    familyId: "non-existent-input-no-index",
    stepCount: 4,
    deployedStepContractNames: ["fraudProofNonExistentInputNoIndex"],
  },
  invalidRange: {
    familyId: "invalid-range",
    stepCount: 2,
    deployedStepContractNames: ["fraudProofInvalidRange"],
  },
  transitionTrace: {
    familyId: "transition-trace",
    stepCount: 9,
    deployedStepContractNames: [
      "fraudProofTransitionTrace",
      "fraudProofTransitionTraceControl",
      "fraudProofTransitionTraceSource",
      "fraudProofTransitionTraceWithdrawal",
      "fraudProofTransitionTraceForced",
      "fraudProofTransitionTraceAcceptedTransaction",
      "fraudProofTransitionTraceDeposit",
      "fraudProofTransitionTraceL1Event",
      "fraudProofTransitionTraceDuplicate",
    ],
  },
  zeroInput: {
    familyId: "zero-input",
    stepCount: 2,
    deployedStepContractNames: ["fraudProofZeroInput"],
  },
  validationTraceDispute: {
    familyId: "validation-trace-dispute",
    stepCount: 121,
    deployedStepContractNames: [
      "validationTraceDispute",
      "validationTraceDisputeSource",
      "validationTraceDisputeGame",
      "validationTraceDisputeBoundary",
      "validationTraceDisputeTimeout",
      "validationTraceDisputeAward",
    ],
  },
  daHashPreimage: {
    familyId: "da-hash-preimage",
    stepCount: 2,
    deployedStepContractNames: ["fraudProofDaHashPreimage"],
  },
  noReferenceInput: {
    familyId: "no-reference-input",
    stepCount: 4,
    deployedStepContractNames: ["fraudProofNoReferenceInput"],
  },
  referenceInputNoIdx: {
    familyId: "reference-input-no-idx",
    stepCount: 4,
    deployedStepContractNames: ["fraudProofReferenceInputNoIdx"],
  },
  invalidSignature: {
    familyId: "invalid-signature",
    stepCount: 2,
    deployedStepContractNames: ["fraudProofInvalidSignature"],
  },
  fabricatedDeposit: {
    familyId: "fabricated-deposit",
    stepCount: 4,
    deployedStepContractNames: [
      "fraudProofFabricatedDeposit",
      "fraudProofFabricatedDepositStep02",
      "fraudProofFabricatedDepositStep03",
      "fraudProofFabricatedDepositStep04",
    ],
  },
  fabricatedWithdrawal: {
    familyId: "fabricated-withdrawal",
    stepCount: 4,
    deployedStepContractNames: [
      "fraudProofFabricatedWithdrawal",
      "fraudProofFabricatedWithdrawalStep02",
      "fraudProofFabricatedWithdrawalStep03",
      "fraudProofFabricatedWithdrawalStep04",
    ],
  },
  nativeScriptDecoding: {
    familyId: "native-script-decoding",
    stepCount: 4,
    deployedStepContractNames: [
      "fraudProofNativeScriptDecoding",
      "fraudProofNativeScriptDecodingStep02",
      "fraudProofNativeScriptDecodingStep03",
      "fraudProofNativeScriptDecodingStep04",
    ],
  },
  missingSignature: {
    familyId: "missing-signature",
    stepCount: 4,
    deployedStepContractNames: [
      "fraudProofMissingSignature",
      "fraudProofMissingSignatureStep02",
      "fraudProofMissingSignatureStep03",
      "fraudProofMissingSignatureStep04",
    ],
  },
  missingNativeScriptTx: {
    familyId: "missing-native-script-tx",
    stepCount: 6,
    deployedStepContractNames: [
      "fraudProofMissingNativeScriptTx",
      "fraudProofMissingNativeScriptTxStep02",
      "fraudProofMissingNativeScriptTxStep03",
      "fraudProofMissingNativeScriptTxStep04",
      "fraudProofMissingNativeScriptTxStep05",
      "fraudProofMissingNativeScriptTxStep06",
    ],
  },
  withdrawnReferenceInput: {
    familyId: "withdrawn-reference-input",
    stepCount: 3,
    deployedStepContractNames: [
      "fraudProofWithdrawnReferenceInput",
      "fraudProofWithdrawnReferenceInputStep02",
      "fraudProofWithdrawnReferenceInputStep03",
    ],
  },
  canonicalDecodability: {
    familyId: "canonical-decodability",
    stepCount: 2,
    deployedStepContractNames: [
      "fraudProofCanonicalDecodability",
      "fraudProofCanonicalDecodabilityStep02",
    ],
  },
  committedFieldShape: {
    familyId: "committed-field-shape",
    stepCount: 2,
    deployedStepContractNames: [
      "fraudProofCommittedFieldShape",
      "fraudProofCommittedFieldShapeStep02",
    ],
  },
  minFee: {
    familyId: "min-fee",
    stepCount: 2,
    deployedStepContractNames: ["fraudProofMinFee", "fraudProofMinFeeStep02"],
  },
  withdrawalMistag: {
    familyId: "withdrawal-mistag",
    stepCount: 5,
    deployedStepContractNames: [
      "fraudProofWithdrawalMistag",
      "fraudProofWithdrawalMistagStep02",
      "fraudProofWithdrawalMistagStep03",
      "fraudProofWithdrawalMistagStep04",
      "fraudProofWithdrawalMistagStep05",
    ],
  },
  doubleWithdraw: {
    familyId: "double-withdraw",
    stepCount: 2,
    deployedStepContractNames: [
      "fraudProofDoubleWithdraw",
      "fraudProofDoubleWithdrawStep02",
    ],
  },
  crossBlockDuplicateEvent: {
    familyId: "cross-block-duplicate-event",
    stepCount: 2,
    deployedStepContractNames: [
      "fraudProofCrossBlockDuplicateEvent",
      "fraudProofCrossBlockDuplicateEventStep02",
    ],
  },
  l2TxMistag: {
    familyId: "l2-tx-mistag",
    stepCount: 2,
    deployedStepContractNames: [
      "fraudProofL2TxMistag",
      "fraudProofL2TxMistagStep02",
    ],
  },
  withdrawnInput: {
    familyId: "withdrawn-input",
    stepCount: 3,
    deployedStepContractNames: [
      "fraudProofWithdrawnInput",
      "fraudProofWithdrawnInputStep02",
      "fraudProofWithdrawnInputStep03",
    ],
  },
} as const satisfies Readonly<
  Record<
    DeploymentManifestV1FraudProofCatalogueCategory,
    Readonly<{
      familyId: string;
      stepCount: number;
      deployedStepContractNames: readonly string[];
    }>
  >
>);

export const WATCHER_PROOF_THREAD_REASON_CODES_V1 = [
  "init_pending",
  "init_confirmed",
  "step_pending",
  "step_confirmed",
  "success_pending",
  "success_confirmed",
  "cancel_pending",
  "cancel_confirmed",
  "removal_pending",
  "removal_confirmed",
  "rollback_confirmed",
  "duplicate_observation",
  "malformed_policy",
  "malformed_state",
  "malformed_observation",
  "malformed_public_context",
  "deployment_mismatch",
  "public_evidence_mismatch",
  "durable_evidence_mismatch",
  "journal_mismatch",
  "transaction_mismatch",
  "lifecycle_mismatch",
  "rollback_mismatch",
  "rollback_authority_mismatch",
  "post_finality_quarantine",
  "stale_state",
  "history_limit_exceeded",
] as const;

export const WATCHER_PROOF_THREAD_ALERT_CODES_V1 = [
  "watcher_proof_thread_input_rejected",
  "watcher_proof_thread_binding_rejected",
  "watcher_proof_thread_state_rejected",
  "watcher_proof_thread_transition_rejected",
  "watcher_proof_thread_post_finality_incident",
] as const;

export type WatcherProofThreadReasonCodeV1 =
  (typeof WATCHER_PROOF_THREAD_REASON_CODES_V1)[number];
export type WatcherProofThreadAlertCodeV1 =
  (typeof WATCHER_PROOF_THREAD_ALERT_CODES_V1)[number];
export type WatcherProofThreadNetworkV1 = "Mainnet" | "Preprod" | "Preview";
export type WatcherProofThreadTransitionKindV1 =
  | "init"
  | "step"
  | "success"
  | "cancel"
  | "removal"
  | "rollback";
export type WatcherProofThreadJournalPhaseV1 =
  | "active"
  | "proven"
  | "cancelled"
  | "removed";
export type WatcherProofThreadConfirmationPhaseV1 = "pending" | "final";

export type WatcherProofThreadFamilyV1 = Readonly<{
  familyId: string;
  catalogueCategory: string;
  categoryId: string;
  firstStepScriptHash: string;
  /**
   * Canonical deployment order for every validator that can own the
   * computation-thread token for this family. This is an authority surface,
   * not a claim that the watcher autonomously detects the underlying fault.
   */
  stepScriptHashes: readonly string[];
  /**
   * Forward-only adjacency list over `stepScriptHashes`. Empty rows are the
   * validators that may finalize. Keeping the graph explicit matters for
   * routed families such as transition-trace: its route validator can select
   * any one of eight finals; those finals are not a linear eight-step chain.
   */
  nextStepIndexes: readonly (readonly string[])[];
}>;

export type WatcherProofThreadPolicyV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION;
  network: WatcherProofThreadNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  deploymentTrustRootId: string;
  requiredFinalityDepth: string;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofSpendScriptHash: string;
  fraudProofAddressHex: string;
  families: readonly WatcherProofThreadFamilyV1[];
  maximumHistoryEntries: string;
  policyDigest: string;
}>;

export type WatcherProofThreadJournalV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION;
  journalId: string;
  faultId: string;
  familyId: string;
  fraudulentBlockHash: string;
  fraudulentHeaderHash: string;
  fraudProver: string;
  computationThreadAssetName: string;
  phase: WatcherProofThreadJournalPhaseV1;
  stepIndex: string | null;
  threadOutRef: string | null;
  proofTokenOutRef: string | null;
  lastSubmissionId: string;
  lastConfirmationId: string;
  correctionId: string | null;
  confirmedTransactionHashes: readonly string[];
  journalDigest: string;
}>;

/**
 * Every index names Cardano's canonical position in the decoded body or the
 * globally ordered W10 redeemer vector. Inapplicable positions are null, so a
 * constructor cannot smuggle an extra role through an ignored numeric field.
 */
export type WatcherProofThreadLayoutV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION;
  threadInputIndex: string | null;
  threadOutputIndex: string | null;
  proofTokenOutputIndex: string | null;
  proofTokenReferenceInputIndex: string | null;
  stepSpendRedeemerGlobalIndex: string | null;
  computationThreadMintRedeemerGlobalIndex: string | null;
  fraudProofMintRedeemerGlobalIndex: string | null;
}>;

export type WatcherProofThreadObservationV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherProofThreadNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  transitionKind: WatcherProofThreadTransitionKindV1;
  confirmationPhase: WatcherProofThreadConfirmationPhaseV1 | null;
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionHash: string | null;
  publicInputDigest: string;
  sourceObservationDigest: string;
  chainPointId: string;
  sourceDurableStoreDigest: string;
  sourceDurableStoreRevision: string;
  durableStoreDigest: string;
  durableStoreRevision: string;
  predecessorStateDigest: string | null;
  submissionId: string | null;
  confirmationId: string | null;
  rollbackTargetStateDigest: string | null;
  layout: WatcherProofThreadLayoutV1 | null;
  journal: WatcherProofThreadJournalV1 | null;
  observationDigest: string;
}>;

export type WatcherProofThreadPublicContextV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION;
  authenticatedProvider: unknown | null;
  l1Observation: unknown | null;
  sourceDurableStore: unknown;
  durableStore: unknown;
  deploymentAuthority: Readonly<{
    signedIdentity: unknown;
    policy: WatcherDeploymentIdentityPolicyV1;
    trustRoots: readonly WatcherDeploymentTrustRootV1[];
    result: VerifiedWatcherDeploymentIdentityV1;
  }>;
  finalityAuthority: Readonly<{
    policy: unknown;
    lineage: readonly Readonly<{
      observations: readonly unknown[];
      consistency: unknown;
      result: unknown;
    }>[];
    previousState: unknown;
    observations: readonly unknown[];
    consistency: unknown;
    result: unknown;
  }> | null;
  rollbackAuthority: Readonly<{
    result: unknown;
    context:
      | WatcherRollbackVerificationContextV1
      | WatcherPostFinalityRecoveryInputV1;
  }> | null;
  sourceJournal: unknown;
  durableJournal: unknown;
}>;

export type WatcherProofThreadHistoryEntryV1 = Readonly<{
  predecessorStateDigest: string | null;
  transitionKind: WatcherProofThreadTransitionKindV1;
  confirmationPhase: WatcherProofThreadConfirmationPhaseV1 | null;
  chainPointId: string;
  transactionHash: string | null;
  submissionId: string | null;
  confirmationId: string | null;
  sourceJournalDigest: string | null;
  journal: WatcherProofThreadJournalV1 | null;
  observation: WatcherProofThreadObservationV1;
  publicContext: WatcherProofThreadPublicContextV1;
  rollbackResult:
    | WatcherRollbackResultV1
    | WatcherPostFinalityRecoveryResultV1
    | null;
  entryDigest: string;
}>;

export type WatcherProofThreadAuditEntryV1 = Readonly<{
  status: "orphaned" | "rollback";
  entry: WatcherProofThreadHistoryEntryV1;
  auditDigest: string;
}>;

export type WatcherProofThreadPendingV1 = Readonly<{
  transitionKind: Exclude<WatcherProofThreadTransitionKindV1, "rollback">;
  transactionHash: string;
  submissionId: string;
  confirmationId: string;
  sourceJournalDigest: string | null;
  journal: WatcherProofThreadJournalV1;
  pendingEntryDigest: string;
}>;

export type WatcherProofThreadStateV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: WatcherProofThreadNetworkV1;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  pointDigest: string;
  durableStoreDigest: string;
  journal: WatcherProofThreadJournalV1 | null;
  pending: WatcherProofThreadPendingV1 | null;
  history: readonly WatcherProofThreadHistoryEntryV1[];
  auditHistory: readonly WatcherProofThreadAuditEntryV1[];
  /** Append-only accepted transitions; restart derives history/audit/head. */
  transitionHistory: readonly WatcherProofThreadHistoryEntryV1[];
  stateDigest: string;
}>;

export type WatcherProofThreadResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_THREAD_RESULT_V1_SCHEMA_VERSION;
  action: "accept" | "duplicate" | "reject";
  protocolDecision: "indexed" | "hold" | "quarantined";
  reasonCodes: readonly WatcherProofThreadReasonCodeV1[];
  alertCodes: readonly WatcherProofThreadAlertCodeV1[];
  state: WatcherProofThreadStateV1 | null;
  resultDigest: string;
}>;

type PlainRecord = Record<string, unknown>;
type CanonicalJson =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const HEX_4 = /^[0-9a-f]{8}$/u;
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const STABLE_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;
const CATALOGUE_CATEGORY = /^[a-z][A-Za-z0-9]*$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;

const canonicalJson = (value: CanonicalJson): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("noncanonical number");
    }
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map((member) => canonicalJson(member)).join(",")}]`;
  }
  const record = value as { readonly [key: string]: CanonicalJson };
  return `{${Object.keys(record)
    .sort()
    .map(
      (key) =>
        `${JSON.stringify(key)}:${canonicalJson(record[key] as CanonicalJson)}`,
    )
    .join(",")}}`;
};

const sha256Bytes = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const sha256Canonical = (value: unknown): string =>
  sha256Bytes(Buffer.from(canonicalJson(value as CanonicalJson), "utf8"));
const same = (left: unknown, right: unknown): boolean =>
  canonicalJson(left as CanonicalJson) ===
  canonicalJson(right as CanonicalJson);

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    return null;
  }
  const actual = Reflect.ownKeys(value);
  if (
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !keys.includes(key))
  ) {
    return null;
  }
  for (const key of keys) {
    const descriptor = Object.getOwnPropertyDescriptor(value, key);
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as PlainRecord;
};

const evidenceWithinBounds = (value: unknown): boolean => {
  const pending: { readonly value: unknown; readonly exiting: boolean }[] = [
    { value, exiting: false },
  ];
  const active = new Set<object>();
  let bytes = typeof value === "string" ? Buffer.byteLength(value, "utf8") : 0;
  let nodes = 1;
  if (
    bytes > WATCHER_PROOF_THREAD_V1_BOUNDS.cumulativeEvidenceBytes ||
    nodes > WATCHER_PROOF_THREAD_V1_BOUNDS.cumulativeEvidenceNodes
  ) {
    return false;
  }
  while (pending.length > 0) {
    const frame = pending.pop()!;
    const candidate = frame.value;
    if (frame.exiting) {
      active.delete(candidate as object);
      continue;
    }
    if (typeof candidate === "object" && candidate !== null) {
      if (active.has(candidate)) {
        return false;
      }
      active.add(candidate);
      const prototype = Object.getPrototypeOf(candidate);
      if (
        prototype !== Object.prototype &&
        prototype !== Array.prototype &&
        prototype !== null
      ) {
        return false;
      }
      if (
        Array.isArray(candidate) &&
        candidate.length >
          WATCHER_PROOF_THREAD_V1_BOUNDS.evidenceContainerEntries
      ) {
        return false;
      }
      const children: unknown[] = [];
      let entryCount = 0;
      for (const key in candidate) {
        if (!Object.prototype.hasOwnProperty.call(candidate, key)) {
          continue;
        }
        entryCount += 1;
        if (
          entryCount > WATCHER_PROOF_THREAD_V1_BOUNDS.evidenceContainerEntries
        ) {
          return false;
        }
        const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
        if (
          descriptor === undefined ||
          descriptor.get !== undefined ||
          descriptor.set !== undefined
        ) {
          return false;
        }
        bytes += Buffer.byteLength(key, "utf8");
        children.push(descriptor.value);
        nodes += 1;
        if (typeof descriptor.value === "string") {
          bytes += Buffer.byteLength(descriptor.value, "utf8");
        }
        if (
          bytes > WATCHER_PROOF_THREAD_V1_BOUNDS.cumulativeEvidenceBytes ||
          nodes > WATCHER_PROOF_THREAD_V1_BOUNDS.cumulativeEvidenceNodes
        ) {
          return false;
        }
      }
      pending.push({ value: candidate, exiting: true });
      for (let index = children.length - 1; index >= 0; index -= 1) {
        pending.push({ value: children[index], exiting: false });
      }
    }
  }
  return true;
};

const exactArray = (
  value: unknown,
  maximum: number,
): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    value.length > maximum ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value;
};

const isHex4 = (value: unknown): value is string =>
  typeof value === "string" && HEX_4.test(value);
const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);
const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
const isOutRef = (value: unknown): value is string =>
  typeof value === "string" && OUT_REF.test(value);
const isNatural = (value: unknown): value is string =>
  typeof value === "string" &&
  NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= WATCHER_PROOF_THREAD_V1_BOUNDS.uint64Maximum;
const isNullableNatural = (value: unknown): value is string | null =>
  value === null || isNatural(value);
const isNullableHex32 = (value: unknown): value is string | null =>
  value === null || isHex32(value);
const isNullableOutRef = (value: unknown): value is string | null =>
  value === null || isOutRef(value);
const isNetwork = (value: unknown): value is WatcherProofThreadNetworkV1 =>
  typeof value === "string" &&
  NETWORKS.includes(value as WatcherProofThreadNetworkV1);
const isStableName = (value: unknown): value is string =>
  typeof value === "string" && STABLE_NAME.test(value);

const parseMarker = (value: unknown): DeploymentMarkerV1 | null => {
  const record = exactRecord(value, ["schemaVersion", "manifestId"]);
  return record !== null &&
    record.schemaVersion === MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION &&
    isHex32(record.manifestId)
    ? Object.freeze({
        schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
        manifestId: record.manifestId,
      })
    : null;
};

const sameMarker = (
  left: DeploymentMarkerV1,
  right: DeploymentMarkerV1,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

const expectedNetworkId = (network: WatcherProofThreadNetworkV1): number =>
  network === "Mainnet" ? 1 : 0;

const addressMatchesScript = (
  addressHex: string,
  scriptHash: string,
  network: WatcherProofThreadNetworkV1,
): boolean => {
  try {
    const address = CML.Address.from_hex(addressHex);
    return (
      address.to_hex() === addressHex &&
      address.network_id() === expectedNetworkId(network) &&
      address.payment_cred()?.as_script()?.to_hex() === scriptHash
    );
  } catch {
    return false;
  }
};

const parseFamily = (value: unknown): WatcherProofThreadFamilyV1 | null => {
  const record = exactRecord(value, [
    "familyId",
    "catalogueCategory",
    "categoryId",
    "firstStepScriptHash",
    "stepScriptHashes",
    "nextStepIndexes",
  ]);
  const steps =
    record === null
      ? null
      : exactArray(
          record.stepScriptHashes,
          WATCHER_PROOF_THREAD_V1_BOUNDS.stepsPerFamily,
        );
  const nextStepIndexesValue =
    record === null
      ? null
      : exactArray(
          record.nextStepIndexes,
          WATCHER_PROOF_THREAD_V1_BOUNDS.stepsPerFamily,
        );
  const nextStepIndexes =
    nextStepIndexesValue?.map((next) =>
      exactArray(next, WATCHER_PROOF_THREAD_V1_BOUNDS.stepsPerFamily),
    ) ?? null;
  if (
    record === null ||
    steps === null ||
    nextStepIndexesValue === null ||
    nextStepIndexes === null ||
    !isStableName(record.familyId) ||
    typeof record.catalogueCategory !== "string" ||
    !CATALOGUE_CATEGORY.test(record.catalogueCategory) ||
    !isHex4(record.categoryId) ||
    !isHex28(record.firstStepScriptHash) ||
    steps.length === 0 ||
    steps.some((step) => !isHex28(step)) ||
    steps[0] !== record.firstStepScriptHash ||
    new Set(steps).size !== steps.length ||
    nextStepIndexes.length !== steps.length ||
    nextStepIndexes.some(
      (next, currentIndex) =>
        next === null ||
        next.some(
          (nextIndex) =>
            !isNatural(nextIndex) ||
            BigInt(nextIndex) <= BigInt(currentIndex) ||
            BigInt(nextIndex) >= BigInt(steps.length),
        ) ||
        next.some(
          (nextIndex, index) =>
            index > 0 &&
            BigInt(next[index - 1] as string) >= BigInt(nextIndex as string),
        ),
    )
  ) {
    return null;
  }
  const parsedNextStepIndexes =
    nextStepIndexes as readonly (readonly string[])[];
  const reachable = new Set([0]);
  for (let index = 0; index < parsedNextStepIndexes.length; index += 1) {
    if (!reachable.has(index)) {
      return null;
    }
    for (const nextIndex of parsedNextStepIndexes[index]!) {
      reachable.add(Number(nextIndex));
    }
  }
  if (
    reachable.size !== steps.length ||
    !parsedNextStepIndexes.some((next) => next.length === 0)
  ) {
    return null;
  }
  return Object.freeze({
    familyId: record.familyId,
    catalogueCategory: record.catalogueCategory,
    categoryId: record.categoryId,
    firstStepScriptHash: record.firstStepScriptHash,
    stepScriptHashes: Object.freeze(steps as string[]),
    nextStepIndexes: Object.freeze(
      parsedNextStepIndexes.map((next) => Object.freeze(next as string[])),
    ),
  });
};

const familyMatchesRegisteredAuthority = (
  family: WatcherProofThreadFamilyV1,
): boolean => {
  if (
    !Object.prototype.hasOwnProperty.call(
      WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1,
      family.catalogueCategory,
    )
  ) {
    return false;
  }
  const category =
    family.catalogueCategory as DeploymentManifestV1FraudProofCatalogueCategory;
  const categoryIndex =
    DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(
      category,
    );
  const authority = WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1[category];
  if (
    categoryIndex < 0 ||
    family.familyId !== authority.familyId ||
    family.categoryId !== categoryIndex.toString(16).padStart(8, "0") ||
    family.stepScriptHashes.length !== authority.stepCount ||
    family.nextStepIndexes.length !== authority.stepCount
  ) {
    return false;
  }
  if (category === "transitionTrace") {
    return family.nextStepIndexes.every((next, index) =>
      index === 0
        ? next.length === authority.stepCount - 1 &&
          next.every(
            (nextIndex, nextIndexOffset) =>
              nextIndex === (nextIndexOffset + 1).toString(),
          )
        : next.length === 0,
    );
  }
  return family.nextStepIndexes.every((next, index) =>
    index === authority.stepCount - 1
      ? next.length === 0
      : next.length === 1 && next[0] === (index + 1).toString(),
  );
};

export const makeWatcherProofThreadPolicyV1 = (
  value: Omit<WatcherProofThreadPolicyV1, "schemaVersion" | "policyDigest">,
): WatcherProofThreadPolicyV1 | null => {
  const canonical = {
    schemaVersion: WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherProofThreadPolicyV1({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherProofThreadPolicyV1 = (
  value: unknown,
): WatcherProofThreadPolicyV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "deploymentTrustRootId",
    "requiredFinalityDepth",
    "computationThreadPolicyId",
    "fraudProofPolicyId",
    "fraudProofSpendScriptHash",
    "fraudProofAddressHex",
    "families",
    "maximumHistoryEntries",
    "policyDigest",
  ]);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  const familiesValue =
    record === null
      ? null
      : exactArray(record.families, WATCHER_PROOF_THREAD_V1_BOUNDS.families);
  const families = familiesValue?.map(parseFamily) ?? null;
  if (
    record === null ||
    marker === null ||
    familiesValue === null ||
    families === null ||
    families.length === 0 ||
    families.some((family) => family === null) ||
    record.schemaVersion !== WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    !isHex32(record.deploymentTrustRootId) ||
    !isNatural(record.requiredFinalityDepth) ||
    record.requiredFinalityDepth === "0" ||
    !isHex28(record.computationThreadPolicyId) ||
    !isHex28(record.fraudProofPolicyId) ||
    !isHex28(record.fraudProofSpendScriptHash) ||
    typeof record.fraudProofAddressHex !== "string" ||
    !HEX_BYTES.test(record.fraudProofAddressHex) ||
    !isNatural(record.maximumHistoryEntries) ||
    record.maximumHistoryEntries === "0" ||
    BigInt(record.maximumHistoryEntries) >
      BigInt(WATCHER_PROOF_THREAD_V1_BOUNDS.historyEntries) ||
    !isHex32(record.policyDigest)
  ) {
    return null;
  }
  const parsedFamilies = families as WatcherProofThreadFamilyV1[];
  const familyByCategory = new Map(
    parsedFamilies.map((family) => [family.catalogueCategory, family]),
  );
  if (
    !parsedFamilies.every(
      (family, index) =>
        index === 0 || parsedFamilies[index - 1]!.familyId < family.familyId,
    ) ||
    new Set(parsedFamilies.map(({ categoryId }) => categoryId)).size !==
      parsedFamilies.length ||
    new Set(parsedFamilies.map(({ catalogueCategory }) => catalogueCategory))
      .size !== parsedFamilies.length ||
    parsedFamilies.length !==
      DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
    !DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.every(
      (category) => {
        const family = familyByCategory.get(category);
        return family !== undefined && familyMatchesRegisteredAuthority(family);
      },
    ) ||
    !addressMatchesScript(
      record.fraudProofAddressHex,
      record.fraudProofSpendScriptHash,
      record.network,
    )
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_POLICY_V1_SCHEMA_VERSION,
    network: record.network,
    releaseEvidenceDigest: record.releaseEvidenceDigest,
    deploymentMarker: marker,
    deploymentTrustRootId: record.deploymentTrustRootId,
    requiredFinalityDepth: record.requiredFinalityDepth,
    computationThreadPolicyId: record.computationThreadPolicyId,
    fraudProofPolicyId: record.fraudProofPolicyId,
    fraudProofSpendScriptHash: record.fraudProofSpendScriptHash,
    fraudProofAddressHex: record.fraudProofAddressHex,
    families: Object.freeze(parsedFamilies),
    maximumHistoryEntries: record.maximumHistoryEntries,
  });
  return sha256Canonical(canonical) === record.policyDigest
    ? Object.freeze({ ...canonical, policyDigest: record.policyDigest })
    : null;
};

const journalWithoutDigest = (
  value: Omit<WatcherProofThreadJournalV1, "journalDigest">,
) => ({ ...value });

export const makeWatcherProofThreadJournalV1 = (
  value: Omit<WatcherProofThreadJournalV1, "schemaVersion" | "journalDigest">,
): WatcherProofThreadJournalV1 | null => {
  const canonical = {
    schemaVersion: WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherProofThreadJournalV1({
    ...canonical,
    journalDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherProofThreadJournalV1 = (
  value: unknown,
): WatcherProofThreadJournalV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "journalId",
    "faultId",
    "familyId",
    "fraudulentBlockHash",
    "fraudulentHeaderHash",
    "fraudProver",
    "computationThreadAssetName",
    "phase",
    "stepIndex",
    "threadOutRef",
    "proofTokenOutRef",
    "lastSubmissionId",
    "lastConfirmationId",
    "correctionId",
    "confirmedTransactionHashes",
    "journalDigest",
  ]);
  const hashes =
    record === null
      ? null
      : exactArray(
          record.confirmedTransactionHashes,
          WATCHER_PROOF_THREAD_V1_BOUNDS.historyEntries,
        );
  const phases: readonly WatcherProofThreadJournalPhaseV1[] = [
    "active",
    "proven",
    "cancelled",
    "removed",
  ];
  if (
    record === null ||
    hashes === null ||
    record.schemaVersion !== WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION ||
    !isHex32(record.journalId) ||
    !isHex32(record.faultId) ||
    !isStableName(record.familyId) ||
    !isHex32(record.fraudulentBlockHash) ||
    !isHex28(record.fraudulentHeaderHash) ||
    !isHex28(record.fraudProver) ||
    !isHex32(record.computationThreadAssetName) ||
    typeof record.phase !== "string" ||
    !phases.includes(record.phase as WatcherProofThreadJournalPhaseV1) ||
    !isNullableNatural(record.stepIndex) ||
    !isNullableOutRef(record.threadOutRef) ||
    !isNullableOutRef(record.proofTokenOutRef) ||
    !isHex32(record.lastSubmissionId) ||
    !isHex32(record.lastConfirmationId) ||
    !isNullableHex32(record.correctionId) ||
    hashes.some((hash) => !isHex32(hash)) ||
    new Set(hashes).size !== hashes.length ||
    !isHex32(record.journalDigest)
  ) {
    return null;
  }
  const phase = record.phase as WatcherProofThreadJournalPhaseV1;
  if (
    (phase === "active") !==
      (record.stepIndex !== null &&
        record.threadOutRef !== null &&
        record.proofTokenOutRef === null &&
        record.correctionId === null) ||
    (phase === "proven") !==
      (record.stepIndex === null &&
        record.threadOutRef === null &&
        record.proofTokenOutRef !== null &&
        record.correctionId === null) ||
    (phase === "cancelled") !==
      (record.stepIndex === null &&
        record.threadOutRef === null &&
        record.proofTokenOutRef === null &&
        record.correctionId === null) ||
    (phase === "removed") !==
      (record.stepIndex === null &&
        record.threadOutRef === null &&
        record.proofTokenOutRef !== null &&
        record.correctionId !== null)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_JOURNAL_V1_SCHEMA_VERSION,
    journalId: record.journalId,
    faultId: record.faultId,
    familyId: record.familyId,
    fraudulentBlockHash: record.fraudulentBlockHash,
    fraudulentHeaderHash: record.fraudulentHeaderHash,
    fraudProver: record.fraudProver,
    computationThreadAssetName: record.computationThreadAssetName,
    phase,
    stepIndex: record.stepIndex,
    threadOutRef: record.threadOutRef,
    proofTokenOutRef: record.proofTokenOutRef,
    lastSubmissionId: record.lastSubmissionId,
    lastConfirmationId: record.lastConfirmationId,
    correctionId: record.correctionId,
    confirmedTransactionHashes: Object.freeze(hashes as string[]),
  });
  return sha256Canonical(journalWithoutDigest(canonical)) ===
    record.journalDigest
    ? Object.freeze({ ...canonical, journalDigest: record.journalDigest })
    : null;
};

const parseLayout = (value: unknown): WatcherProofThreadLayoutV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "threadInputIndex",
    "threadOutputIndex",
    "proofTokenOutputIndex",
    "proofTokenReferenceInputIndex",
    "stepSpendRedeemerGlobalIndex",
    "computationThreadMintRedeemerGlobalIndex",
    "fraudProofMintRedeemerGlobalIndex",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION ||
    !isNullableNatural(record.threadInputIndex) ||
    !isNullableNatural(record.threadOutputIndex) ||
    !isNullableNatural(record.proofTokenOutputIndex) ||
    !isNullableNatural(record.proofTokenReferenceInputIndex) ||
    !isNullableNatural(record.stepSpendRedeemerGlobalIndex) ||
    !isNullableNatural(record.computationThreadMintRedeemerGlobalIndex) ||
    !isNullableNatural(record.fraudProofMintRedeemerGlobalIndex)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION,
    threadInputIndex: record.threadInputIndex,
    threadOutputIndex: record.threadOutputIndex,
    proofTokenOutputIndex: record.proofTokenOutputIndex,
    proofTokenReferenceInputIndex: record.proofTokenReferenceInputIndex,
    stepSpendRedeemerGlobalIndex: record.stepSpendRedeemerGlobalIndex,
    computationThreadMintRedeemerGlobalIndex:
      record.computationThreadMintRedeemerGlobalIndex,
    fraudProofMintRedeemerGlobalIndex: record.fraudProofMintRedeemerGlobalIndex,
  });
};

export const makeWatcherProofThreadLayoutV1 = (
  value: Omit<WatcherProofThreadLayoutV1, "schemaVersion">,
): WatcherProofThreadLayoutV1 | null =>
  parseLayout({
    schemaVersion: WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION,
    ...value,
  });

export const makeWatcherProofThreadObservationV1 = (
  value: Omit<
    WatcherProofThreadObservationV1,
    "schemaVersion" | "observationDigest"
  >,
): WatcherProofThreadObservationV1 | null => {
  const canonical = {
    schemaVersion: WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherProofThreadObservationV1({
    ...canonical,
    observationDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherProofThreadObservationV1 = (
  value: unknown,
): WatcherProofThreadObservationV1 | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "transitionKind",
    "confirmationPhase",
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "transactionHash",
    "publicInputDigest",
    "sourceObservationDigest",
    "chainPointId",
    "sourceDurableStoreDigest",
    "sourceDurableStoreRevision",
    "durableStoreDigest",
    "durableStoreRevision",
    "predecessorStateDigest",
    "submissionId",
    "confirmationId",
    "rollbackTargetStateDigest",
    "layout",
    "journal",
    "observationDigest",
  ]);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  const layout =
    record?.layout === null ? null : parseLayout(record?.layout ?? null);
  const journal =
    record?.journal === null
      ? null
      : parseWatcherProofThreadJournalV1(record?.journal);
  const kinds: readonly WatcherProofThreadTransitionKindV1[] = [
    "init",
    "step",
    "success",
    "cancel",
    "removal",
    "rollback",
  ];
  if (
    record === null ||
    marker === null ||
    record.schemaVersion !==
      WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION ||
    !isHex32(record.policyDigest) ||
    !isNetwork(record.network) ||
    !isHex32(record.releaseEvidenceDigest) ||
    typeof record.transitionKind !== "string" ||
    !kinds.includes(
      record.transitionKind as WatcherProofThreadTransitionKindV1,
    ) ||
    !(
      record.confirmationPhase === null ||
      record.confirmationPhase === "pending" ||
      record.confirmationPhase === "final"
    ) ||
    !isHex32(record.pointDigest) ||
    !isHex32(record.blockHash) ||
    !isNatural(record.slot) ||
    !isNatural(record.blockNo) ||
    !isNullableHex32(record.transactionHash) ||
    !isHex32(record.publicInputDigest) ||
    !isHex32(record.sourceObservationDigest) ||
    !isHex32(record.chainPointId) ||
    !isHex32(record.sourceDurableStoreDigest) ||
    !isNatural(record.sourceDurableStoreRevision) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    !isNullableHex32(record.predecessorStateDigest) ||
    !isNullableHex32(record.submissionId) ||
    !isNullableHex32(record.confirmationId) ||
    !isNullableHex32(record.rollbackTargetStateDigest) ||
    (record.layout !== null && layout === null) ||
    (record.journal !== null && journal === null) ||
    !isHex32(record.observationDigest)
  ) {
    return null;
  }
  const kind = record.transitionKind as WatcherProofThreadTransitionKindV1;
  const rollback = kind === "rollback";
  if (
    rollback !== (record.confirmationPhase === null) ||
    rollback !== (record.transactionHash === null) ||
    rollback !== (record.submissionId === null) ||
    rollback !== (record.confirmationId === null) ||
    rollback !== (record.layout === null) ||
    (!rollback && record.rollbackTargetStateDigest !== null) ||
    rollback !== (record.journal === null) ||
    (!rollback && (layout === null || journal === null))
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_OBSERVATION_V1_SCHEMA_VERSION,
    policyDigest: record.policyDigest,
    network: record.network,
    releaseEvidenceDigest: record.releaseEvidenceDigest,
    deploymentMarker: marker,
    transitionKind: kind,
    confirmationPhase:
      record.confirmationPhase as WatcherProofThreadConfirmationPhaseV1 | null,
    pointDigest: record.pointDigest,
    blockHash: record.blockHash,
    slot: record.slot,
    blockNo: record.blockNo,
    transactionHash: record.transactionHash,
    publicInputDigest: record.publicInputDigest,
    sourceObservationDigest: record.sourceObservationDigest,
    chainPointId: record.chainPointId,
    sourceDurableStoreDigest: record.sourceDurableStoreDigest,
    sourceDurableStoreRevision: record.sourceDurableStoreRevision,
    durableStoreDigest: record.durableStoreDigest,
    durableStoreRevision: record.durableStoreRevision,
    predecessorStateDigest: record.predecessorStateDigest,
    submissionId: record.submissionId,
    confirmationId: record.confirmationId,
    rollbackTargetStateDigest: record.rollbackTargetStateDigest,
    layout,
    journal,
  });
  return sha256Canonical(canonical) === record.observationDigest
    ? Object.freeze({
        ...canonical,
        observationDigest: record.observationDigest,
      })
    : null;
};

type VerifiedPublicContext = Readonly<{
  context: WatcherProofThreadPublicContextV1;
  block: WatcherNormalizedL1BlockV1;
  lineageBlocks: readonly WatcherNormalizedL1BlockV1[];
  transaction: WatcherL1TransactionV1 | null;
  sourceStore: WatcherDurableStoreV1;
  store: WatcherDurableStoreV1;
  deploymentPolicy: WatcherDeploymentIdentityPolicyV1;
  finalityResult: WatcherFinalityResultV1 | null;
  sourceJournal: WatcherProofThreadJournalV1 | null;
  journal: WatcherProofThreadJournalV1 | null;
}>;

const transportAttestationForProvider = (
  provider: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherL1TransportAttestationContextV1 | null => {
  if (
    !Array.isArray(transportAttestations) ||
    transportAttestations.length >
      WATCHER_PROOF_THREAD_V1_BOUNDS.observationsPerFinalityStep
  ) {
    return null;
  }
  const matches = transportAttestations.filter((candidate) => {
    const details = watcherL1TransportAttestationDetailsV1(candidate);
    return details !== null && same(details.provider, provider);
  });
  return matches.length === 1 ? matches[0]! : null;
};

const normalizeTransportAttestedBlock = (
  provider: unknown,
  observation: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): Readonly<{
  block: WatcherNormalizedL1BlockV1;
  transportAttestation: WatcherL1TransportAttestationContextV1;
}> | null => {
  const transportAttestation = transportAttestationForProvider(
    provider,
    transportAttestations,
  );
  if (transportAttestation === null) {
    return null;
  }
  return Object.freeze({
    block: normalizeWatcherL1BlockV1(transportAttestation, observation),
    transportAttestation,
  });
};

const storeDigest = (store: WatcherDurableStoreV1): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStoreV1(store));

const canonicalSuccessor = (
  prior: WatcherProofThreadObservationV1,
  current: WatcherProofThreadObservationV1,
  next: WatcherNormalizedL1BlockV1,
  lineage: readonly WatcherNormalizedL1BlockV1[],
  allowSameTransaction: boolean,
): boolean => {
  if (next.chainPoint.pointDigest === prior.pointDigest) {
    if (
      next.chainPoint.blockHash !== prior.blockHash ||
      next.chainPoint.slot !== prior.slot ||
      next.chainPoint.blockNo !== prior.blockNo ||
      current.transactionHash === null
    ) {
      return false;
    }
    const currentIndex = next.transactions.findIndex(
      ({ txHash }) => txHash === current.transactionHash,
    );
    if (prior.transactionHash === null) {
      return (
        prior.transitionKind === "rollback" &&
        currentIndex >= 0 &&
        next.transactions[currentIndex]?.transactionIndex ===
          currentIndex.toString()
      );
    }
    const priorIndex = next.transactions.findIndex(
      ({ txHash }) => txHash === prior.transactionHash,
    );
    return (
      priorIndex >= 0 &&
      currentIndex >= 0 &&
      (currentIndex > priorIndex ||
        (allowSameTransaction && currentIndex === priorIndex)) &&
      next.transactions[priorIndex]?.transactionIndex ===
        priorIndex.toString() &&
      next.transactions[currentIndex]?.transactionIndex ===
        currentIndex.toString()
    );
  }
  if (
    BigInt(next.chainPoint.blockNo) <= BigInt(prior.blockNo) ||
    BigInt(next.chainPoint.slot) <= BigInt(prior.slot)
  ) {
    return false;
  }
  const ancestors = new Map<string, WatcherNormalizedL1BlockV1>();
  for (const candidate of lineage) {
    const existing = ancestors.get(candidate.chainPoint.blockHash);
    if (
      existing !== undefined &&
      (existing.chainPoint.pointDigest !== candidate.chainPoint.pointDigest ||
        existing.chainPoint.parentBlockHash !==
          candidate.chainPoint.parentBlockHash)
    ) {
      return false;
    }
    ancestors.set(candidate.chainPoint.blockHash, candidate);
  }
  let cursor = next;
  const visited = new Set<string>();
  while (!visited.has(cursor.chainPoint.blockHash)) {
    visited.add(cursor.chainPoint.blockHash);
    const parentHash = cursor.chainPoint.parentBlockHash;
    if (parentHash === prior.blockHash) {
      return (
        BigInt(cursor.chainPoint.blockNo) === BigInt(prior.blockNo) + 1n &&
        BigInt(cursor.chainPoint.slot) > BigInt(prior.slot)
      );
    }
    if (parentHash === null) {
      return false;
    }
    const parent = ancestors.get(parentHash);
    if (
      parent === undefined ||
      BigInt(cursor.chainPoint.blockNo) !==
        BigInt(parent.chainPoint.blockNo) + 1n ||
      BigInt(cursor.chainPoint.slot) <= BigInt(parent.chainPoint.slot)
    ) {
      return false;
    }
    cursor = parent;
  }
  return false;
};

const durableStoreExtends = (
  prior: WatcherDurableStoreV1,
  source: WatcherDurableStoreV1,
): boolean => {
  const priorRevision = BigInt(prior.revision);
  const sourceRevision = BigInt(source.revision);
  if (
    sourceRevision < priorRevision ||
    !sameMarker(source.deploymentMarker, prior.deploymentMarker)
  ) {
    return false;
  }
  if (sourceRevision === priorRevision) {
    return same(source, prior);
  }
  for (const field of [
    "l1Observations",
    "chainPoints",
    "protocolUtxos",
    "spentProtocolUtxos",
    "daProofInputs",
    "reconstructedStates",
    "decisions",
    "faults",
    "submissions",
    "confirmations",
    "retries",
    "deadlines",
    "correctionResults",
  ] as const) {
    if (
      !prior[field].every((entry) =>
        source[field].some((candidate) => same(candidate, entry)),
      )
    ) {
      return false;
    }
  }
  return true;
};

const bodyInputs = (body: CML.TransactionBody): readonly string[] => {
  const inputs = body.inputs();
  const result: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

const bodyReferenceInputs = (body: CML.TransactionBody): readonly string[] => {
  const inputs = body.reference_inputs();
  if (inputs === undefined) {
    return [];
  }
  const result: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    result.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return result;
};

const allMint = (body: CML.TransactionBody): ReadonlyMap<string, bigint> => {
  const result = new Map<string, bigint>();
  const mint = body.mint();
  if (mint === undefined) {
    return result;
  }
  const policies = mint.keys();
  for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
    const policy = policies.get(policyIndex);
    const assets = mint.get_assets(policy);
    if (assets === undefined) {
      throw new Error("missing mint assets");
    }
    const names = assets.keys();
    for (let assetIndex = 0; assetIndex < names.len(); assetIndex += 1) {
      const name = names.get(assetIndex);
      const quantity = assets.get(name);
      if (quantity === undefined || quantity === 0n) {
        throw new Error("zero mint");
      }
      result.set(`${policy.to_hex()}${name.to_hex()}`, quantity);
    }
  }
  return result;
};

const mintPolicyIndex = (
  body: CML.TransactionBody,
  policyId: string,
): number => {
  const mint = body.mint();
  if (mint === undefined) {
    return -1;
  }
  const policies = mint.keys();
  for (let index = 0; index < policies.len(); index += 1) {
    if (policies.get(index).to_hex() === policyId) {
      return index;
    }
  }
  return -1;
};

const exactMint = (
  body: CML.TransactionBody,
  expected: ReadonlyMap<string, bigint>,
): boolean => {
  try {
    const actual = allMint(body);
    return (
      actual.size === expected.size &&
      [...expected].every(([unit, quantity]) => actual.get(unit) === quantity)
    );
  } catch {
    return false;
  }
};

const outputAssets = (
  output: CML.TransactionOutput,
): ReadonlyMap<string, bigint> => {
  const result = new Map<string, bigint>();
  for (const [unit, quantity] of Object.entries(
    valueToAssets(output.amount()),
  )) {
    if (unit !== "lovelace") {
      result.set(unit, quantity);
    }
  }
  return result;
};

const exactTokenOutput = (
  output: CML.TransactionOutput,
  policyId: string,
  assetName: string,
  quantity = 1n,
): boolean => {
  const assets = outputAssets(output);
  return (
    assets.size === 1 && assets.get(`${policyId}${assetName}`) === quantity
  );
};

const outputScriptHash = (output: CML.TransactionOutput): string | null =>
  output.address().payment_cred()?.as_script()?.to_hex() ?? null;

const outputDatumHex = (output: CML.TransactionOutput): string | null =>
  output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null;

type ProofThreadRoleClassification = "owned" | "foreign" | "invalid";

const classifyProofThreadRole = (
  policy: WatcherProofThreadPolicyV1,
  deploymentPolicy: WatcherDeploymentIdentityPolicyV1,
  durable: WatcherProtocolUtxoV1,
): ProofThreadRoleClassification => {
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    if (output.to_canonical_cbor_hex() !== durable.output.cborHex) {
      return "invalid";
    }
    const assets = outputAssets(output);
    const onlyAsset =
      assets.size === 1 ? ([...assets.entries()][0] ?? null) : null;
    const assetPolicyId = onlyAsset?.[0].slice(0, 56) ?? null;
    const assetName = onlyAsset?.[0].slice(56) ?? null;
    const scriptHash = outputScriptHash(output);
    const computationScriptHashes = new Set(
      policy.families.flatMap(({ stepScriptHashes }) => stepScriptHashes),
    );
    const computation =
      onlyAsset !== null &&
      assetPolicyId === policy.computationThreadPolicyId &&
      onlyAsset[1] === 1n &&
      computationScriptHashes.has(scriptHash ?? "") &&
      stepDatum(output) !== null;
    const proof =
      onlyAsset !== null &&
      assetPolicyId === policy.fraudProofPolicyId &&
      onlyAsset[1] === 1n &&
      output.address().to_hex() === policy.fraudProofAddressHex &&
      scriptHash === policy.fraudProofSpendScriptHash &&
      proofTokenDatum(output) !== null;
    const daAttestationPolicyId =
      deploymentPolicy.appliedScriptHashes.daAttestationMint;
    const daAttestationSpendScriptHash =
      deploymentPolicy.appliedScriptHashes.daAttestationSpend;
    const daDatumHex = outputDatumHex(output);
    const daDatum =
      daDatumHex === null
        ? null
        : dataRoundTrip<Readonly<{ header_hash: string }>>(
            daDatumHex,
            DaAttestationDatum,
          );
    const daHeaderHash =
      assetName?.startsWith(DA_ATTESTATION_ASSET_NAME_PREFIX) === true
        ? assetName.slice(DA_ATTESTATION_ASSET_NAME_PREFIX.length)
        : null;
    const daAttestation =
      onlyAsset !== null &&
      assetPolicyId === daAttestationPolicyId &&
      onlyAsset[1] === 1n &&
      scriptHash === daAttestationSpendScriptHash &&
      output.address().network_id() === expectedNetworkId(policy.network) &&
      output.address().staking_cred() === undefined &&
      daHeaderHash !== null &&
      isHex28(daHeaderHash) &&
      daDatum?.header_hash === daHeaderHash;
    const matches = [computation, proof, daAttestation].filter(Boolean).length;
    if (matches > 1) {
      return "invalid";
    }
    if (computation) {
      return durable.role === "computation_thread" ? "owned" : "invalid";
    }
    if (proof) {
      return durable.role === "proof_thread" ? "owned" : "invalid";
    }
    if (daAttestation) {
      return durable.role === "proof_thread" ? "foreign" : "invalid";
    }
    return "foreign";
  } catch {
    return "invalid";
  }
};

const proofThreadRolesMatchOutputs = (
  policy: WatcherProofThreadPolicyV1,
  deploymentPolicy: WatcherDeploymentIdentityPolicyV1,
  store: WatcherDurableStoreV1,
): boolean =>
  [...store.protocolUtxos, ...store.spentProtocolUtxos].every(
    (durable) =>
      classifyProofThreadRole(policy, deploymentPolicy, durable) !== "invalid",
  );

const proofThreadForeignRecordsPreserved = (
  policy: WatcherProofThreadPolicyV1,
  deploymentPolicy: WatcherDeploymentIdentityPolicyV1,
  source: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
): boolean => {
  const foreign = (records: readonly WatcherProtocolUtxoV1[]) =>
    records.filter(
      (durable) =>
        classifyProofThreadRole(policy, deploymentPolicy, durable) ===
        "foreign",
    );
  return (
    same(foreign(source.protocolUtxos), foreign(next.protocolUtxos)) &&
    same(foreign(source.spentProtocolUtxos), foreign(next.spentProtocolUtxos))
  );
};

const canonicalTransaction = (
  transaction: WatcherL1TransactionV1,
): Readonly<{
  body: CML.TransactionBody;
  outputs: readonly CML.TransactionOutput[];
}> | null => {
  try {
    const body = CML.TransactionBody.from_cbor_hex(transaction.body.bytesHex);
    if (
      body.to_canonical_cbor_hex() !== transaction.body.bytesHex ||
      CML.hash_transaction(body).to_hex() !== transaction.txHash ||
      body.outputs().len() !== transaction.utxos.length
    ) {
      return null;
    }
    const outputs: CML.TransactionOutput[] = [];
    for (let index = 0; index < body.outputs().len(); index += 1) {
      const output = body.outputs().get(index);
      const evidence = transaction.utxos.find(
        ({ outputIndex }) => outputIndex === index.toString(),
      );
      const datumHex = outputDatumHex(output);
      if (
        evidence === undefined ||
        evidence.outRef !== `${transaction.txHash}#${index.toString()}` ||
        evidence.output.bytesHex !== output.to_canonical_cbor_hex() ||
        (evidence.datum?.bytes.bytesHex ?? null) !== datumHex ||
        output.script_ref() !== undefined
      ) {
        return null;
      }
      outputs.push(output);
    }
    return Object.freeze({ body, outputs: Object.freeze(outputs) });
  } catch {
    return null;
  }
};

const redeemerAtGlobal = (
  transaction: WatcherL1TransactionV1,
  globalIndex: string | null,
  purpose: "spend" | "mint",
  localIndex: number,
): WatcherL1RedeemerV1 | null => {
  if (
    globalIndex === null ||
    BigInt(globalIndex) >= BigInt(transaction.redeemers.length)
  ) {
    return null;
  }
  const redeemer = transaction.redeemers[Number(globalIndex)]!;
  return redeemer.purpose === purpose &&
    redeemer.index === localIndex.toString()
    ? redeemer
    : null;
};

const dataRoundTrip = <T>(cborHex: string, schema: unknown): T | null => {
  try {
    const data = CML.PlutusData.from_cbor_hex(cborHex);
    if (data.to_canonical_cbor_hex() !== cborHex) {
      return null;
    }
    const decoded = Data.from(cborHex, schema as never) as T;
    const reencoded = CML.PlutusData.from_cbor_hex(
      Data.to(decoded as never, schema as never),
    ).to_canonical_cbor_hex();
    return reencoded === cborHex ? decoded : null;
  } catch {
    return null;
  }
};

const genericStepRedeemer = (
  cborHex: string,
): Readonly<
  | { kind: "cancel"; inputIndex: bigint; mintRedeemerGlobalIndex: bigint }
  | { kind: "continue" }
> | null => {
  try {
    const data = CML.PlutusData.from_cbor_hex(cborHex);
    if (data.to_canonical_cbor_hex() !== cborHex) {
      return null;
    }
    const constructor = data.as_constr_plutus_data();
    if (constructor === undefined || constructor.fields().len() !== 1) {
      return null;
    }
    if (constructor.alternative() === 1n) {
      return Object.freeze({ kind: "continue" });
    }
    if (constructor.alternative() !== 0n) {
      return null;
    }
    const payload = constructor.fields().get(0).as_constr_plutus_data();
    if (
      payload === undefined ||
      payload.alternative() !== 0n ||
      payload.fields().len() !== 2
    ) {
      return null;
    }
    const inputValue = payload.fields().get(0).as_integer();
    const mintValue = payload.fields().get(1).as_integer();
    if (inputValue === undefined || mintValue === undefined) {
      return null;
    }
    const input = BigInt(inputValue.to_str());
    const mint = BigInt(mintValue.to_str());
    if (input < 0n || mint < 0n) {
      return null;
    }
    return Object.freeze({
      kind: "cancel",
      inputIndex: input,
      mintRedeemerGlobalIndex: mint,
    });
  } catch {
    return null;
  }
};

const stepDatum = (
  output: CML.TransactionOutput,
): Readonly<{ fraud_prover: string; data: unknown | null }> | null => {
  const cborHex = outputDatumHex(output);
  return cborHex === null
    ? null
    : dataRoundTrip<Readonly<{ fraud_prover: string; data: unknown | null }>>(
        cborHex,
        FraudProofComputationThreadStepDatum,
      );
};

const proofTokenDatum = (
  output: CML.TransactionOutput,
): Readonly<{ fraud_prover: string }> | null => {
  const cborHex = outputDatumHex(output);
  return cborHex === null
    ? null
    : dataRoundTrip<Readonly<{ fraud_prover: string }>>(
        cborHex,
        FraudProofTokenDatum,
      );
};

const protocolUtxo = (
  store: WatcherDurableStoreV1,
  outRef: string,
  role: WatcherProtocolUtxoV1["role"],
): WatcherProtocolUtxoV1 | null => {
  const matches = store.protocolUtxos.filter(
    (candidate) => candidate.outRef === outRef && candidate.role === role,
  );
  return matches.length === 1 ? matches[0]! : null;
};

const durableOutputMatches = (
  durable: WatcherProtocolUtxoV1,
  output: CML.TransactionOutput,
  chainPointId?: string,
): boolean =>
  durable.output.cborHex === output.to_canonical_cbor_hex() &&
  durable.output.sha256 ===
    sha256Bytes(Buffer.from(output.to_canonical_cbor_hex(), "hex")) &&
  (chainPointId === undefined || durable.chainPointId === chainPointId);

const durableOutputMatchesLifecycle = (
  durable: WatcherProtocolUtxoV1,
  output: CML.TransactionOutput,
  observation: WatcherProofThreadObservationV1,
  sourceStore: WatcherDurableStoreV1,
): boolean =>
  durableOutputMatches(durable, output) &&
  (durable.chainPointId === observation.chainPointId ||
    (observation.confirmationPhase === "final" &&
      sourceStore.protocolUtxos.some((candidate) => same(candidate, durable))));

const verifyDeploymentAuthority = (
  policy: WatcherProofThreadPolicyV1,
  authority: WatcherProofThreadPublicContextV1["deploymentAuthority"],
): WatcherDeploymentIdentityPolicyV1 | null => {
  try {
    const verified = verifyWatcherDeploymentIdentityV1({
      signedIdentity: authority.signedIdentity,
      policy: authority.policy,
      trustRoots: authority.trustRoots,
      durableMarker: policy.deploymentMarker,
    });
    const categories = authority.policy.fraudProofCatalogue.categories;
    const catalogueMatches =
      Object.keys(categories).length === policy.families.length &&
      policy.families.every((family) => {
        const category = categories[
          family.catalogueCategory as keyof typeof categories
        ] as Readonly<{ categoryId: string; scriptHash: string }> | undefined;
        return (
          category?.categoryId === family.categoryId &&
          category.scriptHash === family.firstStepScriptHash
        );
      });
    const deployedStepsMatch = policy.families.every((family) => {
      const category =
        family.catalogueCategory as DeploymentManifestV1FraudProofCatalogueCategory;
      return WATCHER_PROOF_THREAD_FAMILY_AUTHORITY_V1[
        category
      ].deployedStepContractNames.every(
        (contractName, stepIndex) =>
          authority.policy.appliedScriptHashes[contractName] ===
          family.stepScriptHashes[stepIndex],
      );
    });
    const computationCommitment = sha256Canonical({
      computationThreadPolicyId: policy.computationThreadPolicyId,
    });
    const catalogueCommitment = sha256Canonical(
      policy.families.map((family) => ({
        familyId: family.familyId,
        catalogueCategory: family.catalogueCategory,
        categoryId: family.categoryId,
        firstStepScriptHash: family.firstStepScriptHash,
        stepScriptHashes: family.stepScriptHashes,
        nextStepIndexes: family.nextStepIndexes,
      })),
    );
    return same(verified, authority.result) &&
      verified.network === policy.network &&
      verified.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
      verified.trustRootId === policy.deploymentTrustRootId &&
      sameMarker(verified.durableMarker, policy.deploymentMarker) &&
      authority.policy.appliedScriptHashes.fraudProofMint ===
        policy.fraudProofPolicyId &&
      authority.policy.appliedScriptHashes.fraudProofSpend ===
        policy.fraudProofSpendScriptHash &&
      authority.policy.programCommitments["computation-thread-policy-v1"] ===
        computationCommitment &&
      authority.policy.programCommitments["proof-thread-catalogue-v1"] ===
        catalogueCommitment &&
      catalogueMatches &&
      deployedStepsMatch
      ? authority.policy
      : null;
  } catch {
    return null;
  }
};

const replaceSortedBy = <T>(
  values: readonly T[],
  replacement: T,
  key: (value: T) => string,
): readonly T[] =>
  Object.freeze(
    [
      ...values.filter((value) => key(value) !== key(replacement)),
      replacement,
    ].sort((left, right) => key(left).localeCompare(key(right))),
  );

const sameUnaffectedStoreRecords = (
  source: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
): boolean =>
  same(source.daProofInputs, next.daProofInputs) &&
  same(source.reconstructedStates, next.reconstructedStates) &&
  same(source.decisions, next.decisions) &&
  same(source.faults, next.faults) &&
  same(source.submissions, next.submissions) &&
  same(source.retries, next.retries) &&
  same(source.deadlines, next.deadlines);

const expectedProtocolUtxosAfterTransaction = (
  source: WatcherDurableStoreV1,
  transaction: WatcherL1TransactionV1,
  body: CML.TransactionBody,
  chainPointId: string,
  next: WatcherDurableStoreV1,
): readonly WatcherProtocolUtxoV1[] | null => {
  const consumed = new Set(bodyInputs(body));
  const retained = source.protocolUtxos.filter(
    ({ outRef }) => !consumed.has(outRef),
  );
  const sourceOutRefs = new Set(
    source.protocolUtxos.map(({ outRef }) => outRef),
  );
  const created = next.protocolUtxos.filter(
    ({ outRef }) => !sourceOutRefs.has(outRef),
  );
  for (const durable of created) {
    const outputEvidence = transaction.utxos.find(
      ({ outRef }) => outRef === durable.outRef,
    );
    if (
      outputEvidence === undefined ||
      durable.chainPointId !== chainPointId ||
      durable.output.cborHex !== outputEvidence.output.bytesHex ||
      durable.output.sha256 !==
        sha256Bytes(Buffer.from(outputEvidence.output.bytesHex, "hex"))
    ) {
      return null;
    }
  }
  return Object.freeze(
    [...retained, ...created].sort((left, right) =>
      left.outRef.localeCompare(right.outRef),
    ),
  );
};

const storeTransitionMatches = (
  source: WatcherDurableStoreV1,
  next: WatcherDurableStoreV1,
  block: WatcherNormalizedL1BlockV1,
  transaction: WatcherL1TransactionV1,
  observation: WatcherProofThreadObservationV1,
  applyTransactionEffects: boolean,
): boolean => {
  if (
    BigInt(next.revision) !== BigInt(source.revision) + 1n ||
    !sameMarker(source.deploymentMarker, next.deploymentMarker) ||
    !sameUnaffectedStoreRecords(source, next)
  ) {
    return false;
  }
  const encodedBlock = encodeWatcherNormalizedL1BlockV1(block).toString("hex");
  const observationRecord = {
    observationId: block.observationDigest,
    providerId: block.provider.providerId,
    chainPointId: block.chainPoint.chainPointId,
    payload: {
      cborHex: encodedBlock,
      sha256: sha256Bytes(Buffer.from(encodedBlock, "hex")),
    },
  };
  const pointRecord = {
    chainPointId: block.chainPoint.chainPointId,
    providerId: block.provider.providerId,
    blockHash: block.chainPoint.blockHash,
    slot: block.chainPoint.slot,
    blockNo: block.chainPoint.blockNo,
    depth: block.chainPoint.depth,
  };
  const expectedObservations = replaceSortedBy(
    source.l1Observations,
    observationRecord,
    ({ observationId }) => observationId,
  );
  const expectedPoints = replaceSortedBy(
    source.chainPoints,
    pointRecord,
    ({ chainPointId }) => chainPointId,
  );
  const submission = source.submissions.find(
    ({ submissionId }) => submissionId === observation.submissionId,
  );
  if (
    submission === undefined ||
    submission.faultId !== observation.journal?.faultId ||
    submission.txBodyHash !== transaction.txHash ||
    submission.status !== "submitted"
  ) {
    return false;
  }
  const confirmation: WatcherConfirmationV1 = {
    confirmationId: observation.confirmationId!,
    submissionId: submission.submissionId,
    txHash: transaction.txHash,
    chainPointId: block.chainPoint.chainPointId,
    depth: block.chainPoint.depth,
    status:
      observation.confirmationPhase === "pending" ? "observed" : "confirmed",
  };
  const priorConfirmation = source.confirmations.find(
    ({ confirmationId }) => confirmationId === confirmation.confirmationId,
  );
  if (
    observation.confirmationPhase === "pending"
      ? priorConfirmation !== undefined
      : priorConfirmation !== undefined &&
        (priorConfirmation.submissionId !== confirmation.submissionId ||
          priorConfirmation.txHash !== confirmation.txHash ||
          priorConfirmation.status !== "observed")
  ) {
    return false;
  }
  const expectedConfirmations = replaceSortedBy(
    source.confirmations,
    confirmation,
    ({ confirmationId }) => confirmationId,
  );
  let expectedCorrections = source.correctionResults;
  if (
    observation.transitionKind === "removal" &&
    observation.confirmationPhase === "final"
  ) {
    const correction = next.correctionResults.find(
      ({ correctionId }) => correctionId === observation.journal?.correctionId,
    );
    if (
      correction === undefined ||
      correction.faultId !== observation.journal?.faultId ||
      correction.confirmationId !== observation.confirmationId ||
      ![
        "removed",
        "removed_and_slashed",
        "removed_slashed_and_rewarded",
      ].includes(correction.outcome)
    ) {
      return false;
    }
    expectedCorrections = replaceSortedBy(
      source.correctionResults,
      correction,
      ({ correctionId }) => correctionId,
    );
  }
  const canonical = canonicalTransaction(transaction);
  if (canonical === null) {
    return false;
  }
  const expectedProtocol = applyTransactionEffects
    ? expectedProtocolUtxosAfterTransaction(
        source,
        transaction,
        canonical.body,
        block.chainPoint.chainPointId,
        next,
      )
    : source.protocolUtxos;
  let expectedSpent = source.spentProtocolUtxos;
  if (applyTransactionEffects && expectedProtocol !== null) {
    try {
      expectedSpent = journalWatcherProtocolUtxoTransitionV1({
        sourceStore: source,
        nextChainPoints: expectedPoints,
        nextProtocolUtxos: expectedProtocol,
        spentAtChainPointId: block.chainPoint.chainPointId,
      }).spentProtocolUtxos;
    } catch {
      return false;
    }
  }
  return (
    expectedProtocol !== null &&
    same(next.l1Observations, expectedObservations) &&
    same(next.chainPoints, expectedPoints) &&
    same(next.protocolUtxos, expectedProtocol) &&
    same(next.spentProtocolUtxos, expectedSpent) &&
    same(next.confirmations, expectedConfirmations) &&
    same(next.correctionResults, expectedCorrections)
  );
};

const decodePersistedRollbackObservation = (
  sourceStore: WatcherDurableStoreV1,
  observationDigest: string,
): WatcherNormalizedL1BlockV1 | null => {
  const matches: WatcherNormalizedL1BlockV1[] = [];
  for (const durable of sourceStore.l1Observations) {
    try {
      const bytes = Buffer.from(durable.payload.cborHex, "hex");
      const decoded = JSON.parse(
        new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      ) as unknown;
      if (
        exactRecord(decoded, [
          "schemaVersion",
          "network",
          "provider",
          "chainPoint",
          "transactions",
          "blockContentDigest",
          "observationDigest",
        ]) === null
      ) {
        continue;
      }
      const block = decoded as WatcherNormalizedL1BlockV1;
      if (
        block.observationDigest !== observationDigest ||
        !bytes.equals(encodeWatcherNormalizedL1BlockV1(block)) ||
        durable.providerId !== block.provider.providerId ||
        durable.chainPointId !== block.chainPoint.chainPointId
      ) {
        continue;
      }
      const point = sourceStore.chainPoints.find(
        (candidate) =>
          candidate.chainPointId === block.chainPoint.chainPointId &&
          candidate.providerId === block.provider.providerId,
      );
      if (
        point === undefined ||
        point.blockHash !== block.chainPoint.blockHash ||
        point.slot !== block.chainPoint.slot ||
        point.blockNo !== block.chainPoint.blockNo ||
        point.depth !== block.chainPoint.depth
      ) {
        continue;
      }
      matches.push(block);
    } catch {
      continue;
    }
  }
  return matches.length === 1 ? matches[0]! : null;
};

const rollbackReplacementBlock = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  sourceStore: WatcherDurableStoreV1,
  rollback: WatcherRollbackResultV1,
): WatcherNormalizedL1BlockV1 | null => {
  const transition = rollback.rollbackState?.transitions.at(-1);
  const finality = transition?.finalityResult;
  const consistency = transition?.consistency;
  const replacement = finality?.state?.pending;
  const block = decodePersistedRollbackObservation(
    sourceStore,
    observation.sourceObservationDigest,
  );
  if (
    transition === undefined ||
    finality?.action !== "rewind_pending" ||
    finality.protocolDecision !== "rewind_required" ||
    replacement === null ||
    replacement === undefined ||
    consistency === undefined ||
    consistency.status !== "agreed" ||
    consistency.protocolDecision !== "allowed" ||
    consistency.agreement === null ||
    !consistency.observationEvidenceDigests.includes(
      observation.sourceObservationDigest,
    ) ||
    (consistency.sourceMode === "local_node" &&
      consistency.chainAuthorityObservationDigest !==
        observation.sourceObservationDigest) ||
    block === null ||
    block.network !== policy.network ||
    block.chainPoint.pointDigest !== replacement.pointDigest ||
    block.chainPoint.blockHash !== replacement.blockHash ||
    block.chainPoint.slot !== replacement.slot ||
    block.chainPoint.blockNo !== replacement.blockNo ||
    block.chainPoint.depth !== replacement.currentDepth ||
    block.blockContentDigest !== replacement.blockContentDigest ||
    block.chainPoint.pointDigest !== consistency.agreement.pointDigest ||
    block.chainPoint.blockHash !== consistency.agreement.blockHash ||
    block.chainPoint.slot !== consistency.agreement.slot ||
    block.chainPoint.blockNo !== consistency.agreement.blockNo ||
    block.chainPoint.depth !== consistency.agreement.minimumDepth ||
    block.blockContentDigest !== consistency.agreement.blockContentDigest ||
    block.chainPoint.chainPointId !== observation.chainPointId ||
    block.chainPoint.pointDigest !== observation.pointDigest ||
    block.chainPoint.blockHash !== observation.blockHash ||
    block.chainPoint.slot !== observation.slot ||
    block.chainPoint.blockNo !== observation.blockNo ||
    block.observationDigest !== observation.sourceObservationDigest ||
    sha256Bytes(encodeWatcherNormalizedL1BlockV1(block)) !==
      observation.publicInputDigest
  ) {
    return null;
  }
  return block;
};

const postFinalityRecoveryCommonAncestorBlock = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  sourceStore: WatcherDurableStoreV1,
  recovery: WatcherPostFinalityRecoveryResultV1,
  input: WatcherPostFinalityRecoveryInputV1,
): WatcherNormalizedL1BlockV1 | null => {
  const replacementPath = Array.isArray(input.replacementCanonicalPath)
    ? (input.replacementCanonicalPath as readonly unknown[])
    : [];
  const consistency =
    replacementPath.length === 0
      ? null
      : (replacementPath[0] as WatcherMultiProviderConsistencyV1);
  const path = recovery.recoveryState?.path;
  const block = decodePersistedRollbackObservation(
    sourceStore,
    observation.sourceObservationDigest,
  );
  if (
    recovery.action !== "rewind_and_replay" ||
    recovery.protocolDecision !== "resume_replay" ||
    recovery.nextStore === null ||
    recovery.resumableFinalityState === null ||
    path === undefined ||
    consistency === null ||
    consistency.status !== "agreed" ||
    consistency.protocolDecision !== "allowed" ||
    consistency.agreement === null ||
    consistency.consistencyDigest !== path.replacementConsistencyDigests[0] ||
    !consistency.observationEvidenceDigests.includes(
      observation.sourceObservationDigest,
    ) ||
    (consistency.sourceMode === "local_node"
      ? consistency.independentProviderCount !== 1 ||
        consistency.chainAuthorityObservationDigest !==
          observation.sourceObservationDigest
      : consistency.independentProviderCount < 2) ||
    block === null ||
    block.network !== policy.network ||
    block.chainPoint.pointDigest !== path.commonAncestorPointDigest ||
    block.chainPoint.blockHash !== path.commonAncestorBlockHash ||
    block.chainPoint.blockNo !== path.commonAncestorBlockNo ||
    block.chainPoint.pointDigest !== consistency.agreement.pointDigest ||
    block.chainPoint.blockHash !== consistency.agreement.blockHash ||
    block.chainPoint.slot !== consistency.agreement.slot ||
    block.chainPoint.blockNo !== consistency.agreement.blockNo ||
    block.chainPoint.depth !== consistency.agreement.minimumDepth ||
    block.blockContentDigest !== consistency.agreement.blockContentDigest ||
    block.chainPoint.chainPointId !== observation.chainPointId ||
    block.chainPoint.pointDigest !== observation.pointDigest ||
    block.chainPoint.blockHash !== observation.blockHash ||
    block.chainPoint.slot !== observation.slot ||
    block.chainPoint.blockNo !== observation.blockNo ||
    block.observationDigest !== observation.sourceObservationDigest ||
    sha256Bytes(encodeWatcherNormalizedL1BlockV1(block)) !==
      observation.publicInputDigest
  ) {
    return null;
  }
  return block;
};

const parsePublicContext = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  value: unknown,
  applyTransactionEffects: boolean,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): VerifiedPublicContext | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const record = exactRecord(value, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "finalityAuthority",
    "rollbackAuthority",
    "sourceJournal",
    "durableJournal",
  ]);
  const deployment =
    record === null
      ? null
      : exactRecord(record.deploymentAuthority, [
          "signedIdentity",
          "policy",
          "trustRoots",
          "result",
        ]);
  if (
    record === null ||
    deployment === null ||
    !Array.isArray(deployment.trustRoots) ||
    record.schemaVersion !==
      WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION
  ) {
    return null;
  }
  const deploymentAuthority = Object.freeze({
    signedIdentity: deployment.signedIdentity,
    policy: deployment.policy as WatcherDeploymentIdentityPolicyV1,
    trustRoots:
      deployment.trustRoots as readonly WatcherDeploymentTrustRootV1[],
    result: deployment.result as VerifiedWatcherDeploymentIdentityV1,
  });
  const deploymentPolicy = verifyDeploymentAuthority(
    policy,
    deploymentAuthority,
  );
  if (deploymentPolicy === null) {
    return null;
  }
  let sourceStore: WatcherDurableStoreV1;
  let store: WatcherDurableStoreV1;
  try {
    sourceStore = parseWatcherDurableStoreV1(record.sourceDurableStore);
    store = parseWatcherDurableStoreV1(record.durableStore);
  } catch {
    return null;
  }
  const sourceJournal =
    record.sourceJournal === null
      ? null
      : parseWatcherProofThreadJournalV1(record.sourceJournal);
  const journal =
    record.durableJournal === null
      ? null
      : parseWatcherProofThreadJournalV1(record.durableJournal);
  if (
    (record.sourceJournal !== null && sourceJournal === null) ||
    (record.durableJournal !== null && journal === null) ||
    storeDigest(sourceStore) !== observation.sourceDurableStoreDigest ||
    sourceStore.revision !== observation.sourceDurableStoreRevision ||
    storeDigest(store) !== observation.durableStoreDigest ||
    store.revision !== observation.durableStoreRevision ||
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    !sameMarker(store.deploymentMarker, policy.deploymentMarker) ||
    !proofThreadRolesMatchOutputs(policy, deploymentPolicy, sourceStore) ||
    !proofThreadRolesMatchOutputs(policy, deploymentPolicy, store) ||
    !proofThreadForeignRecordsPreserved(
      policy,
      deploymentPolicy,
      sourceStore,
      store,
    )
  ) {
    return null;
  }
  const rollbackRecord =
    record.rollbackAuthority === null
      ? null
      : exactRecord(record.rollbackAuthority, ["result", "context"]);
  const rollbackAuthority =
    record.rollbackAuthority === null
      ? null
      : rollbackRecord === null
        ? undefined
        : (record.rollbackAuthority as NonNullable<
            WatcherProofThreadPublicContextV1["rollbackAuthority"]
          >);
  if (rollbackAuthority === undefined) {
    return null;
  }
  if (observation.transitionKind === "rollback") {
    if (
      record.authenticatedProvider !== null ||
      record.l1Observation !== null ||
      record.finalityAuthority !== null ||
      rollbackAuthority === null
    ) {
      return null;
    }
    const rollbackResult = parseWatcherRollbackResultV1(
      rollbackAuthority.result,
      {
        ...(rollbackAuthority.context as WatcherRollbackVerificationContextV1),
        transportAttestations,
      },
    );
    const recoveryResult =
      rollbackResult === null
        ? parseWatcherPostFinalityRecoveryResultV1(rollbackAuthority.result, {
            ...(rollbackAuthority.context as WatcherPostFinalityRecoveryInputV1),
            transportAttestations,
          })
        : null;
    const result = rollbackResult ?? recoveryResult;
    if (
      result === null ||
      result.nextStore === null ||
      (recoveryResult !== null &&
        (recoveryResult.action !== "rewind_and_replay" ||
          recoveryResult.recoveryState === null)) ||
      !same(result.nextStore, store) ||
      !same(rollbackAuthority.context.sourceStore, sourceStore)
    ) {
      return null;
    }
    if (
      recoveryResult !== null &&
      (recoveryResult.recoveryState === null ||
        recoveryResult.resumableFinalityState === null ||
        recoveryResult.recoveryState.network !== policy.network ||
        recoveryResult.recoveryState.releaseEvidenceDigest !==
          policy.releaseEvidenceDigest ||
        !sameMarker(
          recoveryResult.recoveryState.deploymentMarker,
          policy.deploymentMarker,
        ))
    ) {
      return null;
    }
    const replacementBlock =
      rollbackResult !== null
        ? rollbackReplacementBlock(
            policy,
            observation,
            sourceStore,
            rollbackResult,
          )
        : postFinalityRecoveryCommonAncestorBlock(
            policy,
            observation,
            sourceStore,
            recoveryResult!,
            rollbackAuthority.context as WatcherPostFinalityRecoveryInputV1,
          );
    if (replacementBlock === null) {
      return null;
    }
    return Object.freeze({
      context: {
        schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
        authenticatedProvider: null,
        l1Observation: null,
        sourceDurableStore: sourceStore,
        durableStore: store,
        deploymentAuthority,
        finalityAuthority: null,
        rollbackAuthority,
        sourceJournal,
        durableJournal: journal,
      },
      block: replacementBlock,
      lineageBlocks: Object.freeze([]),
      transaction: null,
      sourceStore,
      store,
      deploymentPolicy,
      finalityResult: null,
      sourceJournal,
      journal,
    });
  }
  const finality =
    record.finalityAuthority === null
      ? null
      : exactRecord(record.finalityAuthority, [
          "policy",
          "lineage",
          "previousState",
          "observations",
          "consistency",
          "result",
        ]);
  if (
    record.authenticatedProvider === null ||
    record.l1Observation === null ||
    finality === null ||
    !Array.isArray(finality.lineage) ||
    finality.lineage.length >
      WATCHER_PROOF_THREAD_V1_BOUNDS.finalityLineageSteps ||
    !Array.isArray(finality.observations) ||
    finality.observations.length >
      WATCHER_PROOF_THREAD_V1_BOUNDS.observationsPerFinalityStep ||
    rollbackAuthority !== null
  ) {
    return null;
  }
  let block: WatcherNormalizedL1BlockV1;
  let transaction: WatcherL1TransactionV1 | undefined;
  let finalityResult: WatcherFinalityResultV1;
  let verifiedLineageBlocks: readonly WatcherNormalizedL1BlockV1[] = [];
  try {
    const normalizedBlock = normalizeTransportAttestedBlock(
      record.authenticatedProvider,
      record.l1Observation,
      transportAttestations,
    );
    if (normalizedBlock === null) {
      return null;
    }
    block = normalizedBlock.block;
    const observationsWithAttestations = (
      finality.observations as readonly unknown[]
    ).map((candidate) => {
      const authority = exactRecord(candidate, [
        "authenticatedProvider",
        "l1Observation",
      ]);
      if (authority === null) {
        throw new Error("malformed W10 authority");
      }
      return normalizeTransportAttestedBlock(
        authority.authenticatedProvider,
        authority.l1Observation,
        transportAttestations,
      );
    });
    if (observationsWithAttestations.some((entry) => entry === null)) {
      return null;
    }
    const observations = observationsWithAttestations.map(
      (entry) => entry!.block,
    );
    const finalityPolicy = parseWatcherFinalityPolicyV1(finality.policy);
    if (
      finalityPolicy === null ||
      finalityPolicy.network !== policy.network ||
      finalityPolicy.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
      finalityPolicy.confirmationDepth !== policy.requiredFinalityDepth ||
      !sameMarker(finalityPolicy.deploymentMarker, policy.deploymentMarker)
    ) {
      return null;
    }
    const replayedLineage: {
      observations: readonly unknown[];
      consistency: unknown;
      result: unknown;
    }[] = [];
    const lineageBlocks: WatcherNormalizedL1BlockV1[] = [];
    let replayedState: unknown = null;
    for (const candidate of finality.lineage as readonly unknown[]) {
      const step = exactRecord(candidate, [
        "observations",
        "consistency",
        "result",
      ]);
      if (
        step === null ||
        !Array.isArray(step.observations) ||
        step.observations.length >
          WATCHER_PROOF_THREAD_V1_BOUNDS.observationsPerFinalityStep
      ) {
        return null;
      }
      const stepObservationsWithAttestations = step.observations.map(
        (stepObservation) => {
          const authority = exactRecord(stepObservation, [
            "authenticatedProvider",
            "l1Observation",
          ]);
          if (authority === null) {
            throw new Error("malformed W10 lineage authority");
          }
          return normalizeTransportAttestedBlock(
            authority.authenticatedProvider,
            authority.l1Observation,
            transportAttestations,
          );
        },
      );
      if (stepObservationsWithAttestations.some((entry) => entry === null)) {
        return null;
      }
      const stepObservations = stepObservationsWithAttestations.map(
        (entry) => entry!.block,
      );
      lineageBlocks.push(...stepObservations);
      const stepConsistency = evaluateWatcherMultiProviderConsistencyV1(
        watcherFinalityConfiguredSourceV1(finalityPolicy),
        stepObservations,
        stepObservationsWithAttestations.map(
          (entry) => entry!.transportAttestation,
        ),
      );
      const stepResult = evaluateWatcherFinalityV1(
        finalityPolicy,
        replayedState,
        stepConsistency,
      );
      if (
        !same(stepConsistency, step.consistency) ||
        !same(stepResult, step.result) ||
        stepResult.state === null
      ) {
        return null;
      }
      replayedState = stepResult.state;
      replayedLineage.push({
        observations: step.observations,
        consistency: step.consistency,
        result: step.result,
      });
    }
    if (!same(replayedState, finality.previousState)) {
      return null;
    }
    const consistency = evaluateWatcherMultiProviderConsistencyV1(
      watcherFinalityConfiguredSourceV1(finalityPolicy),
      observations,
      observationsWithAttestations.map((entry) => entry!.transportAttestation),
    );
    finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      finality.previousState,
      consistency,
    );
    if (
      !same(consistency, finality.consistency) ||
      !same(finalityResult, finality.result) ||
      !observations.some(
        (candidate) => candidate.observationDigest === block.observationDigest,
      ) ||
      (finalityPolicy.sourceMode === "local_node" &&
        consistency.chainAuthorityObservationDigest !== block.observationDigest)
    ) {
      return null;
    }
    transaction = block.transactions.find(
      ({ txHash, isValid }) =>
        isValid && txHash === observation.transactionHash,
    );
    verifiedLineageBlocks = lineageBlocks;
  } catch {
    return null;
  }
  const bound =
    observation.confirmationPhase === "final"
      ? finalityResult.state?.finalized
      : finalityResult.state?.pending;
  if (
    transaction === undefined ||
    transaction.transactionIndex !==
      block.transactions.indexOf(transaction).toString() ||
    block.network !== observation.network ||
    block.chainPoint.chainPointId !== observation.chainPointId ||
    block.chainPoint.pointDigest !== observation.pointDigest ||
    block.chainPoint.blockHash !== observation.blockHash ||
    block.chainPoint.slot !== observation.slot ||
    block.chainPoint.blockNo !== observation.blockNo ||
    block.observationDigest !== observation.sourceObservationDigest ||
    sha256Bytes(encodeWatcherNormalizedL1BlockV1(block)) !==
      observation.publicInputDigest ||
    (observation.confirmationPhase === "final"
      ? finalityResult.protocolDecision !== "finality_granted" ||
        bound?.pointDigest !== block.chainPoint.pointDigest ||
        bound.blockContentDigest !== block.blockContentDigest ||
        BigInt(bound.currentDepth) < BigInt(policy.requiredFinalityDepth)
      : finalityResult.protocolDecision !== "hold" ||
        finalityResult.state?.phase !== "pending" ||
        bound?.pointDigest !== block.chainPoint.pointDigest ||
        bound.blockContentDigest !== block.blockContentDigest) ||
    !storeTransitionMatches(
      sourceStore,
      store,
      block,
      transaction,
      observation,
      applyTransactionEffects,
    )
  ) {
    return null;
  }
  return Object.freeze({
    context: {
      schemaVersion: WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION,
      authenticatedProvider: record.authenticatedProvider,
      l1Observation: record.l1Observation,
      sourceDurableStore: sourceStore,
      durableStore: store,
      deploymentAuthority,
      finalityAuthority: {
        policy: finality.policy,
        lineage: finality.lineage as readonly Readonly<{
          observations: readonly unknown[];
          consistency: unknown;
          result: unknown;
        }>[],
        previousState: finality.previousState,
        observations: finality.observations as readonly unknown[],
        consistency: finality.consistency,
        result: finality.result,
      },
      rollbackAuthority: null,
      sourceJournal,
      durableJournal: journal,
    },
    block,
    lineageBlocks: Object.freeze(verifiedLineageBlocks),
    transaction,
    sourceStore,
    store,
    deploymentPolicy,
    finalityResult,
    sourceJournal,
    journal,
  });
};

const layoutEquals = (
  layout: WatcherProofThreadLayoutV1,
  expected: Omit<WatcherProofThreadLayoutV1, "schemaVersion">,
): boolean =>
  same(layout, {
    schemaVersion: WATCHER_PROOF_THREAD_LAYOUT_V1_SCHEMA_VERSION,
    ...expected,
  });

const familyForJournal = (
  policy: WatcherProofThreadPolicyV1,
  journal: WatcherProofThreadJournalV1,
): WatcherProofThreadFamilyV1 | null => {
  const matches = policy.families.filter(
    ({ familyId }) => familyId === journal.familyId,
  );
  return matches.length === 1 ? matches[0]! : null;
};

const journalId = (
  policy: WatcherProofThreadPolicyV1,
  faultId: string,
): string =>
  sha256Canonical({
    domain: "midgard-watcher-proof-thread-journal-identity-v1",
    manifestId: policy.deploymentMarker.manifestId,
    faultId,
  });

const indexedOutput = (
  transaction: WatcherL1TransactionV1,
  canonical: NonNullable<ReturnType<typeof canonicalTransaction>>,
  indexText: string | null,
): Readonly<{
  index: number;
  output: CML.TransactionOutput;
  outRef: string;
}> | null => {
  if (
    indexText === null ||
    BigInt(indexText) >= BigInt(canonical.outputs.length)
  ) {
    return null;
  }
  const index = Number(indexText);
  return Object.freeze({
    index,
    output: canonical.outputs[index]!,
    outRef: `${transaction.txHash}#${indexText}`,
  });
};

const sourceThreadOutput = (
  store: WatcherDurableStoreV1,
  journal: WatcherProofThreadJournalV1,
): Readonly<{
  durable: WatcherProtocolUtxoV1;
  output: CML.TransactionOutput;
}> | null => {
  if (journal.threadOutRef === null) {
    return null;
  }
  const matches = [...store.protocolUtxos, ...store.spentProtocolUtxos].filter(
    (candidate) =>
      candidate.outRef === journal.threadOutRef &&
      candidate.role === "computation_thread",
  );
  if (matches.length !== 1) {
    return null;
  }
  const durable = matches[0]!;
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    return output.to_canonical_cbor_hex() === durable.output.cborHex
      ? Object.freeze({ durable, output })
      : null;
  } catch {
    return null;
  }
};

const sourceProofTokenOutput = (
  store: WatcherDurableStoreV1,
  journal: WatcherProofThreadJournalV1,
): Readonly<{
  durable: WatcherProtocolUtxoV1;
  output: CML.TransactionOutput;
}> | null => {
  if (journal.proofTokenOutRef === null) {
    return null;
  }
  const durable = protocolUtxo(store, journal.proofTokenOutRef, "proof_thread");
  if (durable === null) {
    return null;
  }
  try {
    const output = CML.TransactionOutput.from_cbor_hex(durable.output.cborHex);
    return output.to_canonical_cbor_hex() === durable.output.cborHex
      ? Object.freeze({ durable, output })
      : null;
  } catch {
    return null;
  }
};

const baseJournalIdentityMatches = (
  policy: WatcherProofThreadPolicyV1,
  journal: WatcherProofThreadJournalV1,
  store: WatcherDurableStoreV1,
): WatcherProofThreadFamilyV1 | null => {
  const family = familyForJournal(policy, journal);
  const fault = store.faults.find(({ faultId }) => faultId === journal.faultId);
  return family !== null &&
    journal.journalId === journalId(policy, journal.faultId) &&
    journal.computationThreadAssetName ===
      `${family.categoryId}${journal.fraudulentHeaderHash}` &&
    fault?.familyId === journal.familyId &&
    fault.blockHash === journal.fraudulentBlockHash
    ? family
    : null;
};

const expectedJournal = (
  source: WatcherProofThreadJournalV1 | null,
  proposed: WatcherProofThreadJournalV1,
  transactionHash: string,
  submissionId: string,
  confirmationId: string,
  changes: Readonly<{
    phase: WatcherProofThreadJournalPhaseV1;
    stepIndex: string | null;
    threadOutRef: string | null;
    proofTokenOutRef: string | null;
    correctionId: string | null;
  }>,
): WatcherProofThreadJournalV1 | null => {
  const hashes = [
    ...(source?.confirmedTransactionHashes ?? []),
    transactionHash,
  ];
  const value = makeWatcherProofThreadJournalV1({
    journalId: proposed.journalId,
    faultId: proposed.faultId,
    familyId: proposed.familyId,
    fraudulentBlockHash: proposed.fraudulentBlockHash,
    fraudulentHeaderHash: proposed.fraudulentHeaderHash,
    fraudProver: proposed.fraudProver,
    computationThreadAssetName: proposed.computationThreadAssetName,
    phase: changes.phase,
    stepIndex: changes.stepIndex,
    threadOutRef: changes.threadOutRef,
    proofTokenOutRef: changes.proofTokenOutRef,
    lastSubmissionId: submissionId,
    lastConfirmationId: confirmationId,
    correctionId: changes.correctionId,
    confirmedTransactionHashes: hashes,
  });
  return value !== null && same(value, proposed) ? value : null;
};

const verifyInitTransaction = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  family: WatcherProofThreadFamilyV1,
): boolean => {
  const transaction = verified.transaction!;
  const canonical = canonicalTransaction(transaction);
  const layout = observation.layout!;
  const journal = observation.journal!;
  if (
    canonical === null ||
    !layoutEquals(layout, {
      threadInputIndex: null,
      threadOutputIndex: layout.threadOutputIndex,
      proofTokenOutputIndex: null,
      proofTokenReferenceInputIndex: null,
      stepSpendRedeemerGlobalIndex: null,
      computationThreadMintRedeemerGlobalIndex:
        layout.computationThreadMintRedeemerGlobalIndex,
      fraudProofMintRedeemerGlobalIndex: null,
    })
  ) {
    return false;
  }
  const thread = indexedOutput(
    transaction,
    canonical,
    layout.threadOutputIndex,
  );
  const mintPolicy = mintPolicyIndex(
    canonical.body,
    policy.computationThreadPolicyId,
  );
  const mintRedeemer = redeemerAtGlobal(
    transaction,
    layout.computationThreadMintRedeemerGlobalIndex,
    "mint",
    mintPolicy,
  );
  if (thread === null || mintRedeemer === null || mintPolicy < 0) {
    return false;
  }
  const decoded = dataRoundTrip<
    Readonly<{
      Init: Readonly<{
        first_step_output_index: bigint;
        fraud_category_id: string;
        fraud_category: string;
        fraud_proof_catalogue_ref_input_index: bigint;
        inclusion_proof_script_redeemer_index: bigint;
        hub_oracle_ref_input_index: bigint;
        fraudulent_block_ref_input_index: bigint;
      }>;
    }>
  >(mintRedeemer.bytes.bytesHex, FraudProofComputationThreadRedeemer);
  const init = decoded?.Init;
  const references = bodyReferenceInputs(canonical.body);
  if (
    init === undefined ||
    init.first_step_output_index !== BigInt(thread.index) ||
    init.fraud_category_id !== family.categoryId ||
    init.fraud_category !== family.firstStepScriptHash ||
    init.fraud_proof_catalogue_ref_input_index < 0n ||
    init.fraud_proof_catalogue_ref_input_index >= BigInt(references.length) ||
    init.hub_oracle_ref_input_index < 0n ||
    init.hub_oracle_ref_input_index >= BigInt(references.length) ||
    init.fraudulent_block_ref_input_index < 0n ||
    init.fraudulent_block_ref_input_index >= BigInt(references.length) ||
    init.inclusion_proof_script_redeemer_index < 0n ||
    init.inclusion_proof_script_redeemer_index >=
      BigInt(transaction.redeemers.length) ||
    transaction.redeemers[Number(init.inclusion_proof_script_redeemer_index)]
      ?.purpose !== "withdrawal" ||
    protocolUtxo(
      verified.sourceStore,
      references[Number(init.hub_oracle_ref_input_index)]!,
      "hub_oracle",
    ) === null ||
    protocolUtxo(
      verified.sourceStore,
      references[Number(init.fraudulent_block_ref_input_index)]!,
      "state_queue",
    ) === null ||
    !exactMint(
      canonical.body,
      new Map([
        [
          `${policy.computationThreadPolicyId}${journal.computationThreadAssetName}`,
          1n,
        ],
      ]),
    ) ||
    outputScriptHash(thread.output) !== family.firstStepScriptHash ||
    !exactTokenOutput(
      thread.output,
      policy.computationThreadPolicyId,
      journal.computationThreadAssetName,
    )
  ) {
    return false;
  }
  const datum = stepDatum(thread.output);
  const durable = protocolUtxo(
    verified.store,
    thread.outRef,
    "computation_thread",
  );
  return (
    datum?.fraud_prover === journal.fraudProver &&
    datum.data === null &&
    durable !== null &&
    durableOutputMatchesLifecycle(
      durable,
      thread.output,
      observation,
      verified.sourceStore,
    )
  );
};

const verifyThreadInput = (
  policy: WatcherProofThreadPolicyV1,
  source: WatcherProofThreadJournalV1,
  verified: VerifiedPublicContext,
  body: CML.TransactionBody,
  inputIndexText: string | null,
): Readonly<{ inputIndex: number; output: CML.TransactionOutput }> | null => {
  if (
    inputIndexText === null ||
    source.threadOutRef === null ||
    BigInt(inputIndexText) >= BigInt(body.inputs().len()) ||
    bodyInputs(body)[Number(inputIndexText)] !== source.threadOutRef
  ) {
    return null;
  }
  const input = sourceThreadOutput(verified.sourceStore, source);
  const datum = input === null ? null : stepDatum(input.output);
  return input !== null &&
    datum?.fraud_prover === source.fraudProver &&
    exactTokenOutput(
      input.output,
      policy.computationThreadPolicyId,
      source.computationThreadAssetName,
    )
    ? Object.freeze({
        inputIndex: Number(inputIndexText),
        output: input.output,
      })
    : null;
};

const verifyStepTransaction = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  source: WatcherProofThreadJournalV1,
  family: WatcherProofThreadFamilyV1,
): boolean => {
  const transaction = verified.transaction!;
  const canonical = canonicalTransaction(transaction);
  const layout = observation.layout!;
  const journal = observation.journal!;
  if (
    canonical === null ||
    !layoutEquals(layout, {
      threadInputIndex: layout.threadInputIndex,
      threadOutputIndex: layout.threadOutputIndex,
      proofTokenOutputIndex: null,
      proofTokenReferenceInputIndex: null,
      stepSpendRedeemerGlobalIndex: layout.stepSpendRedeemerGlobalIndex,
      computationThreadMintRedeemerGlobalIndex: null,
      fraudProofMintRedeemerGlobalIndex: null,
    })
  ) {
    return false;
  }
  const input = verifyThreadInput(
    policy,
    source,
    verified,
    canonical.body,
    layout.threadInputIndex,
  );
  const output = indexedOutput(
    transaction,
    canonical,
    layout.threadOutputIndex,
  );
  const spend =
    input === null
      ? null
      : redeemerAtGlobal(
          transaction,
          layout.stepSpendRedeemerGlobalIndex,
          "spend",
          input.inputIndex,
        );
  const datum = output === null ? null : stepDatum(output.output);
  const outputHash = output === null ? null : outputScriptHash(output.output);
  const sourceStep = Number(source.stepIndex!);
  const nextStep = family.nextStepIndexes[sourceStep]?.find(
    (index) => family.stepScriptHashes[Number(index)] === outputHash,
  );
  const durable =
    output === null
      ? null
      : protocolUtxo(verified.store, output.outRef, "computation_thread");
  return (
    input !== null &&
    output !== null &&
    spend !== null &&
    genericStepRedeemer(spend.bytes.bytesHex)?.kind === "continue" &&
    allMint(canonical.body).size === 0 &&
    nextStep !== undefined &&
    exactTokenOutput(
      output.output,
      policy.computationThreadPolicyId,
      source.computationThreadAssetName,
    ) &&
    datum?.fraud_prover === source.fraudProver &&
    datum.data !== null &&
    journal.stepIndex === nextStep &&
    durable !== null &&
    durableOutputMatchesLifecycle(
      durable,
      output.output,
      observation,
      verified.sourceStore,
    )
  );
};

const verifySuccessTransaction = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  source: WatcherProofThreadJournalV1,
  family: WatcherProofThreadFamilyV1,
): boolean => {
  const transaction = verified.transaction!;
  const canonical = canonicalTransaction(transaction);
  const layout = observation.layout!;
  const journal = observation.journal!;
  if (
    canonical === null ||
    !layoutEquals(layout, {
      threadInputIndex: layout.threadInputIndex,
      threadOutputIndex: null,
      proofTokenOutputIndex: layout.proofTokenOutputIndex,
      proofTokenReferenceInputIndex: null,
      stepSpendRedeemerGlobalIndex: layout.stepSpendRedeemerGlobalIndex,
      computationThreadMintRedeemerGlobalIndex:
        layout.computationThreadMintRedeemerGlobalIndex,
      fraudProofMintRedeemerGlobalIndex:
        layout.fraudProofMintRedeemerGlobalIndex,
    }) ||
    family.nextStepIndexes[Number(source.stepIndex!)]?.length !== 0
  ) {
    return false;
  }
  const input = verifyThreadInput(
    policy,
    source,
    verified,
    canonical.body,
    layout.threadInputIndex,
  );
  const proof = indexedOutput(
    transaction,
    canonical,
    layout.proofTokenOutputIndex,
  );
  const ctPolicyIndex = mintPolicyIndex(
    canonical.body,
    policy.computationThreadPolicyId,
  );
  const proofPolicyIndex = mintPolicyIndex(
    canonical.body,
    policy.fraudProofPolicyId,
  );
  const stepRedeemer =
    input === null
      ? null
      : redeemerAtGlobal(
          transaction,
          layout.stepSpendRedeemerGlobalIndex,
          "spend",
          input.inputIndex,
        );
  const ctRedeemer = redeemerAtGlobal(
    transaction,
    layout.computationThreadMintRedeemerGlobalIndex,
    "mint",
    ctPolicyIndex,
  );
  const proofRedeemer = redeemerAtGlobal(
    transaction,
    layout.fraudProofMintRedeemerGlobalIndex,
    "mint",
    proofPolicyIndex,
  );
  if (
    input === null ||
    proof === null ||
    stepRedeemer === null ||
    ctRedeemer === null ||
    proofRedeemer === null ||
    ctPolicyIndex < 0 ||
    proofPolicyIndex < 0 ||
    genericStepRedeemer(stepRedeemer.bytes.bytesHex)?.kind !== "continue"
  ) {
    return false;
  }
  const decodedCt = dataRoundTrip<
    Readonly<{ Success: { burning_token_asset_name: string } }>
  >(ctRedeemer.bytes.bytesHex, FraudProofComputationThreadRedeemer);
  const decodedProof = dataRoundTrip<
    Readonly<{
      computation_thread_token_asset_name: string;
      computation_thread_mint_redeemer_index: bigint;
    }>
  >(proofRedeemer.bytes.bytesHex, FraudProofTokenMintRedeemer);
  const datum = proofTokenDatum(proof.output);
  const durable = protocolUtxo(verified.store, proof.outRef, "proof_thread");
  return (
    decodedCt?.Success.burning_token_asset_name ===
      source.computationThreadAssetName &&
    decodedProof?.computation_thread_token_asset_name ===
      source.computationThreadAssetName &&
    decodedProof.computation_thread_mint_redeemer_index ===
      BigInt(layout.computationThreadMintRedeemerGlobalIndex!) &&
    exactMint(
      canonical.body,
      new Map([
        [
          `${policy.computationThreadPolicyId}${source.computationThreadAssetName}`,
          -1n,
        ],
        [
          `${policy.fraudProofPolicyId}${source.computationThreadAssetName}`,
          1n,
        ],
      ]),
    ) &&
    outputScriptHash(proof.output) === policy.fraudProofSpendScriptHash &&
    proof.output.address().to_hex() === policy.fraudProofAddressHex &&
    exactTokenOutput(
      proof.output,
      policy.fraudProofPolicyId,
      source.computationThreadAssetName,
    ) &&
    datum?.fraud_prover === source.fraudProver &&
    journal.proofTokenOutRef === proof.outRef &&
    durable !== null &&
    durableOutputMatchesLifecycle(
      durable,
      proof.output,
      observation,
      verified.sourceStore,
    )
  );
};

const verifyCancelTransaction = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  source: WatcherProofThreadJournalV1,
): boolean => {
  const transaction = verified.transaction!;
  const canonical = canonicalTransaction(transaction);
  const layout = observation.layout!;
  if (
    canonical === null ||
    !layoutEquals(layout, {
      threadInputIndex: layout.threadInputIndex,
      threadOutputIndex: null,
      proofTokenOutputIndex: null,
      proofTokenReferenceInputIndex: null,
      stepSpendRedeemerGlobalIndex: layout.stepSpendRedeemerGlobalIndex,
      computationThreadMintRedeemerGlobalIndex:
        layout.computationThreadMintRedeemerGlobalIndex,
      fraudProofMintRedeemerGlobalIndex: null,
    })
  ) {
    return false;
  }
  const input = verifyThreadInput(
    policy,
    source,
    verified,
    canonical.body,
    layout.threadInputIndex,
  );
  const ctPolicyIndex = mintPolicyIndex(
    canonical.body,
    policy.computationThreadPolicyId,
  );
  const step =
    input === null
      ? null
      : redeemerAtGlobal(
          transaction,
          layout.stepSpendRedeemerGlobalIndex,
          "spend",
          input.inputIndex,
        );
  const ct = redeemerAtGlobal(
    transaction,
    layout.computationThreadMintRedeemerGlobalIndex,
    "mint",
    ctPolicyIndex,
  );
  if (input === null || step === null || ct === null || ctPolicyIndex < 0) {
    return false;
  }
  const stepDecoded = genericStepRedeemer(step.bytes.bytesHex);
  const ctDecoded = dataRoundTrip<
    Readonly<{ BurnForCancellation: { burning_token_asset_name: string } }>
  >(ct.bytes.bytesHex, FraudProofComputationThreadRedeemer);
  return (
    stepDecoded?.kind === "cancel" &&
    stepDecoded.inputIndex === BigInt(input.inputIndex) &&
    stepDecoded.mintRedeemerGlobalIndex ===
      BigInt(layout.computationThreadMintRedeemerGlobalIndex!) &&
    ctDecoded?.BurnForCancellation.burning_token_asset_name ===
      source.computationThreadAssetName &&
    exactMint(
      canonical.body,
      new Map([
        [
          `${policy.computationThreadPolicyId}${source.computationThreadAssetName}`,
          -1n,
        ],
      ]),
    ) &&
    !verified.store.protocolUtxos.some(
      ({ role, outRef }) =>
        (role === "computation_thread" || role === "proof_thread") &&
        outRef.startsWith(`${transaction.txHash}#`),
    )
  );
};

const verifyRemovalTransaction = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  source: WatcherProofThreadJournalV1,
): boolean => {
  const transaction = verified.transaction!;
  const canonical = canonicalTransaction(transaction);
  const layout = observation.layout!;
  if (
    canonical === null ||
    !layoutEquals(layout, {
      threadInputIndex: null,
      threadOutputIndex: null,
      proofTokenOutputIndex: null,
      proofTokenReferenceInputIndex: layout.proofTokenReferenceInputIndex,
      stepSpendRedeemerGlobalIndex: null,
      computationThreadMintRedeemerGlobalIndex: null,
      fraudProofMintRedeemerGlobalIndex: null,
    }) ||
    layout.proofTokenReferenceInputIndex === null
  ) {
    return false;
  }
  const references = bodyReferenceInputs(canonical.body);
  if (
    BigInt(layout.proofTokenReferenceInputIndex) >= BigInt(references.length) ||
    references[Number(layout.proofTokenReferenceInputIndex)] !==
      source.proofTokenOutRef
  ) {
    return false;
  }
  const proof = sourceProofTokenOutput(verified.sourceStore, source);
  const datum = proof === null ? null : proofTokenDatum(proof.output);
  return (
    proof !== null &&
    outputScriptHash(proof.output) === policy.fraudProofSpendScriptHash &&
    exactTokenOutput(
      proof.output,
      policy.fraudProofPolicyId,
      source.computationThreadAssetName,
    ) &&
    datum?.fraud_prover === source.fraudProver &&
    !bodyInputs(canonical.body).includes(source.proofTokenOutRef!)
  );
};

type LifecycleVerification = Readonly<{
  journal: WatcherProofThreadJournalV1;
  sourceJournal: WatcherProofThreadJournalV1 | null;
}>;

const verifyLifecycle = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  verified: VerifiedPublicContext,
  expectedSourceJournal: WatcherProofThreadJournalV1 | null,
): LifecycleVerification | null => {
  if (
    verified.transaction === null ||
    observation.journal === null ||
    observation.submissionId === null ||
    observation.confirmationId === null ||
    observation.layout === null ||
    !same(verified.sourceJournal, expectedSourceJournal) ||
    !same(verified.journal, observation.journal)
  ) {
    return null;
  }
  const proposed = observation.journal;
  const family = baseJournalIdentityMatches(policy, proposed, verified.store);
  if (
    family === null ||
    (expectedSourceJournal !== null &&
      (!same(
        {
          journalId: expectedSourceJournal.journalId,
          faultId: expectedSourceJournal.faultId,
          familyId: expectedSourceJournal.familyId,
          fraudulentBlockHash: expectedSourceJournal.fraudulentBlockHash,
          fraudulentHeaderHash: expectedSourceJournal.fraudulentHeaderHash,
          fraudProver: expectedSourceJournal.fraudProver,
          computationThreadAssetName:
            expectedSourceJournal.computationThreadAssetName,
        },
        {
          journalId: proposed.journalId,
          faultId: proposed.faultId,
          familyId: proposed.familyId,
          fraudulentBlockHash: proposed.fraudulentBlockHash,
          fraudulentHeaderHash: proposed.fraudulentHeaderHash,
          fraudProver: proposed.fraudProver,
          computationThreadAssetName: proposed.computationThreadAssetName,
        },
      ) ||
        expectedSourceJournal.phase !== "active") &&
      observation.transitionKind !== "removal")
  ) {
    return null;
  }
  let expected: WatcherProofThreadJournalV1 | null = null;
  let transactionValid = false;
  switch (observation.transitionKind) {
    case "init": {
      if (expectedSourceJournal !== null) {
        return null;
      }
      transactionValid = verifyInitTransaction(
        policy,
        observation,
        verified,
        family,
      );
      expected = expectedJournal(
        null,
        proposed,
        verified.transaction.txHash,
        observation.submissionId,
        observation.confirmationId,
        {
          phase: "active",
          stepIndex: "0",
          threadOutRef: `${verified.transaction.txHash}#${observation.layout.threadOutputIndex!}`,
          proofTokenOutRef: null,
          correctionId: null,
        },
      );
      break;
    }
    case "step": {
      if (expectedSourceJournal?.phase !== "active") {
        return null;
      }
      transactionValid = verifyStepTransaction(
        policy,
        observation,
        verified,
        expectedSourceJournal,
        family,
      );
      expected = expectedJournal(
        expectedSourceJournal,
        proposed,
        verified.transaction.txHash,
        observation.submissionId,
        observation.confirmationId,
        {
          phase: "active",
          stepIndex: (BigInt(expectedSourceJournal.stepIndex!) + 1n).toString(),
          threadOutRef: `${verified.transaction.txHash}#${observation.layout.threadOutputIndex!}`,
          proofTokenOutRef: null,
          correctionId: null,
        },
      );
      break;
    }
    case "success": {
      if (expectedSourceJournal?.phase !== "active") {
        return null;
      }
      transactionValid = verifySuccessTransaction(
        policy,
        observation,
        verified,
        expectedSourceJournal,
        family,
      );
      expected = expectedJournal(
        expectedSourceJournal,
        proposed,
        verified.transaction.txHash,
        observation.submissionId,
        observation.confirmationId,
        {
          phase: "proven",
          stepIndex: null,
          threadOutRef: null,
          proofTokenOutRef: `${verified.transaction.txHash}#${observation.layout.proofTokenOutputIndex!}`,
          correctionId: null,
        },
      );
      break;
    }
    case "cancel": {
      if (expectedSourceJournal?.phase !== "active") {
        return null;
      }
      transactionValid = verifyCancelTransaction(
        policy,
        observation,
        verified,
        expectedSourceJournal,
      );
      expected = expectedJournal(
        expectedSourceJournal,
        proposed,
        verified.transaction.txHash,
        observation.submissionId,
        observation.confirmationId,
        {
          phase: "cancelled",
          stepIndex: null,
          threadOutRef: null,
          proofTokenOutRef: null,
          correctionId: null,
        },
      );
      break;
    }
    case "removal": {
      if (
        expectedSourceJournal?.phase !== "proven" ||
        proposed.correctionId === null
      ) {
        return null;
      }
      transactionValid = verifyRemovalTransaction(
        policy,
        observation,
        verified,
        expectedSourceJournal,
      );
      expected = expectedJournal(
        expectedSourceJournal,
        proposed,
        verified.transaction.txHash,
        observation.submissionId,
        observation.confirmationId,
        {
          phase: "removed",
          stepIndex: null,
          threadOutRef: null,
          proofTokenOutRef: expectedSourceJournal.proofTokenOutRef,
          correctionId: proposed.correctionId,
        },
      );
      break;
    }
    case "rollback":
      return null;
  }
  return transactionValid && expected !== null
    ? Object.freeze({ journal: expected, sourceJournal: expectedSourceJournal })
    : null;
};

const contextStructural = (
  value: unknown,
): WatcherProofThreadPublicContextV1 | null => {
  if (!evidenceWithinBounds(value)) {
    return null;
  }
  const record = exactRecord(value, [
    "schemaVersion",
    "authenticatedProvider",
    "l1Observation",
    "sourceDurableStore",
    "durableStore",
    "deploymentAuthority",
    "finalityAuthority",
    "rollbackAuthority",
    "sourceJournal",
    "durableJournal",
  ]);
  const deployment =
    record === null
      ? null
      : exactRecord(record.deploymentAuthority, [
          "signedIdentity",
          "policy",
          "trustRoots",
          "result",
        ]);
  const finality =
    record?.finalityAuthority === null
      ? null
      : exactRecord(record?.finalityAuthority, [
          "policy",
          "lineage",
          "previousState",
          "observations",
          "consistency",
          "result",
        ]);
  const rollback =
    record?.rollbackAuthority === null
      ? null
      : exactRecord(record?.rollbackAuthority, ["result", "context"]);
  if (
    record === null ||
    deployment === null ||
    !Array.isArray(deployment.trustRoots) ||
    record.schemaVersion !==
      WATCHER_PROOF_THREAD_PUBLIC_CONTEXT_V1_SCHEMA_VERSION ||
    (record.finalityAuthority !== null &&
      (finality === null ||
        !Array.isArray(finality.lineage) ||
        finality.lineage.length >
          WATCHER_PROOF_THREAD_V1_BOUNDS.finalityLineageSteps ||
        !Array.isArray(finality.observations) ||
        finality.observations.length >
          WATCHER_PROOF_THREAD_V1_BOUNDS.observationsPerFinalityStep ||
        finality.lineage.some((candidate) => {
          const step = exactRecord(candidate, [
            "observations",
            "consistency",
            "result",
          ]);
          return (
            step === null ||
            !Array.isArray(step.observations) ||
            step.observations.length >
              WATCHER_PROOF_THREAD_V1_BOUNDS.observationsPerFinalityStep
          );
        }))) ||
    (record.rollbackAuthority !== null && rollback === null)
  ) {
    return null;
  }
  return value as WatcherProofThreadPublicContextV1;
};

const entryWithoutDigest = (
  value: Omit<WatcherProofThreadHistoryEntryV1, "entryDigest">,
) => ({ ...value });

const makeEntry = (
  observation: WatcherProofThreadObservationV1,
  context: WatcherProofThreadPublicContextV1,
  journal: WatcherProofThreadJournalV1 | null,
  sourceJournal: WatcherProofThreadJournalV1 | null,
  rollbackResult:
    | WatcherRollbackResultV1
    | WatcherPostFinalityRecoveryResultV1
    | null,
): WatcherProofThreadHistoryEntryV1 => {
  const canonical = Object.freeze({
    predecessorStateDigest: observation.predecessorStateDigest,
    transitionKind: observation.transitionKind,
    confirmationPhase: observation.confirmationPhase,
    chainPointId: observation.chainPointId,
    transactionHash: observation.transactionHash,
    submissionId: observation.submissionId,
    confirmationId: observation.confirmationId,
    sourceJournalDigest: sourceJournal?.journalDigest ?? null,
    journal,
    observation,
    publicContext: context,
    rollbackResult,
  });
  return Object.freeze({
    ...canonical,
    entryDigest: sha256Canonical(entryWithoutDigest(canonical)),
  });
};

const parseEntryStructural = (
  value: unknown,
): WatcherProofThreadHistoryEntryV1 | null => {
  const record = exactRecord(value, [
    "predecessorStateDigest",
    "transitionKind",
    "confirmationPhase",
    "chainPointId",
    "transactionHash",
    "submissionId",
    "confirmationId",
    "sourceJournalDigest",
    "journal",
    "observation",
    "publicContext",
    "rollbackResult",
    "entryDigest",
  ]);
  const observation =
    record === null
      ? null
      : parseWatcherProofThreadObservationV1(record.observation);
  const context =
    record === null ? null : contextStructural(record.publicContext);
  const journal =
    record?.journal === null
      ? null
      : parseWatcherProofThreadJournalV1(record?.journal);
  if (
    record === null ||
    observation === null ||
    context === null ||
    (record.journal !== null && journal === null) ||
    !isNullableHex32(record.predecessorStateDigest) ||
    record.predecessorStateDigest !== observation.predecessorStateDigest ||
    record.transitionKind !== observation.transitionKind ||
    record.confirmationPhase !== observation.confirmationPhase ||
    record.chainPointId !== observation.chainPointId ||
    record.transactionHash !== observation.transactionHash ||
    record.submissionId !== observation.submissionId ||
    record.confirmationId !== observation.confirmationId ||
    !isNullableHex32(record.sourceJournalDigest) ||
    !isHex32(record.entryDigest) ||
    (observation.transitionKind !== "rollback" &&
      !same(record.journal, observation.journal))
  ) {
    return null;
  }
  if (
    (observation.transitionKind === "rollback") !==
    (record.rollbackResult !== null)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    predecessorStateDigest: record.predecessorStateDigest,
    transitionKind: observation.transitionKind,
    confirmationPhase: observation.confirmationPhase,
    chainPointId: observation.chainPointId,
    transactionHash: observation.transactionHash,
    submissionId: observation.submissionId,
    confirmationId: observation.confirmationId,
    sourceJournalDigest: record.sourceJournalDigest,
    journal,
    observation,
    publicContext: context,
    rollbackResult: record.rollbackResult as
      | WatcherRollbackResultV1
      | WatcherPostFinalityRecoveryResultV1
      | null,
  });
  return sha256Canonical(entryWithoutDigest(canonical)) === record.entryDigest
    ? Object.freeze({ ...canonical, entryDigest: record.entryDigest })
    : null;
};

const auditWithoutDigest = (
  value: Omit<WatcherProofThreadAuditEntryV1, "auditDigest">,
) => ({ ...value });

const makeAudit = (
  status: WatcherProofThreadAuditEntryV1["status"],
  entry: WatcherProofThreadHistoryEntryV1,
): WatcherProofThreadAuditEntryV1 => {
  const canonical = Object.freeze({ status, entry });
  return Object.freeze({
    ...canonical,
    auditDigest: sha256Canonical(auditWithoutDigest(canonical)),
  });
};

const parseAuditStructural = (
  value: unknown,
): WatcherProofThreadAuditEntryV1 | null => {
  const record = exactRecord(value, ["status", "entry", "auditDigest"]);
  const entry = record === null ? null : parseEntryStructural(record.entry);
  if (
    record === null ||
    entry === null ||
    (record.status !== "orphaned" && record.status !== "rollback") ||
    !isHex32(record.auditDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({ status: record.status, entry });
  return sha256Canonical(auditWithoutDigest(canonical)) === record.auditDigest
    ? Object.freeze({ ...canonical, auditDigest: record.auditDigest })
    : null;
};

const pendingStructural = (
  value: unknown,
): WatcherProofThreadPendingV1 | null => {
  const record = exactRecord(value, [
    "transitionKind",
    "transactionHash",
    "submissionId",
    "confirmationId",
    "sourceJournalDigest",
    "journal",
    "pendingEntryDigest",
  ]);
  const journal =
    record === null ? null : parseWatcherProofThreadJournalV1(record.journal);
  if (
    record === null ||
    journal === null ||
    !["init", "step", "success", "cancel", "removal"].includes(
      record.transitionKind as string,
    ) ||
    !isHex32(record.transactionHash) ||
    !isHex32(record.submissionId) ||
    !isHex32(record.confirmationId) ||
    !isNullableHex32(record.sourceJournalDigest) ||
    !isHex32(record.pendingEntryDigest)
  ) {
    return null;
  }
  return Object.freeze({
    transitionKind: record.transitionKind as Exclude<
      WatcherProofThreadTransitionKindV1,
      "rollback"
    >,
    transactionHash: record.transactionHash,
    submissionId: record.submissionId,
    confirmationId: record.confirmationId,
    sourceJournalDigest: record.sourceJournalDigest,
    journal,
    pendingEntryDigest: record.pendingEntryDigest,
  });
};

const pendingFromEntry = (
  entry: WatcherProofThreadHistoryEntryV1,
): WatcherProofThreadPendingV1 =>
  Object.freeze({
    transitionKind: entry.transitionKind as Exclude<
      WatcherProofThreadTransitionKindV1,
      "rollback"
    >,
    transactionHash: entry.transactionHash!,
    submissionId: entry.submissionId!,
    confirmationId: entry.confirmationId!,
    sourceJournalDigest: entry.sourceJournalDigest,
    journal: entry.journal!,
    pendingEntryDigest: entry.entryDigest,
  });

const pendingMatchesFinal = (
  pending: WatcherProofThreadPendingV1,
  entry: WatcherProofThreadHistoryEntryV1,
): boolean =>
  pending.transitionKind === entry.transitionKind &&
  pending.transactionHash === entry.transactionHash &&
  pending.submissionId === entry.submissionId &&
  pending.confirmationId === entry.confirmationId &&
  pending.sourceJournalDigest === entry.sourceJournalDigest &&
  same(pending.journal, entry.journal);

type ReplayedHistory = Readonly<{
  journal: WatcherProofThreadJournalV1 | null;
  pending: WatcherProofThreadPendingV1 | null;
  verified: readonly VerifiedPublicContext[];
}>;

const verifyHistory = (
  policy: WatcherProofThreadPolicyV1,
  entries: readonly WatcherProofThreadHistoryEntryV1[],
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): ReplayedHistory | null => {
  let journal: WatcherProofThreadJournalV1 | null = null;
  let pending: WatcherProofThreadPendingV1 | null = null;
  const verifiedContexts: VerifiedPublicContext[] = [];
  for (const entry of entries) {
    if (entry.transitionKind === "rollback") {
      return null;
    }
    const promotion =
      entry.confirmationPhase === "final" &&
      pending !== null &&
      pendingMatchesFinal(pending, entry);
    if (
      (entry.confirmationPhase === "pending" && pending !== null) ||
      (entry.confirmationPhase === "final" && pending !== null && !promotion)
    ) {
      return null;
    }
    const verified = parsePublicContext(
      policy,
      entry.observation,
      entry.publicContext,
      !promotion,
      transportAttestations,
    );
    const lifecycle: LifecycleVerification | null =
      verified === null
        ? null
        : verifyLifecycle(policy, entry.observation, verified, journal);
    if (
      verified === null ||
      lifecycle === null ||
      entry.sourceJournalDigest !== (journal?.journalDigest ?? null) ||
      !same(entry.journal, lifecycle.journal)
    ) {
      return null;
    }
    if (entry.confirmationPhase === "pending") {
      pending = pendingFromEntry(entry);
    } else {
      journal = lifecycle.journal;
      pending = null;
    }
    verifiedContexts.push(verified);
  }
  return Object.freeze({
    journal,
    pending,
    verified: Object.freeze(verifiedContexts),
  });
};

const stateWithoutDigest = (
  value:
    | WatcherProofThreadStateV1
    | Omit<WatcherProofThreadStateV1, "stateDigest">,
) => {
  const { stateDigest: _stateDigest, ...canonical } =
    value as WatcherProofThreadStateV1;
  return canonical;
};

const makeState = (
  policy: WatcherProofThreadPolicyV1,
  observation: WatcherProofThreadObservationV1,
  journal: WatcherProofThreadJournalV1 | null,
  pending: WatcherProofThreadPendingV1 | null,
  history: readonly WatcherProofThreadHistoryEntryV1[],
  auditHistory: readonly WatcherProofThreadAuditEntryV1[],
  transitionHistory: readonly WatcherProofThreadHistoryEntryV1[],
): WatcherProofThreadStateV1 => {
  const canonical = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    pointDigest: observation.pointDigest,
    durableStoreDigest: observation.durableStoreDigest,
    journal,
    pending,
    history: Object.freeze([...history]),
    auditHistory: Object.freeze([...auditHistory]),
    transitionHistory: Object.freeze([...transitionHistory]),
  });
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(stateWithoutDigest(canonical)),
  });
};

const currentStateAnchorEntry = (
  pointDigest: string,
  durableStoreDigest: string,
  history: readonly WatcherProofThreadHistoryEntryV1[],
  auditHistory: readonly WatcherProofThreadAuditEntryV1[],
): WatcherProofThreadHistoryEntryV1 | null => {
  const historyEntry = history.at(-1);
  if (
    historyEntry !== undefined &&
    historyEntry.observation.pointDigest === pointDigest &&
    historyEntry.observation.durableStoreDigest === durableStoreDigest
  ) {
    return historyEntry;
  }
  const rollbackAudit = auditHistory.at(-1);
  return rollbackAudit?.status === "rollback" &&
    rollbackAudit.entry.observation.pointDigest === pointDigest &&
    rollbackAudit.entry.observation.durableStoreDigest === durableStoreDigest
    ? rollbackAudit.entry
    : null;
};

const authenticatedStateStore = (
  state: WatcherProofThreadStateV1,
): Readonly<{
  anchor: WatcherProofThreadHistoryEntryV1;
  store: WatcherDurableStoreV1;
}> | null => {
  const anchor = currentStateAnchorEntry(
    state.pointDigest,
    state.durableStoreDigest,
    state.history,
    state.auditHistory,
  );
  if (anchor === null) {
    return null;
  }
  try {
    const store = parseWatcherDurableStoreV1(anchor.publicContext.durableStore);
    return storeDigest(store) === state.durableStoreDigest
      ? Object.freeze({ anchor, store })
      : null;
  } catch {
    return null;
  }
};

export const parseWatcherProofThreadStateV1 = (
  value: unknown,
  policyInput: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherProofThreadStateV1 | null => {
  if (!evidenceWithinBounds({ policy: policyInput, state: value })) {
    return null;
  }
  const policy = parseWatcherProofThreadPolicyV1(policyInput);
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "pointDigest",
    "durableStoreDigest",
    "journal",
    "pending",
    "history",
    "auditHistory",
    "transitionHistory",
    "stateDigest",
  ]);
  const marker = record === null ? null : parseMarker(record.deploymentMarker);
  const journal =
    record?.journal === null
      ? null
      : parseWatcherProofThreadJournalV1(record?.journal);
  const pending =
    record?.pending === null ? null : pendingStructural(record?.pending);
  const historyValues =
    record === null
      ? null
      : exactArray(
          record.history,
          WATCHER_PROOF_THREAD_V1_BOUNDS.historyEntries,
        );
  const auditValues =
    record === null
      ? null
      : exactArray(
          record.auditHistory,
          WATCHER_PROOF_THREAD_V1_BOUNDS.auditEntries,
        );
  const transitionValues =
    record === null
      ? null
      : exactArray(
          record.transitionHistory,
          WATCHER_PROOF_THREAD_V1_BOUNDS.transitionEntries,
        );
  const history = historyValues?.map(parseEntryStructural) ?? null;
  const audit = auditValues?.map(parseAuditStructural) ?? null;
  const transitions = transitionValues?.map(parseEntryStructural) ?? null;
  if (
    policy === null ||
    record === null ||
    marker === null ||
    historyValues === null ||
    auditValues === null ||
    transitionValues === null ||
    history === null ||
    audit === null ||
    transitions === null ||
    history.some((entry) => entry === null) ||
    audit.some((entry) => entry === null) ||
    transitions.some((entry) => entry === null) ||
    (record.journal !== null && journal === null) ||
    (record.pending !== null && pending === null) ||
    record.schemaVersion !== WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION ||
    record.policyDigest !== policy.policyDigest ||
    record.network !== policy.network ||
    record.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(marker, policy.deploymentMarker) ||
    !isHex32(record.pointDigest) ||
    !isHex32(record.durableStoreDigest) ||
    !isHex32(record.stateDigest) ||
    history.length > Number(policy.maximumHistoryEntries) ||
    transitions.length === 0 ||
    new Set(history.map((entry) => entry!.entryDigest)).size !==
      history.length ||
    new Set(audit.map((entry) => entry!.auditDigest)).size !== audit.length ||
    new Set(transitions.map((entry) => entry!.entryDigest)).size !==
      transitions.length
  ) {
    return null;
  }
  const canonicalHistory = history as WatcherProofThreadHistoryEntryV1[];
  const canonicalAudit = audit as WatcherProofThreadAuditEntryV1[];
  const canonicalTransitions =
    transitions as WatcherProofThreadHistoryEntryV1[];
  const claimed = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_STATE_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: marker,
    pointDigest: record.pointDigest,
    durableStoreDigest: record.durableStoreDigest,
    journal,
    pending,
    history: Object.freeze(canonicalHistory),
    auditHistory: Object.freeze(canonicalAudit),
    transitionHistory: Object.freeze(canonicalTransitions),
    stateDigest: record.stateDigest,
  });
  if (sha256Canonical(stateWithoutDigest(claimed)) !== record.stateDigest) {
    return null;
  }
  let replayedState: WatcherProofThreadStateV1 | null = null;
  for (const transition of canonicalTransitions) {
    const replayed = applyWatcherProofThreadTransitionV1(
      policy,
      replayedState,
      transition.observation,
      transition.publicContext,
      transportAttestations,
    );
    if (
      replayed.action !== "accept" ||
      replayed.state === null ||
      !same(replayed.state.transitionHistory.at(-1), transition)
    ) {
      return null;
    }
    replayedState = replayed.state;
  }
  if (replayedState !== null && same(replayedState, claimed)) {
    return replayedState;
  }
  return null;
};

const resultWithoutDigest = (
  value: Omit<WatcherProofThreadResultV1, "resultDigest">,
) => ({ ...value });

const makeResult = (
  value: Omit<WatcherProofThreadResultV1, "schemaVersion" | "resultDigest">,
): WatcherProofThreadResultV1 => {
  const canonical = Object.freeze({
    schemaVersion: WATCHER_PROOF_THREAD_RESULT_V1_SCHEMA_VERSION,
    ...value,
  });
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(resultWithoutDigest(canonical)),
  });
};

const rejected = (
  reason: WatcherProofThreadReasonCodeV1,
  alert: WatcherProofThreadAlertCodeV1 = "watcher_proof_thread_input_rejected",
  quarantined = false,
): WatcherProofThreadResultV1 =>
  makeResult({
    action: "reject",
    protocolDecision: quarantined ? "quarantined" : "hold",
    reasonCodes: Object.freeze([reason]),
    alertCodes: Object.freeze([alert]),
    state: null,
  });

const transitionReason = (
  kind: Exclude<WatcherProofThreadTransitionKindV1, "rollback">,
  phase: WatcherProofThreadConfirmationPhaseV1,
): WatcherProofThreadReasonCodeV1 =>
  `${kind}_${phase === "final" ? "confirmed" : "pending"}` as WatcherProofThreadReasonCodeV1;

const relevantRemovedSetMatchesSuffix = (
  previous: WatcherProofThreadStateV1,
  suffix: readonly WatcherProofThreadHistoryEntryV1[],
  rollback: WatcherRollbackResultV1 | WatcherPostFinalityRecoveryResultV1,
): boolean => {
  const suffixSubmissions = new Set(
    suffix.flatMap(({ submissionId }) =>
      submissionId === null ? [] : [submissionId],
    ),
  );
  const suffixConfirmations = new Set(
    suffix.flatMap(({ confirmationId }) =>
      confirmationId === null ? [] : [confirmationId],
    ),
  );
  const suffixPoints = new Set(suffix.map(({ chainPointId }) => chainPointId));
  const allSubmissions = new Set(
    previous.history.flatMap(({ submissionId }) =>
      submissionId === null ? [] : [submissionId],
    ),
  );
  const allConfirmations = new Set(
    previous.history.flatMap(({ confirmationId }) =>
      confirmationId === null ? [] : [confirmationId],
    ),
  );
  const allPoints = new Set(
    previous.history.map(({ chainPointId }) => chainPointId),
  );
  return (
    rollback.removedRecords.submissionIds
      .filter((id) => allSubmissions.has(id))
      .every((id) => suffixSubmissions.has(id)) &&
    rollback.removedRecords.confirmationIds
      .filter((id) => allConfirmations.has(id))
      .every((id) => suffixConfirmations.has(id)) &&
    [...suffixConfirmations].every((id) =>
      rollback.removedRecords.confirmationIds.includes(id),
    ) &&
    rollback.removedRecords.chainPointIds
      .filter((id) => allPoints.has(id))
      .every((id) => suffixPoints.has(id)) &&
    [...suffixPoints].every((id) =>
      rollback.removedRecords.chainPointIds.includes(id),
    )
  );
};

const applyWatcherProofThreadTransitionV1 = (
  policy: WatcherProofThreadPolicyV1,
  previous: WatcherProofThreadStateV1 | null,
  observation: WatcherProofThreadObservationV1,
  publicContextInput: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherProofThreadResultV1 => {
  if (
    observation.policyDigest !== policy.policyDigest ||
    observation.network !== policy.network ||
    observation.releaseEvidenceDigest !== policy.releaseEvidenceDigest ||
    !sameMarker(observation.deploymentMarker, policy.deploymentMarker)
  ) {
    return rejected(
      "deployment_mismatch",
      "watcher_proof_thread_binding_rejected",
    );
  }
  if (
    observation.predecessorStateDigest !== (previous?.stateDigest ?? null) ||
    (previous === null && observation.transitionKind !== "init")
  ) {
    return rejected("stale_state");
  }
  if (
    previous?.history.some(
      ({ observation: prior }) =>
        prior.observationDigest === observation.observationDigest,
    )
  ) {
    return makeResult({
      action: "duplicate",
      protocolDecision: "hold",
      reasonCodes: Object.freeze(["duplicate_observation"]),
      alertCodes: Object.freeze([]),
      state: previous,
    });
  }
  if (
    previous !== null &&
    previous.transitionHistory.length >=
      WATCHER_PROOF_THREAD_V1_BOUNDS.transitionEntries
  ) {
    return rejected("history_limit_exceeded");
  }
  if (observation.transitionKind === "rollback") {
    if (previous === null) {
      return rejected("rollback_mismatch");
    }
    const verified = parsePublicContext(
      policy,
      observation,
      publicContextInput,
      false,
      transportAttestations,
    );
    const authority = verified?.context.rollbackAuthority;
    const rollbackResult =
      authority === null || authority === undefined
        ? null
        : parseWatcherRollbackResultV1(authority.result, {
            ...(authority.context as WatcherRollbackVerificationContextV1),
            transportAttestations,
          });
    const recoveryResult =
      rollbackResult === null && authority !== null && authority !== undefined
        ? parseWatcherPostFinalityRecoveryResultV1(authority.result, {
            ...(authority.context as WatcherPostFinalityRecoveryInputV1),
            transportAttestations,
          })
        : null;
    const rollback = rollbackResult ?? recoveryResult;
    const previousAuthenticated = authenticatedStateStore(previous);
    if (
      verified === null ||
      rollback === null ||
      rollback.nextStore === null ||
      (rollbackResult !== null &&
        rollbackResult.action === "quarantine_incident") ||
      (recoveryResult !== null &&
        recoveryResult.action !== "rewind_and_replay") ||
      previousAuthenticated === null ||
      !durableStoreExtends(previousAuthenticated.store, verified.sourceStore)
    ) {
      return rejected(
        rollbackResult?.action === "quarantine_incident"
          ? "post_finality_quarantine"
          : "rollback_authority_mismatch",
        rollbackResult?.action === "quarantine_incident"
          ? "watcher_proof_thread_post_finality_incident"
          : "watcher_proof_thread_binding_rejected",
        rollbackResult?.action === "quarantine_incident",
      );
    }
    if (rollbackResult?.action === "duplicate_rewind") {
      if (
        !same(verified.sourceJournal, previous.journal) ||
        !same(verified.journal, previous.journal) ||
        !same(verified.sourceStore, verified.store)
      ) {
        return rejected("rollback_authority_mismatch");
      }
      return makeResult({
        action: "duplicate",
        protocolDecision: "hold",
        reasonCodes: Object.freeze(["duplicate_observation"]),
        alertCodes: Object.freeze([]),
        state: previous,
      });
    }
    const removedSubmissions = new Set(rollback.removedRecords.submissionIds);
    const removedConfirmations = new Set(
      rollback.removedRecords.confirmationIds,
    );
    const removedPoints = new Set(rollback.removedRecords.chainPointIds);
    const commonAncestorBlockNo =
      recoveryResult?.recoveryState?.path.commonAncestorBlockNo ?? null;
    const firstOrphan =
      recoveryResult === null
        ? previous.history.findIndex(
            (entry) =>
              (entry.submissionId !== null &&
                removedSubmissions.has(entry.submissionId)) ||
              (entry.confirmationId !== null &&
                removedConfirmations.has(entry.confirmationId)) ||
              removedPoints.has(entry.chainPointId),
          )
        : previous.history.findIndex(
            (entry) =>
              commonAncestorBlockNo !== null &&
              BigInt(entry.observation.blockNo) > BigInt(commonAncestorBlockNo),
          );
    if (recoveryResult === null && firstOrphan < 0) {
      return rejected("rollback_mismatch");
    }
    const retained = previous.history.slice(0, firstOrphan);
    const orphaned = firstOrphan < 0 ? [] : previous.history.slice(firstOrphan);
    const retainedHistory = firstOrphan < 0 ? [...previous.history] : retained;
    const target = verifyHistory(
      policy,
      retainedHistory,
      transportAttestations,
    );
    let targetStore: WatcherDurableStoreV1 | null = null;
    const targetAnchor = retainedHistory.at(-1);
    try {
      targetStore =
        targetAnchor === undefined
          ? null
          : parseWatcherDurableStoreV1(targetAnchor.publicContext.durableStore);
    } catch {
      return rejected("rollback_mismatch");
    }
    if (
      target === null ||
      observation.rollbackTargetStateDigest !==
        (orphaned.length === 0
          ? previous.stateDigest
          : orphaned[0]!.predecessorStateDigest) ||
      (recoveryResult !== null &&
        (commonAncestorBlockNo === null ||
          retainedHistory.some(
            (entry) =>
              BigInt(entry.observation.blockNo) > BigInt(commonAncestorBlockNo),
          ) ||
          retainedHistory.some(
            (entry) =>
              (entry.submissionId !== null &&
                removedSubmissions.has(entry.submissionId)) ||
              (entry.confirmationId !== null &&
                removedConfirmations.has(entry.confirmationId)) ||
              removedPoints.has(entry.chainPointId),
          ))) ||
      !relevantRemovedSetMatchesSuffix(previous, orphaned, rollback) ||
      !same(verified.sourceJournal, previous.journal) ||
      !same(verified.journal, target.journal) ||
      (targetAnchor !== undefined &&
        (targetStore === null ||
          storeDigest(targetStore) !==
            targetAnchor.observation.durableStoreDigest ||
          !durableStoreExtends(targetStore, verified.store)))
    ) {
      return rejected("rollback_mismatch");
    }
    const retainedOutRefs = new Set(
      [target.journal?.threadOutRef, target.journal?.proofTokenOutRef].filter(
        (outRef): outRef is string => outRef !== null && outRef !== undefined,
      ),
    );
    if (
      rollback.removedRecords.protocolUtxoOutRefs.some((outRef) =>
        retainedOutRefs.has(outRef),
      )
    ) {
      return rejected("rollback_mismatch");
    }
    const rollbackEntry = makeEntry(
      observation,
      verified.context,
      target.journal,
      previous.journal,
      rollback,
    );
    const audit = [
      ...previous.auditHistory,
      ...orphaned.map((entry) => makeAudit("orphaned", entry)),
      makeAudit("rollback", rollbackEntry),
    ];
    if (audit.length > WATCHER_PROOF_THREAD_V1_BOUNDS.auditEntries) {
      return rejected("history_limit_exceeded");
    }
    const state = makeState(
      policy,
      observation,
      target.journal,
      target.pending,
      retainedHistory,
      audit,
      [...previous.transitionHistory, rollbackEntry],
    );
    return makeResult({
      action: "accept",
      protocolDecision: "indexed",
      reasonCodes: Object.freeze(["rollback_confirmed"]),
      alertCodes: Object.freeze([]),
      state,
    });
  }
  if (
    previous !== null &&
    previous.history.length >= Number(policy.maximumHistoryEntries)
  ) {
    return rejected("history_limit_exceeded");
  }
  const promotion =
    observation.confirmationPhase === "final" &&
    previous?.pending !== null &&
    previous?.pending !== undefined &&
    previous.pending.transitionKind === observation.transitionKind &&
    previous.pending.transactionHash === observation.transactionHash &&
    previous.pending.submissionId === observation.submissionId &&
    previous.pending.confirmationId === observation.confirmationId &&
    same(previous.pending.journal, observation.journal);
  if (
    previous?.pending !== null &&
    previous?.pending !== undefined &&
    !promotion
  ) {
    return rejected("lifecycle_mismatch");
  }
  const verified = parsePublicContext(
    policy,
    observation,
    publicContextInput,
    !promotion,
    transportAttestations,
  );
  if (verified === null) {
    return rejected(
      "malformed_public_context",
      "watcher_proof_thread_binding_rejected",
    );
  }
  let previousStore: WatcherDurableStoreV1 | null = null;
  const previousAnchor =
    previous === null
      ? null
      : currentStateAnchorEntry(
          previous.pointDigest,
          previous.durableStoreDigest,
          previous.history,
          previous.auditHistory,
        );
  try {
    previousStore =
      previous === null
        ? null
        : parseWatcherDurableStoreV1(
            previousAnchor?.publicContext.durableStore,
          );
  } catch {
    return rejected("stale_state", "watcher_proof_thread_binding_rejected");
  }
  if (
    previous !== null &&
    (previousStore === null ||
      previousAnchor === null ||
      storeDigest(previousStore) !== previous.durableStoreDigest ||
      !durableStoreExtends(previousStore, verified.sourceStore) ||
      !canonicalSuccessor(
        previousAnchor.observation,
        observation,
        verified.block,
        verified.lineageBlocks,
        promotion,
      ))
  ) {
    return rejected("stale_state", "watcher_proof_thread_binding_rejected");
  }
  const lifecycle = verifyLifecycle(
    policy,
    observation,
    verified,
    previous?.journal ?? null,
  );
  if (lifecycle === null) {
    return rejected(
      "transaction_mismatch",
      "watcher_proof_thread_transition_rejected",
    );
  }
  const entry = makeEntry(
    observation,
    verified.context,
    lifecycle.journal,
    lifecycle.sourceJournal,
    null,
  );
  if (promotion && !pendingMatchesFinal(previous!.pending!, entry)) {
    return rejected("journal_mismatch");
  }
  const history = [...(previous?.history ?? []), entry];
  const final = observation.confirmationPhase === "final";
  const journal = final ? lifecycle.journal : (previous?.journal ?? null);
  const pending = final ? null : pendingFromEntry(entry);
  const state = makeState(
    policy,
    observation,
    journal,
    pending,
    history,
    previous?.auditHistory ?? [],
    [...(previous?.transitionHistory ?? []), entry],
  );
  return makeResult({
    action: "accept",
    protocolDecision: final ? "indexed" : "hold",
    reasonCodes: Object.freeze([
      transitionReason(
        observation.transitionKind,
        observation.confirmationPhase!,
      ),
    ]),
    alertCodes: Object.freeze([]),
    state,
  });
};

export const evaluateWatcherProofThreadIndexerV1 = (
  policyInput: unknown,
  previousStateInput: unknown,
  observationInput: unknown,
  publicContextInput: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[],
): WatcherProofThreadResultV1 => {
  const policy = parseWatcherProofThreadPolicyV1(policyInput);
  if (policy === null) {
    return rejected("malformed_policy");
  }
  const observation = parseWatcherProofThreadObservationV1(observationInput);
  if (observation === null) {
    return rejected("malformed_observation");
  }
  const previous =
    previousStateInput === null
      ? null
      : parseWatcherProofThreadStateV1(
          previousStateInput,
          policy,
          transportAttestations,
        );
  if (previousStateInput !== null && previous === null) {
    return rejected("malformed_state", "watcher_proof_thread_state_rejected");
  }
  return applyWatcherProofThreadTransitionV1(
    policy,
    previous,
    observation,
    publicContextInput,
    transportAttestations,
  );
};

export type WatcherProofThreadResultVerificationContextV1 = Readonly<{
  policy: unknown;
  previousState: unknown;
  observation: unknown;
  publicContext: unknown;
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
}>;

export const parseWatcherProofThreadResultV1 = (
  value: unknown,
  context: WatcherProofThreadResultVerificationContextV1,
): WatcherProofThreadResultV1 | null => {
  const expected = evaluateWatcherProofThreadIndexerV1(
    context.policy,
    context.previousState,
    context.observation,
    context.publicContext,
    context.transportAttestations,
  );
  return same(value, expected) ? expected : null;
};

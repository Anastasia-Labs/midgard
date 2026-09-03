import { formatUnknownError } from "@al-ft/midgard-core";
import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  type AuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { canonicalDecodabilityArtifactFromRawEvidence } from "../evidence/canonical-decodability-raw-evidence-v1.js";
import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
} from "../evidence/production-fraud-proof-evidence-v1.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type CanonicalBlockClassification,
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "./classification-v1.js";
import {
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay-v1.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal-v1.js";
import {
  assertWorkflowJournalActuation,
  workflowActuationDecisionDigest,
} from "./production-actuation-permit-v1.js";
import {
  abandonWorkflowFundingReservationTransaction,
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  prepareWorkflowFundingReservationTransaction,
  releaseWorkflowFundingReservation,
} from "./production-funding-reservation-permit-v1.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy-v1.js";
import { copyWorkflowPreflightTransaction } from "./transaction-boundary-v1.js";

export const FRAUD_PROOF_WORKFLOW_ADAPTER =
  "midgard-fraud-proof-workflow-adapter-v1" as const;
export const FRAUD_PROOF_WORKFLOW_SAFETY = Object.freeze({
  evidenceSource: "authenticated-l1-public-retained-da-v1",
  scriptCarriage: "reference-script-only",
  localEvaluation: "required-before-submit",
} as const);

export type FraudProofWorkflowAction = {
  /** Stable within a workflow; changing action data requires a new id. */
  readonly actionId: string;
  /** Public, journal-safe inputs needed to reconstruct this transaction. */
  readonly input: JournalJsonObject;
};

export type FraudProofWorkflowReferenceScript = {
  readonly role: string;
  readonly outRef: string;
  readonly scriptHash: string;
};

export type FraudProofWorkflowPreflight = {
  readonly actionId: string;
  /** Hash of the exact transaction body that passed local evaluation. */
  readonly txHash: string;
  readonly scriptExecution: "none" | "reference_scripts";
  readonly localUplcEvaluation: {
    readonly status: "passed";
    readonly evaluator: string;
  };
  readonly referenceScripts: readonly FraudProofWorkflowReferenceScript[];
  /**
   * Public, journal-safe coordinator state needed to recover this exact
   * action after process loss. It is copied into the durable intent before
   * any network submission.
   */
  readonly durableRecovery?: JournalJsonObject;
};

export type FraudProofWorkflowObservation =
  | {
      readonly kind: "action_required";
      readonly action: FraudProofWorkflowAction;
    }
  | {
      readonly kind: "completed";
      /** Candidate facts; the adapter cannot authenticate these itself. */
      readonly terminal: FraudProofWorkflowTerminal;
    }
  | {
      readonly kind: "conflict";
      readonly reason: string;
    };

export type FraudProofWorkflowSubmitResult =
  | { readonly kind: "submitted"; readonly txHash: string }
  | {
      readonly kind: "ambiguous";
      readonly txHash?: string;
      readonly detail: string;
    };

export type FraudProofWorkflowReconcileResult =
  | { readonly kind: "confirmed"; readonly txHash: string }
  | { readonly kind: "pending"; readonly txHash?: string }
  | { readonly kind: "not_found" }
  | { readonly kind: "conflict"; readonly reason: string };

export const FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER =
  "midgard-authenticated-l1-workflow-terminal-verifier-v1" as const;

/**
 * Independent chain-state authority used only for terminal closure.  It must
 * inspect authenticated Cardano L1 state; a family adapter's own observation
 * is deliberately insufficient to mark a workflow complete.
 */
export interface FraudProofWorkflowTerminalVerifier {
  readonly verifierVersion: typeof FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER;
  verify(input: {
    readonly identity: FraudProofWorkflowIdentity;
    readonly workflowId: string;
    /** Deployment-manifest-bound finality identity this terminal must meet. */
    readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
    readonly candidate: FraudProofWorkflowTerminal;
    readonly artifact: JournalJsonObject;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  }): Promise<FraudProofWorkflowTerminal>;
}

type FraudProofWorkflowAdapterContext = {
  readonly identity: FraudProofWorkflowIdentity;
  readonly workflowId: string;
  readonly artifact: JournalJsonObject;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
};

/**
 * One explicit adapter wraps one existing family builder/submitter chain.
 * Implementations must call the current family submitters; those retain local
 * UPLC evaluation and authenticated reference-script-only semantics.
 */
export interface FraudProofFamilyWorkflowAdapter {
  readonly adapterVersion: typeof FRAUD_PROOF_WORKFLOW_ADAPTER;
  readonly category: FraudProofCatalogueCategoryName;
  readonly safety: typeof FRAUD_PROOF_WORKFLOW_SAFETY;
  prepare(input: {
    readonly evidence: CanonicalBlockEvidence;
    /** Opaque, admitted predecessor authority for ledger-relative families. */
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    >;
  }): Promise<JournalJsonObject>;
  observe(
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation>;
  preflight(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
    },
  ): Promise<FraudProofWorkflowPreflight>;
  /** Called only after a durable `submission_intent` journal entry exists. */
  submit(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
      readonly preflight: FraudProofWorkflowPreflight;
    },
  ): Promise<FraudProofWorkflowSubmitResult>;
  /**
   * Must inspect authenticated L1 state. It is called before every retry when
   * an intent/submission has an uncertain or merely submitted outcome.
   */
  reconcile(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
      readonly txHash?: string;
      readonly durableRecovery?: JournalJsonObject;
    },
  ): Promise<FraudProofWorkflowReconcileResult>;
}

export type FraudProofWorkflowRegistry = ReadonlyMap<
  FraudProofCatalogueCategoryName,
  FraudProofFamilyWorkflowAdapter
>;

const freezeWorkflowAdapter = (
  adapter: FraudProofFamilyWorkflowAdapter,
): FraudProofFamilyWorkflowAdapter => {
  const prepare = adapter.prepare;
  const observe = adapter.observe;
  const preflight = adapter.preflight;
  const submit = adapter.submit;
  const reconcile = adapter.reconcile;
  return Object.freeze({
    adapterVersion: adapter.adapterVersion,
    category: adapter.category,
    safety: Object.freeze({ ...adapter.safety }),
    prepare: Object.freeze((input: Parameters<typeof prepare>[0]) =>
      prepare(input),
    ),
    observe: Object.freeze((input: Parameters<typeof observe>[0]) =>
      observe(input),
    ),
    preflight: Object.freeze((input: Parameters<typeof preflight>[0]) =>
      preflight(input),
    ),
    submit: Object.freeze((input: Parameters<typeof submit>[0]) =>
      submit(input),
    ),
    reconcile: Object.freeze((input: Parameters<typeof reconcile>[0]) =>
      reconcile(input),
    ),
  });
};

class ImmutableFraudProofWorkflowRegistry
  implements FraudProofWorkflowRegistry
{
  readonly #entries: ReadonlyMap<
    FraudProofCatalogueCategoryName,
    FraudProofFamilyWorkflowAdapter
  >;

  constructor(
    entries: readonly (readonly [
      FraudProofCatalogueCategoryName,
      FraudProofFamilyWorkflowAdapter,
    ])[],
  ) {
    this.#entries = new Map(entries);
    Object.freeze(this);
  }

  get size(): number {
    return this.#entries.size;
  }

  get [Symbol.toStringTag](): string {
    return "ImmutableFraudProofWorkflowRegistryV1";
  }

  get(category: FraudProofCatalogueCategoryName) {
    return this.#entries.get(category);
  }

  has(category: FraudProofCatalogueCategoryName): boolean {
    return this.#entries.has(category);
  }

  entries() {
    return this.#entries.entries();
  }

  keys() {
    return this.#entries.keys();
  }

  values() {
    return this.#entries.values();
  }

  forEach(
    callbackfn: (
      value: FraudProofFamilyWorkflowAdapter,
      key: FraudProofCatalogueCategoryName,
      map: FraudProofWorkflowRegistry,
    ) => void,
    thisArg?: unknown,
  ): void {
    for (const [key, value] of this.#entries) {
      callbackfn.call(thisArg, value, key, this);
    }
  }

  [Symbol.iterator]() {
    return this.#entries[Symbol.iterator]();
  }
}

const sameSafety = (
  safety: FraudProofFamilyWorkflowAdapter["safety"],
): boolean =>
  safety.evidenceSource === FRAUD_PROOF_WORKFLOW_SAFETY.evidenceSource &&
  safety.scriptCarriage === FRAUD_PROOF_WORKFLOW_SAFETY.scriptCarriage &&
  safety.localEvaluation === FRAUD_PROOF_WORKFLOW_SAFETY.localEvaluation;

/**
 * Creates a closed, versioned registry for the supplied launch scope. Missing,
 * duplicate, extra, legacy-inline-script, or non-local-evaluation adapters fail
 * startup instead of becoming a runtime fallback.
 */
export const createFraudProofWorkflowRegistry = ({
  adapters,
  launchScope = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
}: {
  readonly adapters: readonly FraudProofFamilyWorkflowAdapter[];
  readonly launchScope?: readonly FraudProofCatalogueCategoryName[];
}): FraudProofWorkflowRegistry => {
  const scope = new Set<FraudProofCatalogueCategoryName>();
  for (const category of launchScope) {
    if (scope.has(category)) {
      throw new Error(`duplicate launch-scope category: ${category}`);
    }
    scope.add(category);
  }
  const registry = new Map<
    FraudProofCatalogueCategoryName,
    FraudProofFamilyWorkflowAdapter
  >();
  for (const adapter of adapters) {
    if (adapter.adapterVersion !== FRAUD_PROOF_WORKFLOW_ADAPTER) {
      throw new Error(`adapter ${adapter.category} has an unsupported version`);
    }
    if (!scope.has(adapter.category)) {
      throw new Error(`adapter ${adapter.category} is outside launch scope`);
    }
    if (!sameSafety(adapter.safety)) {
      throw new Error(
        `adapter ${adapter.category} does not enforce canonical evidence, local UPLC evaluation, and reference-script-only carriage`,
      );
    }
    if (registry.has(adapter.category)) {
      throw new Error(`duplicate workflow adapter: ${adapter.category}`);
    }
    registry.set(adapter.category, freezeWorkflowAdapter(adapter));
  }
  const missing = [...scope].filter((category) => !registry.has(category));
  if (missing.length > 0) {
    throw new Error(
      `missing launch-scope workflow adapters: ${missing.join(", ")}`,
    );
  }
  return new ImmutableFraudProofWorkflowRegistry([...registry]);
};

type WorkflowEvidenceBinding = JournalJsonObject &
  (
    | {
        readonly route: "canonical_block";
        readonly headerHash: string;
        readonly payloadEnvelopeSha256: string;
        readonly payloadSha256: string;
        readonly l1BlockHash: string;
        readonly l1Slot: string;
      }
    | {
        readonly route: "authenticated_source_leaf";
        readonly headerHash: string;
        readonly payloadEnvelopeSha256: string;
        readonly payloadSha256: string;
        readonly committedTransactionsRoot: string;
        readonly l2TransactionCount: string;
        readonly committedTxId: string;
        readonly l1BlockHash: string;
        readonly l1Slot: string;
      }
  );

type PersistedArtifactEnvelope = JournalJsonObject & {
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly familyArtifact: JournalJsonObject;
};

const persistedArtifact = ({
  evidenceBinding,
  releaseFinality,
  familyArtifact,
}: {
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly familyArtifact: JournalJsonObject;
}): PersistedArtifactEnvelope =>
  normalizeJournalJson({
    evidenceBinding,
    releaseFinality,
    familyArtifact,
  }) as PersistedArtifactEnvelope;

const requirePreparedArtifact = ({
  entries,
  evidenceBinding,
  releaseFinality,
}: {
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): PersistedArtifactEnvelope | undefined => {
  const prepared = entries.find((entry) => entry.event.kind === "prepared");
  if (prepared === undefined || prepared.event.kind !== "prepared") {
    return undefined;
  }
  if (
    journalJsonDigest(prepared.event.artifact) !== prepared.event.artifactDigest
  ) {
    throw new Error("journaled prepared artifact digest mismatch");
  }
  const envelope = prepared.event.artifact as PersistedArtifactEnvelope;
  if (
    JSON.stringify(envelope.evidenceBinding) !==
      JSON.stringify(evidenceBinding) ||
    envelope.releaseFinality.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    envelope.releaseFinality.releaseIdentityDigest !==
      releaseFinality.releaseIdentityDigest ||
    envelope.releaseFinality.policyDigest !== releaseFinality.policyDigest
  ) {
    throw new Error(
      "current authenticated evidence or release-finality identity does not match the proof-critical artifact persisted before submission",
    );
  }
  return envelope;
};

const canonicalEvidenceBinding = (
  evidence: CanonicalBlockEvidence,
): WorkflowEvidenceBinding =>
  normalizeJournalJson({
    route: "canonical_block",
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    l1BlockHash: evidence.observation.chainPoint.blockHash,
    l1Slot: evidence.observation.chainPoint.slot.toString(),
  }) as WorkflowEvidenceBinding;

const verifiedReleaseFinality = async ({
  deploymentFingerprint,
  authority,
}: {
  readonly deploymentFingerprint: string;
  readonly authority: FraudProofReleaseFinalityAuthority;
}): Promise<{
  readonly deploymentFingerprint: string;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}> => {
  if (authority.authorityVersion !== FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY) {
    throw new Error(
      "workflow requires the deployment-manifest release finality authority",
    );
  }
  const normalized = normalizeDaDeploymentFingerprintHex(deploymentFingerprint);
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await authority.verifyForWorkflow({ deploymentFingerprint: normalized }),
  );
  if (releaseFinality.deploymentIdentityDigest !== normalized) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  return { deploymentFingerprint: normalized, releaseFinality };
};

const normalizeTxHash = (value: string, field: string): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(normalized)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return normalized;
};

const normalizeOutRef = (value: string, field: string): string => {
  const normalized = value.trim().toLowerCase();
  if (
    normalized !== value ||
    !/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(normalized)
  ) {
    throw new Error(`${field} must be a canonical transaction outRef`);
  }
  return normalized;
};

const validateAction = (
  action: FraudProofWorkflowAction,
): FraudProofWorkflowAction => {
  if (
    action.actionId.length === 0 ||
    action.actionId.trim() !== action.actionId
  ) {
    throw new Error("workflow actionId must be a canonical non-empty string");
  }
  return {
    actionId: action.actionId,
    input: normalizeJournalJson(
      action.input,
      `workflow action ${action.actionId}`,
    ) as JournalJsonObject,
  };
};

const validatePreflight = ({
  action,
  preflight,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly preflight: FraudProofWorkflowPreflight;
}): FraudProofWorkflowPreflight => {
  if (preflight.actionId !== action.actionId) {
    throw new Error("workflow preflight returned a different actionId");
  }
  const txHash = normalizeTxHash(
    preflight.txHash,
    "workflow preflight transaction hash",
  );
  if (
    preflight.localUplcEvaluation.status !== "passed" ||
    preflight.localUplcEvaluation.evaluator.trim().length === 0
  ) {
    throw new Error(
      "workflow submission requires a passed local UPLC evaluation",
    );
  }
  if (
    preflight.scriptExecution === "reference_scripts" &&
    preflight.referenceScripts.length === 0
  ) {
    throw new Error(
      "script-executing workflow submission requires reference scripts",
    );
  }
  if (
    preflight.scriptExecution === "none" &&
    preflight.referenceScripts.length !== 0
  ) {
    throw new Error(
      "script-free workflow submission reported reference scripts",
    );
  }
  const roles = new Set<string>();
  for (const reference of preflight.referenceScripts) {
    if (reference.role.trim().length === 0 || roles.has(reference.role)) {
      throw new Error(
        "workflow reference-script roles must be unique and non-empty",
      );
    }
    roles.add(reference.role);
    normalizeOutRef(reference.outRef, "workflow reference-script outRef");
    if (!/^[0-9a-f]{56}$/u.test(reference.scriptHash)) {
      throw new Error("workflow reference-script hash must be 28-byte hex");
    }
  }
  return copyWorkflowPreflightTransaction({
    from: preflight,
    to: {
      ...preflight,
      txHash,
      ...(preflight.durableRecovery === undefined
        ? {}
        : {
            durableRecovery: normalizeJournalJson(
              preflight.durableRecovery,
              `workflow preflight ${action.actionId} durable recovery`,
            ) as JournalJsonObject,
          }),
    },
  });
};

const normalizeNonNegativeLovelace = (value: string, field: string): string => {
  if (!/^(0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be canonical non-negative lovelace`);
  }
  return value;
};

const normalizeTerminal = ({
  identity,
  terminal,
  entries,
  releaseFinality,
}: {
  readonly identity: FraudProofWorkflowIdentity;
  readonly terminal: FraudProofWorkflowTerminal;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): FraudProofWorkflowTerminal => {
  if (terminal.schemaVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION) {
    throw new Error("workflow terminal has an unsupported schema");
  }
  if (
    identity.target.kind !== "state_queue_header" ||
    terminal.category !== identity.category ||
    terminal.headerHash !== identity.target.headerHash
  ) {
    throw new Error("workflow terminal does not match workflow identity");
  }
  const createdByTxHash = normalizeTxHash(
    terminal.proofToken.createdByTxHash,
    "terminal proof-token creation transaction hash",
  );
  const removalTxHash = normalizeTxHash(
    terminal.correction.removalTxHash,
    "terminal removal transaction hash",
  );
  if (createdByTxHash === removalTxHash) {
    throw new Error(
      "terminal proof-token creation and removal must be distinct transactions",
    );
  }
  const confirmed = new Set(
    entries
      .filter(
        (
          entry,
        ): entry is FraudProofWorkflowJournalEntry & {
          readonly event: Extract<
            FraudProofWorkflowJournalEvent,
            { readonly kind: "confirmed" }
          >;
        } => entry.event.kind === "confirmed",
      )
      .map((entry) => entry.event.txHash),
  );
  if (!confirmed.has(createdByTxHash) || !confirmed.has(removalTxHash)) {
    throw new Error(
      "terminal proof-token creation and removal must both be confirmed in this workflow journal",
    );
  }
  if (!/^(?:[0-9a-f]{2}){28,60}$/u.test(terminal.proofToken.unit)) {
    throw new Error("terminal proof-token unit must be canonical hex");
  }
  normalizeOutRef(terminal.proofToken.outRef, "terminal proof-token outRef");
  normalizeOutRef(
    terminal.correction.removedStateQueueOutRef,
    "terminal removed state-queue outRef",
  );
  normalizeOutRef(
    terminal.correction.referencedProofTokenOutRef,
    "terminal referenced proof-token outRef",
  );
  if (
    terminal.correction.referencedProofTokenOutRef !==
    terminal.proofToken.outRef
  ) {
    throw new Error(
      "terminal removal did not reference the retained proof token",
    );
  }
  if (
    terminal.proofToken.retainedAtFinalState !== true ||
    "spentByTxHash" in terminal.proofToken ||
    "proofTokenSpent" in terminal.correction
  ) {
    throw new Error(
      "terminal must prove the permanent proof token remains unspent",
    );
  }
  if (
    terminal.correction.fraudulentHeaderAbsent !== true ||
    terminal.economics.duplicateRewardAbsent !== true
  ) {
    throw new Error(
      "workflow terminal omitted mandatory correction/economic facts",
    );
  }
  if (
    !/^[0-9a-f]{56}$/u.test(terminal.economics.operatorCredential) ||
    !/^[0-9a-f]{56}$/u.test(terminal.economics.proverCredential)
  ) {
    throw new Error("terminal economic credentials must be canonical hex");
  }
  if (terminal.economics.operatorBondInputOutRef !== null) {
    normalizeOutRef(
      terminal.economics.operatorBondInputOutRef,
      "terminal operator-bond input outRef",
    );
  }
  if (terminal.economics.proverRewardOutputOutRef !== null) {
    normalizeOutRef(
      terminal.economics.proverRewardOutputOutRef,
      "terminal prover-reward output outRef",
    );
  }
  normalizeNonNegativeLovelace(
    terminal.economics.operatorBondInputLovelace,
    "terminal operatorBondInputLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.slashedLovelace,
    "terminal slashedLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.proverRewardLovelace,
    "terminal proverRewardLovelace",
  );
  normalizeNonNegativeLovelace(
    terminal.economics.removalFeeLovelace,
    "terminal removalFeeLovelace",
  );
  if (
    (terminal.economics.operatorBondInputOutRef === null) !==
      (terminal.economics.operatorBondInputLovelace === "0") ||
    (terminal.economics.proverRewardOutputOutRef === null) !==
      (terminal.economics.proverRewardLovelace === "0")
  ) {
    throw new Error(
      "terminal economic output references do not match their lovelace amounts",
    );
  }
  if (!/^(0|[1-9][0-9]*)$/u.test(terminal.observedAt.slot)) {
    throw new Error("terminal chain-point slot must be canonical");
  }
  if (!/^[0-9a-f]{64}$/u.test(terminal.observedAt.blockHash)) {
    throw new Error("terminal chain-point block hash must be 32-byte hex");
  }
  if (
    !Number.isSafeInteger(terminal.observedAt.confirmationDepth) ||
    terminal.observedAt.confirmationDepth <
      releaseFinality.policy.confirmationDepth
  ) {
    throw new Error(
      `terminal observation confirmation depth is below the release threshold: required=${releaseFinality.policy.confirmationDepth.toString()} actual=${String(terminal.observedAt.confirmationDepth)} policy=${releaseFinality.policyDigest}`,
    );
  }
  return {
    ...terminal,
    proofToken: {
      ...terminal.proofToken,
      createdByTxHash,
    },
    correction: {
      ...terminal.correction,
      removalTxHash,
    },
  };
};

const lastActionEvent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): FraudProofWorkflowJournalEvent | undefined =>
  [...entries]
    .reverse()
    .map((entry) => entry.event)
    .find((event) => "actionId" in event && event.actionId === actionId);

const lastKnownTxHash = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): string | undefined => {
  let latestIntentIndex = -1;
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const event = entries[index]!.event;
    if (event.kind === "submission_intent" && event.actionId === actionId) {
      latestIntentIndex = index;
      break;
    }
  }
  if (latestIntentIndex < 0) {
    return undefined;
  }
  for (let index = entries.length - 1; index >= latestIntentIndex; index -= 1) {
    const entry = entries[index]!;
    const event = entry.event;
    if (!("actionId" in event) || event.actionId !== actionId) {
      continue;
    }
    if ("txHash" in event && event.txHash !== undefined) {
      return event.txHash;
    }
  }
  return undefined;
};

const attemptCount = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): number =>
  entries.filter(
    (entry) =>
      entry.event.kind === "submission_intent" &&
      entry.event.actionId === actionId,
  ).length;

const latestSubmissionIntent = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
):
  | Extract<
      FraudProofWorkflowJournalEvent,
      { readonly kind: "submission_intent" }
    >
  | undefined =>
  [...entries]
    .reverse()
    .map((entry) => entry.event)
    .find(
      (
        event,
      ): event is Extract<
        FraudProofWorkflowJournalEvent,
        { readonly kind: "submission_intent" }
      > => event.kind === "submission_intent" && event.actionId === actionId,
    );

export type FraudProofWorkflowRunResult =
  | {
      readonly kind: "no_fault_detected" | "unprovable_gap";
      readonly classification: CanonicalBlockClassification;
    }
  | {
      readonly kind: "completed";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
    }
  | {
      readonly kind: "pending" | "stalled";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly reason: string;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
    };

/**
 * Q51/W-O4 single-command core. Preparation, every submission intent,
 * ambiguous result, reconciliation, submitted hash, and confirmation are
 * durable. An unresolved submission is always reconciled against authenticated
 * L1 state before any retry.
 */
const runAdmittedFraudProofWorkflow = async ({
  deploymentFingerprint,
  category,
  headerHash,
  evidenceBinding,
  prepareFamilyArtifact,
  registry,
  journal,
  terminalVerifier,
  releaseFinality,
  maxSubmissionAttempts = 3,
  maxActions = 64,
  now = () => new Date(),
}: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly evidenceBinding: WorkflowEvidenceBinding;
  readonly prepareFamilyArtifact: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => Promise<JournalJsonObject>;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    !Number.isSafeInteger(maxSubmissionAttempts) ||
    maxSubmissionAttempts < 1
  ) {
    throw new Error("maxSubmissionAttempts must be a positive safe integer");
  }
  if (!Number.isSafeInteger(maxActions) || maxActions < 1) {
    throw new Error("maxActions must be a positive safe integer");
  }
  if (
    terminalVerifier.verifierVersion !== FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER
  ) {
    throw new Error("workflow requires the authenticated L1 terminal verifier");
  }
  if (releaseFinality.deploymentIdentityDigest !== deploymentFingerprint) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  const adapter = registry.get(category);
  if (adapter === undefined) {
    throw new Error(`classified family ${category} has no workflow adapter`);
  }
  assertWorkflowJournalActuation({
    journal,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint: "workflow_resume",
  });
  const decisionDigest = workflowActuationDecisionDigest(journal);
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header", headerHash },
    ...(decisionDigest === undefined ? {} : { decisionDigest }),
  });
  const workflowId = computeFraudProofWorkflowId(identity);
  let entries = [
    ...(await journal.load(workflowId)),
  ] as FraudProofWorkflowJournalEntry[];
  validateFraudProofWorkflowJournal({
    workflowId,
    entries,
    expectedIdentity: identity,
  });

  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entry: FraudProofWorkflowJournalEntry = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: entries.length,
      recordedAt: now().toISOString(),
      event,
    };
    await journal.append(entry, entries.length);
    entries = [...entries, entry];
  };
  const stalled = async (
    reason: string,
  ): Promise<FraudProofWorkflowRunResult> => {
    await append({ kind: "stalled", reason });
    return { kind: "stalled", workflowId, identity, reason, entries };
  };

  if (entries.length === 0) {
    await append({ kind: "started" });
  }
  let envelope = requirePreparedArtifact({
    entries,
    evidenceBinding,
    releaseFinality,
  });
  if (envelope === undefined) {
    const familyArtifact = normalizeJournalJson(
      await prepareFamilyArtifact(adapter),
      `${category} prepared artifact`,
    ) as JournalJsonObject;
    envelope = persistedArtifact({
      evidenceBinding,
      releaseFinality,
      familyArtifact,
    });
    await append({
      kind: "prepared",
      artifact: envelope,
      artifactDigest: journalJsonDigest(envelope),
    });
  }

  for (let actionNumber = 0; actionNumber < maxActions; actionNumber += 1) {
    let context: FraudProofWorkflowAdapterContext = {
      identity,
      workflowId,
      artifact: envelope.familyArtifact,
      entries,
    };

    // Reconciliation precedes family-state observation. Otherwise a tx that
    // reached L1 immediately could make `observe` report completion before its
    // submitted/confirmed journal records had been closed.
    // A diagnostic `stalled` entry does not resolve an in-flight network
    // action.  Resume from the latest lifecycle event so a crash or transient
    // reconciliation failure can never turn uncertainty into a fresh submit.
    const latestJournalEvent = [...entries]
      .reverse()
      .map((entry) => entry.event)
      .find((event) => event.kind !== "stalled");
    if (
      latestJournalEvent?.kind === "reconciled" &&
      latestJournalEvent.outcome === "confirmed"
    ) {
      if (latestJournalEvent.txHash === undefined) {
        return await stalled(
          `confirmed reconciliation for ${latestJournalEvent.actionId} omitted its transaction hash`,
        );
      }
      await append({
        kind: "confirmed",
        actionId: latestJournalEvent.actionId,
        txHash: latestJournalEvent.txHash,
      });
      continue;
    }
    const unresolvedEvent =
      latestJournalEvent?.kind === "submission_intent" ||
      latestJournalEvent?.kind === "submission_ambiguous" ||
      latestJournalEvent?.kind === "submitted" ||
      (latestJournalEvent?.kind === "reconciled" &&
        latestJournalEvent.outcome === "pending")
        ? latestJournalEvent
        : undefined;
    if (unresolvedEvent !== undefined && "actionId" in unresolvedEvent) {
      const intent = latestSubmissionIntent(entries, unresolvedEvent.actionId);
      if (intent === undefined) {
        return await stalled(
          `unresolved submission ${unresolvedEvent.actionId} has no durable intent`,
        );
      }
      const action = validateAction({
        actionId: intent.actionId,
        input: intent.actionInput,
      });
      const priorTxHash = lastKnownTxHash(entries, action.actionId);
      let reconciled: FraudProofWorkflowReconcileResult;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_reconcile",
        });
        reconciled = await adapter.reconcile({
          ...context,
          action,
          ...(priorTxHash === undefined ? {} : { txHash: priorTxHash }),
          ...(intent.durableRecovery === undefined
            ? {}
            : { durableRecovery: intent.durableRecovery }),
        });
      } catch (cause) {
        return await stalled(
          `reconciliation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        );
      }
      if (reconciled.kind === "conflict") {
        await conflictWorkflowFundingReservationTransaction({
          journal,
          transactionHash: priorTxHash ?? intent.txHash,
        });
        return await stalled(
          `reconciliation conflict for ${action.actionId}: ${reconciled.reason}`,
        );
      }
      if (reconciled.kind === "confirmed") {
        const txHash = normalizeTxHash(
          reconciled.txHash,
          "reconciled transaction hash",
        );
        if (priorTxHash !== undefined && txHash !== priorTxHash) {
          return await stalled(
            `reconciliation for ${action.actionId} returned ${txHash}, expected ${priorTxHash}`,
          );
        }
        await confirmWorkflowFundingReservationTransaction({
          journal,
          transactionHash: txHash,
        });
        await append({
          kind: "reconciled",
          actionId: action.actionId,
          outcome: "confirmed",
          txHash,
        });
        await append({ kind: "confirmed", actionId: action.actionId, txHash });
        continue;
      }
      if (reconciled.kind === "pending") {
        const txHash =
          reconciled.txHash === undefined
            ? priorTxHash
            : normalizeTxHash(reconciled.txHash, "pending transaction hash");
        if (
          priorTxHash !== undefined &&
          txHash !== undefined &&
          priorTxHash !== txHash
        ) {
          return await stalled(
            `pending reconciliation for ${action.actionId} changed transaction hash`,
          );
        }
        await append({
          kind: "reconciled",
          actionId: action.actionId,
          outcome: "pending",
          ...(txHash === undefined ? {} : { txHash }),
        });
        return {
          kind: "pending",
          workflowId,
          identity,
          reason: `transaction for ${action.actionId} is pending`,
          entries,
        };
      }
      await abandonWorkflowFundingReservationTransaction({
        journal,
        transactionHash: priorTxHash ?? intent.txHash,
      });
      await append({
        kind: "reconciled",
        actionId: action.actionId,
        outcome: "not_found",
      });
      context = { ...context, entries };
    }

    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_observe",
    });
    const observation = await adapter.observe(context);
    if (observation.kind === "completed") {
      let terminal: FraudProofWorkflowTerminal;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
        terminal = normalizeTerminal({
          identity,
          terminal: await terminalVerifier.verify({
            identity,
            workflowId,
            releaseFinality,
            candidate: observation.terminal,
            artifact: envelope.familyArtifact,
            entries,
          }),
          entries,
          releaseFinality,
        });
      } catch (cause) {
        return await stalled(
          `terminal verification failed: ${formatUnknownError(cause)}`,
        );
      }
      const terminalDigest = journalJsonDigest(
        normalizeJournalJson(terminal, "workflow terminal"),
      );
      if (entries.at(-1)?.event.kind !== "completed") {
        await releaseWorkflowFundingReservation({ journal });
        await append({ kind: "completed", terminal, terminalDigest });
      }
      return {
        kind: "completed",
        workflowId,
        identity,
        terminal,
        entries,
      };
    }
    if (observation.kind === "conflict") {
      return await stalled(`chain conflict: ${observation.reason}`);
    }
    const action = validateAction(observation.action);
    const latest = lastActionEvent(entries, action.actionId);
    if (latest?.kind === "confirmed") {
      return await stalled(
        `confirmed action ${action.actionId} is still reported as required`,
      );
    }

    const priorAttempts = attemptCount(entries, action.actionId);
    if (priorAttempts >= maxSubmissionAttempts) {
      return await stalled(
        `submission attempts exhausted for ${action.actionId}`,
      );
    }
    let preflight: FraudProofWorkflowPreflight;
    try {
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_preflight",
      });
      await beginWorkflowFundingReservationAction({
        journal,
        action,
      });
      preflight = validatePreflight({
        action,
        preflight: await adapter.preflight({ ...context, action }),
      });
    } catch (cause) {
      return await stalled(
        `preflight failed for ${action.actionId}: ${formatUnknownError(cause)}`,
      );
    }
    try {
      await prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
      });
    } catch (cause) {
      return await stalled(
        `funding reservation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
      );
    }
    await append({
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: preflight.txHash,
      localEvaluator: preflight.localUplcEvaluation.evaluator,
      referenceScripts: preflight.referenceScripts,
    });
    const attempt = priorAttempts + 1;
    await append({
      kind: "submission_intent",
      actionId: action.actionId,
      actionInput: action.input,
      ...(preflight.durableRecovery === undefined
        ? {}
        : { durableRecovery: preflight.durableRecovery }),
      attempt,
      txHash: preflight.txHash,
    });
    let submitted: FraudProofWorkflowSubmitResult;
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_submit",
    });
    await assertWorkflowFundingReservationReadyToSubmit({
      journal,
      transactionHash: preflight.txHash,
    });
    try {
      submitted = await adapter.submit({
        ...context,
        entries,
        action,
        preflight,
      });
    } catch (cause) {
      submitted = {
        kind: "ambiguous",
        detail: `submit threw after durable intent: ${formatUnknownError(cause)}`,
      };
    }
    if (submitted.kind === "submitted") {
      const submittedTxHash = normalizeTxHash(
        submitted.txHash,
        "submitted transaction hash",
      );
      if (submittedTxHash !== preflight.txHash) {
        return await stalled(
          `submission for ${action.actionId} returned ${submittedTxHash}, but durable intent permits only ${preflight.txHash}`,
        );
      }
      await append({
        kind: "submitted",
        actionId: action.actionId,
        attempt,
        txHash: submittedTxHash,
      });
    } else {
      const ambiguousTxHash =
        submitted.txHash === undefined
          ? preflight.txHash
          : normalizeTxHash(submitted.txHash, "ambiguous transaction hash");
      if (ambiguousTxHash !== preflight.txHash) {
        return await stalled(
          `ambiguous submission for ${action.actionId} reported ${ambiguousTxHash}, but durable intent permits only ${preflight.txHash}`,
        );
      }
      await append({
        kind: "submission_ambiguous",
        actionId: action.actionId,
        attempt,
        txHash: ambiguousTxHash,
        detail: submitted.detail,
      });
    }
    // The next iteration sees an unresolved action and must reconcile before
    // it can create another submission intent.
  }
  return await stalled(`workflow exceeded ${maxActions.toString()} actions`);
};

/** Canonical-block classified workflow entry retained for all ordinary families. */
export const runFraudProofWorkflow = async ({
  deploymentFingerprint,
  evidence,
  detections,
  replayContext,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly evidence: CanonicalBlockEvidence;
  readonly detections: readonly CanonicalViolationDetection[];
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const verified = await verifiedReleaseFinality({
    deploymentFingerprint,
    authority: releaseFinalityAuthority,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections,
    minimumConfirmationDepth: verified.releaseFinality.policy.confirmationDepth,
  });
  if (classification.decision !== "fault_detected") {
    return { kind: classification.decision, classification };
  }
  return await runAdmittedFraudProofWorkflow({
    deploymentFingerprint: verified.deploymentFingerprint,
    category: classification.category,
    headerHash: evidence.headerHash,
    evidenceBinding: canonicalEvidenceBinding(evidence),
    prepareFamilyArtifact: async (adapter) =>
      await adapter.prepare({
        evidence,
        classification,
        ...(replayContext === undefined ? {} : { replayContext }),
      }),
    registry,
    journal,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};

/**
 * Dedicated Q44 entry. It owns the public-DA fetch and typed raw-leaf route,
 * so no caller-authored classification or durable proof artifact can enter the
 * shared lifecycle. A canonical payload is not silently treated as Q44.
 */
export const runDaHashPreimageWorkflowFromRetainedDa = async ({
  deploymentFingerprint,
  observation,
  sources,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const scope = [...registry.keys()];
  if (scope.length !== 1 || scope[0] !== "daHashPreimage") {
    throw new Error(
      `dedicated Q44 workflow requires the exact daHashPreimage registry; found=${scope.join(",")}`,
    );
  }
  const verified = await verifiedReleaseFinality({
    deploymentFingerprint,
    authority: releaseFinalityAuthority,
  });
  const routed = await fetchFraudProofEvidence({
    observation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: verified.releaseFinality.policy.confirmationDepth,
  });
  if (
    routed.schemaVersion !== FRAUD_PROOF_EVIDENCE_ROUTE ||
    routed.kind !== "da_hash_preimage"
  ) {
    throw new Error(
      "dedicated Q44 workflow found no authenticated raw source-leaf defect",
    );
  }
  const familyArtifact = normalizeJournalJson({
    schemaVersion: "midgard-production-da-hash-preimage-artifact-v1",
    headerHash: routed.plan.headerHash,
    committedTransactionsRoot: routed.plan.committedTransactionsRoot,
    l2TransactionCount: routed.plan.l2TransactionCount,
    committedTxId: routed.plan.violation.committedTxId,
    entries: routed.evidence.entries,
  }) as JournalJsonObject;
  const evidenceBinding = normalizeJournalJson({
    route: "authenticated_source_leaf",
    headerHash: routed.evidence.headerHash,
    payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
    payloadSha256: routed.evidence.payloadSha256,
    committedTransactionsRoot: routed.evidence.committedTransactionsRoot,
    l2TransactionCount: routed.evidence.l2TransactionCount.toString(),
    committedTxId: routed.plan.violation.committedTxId,
    l1BlockHash: routed.evidence.l1ChainPoint.blockHash,
    l1Slot: routed.evidence.l1ChainPoint.slot.toString(),
  }) as WorkflowEvidenceBinding;
  return await runAdmittedFraudProofWorkflow({
    deploymentFingerprint: verified.deploymentFingerprint,
    category: "daHashPreimage",
    headerHash: routed.evidence.headerHash,
    evidenceBinding,
    prepareFamilyArtifact: async () => familyArtifact,
    registry,
    journal,
    terminalVerifier,
    releaseFinality: verified.releaseFinality,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};

/**
 * W-O5 production entry point: fetch the payload only through public retained
 * DA, authenticate it against the L1-observed header, detect/classify locally,
 * then enter the journaled workflow. There is intentionally no REST, database,
 * or local-file evidence option in this API.
 */
export const runFraudProofWorkflowFromRetainedDa = async ({
  deploymentFingerprint,
  observation,
  sources,
  replayer,
  replayContext,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  /** Exact closed replay bundle; arbitrary partial detectors are forbidden. */
  readonly replayer: CompleteCanonicalReplay;
  /** Opaque L1/public-DA-admitted predecessor context, when required. */
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const registryScope = [...registry.keys()];
  if (
    registryScope.length !== replayer.launchScope.length ||
    registryScope.some(
      (category, index) => category !== replayer.launchScope[index],
    )
  ) {
    throw new Error(
      `production retained-DA replay launch scope differs from exact workflow registry order: replay=${replayer.launchScope.join(",")} registry=${registryScope.join(",")}`,
    );
  }
  if (
    releaseFinalityAuthority.authorityVersion !==
    FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY
  ) {
    throw new Error(
      "workflow requires the deployment-manifest release finality authority",
    );
  }
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
    }),
  );
  if (
    releaseFinality.deploymentIdentityDigest !== normalizedDeploymentFingerprint
  ) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  const routed = await fetchFraudProofEvidence({
    observation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
  });
  if (routed.kind === "canonical_decodability") {
    if (
      registryScope.length !== 1 ||
      registryScope[0] !== "canonicalDecodability"
    ) {
      throw new Error(
        `authenticated Q17 committed-field defect requires the exact canonicalDecodability registry; found=${registryScope.join(",")}`,
      );
    }
    const familyArtifact = normalizeJournalJson(
      canonicalDecodabilityArtifactFromRawEvidence(routed.evidence),
    ) as JournalJsonObject;
    const evidenceBinding = normalizeJournalJson({
      route: "authenticated_committed_field_defect",
      headerHash: routed.evidence.headerHash,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      committedTransactionsRoot: routed.evidence.committedTransactionsRoot,
      l2TransactionCount: routed.evidence.l2TransactionCount.toString(),
      selectedTransactionIndex:
        routed.evidence.selected.transactionIndex.toString(),
      selectedTransactionId: routed.evidence.selected.nodeTxId,
      selectedFieldIndex: routed.evidence.selected.fieldIndex.toString(),
      selectedVerdict: routed.evidence.selected.verdict.toString(),
      l1BlockHash: routed.evidence.l1ChainPoint.blockHash,
      l1Slot: routed.evidence.l1ChainPoint.slot.toString(),
    }) as WorkflowEvidenceBinding;
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category: "canonicalDecodability",
      headerHash: routed.evidence.headerHash,
      evidenceBinding,
      prepareFamilyArtifact: async () => familyArtifact,
      registry,
      journal,
      terminalVerifier,
      releaseFinality,
      ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
      ...(maxActions === undefined ? {} : { maxActions }),
      ...(now === undefined ? {} : { now }),
    });
  }
  if (routed.kind === "da_hash_preimage") {
    throw new Error(
      "authenticated Q44 source-leaf defect requires the dedicated daHashPreimage workflow",
    );
  }
  if (routed.kind === "field_preimage_length_mismatch") {
    throw new Error(
      "authenticated field-length source defect requires the dedicated fieldPreimageLengthMismatch workflow",
    );
  }
  if (routed.kind === "mint_declared_asset_limit") {
    throw new Error(
      "authenticated mint declared-asset-limit defect requires the dedicated mintDeclaredAssetLimit workflow",
    );
  }
  if (routed.kind === "observers_forbidden_on_untagged_network") {
    throw new Error(
      "authenticated observer/network defect requires the dedicated observersForbiddenOnUntaggedNetwork workflow",
    );
  }
  const evidence = routed.evidence;
  const replayDecision = await replayer.replay(evidence, replayContext);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision: replayDecision,
    ...(replayContext === undefined ? {} : { context: replayContext }),
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: normalizedDeploymentFingerprint,
    evidence,
    detections,
    ...(replayContext === undefined ? {} : { replayContext }),
    registry,
    journal,
    terminalVerifier,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};

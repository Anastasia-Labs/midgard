import { formatUnknownError } from "@al-ft/midgard-core";
import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { canonicalDecodabilityArtifactFromRawEvidence } from "../evidence/canonical-decodability-raw-evidence.js";
import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
  type FraudProofEvidence,
} from "../evidence/fraud-proof-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import {
  assertWorkflowJournalActuation,
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  type CanonicalBlockClassification,
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "./classification.js";
import {
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay.js";
import {
  abandonWorkflowFundingReservationTransaction,
  acknowledgeWorkflowFundingAbandonment,
  assertWorkflowFundingAbandonmentHandoffJournal,
  assertWorkflowFundingCompletionHandoffJournal,
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  confirmWorkflowFundingReservationTransaction,
  conflictWorkflowFundingReservationTransaction,
  createWorkflowFundingAbandonmentHandoff,
  prepareWorkflowFundingReservationTransaction,
  readWorkflowFundingRecovery,
  reconcileWorkflowFundingSubmissionHandoff,
  releaseIdleWorkflowFundingReservation,
  releaseWorkflowFundingReservation,
  reobserveWorkflowFundingReservationTransaction,
  type WorkflowFundingCompletionHandoff,
  WorkflowFundingReservationUnavailableError,
  type WorkflowFundingSubmissionHandoff,
} from "./funding-reservation-permit.js";
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
} from "./journal.js";
import { LocalKupmiosTransportUnavailableError } from "./local-kupmios-http-ogmios-source.js";
import { LocalKupmiosCheckpointChangedError } from "./local-kupmios-raw-l1-authority.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";
import { copyWorkflowPreflightTransaction } from "./transaction-boundary.js";

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
      /** A prerequisite is awaiting authenticated inclusion or availability. */
      readonly kind: "pending";
      readonly reason: string;
    }
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
  | { readonly kind: "unknown"; readonly reason: string }
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
  /** Authenticate reversible inclusion with the same checks except anchor depth. */
  verifyIncluded?(
    input: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0],
  ): Promise<FraudProofWorkflowTerminal>;
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

export type WorkflowRawFamilyEvidence = Extract<
  FraudProofEvidence,
  {
    readonly kind:
      | "field_preimage_length_mismatch"
      | "mint_declared_asset_limit"
      | "observers_forbidden_on_untagged_network";
  }
>;

type FraudProofWorkflowAdapterContext = {
  /** Set from the admitted journal authority; never grants submission authority. */
  readonly reconciliationOnly?: boolean;
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
  /** Reconstructs typed material from this invocation's authenticated evidence
   * and checks it against the durable artifact before a resumed live capture. */
  validatePreparedArtifact?(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    >;
    readonly artifact: JournalJsonObject;
  }): Promise<void>;
  prepareRaw?(input: WorkflowRawFamilyEvidence): Promise<JournalJsonObject>;
  validatePreparedRawArtifact?(input: {
    readonly routed: WorkflowRawFamilyEvidence;
    readonly artifact: JournalJsonObject;
  }): Promise<void>;
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
      readonly signedTransactionCborHex?: string;
      readonly authorizeResubmission?: (input: {
        readonly transactionHash: string;
        readonly signedTransactionCborHex: string;
      }) => Promise<void>;
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
  const validatePreparedArtifact = adapter.validatePreparedArtifact;
  const prepareRaw = adapter.prepareRaw;
  const validatePreparedRawArtifact = adapter.validatePreparedRawArtifact;
  const observe = adapter.observe;
  const preflight = adapter.preflight;
  const submit = adapter.submit;
  const reconcile = adapter.reconcile;
  return Object.freeze({
    adapterVersion: adapter.adapterVersion,
    category: adapter.category,
    safety: Object.freeze({ ...adapter.safety }),
    ...(validatePreparedArtifact === undefined
      ? {}
      : {
          validatePreparedArtifact: Object.freeze(
            (input: Parameters<typeof validatePreparedArtifact>[0]) =>
              validatePreparedArtifact(input),
          ),
        }),
    ...(prepareRaw === undefined
      ? {}
      : {
          prepareRaw: Object.freeze((input: Parameters<typeof prepareRaw>[0]) =>
            prepareRaw(input),
          ),
        }),
    ...(validatePreparedRawArtifact === undefined
      ? {}
      : {
          validatePreparedRawArtifact: Object.freeze(
            (input: Parameters<typeof validatePreparedRawArtifact>[0]) =>
              validatePreparedRawArtifact(input),
          ),
        }),
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
        readonly route: "authenticated_raw_family";
        readonly category:
          | "fieldPreimageLengthMismatch"
          | "mintDeclaredAssetLimit"
          | "observersForbiddenOnUntaggedNetwork";
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

// Inclusion may move when the same commitment is re-included after rollback.
// Keep the original locator in the journal for provenance; reuse proof material
// only when every content/commitment field still matches fresh admitted evidence.
const workflowEvidenceContentDigest = (
  binding: WorkflowEvidenceBinding,
): string => {
  const { l1BlockHash, l1Slot, ...content } = binding;
  if (
    !/^[0-9a-f]{64}$/u.test(l1BlockHash) ||
    !/^(0|[1-9][0-9]*)$/u.test(l1Slot)
  )
    throw new Error(
      "workflow evidence binding has an invalid L1 source locator",
    );
  return journalJsonDigest(content);
};

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
    workflowEvidenceContentDigest(envelope.evidenceBinding) !==
      workflowEvidenceContentDigest(evidenceBinding) ||
    envelope.releaseFinality.deploymentIdentityDigest !==
      releaseFinality.deploymentIdentityDigest ||
    envelope.releaseFinality.blueprintHash !== releaseFinality.blueprintHash ||
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

export const normalizeWorkflowTerminal = ({
  identity,
  terminal,
  entries,
  releaseFinality,
  inclusionOnly = false,
}: {
  readonly identity: FraudProofWorkflowIdentity;
  readonly terminal: FraudProofWorkflowTerminal;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly inclusionOnly?: boolean;
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
      (inclusionOnly ? 1 : releaseFinality.policy.confirmationDepth)
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
      readonly kind: "terminal_included";
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly terminal: FraudProofWorkflowTerminal;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
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
      /** Keep the objective active and resume on fresh chain observation. */
      readonly resumeOnObservation?: true;
      readonly workflowId: string;
      readonly identity: FraudProofWorkflowIdentity;
      readonly reason: string;
      readonly entries: readonly FraudProofWorkflowJournalEntry[];
      /**
       * Present when a stalled run failed while the adapter built the next
       * transaction. That transaction targets the live L1 tip while the stage
       * it acts on is release-final, so the stall may only mean the
       * authenticated observation still trails the tip; the caller may resume
       * once it has caught up.
       */
      readonly phase?: "preflight";
    };

/** Resume the existing adapter from its durable evidence only. This entrypoint
 * requires an opaque reconciliation permit and cannot prepare a new execution. */
export const resumeRecordedFraudProofWorkflow = async (input: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}): Promise<FraudProofWorkflowRunResult> => {
  if (!workflowJournalIsReconciliationOnly(input.journal))
    throw new Error(
      "recorded workflow recovery requires reconciliation-only authority",
    );
  const identity = normalizeFraudProofWorkflowIdentity({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: input.deploymentFingerprint,
    category: input.category,
    target: { kind: "state_queue_header", headerHash: input.headerHash },
    decisionDigest: workflowActuationDecisionDigest(input.journal),
  });
  const entries = await input.journal.load(
    computeFraudProofWorkflowId(identity),
  );
  validateFraudProofWorkflowJournal({
    workflowId: computeFraudProofWorkflowId(identity),
    entries,
    expectedIdentity: identity,
  });
  const prepared = entries[1];
  if (prepared?.event.kind !== "prepared")
    throw new Error("recorded workflow has no prepared evidence");
  const envelope = prepared.event.artifact as PersistedArtifactEnvelope;
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await input.releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: input.deploymentFingerprint,
    }),
  );
  return await runAdmittedFraudProofWorkflow({
    ...input,
    releaseFinality,
    evidenceBinding: envelope.evidenceBinding,
    registry: createFraudProofWorkflowRegistry({
      adapters: [input.adapter],
      launchScope: [input.category],
    }),
    prepareFamilyArtifact: async () => {
      throw new Error("recorded workflow cannot prepare a new artifact");
    },
  });
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
  validateFamilyArtifact,
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
  readonly validateFamilyArtifact?: (
    adapter: FraudProofFamilyWorkflowAdapter,
    artifact: JournalJsonObject,
  ) => Promise<void>;
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
    phase?: "preflight",
  ): Promise<FraudProofWorkflowRunResult> => {
    await append({ kind: "stalled", reason });
    return {
      kind: "stalled",
      workflowId,
      identity,
      reason,
      entries,
      ...(phase === undefined ? {} : { phase }),
    };
  };
  const resumeOnObservation = (
    reason: string,
  ): FraudProofWorkflowRunResult => ({
    kind: "pending",
    resumeOnObservation: true,
    workflowId,
    identity,
    reason,
    entries,
  });

  if (entries.length === 0) {
    if (workflowJournalIsReconciliationOnly(journal))
      throw new Error(
        "reconciliation authority cannot create a workflow journal",
      );
    await append({ kind: "started" });
  }
  let envelope = requirePreparedArtifact({
    entries,
    evidenceBinding,
    releaseFinality,
  });
  if (envelope === undefined) {
    if (workflowJournalIsReconciliationOnly(journal))
      throw new Error(
        "reconciliation authority requires an existing prepared workflow",
      );
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
  } else if (
    validateFamilyArtifact !== undefined &&
    !workflowJournalIsReconciliationOnly(journal)
  ) {
    await validateFamilyArtifact(adapter, envelope.familyArtifact);
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "workflow_resume",
    });
  }

  let fundingRecovery = await readWorkflowFundingRecovery(journal);
  if (fundingRecovery.submissionHandoff !== null) {
    for (const event of reconcileWorkflowFundingSubmissionHandoff({
      handoff: fundingRecovery.submissionHandoff,
      entries,
    }))
      await append(event);
    validateFraudProofWorkflowJournal({
      workflowId,
      entries,
      expectedIdentity: identity,
    });
  }
  if (fundingRecovery.completionHandoff !== null)
    assertWorkflowFundingCompletionHandoffJournal({
      handoff: fundingRecovery.completionHandoff,
      entries,
    });

  // Limits bound work in this invocation, not the lifetime of the objective.
  // Journal attempt numbers remain monotonic across automatic continuations.
  const attemptsAtStart = entries;
  for (let actionNumber = 0; actionNumber < maxActions; actionNumber += 1) {
    fundingRecovery = await readWorkflowFundingRecovery(journal);
    if (fundingRecovery.abandonmentHandoff !== null)
      assertWorkflowFundingAbandonmentHandoffJournal({
        handoff: fundingRecovery.abandonmentHandoff,
        entries,
      });
    let context: FraudProofWorkflowAdapterContext = {
      reconciliationOnly: workflowJournalIsReconciliationOnly(journal),
      identity,
      workflowId,
      artifact: envelope.familyArtifact,
      entries,
    };

    // The journal records submissions, not permanent progress. When the current
    // chain asks for an older action, reconcile that exact intent first. This
    // also covers restart after rollback without a separate per-family undo log.
    const priorLifecycle = [...entries]
      .reverse()
      .find(({ event }) => event.kind !== "stalled")?.event;
    let currentObservation: FraudProofWorkflowObservation | undefined;
    if (
      priorLifecycle?.kind !== "completed" &&
      fundingRecovery.completionHandoff?.completion.kind !== "completed" &&
      entries.some(({ event }) => event.kind === "confirmed")
    ) {
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_observe",
      });
      const current = await adapter.observe(context);
      currentObservation = current;
      if (current.kind === "action_required") {
        const intent = latestSubmissionIntent(entries, current.action.actionId);
        const last = lastActionEvent(entries, current.action.actionId);
        if (
          intent !== undefined &&
          (last?.kind === "confirmed" ||
            (priorLifecycle !== undefined &&
              "actionId" in priorLifecycle &&
              priorLifecycle.actionId !== current.action.actionId &&
              !(last?.kind === "reconciled" && last.outcome === "not_found")))
        ) {
          const fundingAvailable =
            await reobserveWorkflowFundingReservationTransaction({
              journal,
              transactionHash: intent.txHash,
            });
          if (!fundingAvailable)
            return {
              kind: "pending",
              resumeOnObservation: true,
              workflowId,
              identity,
              entries,
              reason:
                "Required inputs remain reserved by another unresolved transaction",
            };
          assertWorkflowJournalActuation({
            journal,
            deploymentFingerprint,
            category,
            headerHash,
            checkpoint: "before_reconcile",
          });
          await append({
            kind: "reobserved",
            actionId: intent.actionId,
            txHash: intent.txHash,
          });
          continue;
        }
      }
    }

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
      fundingRecovery.abandonmentHandoff?.submissionIntent ??
      (latestJournalEvent?.kind === "submission_intent" ||
      latestJournalEvent?.kind === "reobserved" ||
      latestJournalEvent?.kind === "submission_ambiguous" ||
      latestJournalEvent?.kind === "submitted" ||
      latestJournalEvent?.kind === "rebroadcast_intent" ||
      (latestJournalEvent?.kind === "reconciled" &&
        latestJournalEvent.outcome === "pending")
        ? latestJournalEvent
        : undefined);
    if (unresolvedEvent !== undefined && "actionId" in unresolvedEvent) {
      const intent =
        fundingRecovery.abandonmentHandoff?.submissionIntent ??
        latestSubmissionIntent(entries, unresolvedEvent.actionId);
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
      let rebroadcastAttempted = false;
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
          ...(fundingRecovery.transition === null
            ? {}
            : {
                signedTransactionCborHex:
                  fundingRecovery.transition.signedTransactionCborHex,
                ...(fundingRecovery.abandonmentHandoff !== null ||
                workflowJournalIsReconciliationOnly(journal)
                  ? {}
                  : {
                      authorizeResubmission: async (transaction: {
                        transactionHash: string;
                        signedTransactionCborHex: string;
                      }) => {
                        const recorded =
                          await readWorkflowFundingRecovery(journal);
                        if (
                          recorded.transition === null ||
                          recorded.transition.transactionHash !==
                            intent.txHash ||
                          transaction.transactionHash !== intent.txHash ||
                          transaction.signedTransactionCborHex !==
                            recorded.transition.signedTransactionCborHex
                        )
                          throw new Error(
                            "rebroadcast changed the exact durable transaction intent",
                          );
                        const previousBroadcast = [...entries]
                          .reverse()
                          .find(
                            ({ event }) =>
                              (event.kind === "submission_intent" ||
                                event.kind === "rebroadcast_intent") &&
                              event.txHash === intent.txHash,
                          );
                        // Historical observation catch-up can wake this same
                        // objective repeatedly. Existing journal timestamps
                        // bound network retries; they never establish absence.
                        if (
                          previousBroadcast !== undefined &&
                          now().getTime() -
                            Date.parse(previousBroadcast.recordedAt) <
                            30_000
                        )
                          throw new Error(
                            "Recorded transaction rebroadcast is waiting for its retry interval",
                          );
                        const broadcasts =
                          1 +
                          entries.filter(
                            ({ event }) =>
                              event.kind === "rebroadcast_intent" &&
                              event.txHash === intent.txHash,
                          ).length;
                        await assertWorkflowFundingReservationReadyToSubmit({
                          journal,
                          transactionHash: intent.txHash,
                        });
                        assertWorkflowJournalActuation({
                          journal,
                          deploymentFingerprint,
                          category,
                          headerHash,
                          checkpoint: "before_submit",
                        });
                        await append({
                          kind: "rebroadcast_intent",
                          actionId: action.actionId,
                          txHash: intent.txHash,
                          attempt: broadcasts + 1,
                        });
                        rebroadcastAttempted = true;
                        // Durability is asynchronous; revoke checks must follow it too.
                        assertWorkflowJournalActuation({
                          journal,
                          deploymentFingerprint,
                          category,
                          headerHash,
                          checkpoint: "before_submit",
                        });
                      },
                    }),
              }),
        });
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_reconcile",
        });
      } catch (cause) {
        // A capture exhausted its bounded retries because the canonical head
        // moved. It establishes neither inclusion nor replacement authority;
        // retain the exact signed intent and yield for a fresh observation.
        if (cause instanceof LocalKupmiosCheckpointChangedError)
          return resumeOnObservation(
            `reconciliation awaits a stable boundary for ${action.actionId}: ${cause.message}`,
          );
        if (cause instanceof LocalKupmiosTransportUnavailableError) throw cause;
        return await stalled(
          `reconciliation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        );
      }
      if (
        fundingRecovery.abandonmentHandoff !== null &&
        reconciled.kind !== "not_found"
      ) {
        const reason = `abandoned transaction ${intent.txHash} no longer has authenticated replacement evidence: ${reconciled.kind}; exact outcome remains unresolved`;
        if (workflowJournalIsReconciliationOnly(journal))
          return { kind: "pending", workflowId, identity, reason, entries };
        return await stalled(reason);
      }
      if (reconciled.kind === "unknown") {
        const reason = `reconciliation remains unknown for ${action.actionId}: ${reconciled.reason}`;
        return resumeOnObservation(reason);
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
        // A fresh admitted observation may retry these exact bytes. Do not
        // repeatedly rebroadcast them inside the runner's one-second poll loop.
        if (rebroadcastAttempted)
          return resumeOnObservation(
            `recorded transaction for ${action.actionId} awaits canonical reconciliation`,
          );
        return {
          kind: "pending",
          workflowId,
          identity,
          reason: `transaction for ${action.actionId} is pending`,
          entries,
        };
      }
      const abandonmentHandoff =
        fundingRecovery.abandonmentHandoff ??
        createWorkflowFundingAbandonmentHandoff({
          entries,
          transactionHash: intent.txHash,
        });
      await abandonWorkflowFundingReservationTransaction({
        journal,
        transactionHash: intent.txHash,
        handoff: abandonmentHandoff,
      });
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_reconcile",
      });
      if (
        !assertWorkflowFundingAbandonmentHandoffJournal({
          handoff: abandonmentHandoff,
          entries,
        })
      )
        await append(abandonmentHandoff.reconciliation);
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_reconcile",
      });
      await acknowledgeWorkflowFundingAbandonment({
        journal,
        handoff: abandonmentHandoff,
      });
      fundingRecovery = await readWorkflowFundingRecovery(journal);
      context = { ...context, entries };
      currentObservation = undefined;
    }

    await releaseIdleWorkflowFundingReservation({ journal, workflowId });

    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_observe",
    });
    const observation: FraudProofWorkflowObservation =
      fundingRecovery.completionHandoff?.completion.kind !== "completed"
        ? (currentObservation ??
          (await adapter.observe({
            ...context,
            reconciliationOnly: workflowJournalIsReconciliationOnly(journal),
          })))
        : {
            kind: "completed",
            terminal: fundingRecovery.completionHandoff.completion.terminal,
          };
    if (observation.kind === "pending") {
      return {
        kind: "pending",
        workflowId,
        identity,
        reason: observation.reason,
        entries,
      };
    }
    if (observation.kind === "completed") {
      let terminal: FraudProofWorkflowTerminal;
      const inclusionOnly =
        observation.terminal.observedAt.confirmationDepth <
          releaseFinality.policy.confirmationDepth &&
        terminalVerifier.verifyIncluded !== undefined;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
        terminal = normalizeWorkflowTerminal({
          identity,
          terminal: await (
            inclusionOnly
              ? terminalVerifier.verifyIncluded!
              : terminalVerifier.verify
          )({
            identity,
            workflowId,
            releaseFinality,
            candidate: observation.terminal,
            artifact: envelope.familyArtifact,
            entries,
          }),
          entries,
          releaseFinality,
          inclusionOnly,
        });
        if (
          fundingRecovery.completionHandoff?.completion.kind === "completed"
        ) {
          const saved = fundingRecovery.completionHandoff.completion.terminal;
          const withoutDepth = (value: FraudProofWorkflowTerminal) => ({
            ...value,
            observedAt: {
              slot: value.observedAt.slot,
              blockHash: value.observedAt.blockHash,
            },
          });
          if (
            terminal.observedAt.confirmationDepth <
              saved.observedAt.confirmationDepth ||
            journalJsonDigest(normalizeJournalJson(withoutDepth(saved))) !==
              journalJsonDigest(normalizeJournalJson(withoutDepth(terminal)))
          )
            throw new Error(
              "released workflow terminal facts changed on the canonical chain",
            );
          terminal = normalizeWorkflowTerminal({
            identity,
            terminal: saved,
            entries,
            releaseFinality,
          });
        }
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
      } catch (cause) {
        if (cause instanceof LocalKupmiosTransportUnavailableError) throw cause;
        return await stalled(
          `terminal verification failed: ${formatUnknownError(cause)}`,
        );
      }
      const terminalDigest = journalJsonDigest(
        normalizeJournalJson(terminal, "workflow terminal"),
      );
      const kind = inclusionOnly ? "terminal_included" : "completed";
      const savedInclusion = fundingRecovery.completionHandoff;
      if (
        inclusionOnly &&
        savedInclusion?.completion.kind === "terminal_included"
      ) {
        // Its signed actions and original inclusion handoff already survive a
        // restart. Re-observe next time rather than persisting every depth tick.
        return { kind, workflowId, identity, terminal, entries };
      }
      if (entries.at(-1)?.event.kind !== "completed") {
        const handoff: WorkflowFundingCompletionHandoff = (fundingRecovery
          .completionHandoff?.completion.kind === kind
          ? fundingRecovery.completionHandoff
          : null) ?? {
          workflowId,
          identity,
          preparedArtifactDigest: journalJsonDigest(envelope),
          expectedJournalSequence: entries.length,
          completion: { kind, terminal, terminalDigest },
        };
        await releaseWorkflowFundingReservation({ journal, handoff });
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint,
          category,
          headerHash,
          checkpoint: "before_terminal_verify",
        });
        await append({ kind, terminal, terminalDigest });
      }
      return {
        kind,
        workflowId,
        identity,
        terminal,
        entries,
      };
    }
    if (observation.kind === "conflict") {
      return await stalled(`chain conflict: ${observation.reason}`);
    }
    if (workflowJournalIsReconciliationOnly(journal))
      return {
        kind: "pending",
        workflowId,
        identity,
        entries,
        reason: "Canonical workflow requires fresh submission authority",
      };
    const action = validateAction(observation.action);
    const latest = lastActionEvent(entries, action.actionId);
    if (latest?.kind === "confirmed") {
      return await stalled(
        `confirmed action ${action.actionId} is still reported as required`,
      );
    }

    const priorAttempts = attemptCount(entries, action.actionId);
    if (
      priorAttempts - attemptCount(attemptsAtStart, action.actionId) >=
      maxSubmissionAttempts
    ) {
      return resumeOnObservation(
        `submission batch exhausted for ${action.actionId}; objective remains active`,
      );
    }
    let preflight: FraudProofWorkflowPreflight;
    let adapterPreflightFailed = false;
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
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint,
        category,
        headerHash,
        checkpoint: "before_preflight",
      });
      let captured: Awaited<ReturnType<typeof adapter.preflight>>;
      try {
        captured = await adapter.preflight({ ...context, action });
      } catch (cause) {
        adapterPreflightFailed = true;
        throw cause;
      }
      preflight = validatePreflight({ action, preflight: captured });
    } catch (cause) {
      if (
        cause instanceof WorkflowActionChangedError ||
        cause instanceof WorkflowFundingReservationUnavailableError
      )
        return resumeOnObservation(cause.message);
      if (cause instanceof LocalKupmiosTransportUnavailableError) throw cause;
      return await stalled(
        `preflight failed for ${action.actionId}: ${formatUnknownError(cause)}`,
        adapterPreflightFailed ? "preflight" : undefined,
      );
    }
    const preflightEvent: WorkflowFundingSubmissionHandoff["preflight"] = {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: preflight.txHash,
      localEvaluator: preflight.localUplcEvaluation.evaluator,
      referenceScripts: preflight.referenceScripts,
    };
    const attempt = priorAttempts + 1;
    const submissionIntent: WorkflowFundingSubmissionHandoff["submissionIntent"] =
      {
        kind: "submission_intent",
        actionId: action.actionId,
        actionInput: action.input,
        ...(preflight.durableRecovery === undefined
          ? {}
          : { durableRecovery: preflight.durableRecovery }),
        attempt,
        txHash: preflight.txHash,
      };
    try {
      await prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: {
          workflowId,
          identity,
          preparedArtifactDigest: journalJsonDigest(envelope),
          expectedJournalSequence: entries.length,
          preflight: preflightEvent,
          submissionIntent,
        },
      });
    } catch (cause) {
      if (cause instanceof WorkflowFundingReservationUnavailableError)
        return resumeOnObservation(cause.message);
      if (cause instanceof LocalKupmiosTransportUnavailableError) throw cause;
      return await stalled(
        `funding reservation failed for ${action.actionId}: ${formatUnknownError(cause)}`,
      );
    }
    await append(preflightEvent);
    await append(submissionIntent);
    let submitted: FraudProofWorkflowSubmitResult;
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_submit",
    });
    try {
      await assertWorkflowFundingReservationReadyToSubmit({
        journal,
        transactionHash: preflight.txHash,
      });
    } catch (cause) {
      if (cause instanceof WorkflowFundingReservationUnavailableError)
        return resumeOnObservation(
          `signed transaction ${preflight.txHash} requires canonical reconciliation: ${cause.message}`,
        );
      throw cause;
    }
    assertWorkflowJournalActuation({
      journal,
      deploymentFingerprint,
      category,
      headerHash,
      checkpoint: "before_submit",
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
  return resumeOnObservation(
    `workflow processed ${maxActions.toString()} actions; objective remains active`,
  );
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
    minimumConfirmationDepth: 1,
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
    validateFamilyArtifact: async (adapter, artifact) => {
      await adapter.validatePreparedArtifact?.({
        evidence,
        classification,
        artifact,
        ...(replayContext === undefined ? {} : { replayContext }),
      });
    },
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
    minimumConfirmationDepth: 1,
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
  resolveReplayContext,
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
  readonly resolveReplayContext?: (
    evidence: CanonicalBlockEvidence,
  ) => Promise<CompleteCanonicalReplayContext>;
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
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservation({
      observation,
      minimumConfirmationDepth: 1,
    });
  const routed = await fetchFraudProofEvidence({
    observation: admittedObservation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: 1,
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
  if (
    routed.kind === "field_preimage_length_mismatch" ||
    routed.kind === "mint_declared_asset_limit" ||
    routed.kind === "observers_forbidden_on_untagged_network"
  ) {
    const category =
      routed.kind === "field_preimage_length_mismatch"
        ? "fieldPreimageLengthMismatch"
        : routed.kind === "mint_declared_asset_limit"
          ? "mintDeclaredAssetLimit"
          : "observersForbiddenOnUntaggedNetwork";
    const headerHash =
      routed.kind === "field_preimage_length_mismatch"
        ? routed.evidence.prepared.headerHash
        : routed.evidence.headerHash;
    if (registryScope.length !== 1 || registryScope[0] !== category)
      throw new Error(
        `authenticated raw-family evidence requires exact ${category} registry`,
      );
    const evidenceBinding = normalizeJournalJson({
      route: "authenticated_raw_family",
      category,
      headerHash,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      l1BlockHash: admittedObservation.chainPoint.blockHash,
      l1Slot: admittedObservation.chainPoint.slot.toString(),
    }) as WorkflowEvidenceBinding;
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category,
      headerHash,
      evidenceBinding,
      prepareFamilyArtifact: async (adapter) => {
        if (adapter.prepareRaw === undefined)
          throw new Error(`${category} lacks authenticated raw preparation`);
        return await adapter.prepareRaw(routed);
      },
      validateFamilyArtifact: async (adapter, artifact) => {
        if (adapter.validatePreparedRawArtifact === undefined)
          throw new Error(
            `${category} lacks authenticated raw artifact revalidation`,
          );
        await adapter.validatePreparedRawArtifact({ routed, artifact });
      },
      registry,
      journal,
      terminalVerifier,
      releaseFinality,
      ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
      ...(maxActions === undefined ? {} : { maxActions }),
      ...(now === undefined ? {} : { now }),
    });
  }
  const evidence = routed.evidence;
  if (replayContext !== undefined && resolveReplayContext !== undefined)
    throw new Error(
      "workflow replay context must have one authenticated source",
    );
  const admittedReplayContext =
    resolveReplayContext === undefined
      ? replayContext
      : await resolveReplayContext(evidence);
  const replayDecision = await replayer.replay(evidence, admittedReplayContext);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision: replayDecision,
    ...(admittedReplayContext === undefined
      ? {}
      : { context: admittedReplayContext }),
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: normalizedDeploymentFingerprint,
    evidence,
    detections,
    ...(admittedReplayContext === undefined
      ? {}
      : { replayContext: admittedReplayContext }),
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

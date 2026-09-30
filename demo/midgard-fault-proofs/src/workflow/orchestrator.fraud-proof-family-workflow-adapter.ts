import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type FraudProofEvidence } from "../evidence/fraud-proof-evidence.js";
import { type CanonicalBlockClassification } from "./classification.js";
import { type CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  type JournalJsonObject,
} from "./journal.js";
import { type VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

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

export type FraudProofWorkflowAdapterContext = {
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

export const freezeWorkflowAdapter = (
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

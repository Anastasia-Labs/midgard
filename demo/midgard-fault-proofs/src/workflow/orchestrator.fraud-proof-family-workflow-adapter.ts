import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type FraudProofEvidence } from "../evidence/fraud-proof-evidence.js";
import { type CanonicalBlockClassification } from "./classification.js";
import { type CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTerminal,
  type WorkflowJsonObject,
} from "./identity.js";
import type { FraudProofOwnThreads } from "./raw-l1-family-derivation.own-thread.js";
import { type VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import type { WorkflowFundingShortfallError } from "./workflow-funding-shortfall.js";

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
  readonly input: WorkflowJsonObject;
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

export const FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER =
  "midgard-authenticated-l1-workflow-terminal-verifier-v1" as const;

/**
 * Independent chain-state authority for terminal closure. It must inspect
 * authenticated Cardano L1 state; a family adapter's own observation is
 * deliberately insufficient to mark a workflow complete.
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
    readonly artifact: WorkflowJsonObject;
  }): Promise<FraudProofWorkflowTerminal>;
}

export type WorkflowRawFamilyEvidence = Extract<
  FraudProofEvidence,
  {
    readonly kind:
      | "field_preimage_length_mismatch"
      | "l2_tx_mistag"
      | "mint_declared_asset_limit"
      | "observers_forbidden_on_untagged_network";
  }
>;

export type FraudProofWorkflowAdapterContext = {
  /** True under a permit over a fault that is no longer live: the
   * proof-materials sweep then adds no funding input. */
  readonly reconciliationOnly?: boolean;
  readonly identity: FraudProofWorkflowIdentity;
  readonly workflowId: string;
  readonly artifact: WorkflowJsonObject;
  /**
   * The actions this run already skipped because they did not build: an
   * observation ranks the moves after them instead, and waits rather than
   * reporting the family settled while one of them stands.
   */
  readonly unbuiltActionIds?: ReadonlySet<string>;
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
  }): Promise<WorkflowJsonObject>;
  /** Reconstructs typed material from this invocation's authenticated evidence
   * and checks it against the durable artifact before a resumed live capture. */
  validatePreparedArtifact?(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { readonly decision: "fault_detected" }
    >;
    readonly artifact: WorkflowJsonObject;
  }): Promise<void>;
  prepareRaw?(input: WorkflowRawFamilyEvidence): Promise<WorkflowJsonObject>;
  validatePreparedRawArtifact?(input: {
    readonly routed: WorkflowRawFamilyEvidence;
    readonly artifact: WorkflowJsonObject;
  }): Promise<void>;
  observe(
    context: FraudProofWorkflowAdapterContext,
  ): Promise<FraudProofWorkflowObservation>;
  preflight(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
    },
  ): Promise<FraudProofWorkflowPreflight>;
  /**
   * The error `action` pauses on when the prover wallet cannot fund it,
   * before the action is built: `shortfall`, or the alarm the
   * action's own build names when the wallet is short, such as a dispute
   * move's, naming what must land and by when. Without it, `shortfall`.
   */
  unfunded?(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
      readonly shortfall: WorkflowFundingShortfallError;
    },
  ): Promise<WorkflowFundingShortfallError>;
  /** Sends the signed transaction `preflight` built. */
  submit(
    context: FraudProofWorkflowAdapterContext & {
      readonly action: FraudProofWorkflowAction;
      readonly preflight: FraudProofWorkflowPreflight;
    },
  ): Promise<FraudProofWorkflowSubmitResult>;
  /**
   * Our computation threads on the header as authenticated L1 shows them.
   * The orchestrator never resolves a run by another record while one of
   * them still stands; a family whose cancel is wired in front of it
   * reclaims that thread first.
   */
  observeOwnThreads?(input: {
    readonly headerHash: string;
  }): Promise<FraudProofOwnThreads>;
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
  const observeOwnThreads = adapter.observeOwnThreads;
  const unfunded = adapter.unfunded;
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
    ...(observeOwnThreads === undefined
      ? {}
      : {
          observeOwnThreads: Object.freeze(
            (input: Parameters<typeof observeOwnThreads>[0]) =>
              observeOwnThreads(input),
          ),
        }),
    ...(unfunded === undefined
      ? {}
      : {
          unfunded: Object.freeze((input: Parameters<typeof unfunded>[0]) =>
            unfunded(input),
          ),
        }),
  });
};

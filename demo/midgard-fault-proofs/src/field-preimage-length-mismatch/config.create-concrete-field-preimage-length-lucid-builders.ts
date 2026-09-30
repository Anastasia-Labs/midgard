import type {
  CommittedFieldClaim,
  ForcedInclusionTxV1,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { type FieldPreimageLengthMismatchFaultProofContracts } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type FieldPreimageLengthClaimResolver,
  submitFieldPreimageLengthAcceptedAuthentication,
  submitFieldPreimageLengthAcceptedDispatch,
  submitFieldPreimageLengthCancel,
  submitFieldPreimageLengthForcedAuthentication,
  submitFieldPreimageLengthForcedDispatch,
  submitFieldPreimageLengthInit,
  submitFieldPreimageLengthTerminal,
} from "./submit-lucid.js";
import type {
  FieldPreimageLengthAction,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow.js";

export const FIELD_PREIMAGE_LENGTH_CONFIG =
  "midgard-field-preimage-length-mismatch-production-config-v1" as const;

export const FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS = Object.freeze({
  step01: "fraudProofFieldPreimageLengthMismatch",
  step02Accepted: "fraudProofFieldPreimageLengthMismatchStep02Accepted",
  step02Forced: "fraudProofFieldPreimageLengthMismatchStep02Forced",
  step03: "fraudProofFieldPreimageLengthMismatchStep03",
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
  fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
} as const);

export type FieldPreimageLengthReferenceScripts = Readonly<{
  step01: UTxO;
  step02Accepted: UTxO;
  step02Forced: UTxO;
  step03: UTxO;
  fieldPreimageCertificateMint: UTxO;
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

export type ManifestBoundFieldPreimageLengthConfig = Readonly<{
  schemaVersion: typeof FIELD_PREIMAGE_LENGTH_CONFIG;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  binding: FraudProofWorkflowDeploymentBinding<"fieldPreimageLengthMismatch">;
  contracts: FieldPreimageLengthMismatchFaultProofContracts;
  referenceScripts: FieldPreimageLengthReferenceScripts;
}>;

export type LoadManifestBoundFieldPreimageLengthConfig = Readonly<{
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  referenceScripts: FieldPreimageLengthReferenceScripts;
}>;

export type FieldPreimageLengthLucidSubmissionContext = Readonly<{
  config: ManifestBoundFieldPreimageLengthConfig;
  prepared: PreparedFieldPreimageLengthWorkflow;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
}>;

export type FieldPreimageLengthLucidSubmitter = (
  context: FieldPreimageLengthLucidSubmissionContext,
) => Promise<string>;

/** Concrete builder slots required by production wiring; no stage may no-op. */
export type FieldPreimageLengthLucidBuilders = Readonly<{
  init: FieldPreimageLengthLucidSubmitter;
  dispatchAccepted: FieldPreimageLengthLucidSubmitter;
  dispatchForced: FieldPreimageLengthLucidSubmitter;
  authenticateAccepted: FieldPreimageLengthLucidSubmitter;
  authenticateForced: FieldPreimageLengthLucidSubmitter;
  finalize: FieldPreimageLengthLucidSubmitter;
  remove: FieldPreimageLengthLucidSubmitter;
  cancelDispatch: FieldPreimageLengthLucidSubmitter;
  cancelAuthentication: FieldPreimageLengthLucidSubmitter;
  cancelTerminal: FieldPreimageLengthLucidSubmitter;
}>;

/** Authenticated chain/evidence material resolved afresh before each action. */
export type FieldPreimageLengthStage = Readonly<{
  fraudulentBlockOutRef: string;
  threadOutRef?: string;
  stateQueueBlockOutRef?: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  acceptedClaim?: CommittedFieldClaim;
  acceptedClaimResolver?: FieldPreimageLengthClaimResolver;
  acceptedCarriageReferenceInputs?: readonly UTxO[];
  forcedDirection?: 0n | 1n;
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedClaim?: CommittedFieldClaim;
  forcedClaimResolver?: FieldPreimageLengthClaimResolver;
  forcedCarriageReferenceInputs?: readonly UTxO[];
  cancelStepIndex?: 0 | 1 | 2 | 3;
}>;

const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`field-preimage-length missing ${label}`);
  return value;
};

/**
 * Binds all ten production slots to the real Lucid builders. The resolver is
 * deliberately called per action so restart replay uses authenticated current
 * out-refs rather than journal-cached transaction layout.
 */
export const createConcreteFieldPreimageLengthLucidBuilders = ({
  resolveStage,
  remove,
  boundary,
}: {
  readonly resolveStage: (
    context: FieldPreimageLengthLucidSubmissionContext & {
      readonly action: Exclude<FieldPreimageLengthAction, "complete">;
    },
  ) => Promise<FieldPreimageLengthStage>;
  readonly remove: FieldPreimageLengthLucidSubmitter;
  readonly boundary?: (
    action: Exclude<FieldPreimageLengthAction, "complete">,
    prepared: PreparedFieldPreimageLengthWorkflow,
  ) => FraudProofPreSubmitBoundary;
}): FieldPreimageLengthLucidBuilders => {
  const stage = async (
    context: FieldPreimageLengthLucidSubmissionContext,
    action: Exclude<FieldPreimageLengthAction, "complete">,
  ) => await resolveStage({ ...context, action });
  const cancel =
    (fallback: 0 | 1 | 2 | 3): FieldPreimageLengthLucidSubmitter =>
    async (context) => {
      const resolved = await stage(context, "dispatch");
      const result = await submitFieldPreimageLengthCancel({
        config: context.config,
        threadOutRef: required(resolved.threadOutRef, "cancel thread out-ref"),
        stepIndex: resolved.cancelStepIndex ?? fallback,
        preSubmitBoundary: boundary?.("dispatch", context.prepared),
      });
      return result.txHash;
    };
  return Object.freeze({
    init: async (context) => {
      const resolved = await stage(context, "init");
      return (
        await submitFieldPreimageLengthInit({
          config: context.config,
          fraudulentBlockOutRef: resolved.fraudulentBlockOutRef,
          preSubmitBoundary: boundary?.("init", context.prepared),
        })
      ).txHash;
    },
    dispatchAccepted: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await submitFieldPreimageLengthAcceptedDispatch({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "dispatch thread out-ref",
          ),
          stateQueueBlockOutRef: required(
            resolved.stateQueueBlockOutRef,
            "state-queue block out-ref",
          ),
          inclusion: required(resolved.acceptedInclusion, "accepted inclusion"),
          ...(resolved.acceptedClaim === undefined
            ? {}
            : { claim: resolved.acceptedClaim }),
          ...(resolved.acceptedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.acceptedClaimResolver }),
          carriageReferenceInputs:
            resolved.acceptedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("dispatch", context.prepared),
        })
      ).txHash;
    },
    dispatchForced: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await submitFieldPreimageLengthForcedDispatch({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "dispatch thread out-ref",
          ),
          direction: required(resolved.forcedDirection, "forced direction"),
          preSubmitBoundary: boundary?.("dispatch", context.prepared),
        })
      ).txHash;
    },
    authenticateAccepted: async (context) => {
      const resolved = await stage(context, "authenticate");
      return (
        await submitFieldPreimageLengthAcceptedAuthentication({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "authentication thread out-ref",
          ),
          ...(resolved.acceptedClaim === undefined
            ? {}
            : { claim: resolved.acceptedClaim }),
          ...(resolved.acceptedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.acceptedClaimResolver }),
          prepared: context.prepared,
          carriageReferenceInputs:
            resolved.acceptedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("authenticate", context.prepared),
        })
      ).txHash;
    },
    authenticateForced: async (context) => {
      const resolved = await stage(context, "authenticate");
      return (
        await submitFieldPreimageLengthForcedAuthentication({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "authentication thread out-ref",
          ),
          header: required(resolved.forcedHeader, "forced header"),
          membership: required(resolved.forcedMembership, "forced membership"),
          ...(resolved.forcedClaim === undefined
            ? {}
            : { claim: resolved.forcedClaim }),
          ...(resolved.forcedClaimResolver === undefined
            ? {}
            : { claimResolver: resolved.forcedClaimResolver }),
          prepared: context.prepared,
          carriageReferenceInputs: resolved.forcedCarriageReferenceInputs ?? [],
          preSubmitBoundary: boundary?.("authenticate", context.prepared),
        })
      ).txHash;
    },
    finalize: async (context) => {
      const resolved = await stage(context, "finalize");
      return (
        await submitFieldPreimageLengthTerminal({
          config: context.config,
          threadOutRef: required(
            resolved.threadOutRef,
            "terminal thread out-ref",
          ),
          preSubmitBoundary: boundary?.("finalize", context.prepared),
        })
      ).txHash;
    },
    remove: async (context) =>
      await remove({
        ...context,
        preSubmitBoundary: boundary?.("remove", context.prepared),
      }),
    cancelDispatch: cancel(0),
    cancelAuthentication: cancel(1),
    cancelTerminal: cancel(3),
  });
};

export const TX_ID = /^[0-9a-f]{64}$/u;

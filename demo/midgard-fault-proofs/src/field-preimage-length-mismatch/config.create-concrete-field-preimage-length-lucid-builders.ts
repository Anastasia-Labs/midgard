import type {
  ForcedInclusionTxV1,
  Header,
  OutputReference,
  RootMembershipProof,
} from "@al-ft/midgard-sdk/core";
import type {
  CommittedFieldClaim,
  FieldPreimageLengthMismatchFaultProofContracts,
} from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { type LocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  buildFieldPreimageLengthAcceptedAuthentication,
  buildFieldPreimageLengthAcceptedDispatch,
  buildFieldPreimageLengthCancel,
  buildFieldPreimageLengthForcedAuthentication,
  buildFieldPreimageLengthForcedDispatch,
  buildFieldPreimageLengthForcedOutOfDomain,
  buildFieldPreimageLengthInit,
  buildFieldPreimageLengthTerminal,
} from "./submit-lucid.js";
import { type FieldPreimageLengthClaimResolver } from "./submit-lucid.submit-field-preimage-length-cancel.js";
import {
  type FieldPreimageLengthAction,
  fieldPreimageLengthOutOfDomain,
  type PreparedFieldPreimageLengthEvidence,
  type PreparedFieldPreimageLengthWorkflow,
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
  prepared: PreparedFieldPreimageLengthEvidence;
}>;

/** Builds and signs one stage's transaction for the workflow. */
export type FieldPreimageLengthLucidSubmitter = (
  context: FieldPreimageLengthLucidSubmissionContext,
) => Promise<LocallyEvaluatedTransaction>;

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
  forcedHeader?: Header;
  forcedMembership?: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  forcedClaim?: CommittedFieldClaim;
  forcedClaimResolver?: FieldPreimageLengthClaimResolver;
  forcedCarriageReferenceInputs?: readonly UTxO[];
  cancelStepIndex?: 0 | 1 | 2 | 3;
}>;

const inDomain = (
  prepared: PreparedFieldPreimageLengthEvidence,
): PreparedFieldPreimageLengthWorkflow => {
  if (fieldPreimageLengthOutOfDomain(prepared))
    throw new Error("field-preimage-length field is outside the native layout");
  return prepared;
};

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
}: {
  readonly resolveStage: (
    context: FieldPreimageLengthLucidSubmissionContext & {
      readonly action: Exclude<FieldPreimageLengthAction, "complete">;
    },
  ) => Promise<FieldPreimageLengthStage>;
  readonly remove: FieldPreimageLengthLucidSubmitter;
}): FieldPreimageLengthLucidBuilders => {
  const stage = async (
    context: FieldPreimageLengthLucidSubmissionContext,
    action: Exclude<FieldPreimageLengthAction, "complete">,
  ) => await resolveStage({ ...context, action });
  const cancel =
    (fallback: 0 | 1 | 2 | 3): FieldPreimageLengthLucidSubmitter =>
    async (context) => {
      const resolved = await stage(context, "dispatch");
      const result = await buildFieldPreimageLengthCancel({
        config: context.config,
        threadOutRef: required(resolved.threadOutRef, "cancel thread out-ref"),
        stepIndex: resolved.cancelStepIndex ?? fallback,
      });
      return result.transaction;
    };
  return Object.freeze({
    init: async (context) => {
      const resolved = await stage(context, "init");
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthInit({
            config: context.config,
            fraudulentBlockOutRef: resolved.fraudulentBlockOutRef,
          }),
          "field-preimage-length init",
        )
      ).transaction;
    },
    dispatchAccepted: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthAcceptedDispatch({
            config: context.config,
            threadOutRef: required(
              resolved.threadOutRef,
              "dispatch thread out-ref",
            ),
            stateQueueBlockOutRef: required(
              resolved.stateQueueBlockOutRef,
              "state-queue block out-ref",
            ),
            inclusion: required(
              resolved.acceptedInclusion,
              "accepted inclusion",
            ),
            ...(resolved.acceptedClaim === undefined
              ? {}
              : { claim: resolved.acceptedClaim }),
            ...(resolved.acceptedClaimResolver === undefined
              ? {}
              : { claimResolver: resolved.acceptedClaimResolver }),
            carriageReferenceInputs:
              resolved.acceptedCarriageReferenceInputs ?? [],
          }),
          "field-preimage-length dispatch",
        )
      ).transaction;
    },
    dispatchForced: async (context) => {
      const resolved = await stage(context, "dispatch");
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthForcedDispatch({
            config: context.config,
            threadOutRef: required(
              resolved.threadOutRef,
              "dispatch thread out-ref",
            ),
          }),
          "field-preimage-length dispatch",
        )
      ).transaction;
    },
    authenticateAccepted: async (context) => {
      const prepared = inDomain(context.prepared);
      const resolved = await stage(context, "authenticate");
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthAcceptedAuthentication({
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
            prepared,
            carriageReferenceInputs:
              resolved.acceptedCarriageReferenceInputs ?? [],
          }),
          "field-preimage-length authenticate",
        )
      ).transaction;
    },
    authenticateForced: async (context) => {
      const resolved = await stage(context, "authenticate");
      if (fieldPreimageLengthOutOfDomain(context.prepared))
        return (
          await workflowStepTransaction(
            await buildFieldPreimageLengthForcedOutOfDomain({
              config: context.config,
              threadOutRef: required(
                resolved.threadOutRef,
                "authentication thread out-ref",
              ),
              header: required(resolved.forcedHeader, "forced header"),
              membership: required(
                resolved.forcedMembership,
                "forced membership",
              ),
            }),
            "field-preimage-length authenticate",
          )
        ).transaction;
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthForcedAuthentication({
            config: context.config,
            threadOutRef: required(
              resolved.threadOutRef,
              "authentication thread out-ref",
            ),
            header: required(resolved.forcedHeader, "forced header"),
            membership: required(
              resolved.forcedMembership,
              "forced membership",
            ),
            ...(resolved.forcedClaim === undefined
              ? {}
              : { claim: resolved.forcedClaim }),
            ...(resolved.forcedClaimResolver === undefined
              ? {}
              : { claimResolver: resolved.forcedClaimResolver }),
            prepared: context.prepared,
            carriageReferenceInputs:
              resolved.forcedCarriageReferenceInputs ?? [],
          }),
          "field-preimage-length authenticate",
        )
      ).transaction;
    },
    finalize: async (context) => {
      const resolved = await stage(context, "finalize");
      return (
        await workflowStepTransaction(
          await buildFieldPreimageLengthTerminal({
            config: context.config,
            threadOutRef: required(
              resolved.threadOutRef,
              "terminal thread out-ref",
            ),
          }),
          "field-preimage-length finalize",
        )
      ).transaction;
    },
    remove: async (context) => await remove(context),
    cancelDispatch: cancel(0),
    cancelAuthentication: cancel(1),
    cancelTerminal: cancel(3),
  });
};

export const TX_ID = /^[0-9a-f]{64}$/u;

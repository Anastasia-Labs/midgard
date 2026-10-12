import { type UTxO } from "@lucid-evolution/lucid";

import { journalJsonDigest } from "../workflow/journal.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { submitValidationDisputeSemanticResolution } from "./submit.js";
import {
  isSplitScriptSourcesItemRoute,
  requireStagedOneStepArgument,
} from "./submit/reference-scripts.js";
import { captureCekContinuation } from "./workflow-cek-continuation.js";
import {
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeActuatorConfig,
  type ValidationTraceDisputeCapturedAction,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { hex } from "./workflow-engine.recover-validation-trace-state-index.js";
import { type createValidationTraceOneStepArgumentResolver } from "./workflow-one-step-argument.js";
import {
  stagedRoutePreparationInput,
  withStagedPreparation,
} from "./workflow-staged-route-preparation.js";

type SemanticResolutionAction = Extract<
  ValidationTraceDisputeActuatorAction,
  { stage: "semantic_resolution" }
>;

/**
 * Captures a semantic resolution move: a route's entry stage from the live
 * semantic resolver output, or the next stage of a staged route (CEK core or
 * context, or the split ScriptSources redeemer-item route) against the
 * preparation its entry stage consumed.
 */
export const captureSemanticResolution = async ({
  config,
  oneStepArgumentFor,
  action,
  material,
  retained,
  input,
}: {
  readonly config: ValidationTraceDisputeActuatorConfig;
  readonly oneStepArgumentFor: ReturnType<
    typeof createValidationTraceOneStepArgumentResolver
  >;
  readonly action: SemanticResolutionAction;
  readonly material: ValidationTraceDisputeActuationMaterial;
  readonly retained?: ValidationTraceDisputeRetainedRouteInput;
  readonly input: UTxO;
}): Promise<ValidationTraceDisputeCapturedAction> => {
  if (action.cekPreparedResolutionCbor !== undefined)
    return await withStagedPreparation({
      retained: action.cekPreparedResolutionCbor,
      thread: input,
      source: config.stagedPreparations,
      use: async (preparedResolutionCbor) =>
        await captureCekContinuation({
          config,
          oneStepArgumentFor,
          action,
          material,
          ...(retained === undefined ? {} : { retained }),
          preparedResolutionCbor,
          input,
        }),
    });
  const capture = async (
    resumedPreparation?: string,
  ): Promise<ValidationTraceDisputeCapturedAction> => {
    const { argument: oneStepArgument, delivery } = await oneStepArgumentFor({
      material,
      threadOutRef: action.threadOutRef,
      retained,
      action,
      ...(resumedPreparation === undefined
        ? {}
        : { resolutionInput: { ...input, datum: resumedPreparation } }),
    });
    if (
      resumedPreparation !== undefined &&
      !isSplitScriptSourcesItemRoute(
        oneStepArgument,
        requireStagedOneStepArgument(oneStepArgument),
      )
    )
      throw new Error(
        "ScriptSources item continuation resolved a different semantic route",
      );
    const transaction = await captureLocallyEvaluatedTransaction(
      async (preSubmitBoundary) => {
        await submitValidationDisputeSemanticResolution({
          lucid: config.lucid,
          blueprint: config.blueprint,
          deploymentInfo: config.deploymentInfo,
          network: config.network,
          signer: config.signer,
          threadOutRef: action.threadOutRef,
          oneStepArgument,
          ...(delivery === undefined
            ? {}
            : {
                carriageMaterial: delivery.material,
                referenceScriptUtxo: delivery.semanticReference,
              }),
          ...(resumedPreparation === undefined
            ? {}
            : { scriptSourcesItemPreparedCbor: resumedPreparation }),
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      },
    );
    if (
      delivery !== undefined &&
      journalJsonDigest(
        [
          ...workflowTransactionReferenceInputOutRefs(transaction.signed),
        ].sort(),
      ) !== journalJsonDigest(delivery.durableBinding.referenceOutRefs)
    )
      throw new Error(
        "validation semantic transaction changed its prepared reference set",
      );
    return Object.freeze({
      transaction,
      durableRouteInput: {
        transitionCborHex: hex(oneStepArgument.transitionCbor),
        auxiliaryCborHex: hex(oneStepArgument.auxiliaryCbor),
        ...(delivery === undefined
          ? {}
          : { fieldCarriageBinding: delivery.durableBinding }),
        ...stagedRoutePreparationInput({
          argument: oneStepArgument,
          input,
          ...(resumedPreparation === undefined ? {} : { resumedPreparation }),
        }),
      },
    });
  };
  if (action.scriptSourcesItemPreparedCbor === undefined)
    return await capture();
  return await withStagedPreparation({
    retained: action.scriptSourcesItemPreparedCbor,
    thread: input,
    source: config.stagedPreparations,
    use: capture,
  });
};

import {
  PreparedValidationResolutionDatum,
  PreparedValidationResolutionState,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  resumeValidationCekContext,
  resumeValidationCekCore,
} from "./submit.js";
import {
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeActuatorConfig,
  type ValidationTraceDisputeCapturedAction,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import { hex } from "./workflow-engine.recover-validation-trace-state-index.js";
import { type createValidationTraceOneStepArgumentResolver } from "./workflow-one-step-argument.js";

type SemanticResolutionAction = Extract<
  ValidationTraceDisputeActuatorAction,
  { stage: "semantic_resolution" }
>;

/** CEK context (11/2) and CEK core (11/3): routes of one stage per move. */
export const isStagedCekRoute = (argument: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
}): boolean =>
  argument.resolverIndex === 11 &&
  (argument.semanticResolverIndex === 2 ||
    argument.semanticResolverIndex === 3);

/**
 * Captures the next stage of a CEK core or context route from its live
 * checkpoint. The one-step argument is rebuilt from the admitted material
 * against the prepared resolution the route's first stage consumed, so the
 * resumed stage proves exactly what that first move bound.
 */
export const captureCekContinuation = async ({
  config,
  oneStepArgumentFor,
  action,
  material,
  retained,
  preparedResolutionCbor,
  input,
}: {
  readonly config: ValidationTraceDisputeActuatorConfig;
  readonly oneStepArgumentFor: ReturnType<
    typeof createValidationTraceOneStepArgumentResolver
  >;
  readonly action: SemanticResolutionAction;
  readonly material: ValidationTraceDisputeActuationMaterial;
  readonly retained?: ValidationTraceDisputeRetainedRouteInput;
  readonly preparedResolutionCbor: string;
  readonly input: UTxO;
}): Promise<ValidationTraceDisputeCapturedAction> => {
  const prepared = Data.from(
    preparedResolutionCbor,
    PreparedValidationResolutionDatum,
  ).data;
  if (prepared === null)
    throw new Error("CEK continuation retained a null preparation");
  const { argument, delivery } = await oneStepArgumentFor({
    material,
    threadOutRef: action.threadOutRef,
    retained,
    action,
    resolutionInput: { ...input, datum: preparedResolutionCbor },
  });
  if (!isStagedCekRoute(argument))
    throw new Error("CEK continuation resolved a different semantic route");
  if (delivery !== undefined)
    throw new Error("CEK continuation resolved a field carriage delivery");
  const common = {
    lucid: config.lucid,
    blueprint: config.blueprint,
    deploymentInfo: config.deploymentInfo,
    network: config.network,
    signer: config.signer,
    threadOutRef: action.threadOutRef,
    oneStepArgument: argument,
  } as const;
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary) => {
      if (argument.semanticResolverIndex === 3)
        await resumeValidationCekCore({
          ...common,
          maxTransactions: 1,
          preSubmitBoundary,
        });
      else
        await resumeValidationCekContext({
          ...common,
          preparedCbor: Buffer.from(
            Data.to(prepared, PreparedValidationResolutionState),
            "hex",
          ),
          maxTransactions: 1,
          preSubmitBoundary,
        });
    },
  );
  return Object.freeze({
    transaction,
    durableRouteInput: {
      transitionCborHex: hex(argument.transitionCbor),
      auxiliaryCborHex: hex(argument.auxiliaryCbor),
      cekPreparedResolutionCbor: preparedResolutionCbor,
    },
  });
};

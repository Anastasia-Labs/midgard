import {
  buildValidationOneStepArgument,
  type ValidationOneStepArgument,
} from "@al-ft/midgard-validation";
import { type UTxO } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { journalJsonDigest } from "../workflow/journal.js";
import {
  type ValidationTraceDisputeActuationMaterial,
  type ValidationTraceDisputeActuatorAction,
  type ValidationTraceDisputeActuatorConfig,
  type ValidationTraceDisputeRetainedRouteInput,
} from "./workflow-engine.plan-validation-trace-dispute-move.js";
import {
  hex,
  recoverValidationTraceStateIndex,
} from "./workflow-engine.recover-validation-trace-state-index.js";
import { validationResolutionFromUtxo } from "./workflow-field-carriage.js";

export const createValidationTraceOneStepArgumentResolver = ({
  config,
  threadUtxo,
}: {
  readonly config: ValidationTraceDisputeActuatorConfig;
  readonly threadUtxo: (threadOutRef: string) => Promise<UTxO>;
}) => {
  return async ({
    material,
    threadOutRef,
    retained,
    action,
  }: {
    readonly action: ValidationTraceDisputeActuatorAction;
    readonly material: ValidationTraceDisputeActuationMaterial;
    readonly threadOutRef: string;
    readonly retained?: ValidationTraceDisputeRetainedRouteInput;
  }) => {
    const resolution = validationResolutionFromUtxo(
      await threadUtxo(threadOutRef),
    );
    const stateIndex = recoverValidationTraceStateIndex({
      trace: material.challengerTrace,
      resolution,
    });
    const delivery = await config.fieldCarriage?.resolve(action, stateIndex);
    if (
      retained?.fieldCarriageBinding !== undefined &&
      journalJsonDigest(retained.fieldCarriageBinding) !==
        journalJsonDigest(delivery?.durableBinding ?? {})
    )
      throw new WorkflowActionChangedError(
        "validation field carriage references changed after preparation",
      );
    const argument: ValidationOneStepArgument = buildValidationOneStepArgument({
      trace: material.challengerTrace,
      stateIndex,
      ...(delivery === undefined
        ? {}
        : { resolveFieldCarriage: delivery.resolveFieldCarriage }),
    });
    if (
      retained?.auxiliaryCborHex !== undefined &&
      hex(argument.auxiliaryCbor) !== retained.auxiliaryCborHex
    )
      throw new WorkflowActionChangedError(
        "validation auxiliary differs from its durable prepared binding",
      );
    if (
      retained?.transitionCborHex !== undefined &&
      hex(argument.transitionCbor) !== retained.transitionCborHex
    ) {
      throw new Error(
        "validationTraceDispute retained route input diverged from the recomputed one-step argument",
      );
    }
    return { argument, delivery };
  };
};

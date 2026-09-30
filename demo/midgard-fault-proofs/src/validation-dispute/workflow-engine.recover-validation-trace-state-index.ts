import { type MidgardValidationDispute } from "@al-ft/midgard-core";
import {
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
  ValidationMachineState,
  validationMachineStateDataFromCore,
  type ValidationResolutionState,
} from "@al-ft/midgard-sdk";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { type ValidationTraceDisputeCapturedAction } from "./workflow-engine.plan-validation-trace-dispute-move.js";

export const capture = async (
  submit: Parameters<typeof captureLocallyEvaluatedTransaction>[0],
): Promise<ValidationTraceDisputeCapturedAction> =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

export const hex = (value: Buffer | Uint8Array): string =>
  Buffer.from(value).toString("hex");

/**
 * Recovers the agreed low state index from an on-chain resolution state: the
 * challenger successor hash locates the candidate index in the local trace,
 * and the full pre-state encoding must match before the index is trusted.
 */
export const recoverValidationTraceStateIndex = ({
  trace,
  resolution,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly resolution: ValidationResolutionState;
}): number => {
  const preStateCbor = Data.to(resolution.pre_state, ValidationMachineState);
  for (let index = 0; index + 1 < trace.tree.stateHashes.length; index += 1) {
    if (
      hex(trace.tree.stateHashes[index + 1]!) !==
      resolution.challenger_successor_hash
    ) {
      continue;
    }
    const local = Data.to(
      validationMachineStateDataFromCore(trace.states[index]!),
      ValidationMachineState,
    );
    if (local === preStateCbor) return index;
  }
  throw new Error(
    "validationTraceDispute resolution pre-state is not a position of the local challenger trace",
  );
};

export const requireGameDispute = (utxo: UTxO): MidgardValidationDispute => {
  if (utxo.datum == null) {
    throw new Error("validationTraceDispute game thread lost its datum");
  }
  const datum = Data.from(utxo.datum, ValidationDisputeDatum);
  if (datum.data === null) {
    throw new Error(
      "validationTraceDispute game thread carries a null dispute state",
    );
  }
  return validationDisputeCoreFromData(datum.data.dispute);
};

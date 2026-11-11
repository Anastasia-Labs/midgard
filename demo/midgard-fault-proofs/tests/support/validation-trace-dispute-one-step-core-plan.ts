/** Our plan of a CEK core step of a failing script's run, outside any
 * deployment: the claim material of the run, the plan of one of its steps,
 * a staging of the step's staged operands and the outputs that hold it, and
 * a thread for the hops the plan drives. */
import {
  hashMidgardValidationMachineState,
  MIDGARD_CEK_STAGED_CHUNK_BYTES,
  MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS,
} from "@al-ft/midgard-core";
import type { MidgardCekStagingResult } from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "../../src/validation-trace-dispute/claim-material.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCoreStepPlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import type { ValidationTraceStagingLookup } from "../../src/validation-trace-dispute/one-step-staged.js";
import { plutusExecutionBlockFixture } from "../helpers/plutus-execution-block-fixture.js";
import { claimMaterialOf } from "./validation-trace-claim-material.js";
import { stagedScript } from "./validation-trace-dispute-staged-scripts.js";

export const PROVER = "ab".repeat(28);

/** The bytes a staging output holds: a run of full chunks. */
const STAGING_RUN_BYTES =
  MIDGARD_CEK_STAGED_CHUNK_BYTES * MIDGARD_CEK_STAGED_RUN_MAX_CHUNKS;

/** The claim material of the run of the failing script `hex`. */
export const failingScriptMaterial = async (
  hex: string,
): Promise<ValidationTraceClaimMaterial> =>
  claimMaterialOf(
    await plutusExecutionBlockFixture("failing", {
      spend: { script: stagedScript(hex) },
    }),
  );

/** Our plan of the step from `stateIndex` of `material` as a core step,
 * its staged operands staged where `staging` says and, past its
 * PrepareSelected move, over the evidence `committedEvidenceHash` names. */
export const planCoreStep = async (
  material: ValidationTraceClaimMaterial,
  stateIndex: number,
  {
    staging,
    committedEvidenceHash,
  }: {
    readonly staging?: ValidationTraceStagingLookup | undefined;
    readonly committedEvidenceHash?: (() => string) | undefined;
  } = {},
): Promise<ValidationTraceCoreStepPlan> => {
  const planning = await planValidationTraceOneStep({
    material,
    stateIndex,
    challengerHighHash: hashMidgardValidationMachineState(
      (await material.steps(stateIndex)).states[stateIndex + 1]!,
    ).toString("hex"),
    builtins: true,
    ...(staging === undefined ? {} : { staging }),
    ...(committedEvidenceHash === undefined ? {} : { committedEvidenceHash }),
  });
  if (planning.kind !== "plan" || planning.plan.kind !== "core")
    throw new Error(
      `the step is not planned as a core step: ${planning.kind === "unsupported" ? planning.reason : planning.kind}`,
    );
  return planning.plan;
};

/** A staging of `operands` on outputs of the transactions `ids`, in turn,
 * one output a run. */
export const stagingOn =
  (...ids: number[]) =>
  (operands: readonly Uint8Array[]): MidgardCekStagingResult => {
    let next = 0;
    return operands.map((operand) =>
      Array.from(
        { length: Math.max(1, Math.ceil(operand.length / STAGING_RUN_BYTES)) },
        (_, run) => ({
          transactionId: Buffer.alloc(32, ids[run % ids.length]!),
          outputIndex: next++,
        }),
      ),
    );
  };

/** The staging output at `ref`. */
export const stagingOutput = (ref: {
  transactionId: Uint8Array;
  outputIndex: number;
}): UTxO => ({
  txHash: Buffer.from(ref.transactionId).toString("hex"),
  outputIndex: ref.outputIndex,
  address: "addr_test1wstaging",
  assets: { lovelace: 2_000_000n },
});

/** A thread at the PrepareSelected move's holder, whose datum has the shape
 * every core holder's hop reads. */
export const threadUtxo: UTxO = {
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1wz",
  assets: { lovelace: 5_000_000n },
  datum: Data.to(
    new Constr(0, [PROVER, new Constr(0, [new Constr(0, Array(8).fill(0n))])]),
  ),
};

/** A staged builtin step (arm 31, semantics groups 7 and 8) in our plan and
 * its hops. The step's material names the staging outputs that hold its
 * staged operands, and those names enter the step's evidence, so our plan
 * fills in the staging the driver made before it commits the evidence:
 * over a copy of our trace's window, the trace itself left as it was. The
 * hops from the PrepareSelected move through the arm chain read the staging
 * outputs as reference inputs, so the ledger takes the move that commits
 * the evidence only while the outputs it names are live; the settle reads
 * none. A step not staged yet is planned with its staged operands and no
 * outputs, and none of those hops is built for it. */
import type { MidgardCekStagedOutRef } from "@al-ft/midgard-core";
import {
  type MidgardCekStagingResult,
  stageMidgardCekStagedBuiltinMaterial,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceHop } from "./one-step-builders.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";

/** Where the driver staged the staged operands `operands` of a step,
 * indexed as the step names them; undefined while they are not staged (at
 * their confirmation depth). */
export type ValidationTraceStagingLookup = (
  operands: readonly Uint8Array[],
) => Promise<MidgardCekStagingResult | undefined>;

/** A staged step's staged operands and the staging outputs its evidence
 * names, in ledger order; undefined outputs while it is not staged. */
export type ValidationTraceStagedStep = Readonly<{
  operands: readonly Uint8Array[];
  refs: readonly MidgardCekStagedOutRef[] | undefined;
}>;

/**
 * Our trace's window `trace` with the step from `stateIndex`, when it is a
 * staged step with staged operands, staged where `staging` says, and the
 * step's staging; the window itself, and no staging, for any other step.
 * Throws for a staging that does not name each run of each operand.
 */
export const stagedTraceOf = async (
  trace: ValidationTraceSteps,
  stateIndex: number,
  staging: ValidationTraceStagingLookup | undefined,
): Promise<
  Readonly<{
    trace: ValidationTraceSteps;
    staged: ValidationTraceStagedStep | undefined;
  }>
> => {
  const work = trace.witnesses[stateIndex];
  const auxiliary = work?.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "executeBuiltinStaged" ||
    auxiliary.step.witness.stagedOperands.length === 0
  )
    return { trace, staged: undefined };
  const witness = auxiliary.step.witness;
  const operands = witness.stagedOperands;
  const result = await staging?.(operands);
  if (result === undefined)
    return { trace, staged: Object.freeze({ operands, refs: undefined }) };
  const material = stageMidgardCekStagedBuiltinMaterial(
    witness.material,
    operands,
    result,
  );
  return {
    trace: {
      states: trace.states,
      witnesses: {
        [stateIndex]: {
          ...work!,
          auxiliary: {
            ...auxiliary,
            step: { ...auxiliary.step, witness: { ...witness, material } },
          },
        },
        [stateIndex + 1]: trace.witnesses[stateIndex + 1],
      },
    },
    staged: Object.freeze({ operands, refs: material.runs.refs }),
  };
};

/** The live staging outputs the staged step of `plan` names, in its
 * order; none for any other step. An output not live is left out, which
 * the hops that read the outputs refuse. */
export const stagingOutputs = async (
  lucid: LucidEvolution,
  plan: ValidationTraceOneStepPlan,
): Promise<readonly UTxO[] | undefined> => {
  const refs = plan.kind === "core" ? plan.core.staged?.refs : undefined;
  if (refs === undefined) return undefined;
  const outRefs = refs.map((ref) => ({
    txHash: Buffer.from(ref.transactionId).toString("hex"),
    outputIndex: ref.outputIndex,
  }));
  const live = await lucid.utxosByOutRef(outRefs);
  return outRefs.flatMap(
    ({ txHash, outputIndex }) =>
      live.find(
        (utxo) => utxo.txHash === txHash && utxo.outputIndex === outputIndex,
      ) ?? [],
  );
};

const sameOutRef = (utxo: UTxO, ref: MidgardCekStagedOutRef): boolean =>
  utxo.txHash === Buffer.from(ref.transactionId).toString("hex") &&
  utxo.outputIndex === ref.outputIndex;

/**
 * The hop `hop` of the thread at `holder` in `plan`, reading the staging
 * outputs `staging` when the plan's step is a staged one and the hop is
 * the PrepareSelected move's, the binder's or an arm's; throws while the
 * step is not staged, or for outputs other than those its evidence names,
 * in ledger order.
 */
export const withStagingReads = (
  plan: ValidationTraceOneStepPlan,
  holder: string,
  hop: ValidationTraceHop,
  staging: readonly UTxO[] | undefined,
): ValidationTraceHop => {
  if (plan.kind !== "core" || plan.core.staged === undefined) return hop;
  if (holder === "settle") return hop;
  const { refs } = plan.core.staged;
  if (refs === undefined)
    throw new Error(
      "validation-trace one step's staged operands are not staged yet",
    );
  if (
    staging?.length !== refs.length ||
    staging.some((utxo, index) => !sameOutRef(utxo, refs[index]!))
  )
    throw new Error(
      "validation-trace hop does not read the staging outputs its step names",
    );
  return {
    ...hop,
    reads: {
      references: [...(hop.reads?.references ?? []), ...staging],
      yields: hop.reads?.yields ?? [],
    },
  };
};

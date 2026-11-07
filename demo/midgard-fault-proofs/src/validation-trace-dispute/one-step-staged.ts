/** A staged builtin step (arm 31, semantics groups 7 to 9 and 11) in our
 * plan and its hops. The step's material names the staging outputs that
 * hold its staged operands, and those names enter the step's evidence, so
 * our plan fills in the staging the driver made before it commits the
 * evidence: over a copy of our trace's window, the trace itself left as it
 * was. The hops from the PrepareSelected move through the arm chain read the
 * staging outputs as reference inputs, so the ledger takes the move that
 * commits the evidence only while the outputs it names are live; the settle
 * reads none. A step not staged yet is planned with its staged operands and
 * no outputs, and none of those hops is built for it.
 *
 * A serialiseData step (semantics group 10) is staged and read the same way:
 * its one staged operand is R, its result's bytes, which its loop's hops
 * read from the outputs R is staged on. Its evidence names none, so its
 * window is our trace's own. */
import {
  compareMidgardCekStagedOutRefs,
  type MidgardCekStagedOutRef,
} from "@al-ft/midgard-core";
import {
  type MidgardCekCoreStepWitness,
  type MidgardCekStagingResult,
  stageMidgardCekStagedBuiltinMaterial,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceHop } from "./one-step-builders.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";

/** Where the driver staged the staged operands `operands` of a step,
 * indexed as the step names them; undefined while they are not staged (at
 * their confirmation depth). Of several stagings, one `accepts` holds of
 * when there is one. */
export type ValidationTraceStagingLookup = (
  operands: readonly Uint8Array[],
  accepts?: (staging: MidgardCekStagingResult) => boolean,
) => Promise<MidgardCekStagingResult | undefined>;

/** A staged step's staged operands and the staging outputs its hops read,
 * in ledger order, each once (for a staged builtin step, those its evidence
 * names); undefined outputs while it is not staged. A serialiseData step
 * also has the output each run of R is staged on, in byte order. */
export type ValidationTraceStagedStep = Readonly<{
  operands: readonly Uint8Array[];
  refs: readonly MidgardCekStagedOutRef[] | undefined;
  runs?: readonly MidgardCekStagedOutRef[];
}>;

/** A step witness whose operands past the direct carriage bound are
 * staged: a staged builtin step's (arm 31). Any other witness that names
 * staged operands by `material` and carries them in `stagedOperands` (a
 * staged builtin failure's, arm 36) is staged and read the same way, and
 * joins here. */
type StagedOperandWitness = Extract<
  MidgardCekCoreStepWitness,
  { readonly kind: "executeBuiltinStaged" }
>;

const isStagedOperandWitness = (
  witness: MidgardCekCoreStepWitness,
): witness is StagedOperandWitness => witness.kind === "executeBuiltinStaged";

/** The staging outputs of `staging`, in ledger order, each once. */
const distinctOutputs = (
  staging: MidgardCekStagingResult,
): MidgardCekStagedOutRef[] =>
  staging
    .flat()
    .sort(compareMidgardCekStagedOutRefs)
    .filter(
      (ref, at, all) =>
        at === 0 || compareMidgardCekStagedOutRefs(all[at - 1]!, ref) !== 0,
    );

/** A serialiseData step's window, our trace's own, and its staging: R's
 * runs on the outputs `staging` names. */
const serialiseStaged = async (
  trace: ValidationTraceSteps,
  serialised: Uint8Array,
  staging: ValidationTraceStagingLookup | undefined,
) => {
  const operands = [serialised];
  const result = await staging?.(operands);
  return {
    trace,
    staged: Object.freeze(
      result === undefined
        ? { operands, refs: undefined }
        : { operands, refs: distinctOutputs(result), runs: result[0]! },
    ),
  };
};

/**
 * Our trace's window `trace` with the step from `stateIndex`, when it is a
 * staged step with staged operands, staged where `staging` says, and the
 * step's staging; the window itself, and no staging, for any other step.
 * Of several stagings, the one whose window `committed` holds of. Throws for
 * a staging that does not name each run of each operand, or, given
 * `committed`, for none found or one whose window it does not hold of.
 */
export const stagedTraceOf = async (
  trace: ValidationTraceSteps,
  stateIndex: number,
  staging: ValidationTraceStagingLookup | undefined,
  committed?: (window: ValidationTraceSteps) => boolean,
): Promise<
  Readonly<{
    trace: ValidationTraceSteps;
    staged: ValidationTraceStagedStep | undefined;
  }>
> => {
  const work = trace.witnesses[stateIndex];
  const auxiliary = work?.auxiliary;
  if (
    auxiliary?.kind === "cekCoreStep" &&
    auxiliary.step.witness.kind === "executeBuiltinSerialise"
  )
    return await serialiseStaged(
      trace,
      auxiliary.step.witness.serialised,
      staging,
    );
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    !isStagedOperandWitness(auxiliary.step.witness) ||
    auxiliary.step.witness.stagedOperands.length === 0
  )
    return { trace, staged: undefined };
  const witness = auxiliary.step.witness;
  const operands = witness.stagedOperands;
  const windowOf = (result: MidgardCekStagingResult) => {
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
  const accepts =
    committed &&
    ((result: MidgardCekStagingResult) => {
      try {
        return committed(windowOf(result).trace);
      } catch {
        return false;
      }
    });
  const result = await staging?.(operands, accepts);
  if (result === undefined && committed === undefined)
    return { trace, staged: Object.freeze({ operands, refs: undefined }) };
  const window = result === undefined ? undefined : windowOf(result);
  if (
    window === undefined ||
    (committed !== undefined && !committed(window.trace))
  )
    throw new Error(
      "validation-trace staged step's staging differs from the evidence its thread committed",
    );
  return window;
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

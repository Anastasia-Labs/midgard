/** The one-step resolution of a thread of ours, planned from our own trace:
 * the step from the agreed state at `low_index` to our state after it, its
 * one-step argument (the transition and its auxiliary evidence), and the
 * resolver chain that step selects. The runner drives the CEK core steps
 * whose arm settles in one hop (compute, machine, map conversion and case
 * constant), the CEK script selections when the deployment records
 * scripts' decodes, and the CEK context steps whose stage verifies them in
 * one hop when the deployment installs the context resolution; any other
 * step is reported as one the runner does not drive, and its thread waits
 * under the one-step state. */
import {
  hashMidgardValidationMachineState,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildValidationOneStepArgument,
  CekDecodeRecordRequiredError,
  type CekSelectionDecodeRecordResolver,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { plutusItemScriptDecode } from "../script-decode/item-record.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  planValidationTraceContextStep,
  type ValidationTraceContextStep,
} from "./one-step-context.js";
import { recordedSelectionOf } from "./selected-scripts.js";

/** `resolver_index(Cek)` (validation-resolution-v1). */
export const CEK_RESOLVER_INDEX = 11;

/** The CEK execution selection's semantic resolver in the CEK group. */
export const CEK_SELECTION_SEMANTIC_RESOLVER_INDEX = 1;

/** The CEK context step's semantic resolver in the CEK group. */
export const CEK_CONTEXT_SEMANTIC_RESOLVER_INDEX = 2;

/** The CEK core step's semantic resolver in the CEK group. */
export const CEK_CORE_SEMANTIC_RESOLVER_INDEX = 3;

/** The arms whose step settles after one hop, by their core group. */
export const SINGLE_HOP_CORE_ARMS = {
  0: "compute",
  1: "machine",
  2: "mapConversion",
  11: "caseConstant",
} as const;

export type SingleHopCoreGroup = keyof typeof SINGLE_HOP_CORE_ARMS;

/** `hash_one_step_evidence`'s domain (validation-resolution-v1). */
const ONE_STEP_EVIDENCE_DOMAIN =
  "4d69646761726456616c69646174696f6e4f6e655374657045766964656e63655631";

const MODE = {
  compute: 0n,
  return: 1n,
  lookup: 2n,
  builtin: 3n,
  caseSelect: 6n,
  caseApply: 7n,
  semanticBuiltin: 8n,
} as const;

const blake2b256 = (hex: string): string =>
  Effect.runSync(SDK.hashHexWithBlake2b(hex, 32));

/** `blake2b_256(cbor.serialise(data))` of the Data whose CBOR is `cbor`. */
export const hashSerialisedData = (cbor: string): string =>
  blake2b256(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor));

/** `hash_one_step_evidence(transition, auxiliary)`. */
export const hashOneStepEvidence = (
  transitionCbor: string,
  auxiliaryCbor: string,
): string =>
  blake2b256(
    ONE_STEP_EVIDENCE_DOMAIN +
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        `9f${transitionCbor}${auxiliaryCbor}ff`,
      ),
  );

/**
 * `core_group(mode, witness)` (cek-semantic-chain): the arm chain a core
 * step of `mode` with the witness of constructor `arm` runs, or undefined
 * for a pair the chain refuses.
 */
export const cekCoreGroup = (
  mode: bigint,
  arm: number,
  builtinTag?: bigint,
): number | undefined => {
  switch (mode) {
    case MODE.compute:
      return (arm >= 0 && arm <= 10) || arm === 40 ? 0 : undefined;
    case MODE.lookup:
      return arm === 11 || arm === 12 ? 1 : undefined;
    case MODE.return:
      return arm === 41 ? 11 : arm >= 13 && arm <= 27 ? 1 : undefined;
    case MODE.caseSelect:
      return arm === 28 ? 1 : undefined;
    case MODE.caseApply:
      return arm === 29 ? 1 : undefined;
    case MODE.semanticBuiltin:
      return arm >= 33 && arm <= 35 ? 2 : undefined;
    case MODE.builtin: {
      if (arm === 30)
        return builtinTag === undefined
          ? undefined
          : builtinTag >= 29n &&
              builtinTag <= 69n &&
              builtinTag !== 52n &&
              builtinTag !== 53n
            ? 4
            : 3;
      const groups: Readonly<Record<number, number>> = {
        31: 5,
        32: 6,
        36: 7,
        37: 8,
        38: 9,
        39: 10,
      };
      return groups[arm];
    }
    default:
      return undefined;
  }
};

/** What every step's one-step resolution carries. */
type ValidationTraceOneStepArgument = Readonly<{
  /** The agreed state's index: the step is `stateIndex` to `stateIndex + 1`. */
  stateIndex: number;
  preState: MidgardValidationMachineState;
  resolverIndex: number;
  /** The semantic resolver of the CEK group the step selects. */
  semanticResolverIndex: number;
  /** The transition and its auxiliary evidence, as their Data's CBOR. */
  transitionCbor: string;
  auxiliaryCbor: string;
  /** `hash_one_step_evidence(transition, auxiliary)`. */
  evidenceHash: string;
}>;

/** A CEK core step's resolution, as the runner drives it. */
export type ValidationTraceCoreStepPlan = ValidationTraceOneStepArgument &
  Readonly<{
    kind: "core";
    /** The CEK core step (the auxiliary's one field) and its arm's witness. */
    core: Readonly<{
      stepCbor: string;
      preCbor: string;
      postCbor: string;
      armWitnessCbor: string;
      arm: number;
      group: SingleHopCoreGroup;
    }>;
  }>;

/** A CEK script selection's resolution, as the runner drives it. */
export type ValidationTraceSelectionPlan = ValidationTraceOneStepArgument &
  Readonly<{
    kind: "selection";
    /** The selected script, whose decode record the selection reads;
     * undefined for a selection that reads none. */
    script:
      | Readonly<{ scriptHashHex: string; languageTag: 3 | 128; item: Buffer }>
      | undefined;
  }>;

/** A CEK context step's resolution, as the runner drives it. */
export type ValidationTraceContextStepPlan = ValidationTraceOneStepArgument &
  Readonly<{ kind: "context"; context: ValidationTraceContextStep }>;

/** A step's one-step resolution, as the runner drives it. */
export type ValidationTraceOneStepPlan =
  | ValidationTraceCoreStepPlan
  | ValidationTraceSelectionPlan
  | ValidationTraceContextStepPlan;

/** What the deployment lets the runner drive beyond the CEK core steps. */
export type ValidationTraceOneStepDrives = Readonly<{
  /** Whether the deployment records scripts' decodes, which a CEK script
   * selection reads. */
  decodeRecords?: boolean;
  /** Whether the deployment installs the CEK context resolution. */
  contexts?: boolean;
}>;

export type ValidationTraceOneStepPlanning =
  | Readonly<{ kind: "plan"; plan: ValidationTraceOneStepPlan }>
  | Readonly<{ kind: "unsupported"; reason: string }>;

const constrOf = (cbor: string): Constr<Data> => {
  const data = Data.from(cbor);
  if (!(data instanceof Constr))
    throw new Error("validation-trace one-step evidence is not a constructor");
  return data;
};

/** Each state's index in our trace, by the state's hash. */
const stateIndices = new WeakMap<
  ValidationTraceClaimMaterial,
  Promise<ReadonlyMap<string, number>>
>();

const indexOfState = async (
  material: ValidationTraceClaimMaterial,
  stateHash: string,
): Promise<number | undefined> => {
  let indices = stateIndices.get(material);
  if (indices === undefined) {
    indices = material
      .trace()
      .then(
        ({ states }) =>
          new Map(
            states.map((state, index) => [
              hashMidgardValidationMachineState(state).toString("hex"),
              index,
            ]),
          ),
      );
    stateIndices.set(material, indices);
  }
  return (await indices).get(stateHash);
};

/**
 * The one-step resolution of the step of our trace (in `material`) to our
 * successor hashing to `challengerHighHash`, from the agreed state hashing
 * to `agreedLowHash` when given (the step from `stateIndex` when given, else
 * the step to the state of ours with that hash); or why the runner does not
 * drive it. Throws when our trace holds no such step.
 */
export const planValidationTraceOneStep = async ({
  material,
  stateIndex: given,
  agreedLowHash,
  challengerHighHash,
  decodeRecords = false,
  contexts = false,
}: ValidationTraceOneStepDrives & {
  readonly material: ValidationTraceClaimMaterial;
  readonly stateIndex?: number;
  readonly agreedLowHash?: string;
  readonly challengerHighHash: string;
}): Promise<ValidationTraceOneStepPlanning> => {
  const trace = await material.trace();
  const stateIndex =
    given ?? ((await indexOfState(material, challengerHighHash)) ?? 0) - 1;
  const preState = trace.states[stateIndex];
  const successor = trace.states[stateIndex + 1];
  if (
    preState === undefined ||
    successor === undefined ||
    (agreedLowHash !== undefined &&
      hashMidgardValidationMachineState(preState).toString("hex") !==
        agreedLowHash) ||
    hashMidgardValidationMachineState(successor).toString("hex") !==
      challengerHighHash
  )
    throw new Error(
      `validation-trace thread's step ${stateIndex.toString()} is not the step of our trace`,
    );
  const unsupported = (what: string): ValidationTraceOneStepPlanning => ({
    kind: "unsupported",
    reason: `the step from state ${stateIndex.toString()} is ${what}, whose resolution the runner does not drive`,
  });
  if (preState.phase !== "cek") return unsupported(`a ${preState.phase} step`);
  // A selection whose successor rests on the selected script's decode record
  // is planned over the record our own decode of the script ends in: the
  // record never enters the evidence, and the move that reads it reads the
  // authenticated one the deployment holds.
  const selection = recordedSelectionOf(trace.witnesses[stateIndex]);
  let script: ValidationTraceSelectionPlan["script"];
  let resolveCekDecodeRecord: CekSelectionDecodeRecordResolver | undefined;
  if (selection !== undefined) {
    if (!decodeRecords) throw new CekDecodeRecordRequiredError();
    const item = material.scriptItem(selection);
    const decoded = plutusItemScriptDecode(item);
    if (decoded?.record.k !== "decoded")
      return unsupported(
        "a CEK script selection of a script whose decode refuses it",
      );
    const { record } = decoded;
    script = Object.freeze({
      scriptHashHex: decoded.scriptHashHex,
      languageTag: selection.languageTag as 3 | 128,
      item,
    });
    resolveCekDecodeRecord = ({ languageTag }) => ({
      scriptHash: record.scriptHash,
      languageTag,
      programVersion: record.programVersion as -1 | 0 | 1,
      termRoot: record.termRoot,
      phase2Class: record.phase2Class,
    });
  }
  const argument = buildValidationOneStepArgument({
    trace,
    stateIndex,
    ...(resolveCekDecodeRecord === undefined ? {} : { resolveCekDecodeRecord }),
  });
  const transitionCbor = argument.transitionCbor.toString("hex");
  const auxiliaryCbor = argument.auxiliaryCbor.toString("hex");
  const common = {
    stateIndex,
    preState,
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex: argument.semanticResolverIndex,
    transitionCbor,
    auxiliaryCbor,
    evidenceHash: hashOneStepEvidence(transitionCbor, auxiliaryCbor),
  };
  if (
    argument.resolverIndex === CEK_RESOLVER_INDEX &&
    argument.semanticResolverIndex === CEK_SELECTION_SEMANTIC_RESOLVER_INDEX
  )
    return {
      kind: "plan",
      plan: Object.freeze({ ...common, kind: "selection" as const, script }),
    };
  if (
    argument.resolverIndex === CEK_RESOLVER_INDEX &&
    argument.semanticResolverIndex === CEK_CONTEXT_SEMANTIC_RESOLVER_INDEX
  ) {
    if (!contexts)
      return unsupported(
        "a CEK context step, whose resolution the deployment does not install",
      );
    const context = planValidationTraceContextStep({
      witnessCbor: trace.witnesses[stateIndex]!.cbor.toString("hex"),
      auxiliaryCbor,
      successorWitnessCbor:
        trace.witnesses[stateIndex + 1]!.cbor.toString("hex"),
    });
    if ("driven" in context)
      return unsupported(`a CEK context step at its ${context.stage} stage`);
    return {
      kind: "plan",
      plan: Object.freeze({ ...common, kind: "context" as const, context }),
    };
  }
  if (
    argument.resolverIndex !== CEK_RESOLVER_INDEX ||
    argument.semanticResolverIndex !== CEK_CORE_SEMANTIC_RESOLVER_INDEX
  )
    return unsupported(
      `a CEK step for semantic resolver ${argument.semanticResolverIndex.toString()}`,
    );
  const stepCbor = plutusConstrFieldCbor(auxiliaryCbor, [0]);
  const preCbor = plutusConstrFieldCbor(stepCbor, [0]);
  const armWitnessCbor = plutusConstrFieldCbor(stepCbor, [2]);
  const witness = constrOf(armWitnessCbor);
  const arm = Number(witness.index);
  const mode = constrOf(preCbor).fields[0];
  const tag = witness.fields[0];
  const group = cekCoreGroup(
    typeof mode === "bigint" ? mode : -1n,
    arm,
    typeof tag === "bigint" ? tag : undefined,
  );
  if (group === undefined || !(group in SINGLE_HOP_CORE_ARMS))
    return unsupported(
      `a CEK core step of arm ${arm.toString()}, which settles after more than one hop`,
    );
  return {
    kind: "plan",
    plan: Object.freeze({
      ...common,
      kind: "core" as const,
      core: Object.freeze({
        stepCbor,
        preCbor,
        postCbor: plutusConstrFieldCbor(stepCbor, [1]),
        armWitnessCbor,
        arm,
        group: group as SingleHopCoreGroup,
      }),
    }),
  };
};

/**
 * The one-step resolution of `thread`, ReadyForOneStep on a trace of ours,
 * planned from our trace in `material`; a step our trace does not hold is
 * reported, never driven.
 */
export const planValidationTraceThread = async (
  material: ValidationTraceClaimMaterial,
  thread: SDK.CaseThread,
  drives: ValidationTraceOneStepDrives = {},
): Promise<ValidationTraceOneStepPlanning> => {
  try {
    return await planValidationTraceOneStep({
      material,
      stateIndex: Number(thread.low_index),
      agreedLowHash: thread.agreed_low_hash,
      challengerHighHash: thread.challenger_high_hash,
      ...drives,
    });
  } catch (cause) {
    return {
      kind: "unsupported",
      reason: cause instanceof Error ? cause.message : String(cause),
    };
  }
};

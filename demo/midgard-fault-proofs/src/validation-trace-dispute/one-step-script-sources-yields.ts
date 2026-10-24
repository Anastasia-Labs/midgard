/** The evidence of the script-sources steps whose semantic resolver hands
 * the step's work to a yield it invokes beside the step, whose
 * reference-input index the semantic resolver's redeemer carries last: a
 * non-output step, verified by the middle yield of its role
 * (`middle_role`), and a redeemer descriptor step at stage ten or twelve,
 * whose descriptor advance the redeemer item-step yield verifies; and the
 * redeemer scan begin the stage ten and twelve semantic resolvers verify
 * alone. */
import { advanceMidgardRedeemerItemProof } from "@al-ft/midgard-core";
import {
  asArray,
  asBigInt,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec/cbor";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { ValidationOneStepWitness } from "@al-ft/midgard-sdk";
import { redeemerItemControlData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
} from "../redeemer-item-data.js";
import { constr, integer } from "./one-step-data.js";
import type {
  ValidationTracePhaseItemYield,
  ValidationTracePhaseYield,
} from "./one-step-phase-yields.js";

/** The evidence a script-sources yield step's semantic resolver takes
 * beside the transition: the fields its action carries after it, the
 * action's constructor, and the yield it invokes, if any; or why the
 * runner does not drive the step; undefined for a step the runner does not
 * yet drive. */
export type ScriptSourcesYieldEvidence =
  | Readonly<{
      evidenceCbors: readonly string[];
      action: number;
      itemYield?: ValidationTracePhaseItemYield;
    }>
  | Readonly<{ reason: string }>
  | undefined;

type EvidenceSource = Readonly<{
  transitionCbor: string;
  auxiliaryCbor: string;
}>;

/** `Inline`'s constructor among the field carriages. */
const INLINE_CARRIAGE = 0;

/** Each non-output step, by its script-sources stage and its auxiliary
 * evidence's constructor: its role (`middle_role`) and that role's yield. */
const MIDDLE_ROLES: Readonly<
  Record<string, readonly [number, ValidationTracePhaseYield]>
> = Object.freeze({
  "2:0": [0, "scriptSourcesStageTwoAdvance"],
  "3:7": [1, "scriptSourcesStageThreeReplay"],
  "3:0": [2, "scriptSourcesStageThreeFinish"],
  "4:27": [3, "scriptSourcesStageFourBegin"],
  "4:0": [4, "scriptSourcesStageFourFinish"],
  "6:1": [5, "scriptSourcesStageSixBeginPolicy"],
  "6:35": [6, "scriptSourcesStageSixFoldAsset"],
  "6:0": [7, "scriptSourcesStageSixFinish"],
});

/** The field carriage of the evidence of each role that reads a field's
 * item through it, by role: the stage-four begin evidence's only field, and
 * the stage-six begin-policy evidence's third. */
const ROLE_CARRIAGES: Readonly<Partial<Record<number, number>>> = Object.freeze(
  { 3: 0, 5: 2 },
);

const auxiliaryOf = (auxiliaryCbor: string): Constr<Data> => {
  const auxiliary = Data.from(auxiliaryCbor);
  if (!(auxiliary instanceof Constr))
    throw new Error("validation-trace auxiliary evidence is not a constructor");
  return auxiliary;
};

/** The script-sources stage of the work witness the transition carries. */
const stageOf = (transitionCbor: string): number => {
  const { work_witness_cbor: work } = Data.from(
    transitionCbor,
    ValidationOneStepWitness,
  );
  const control = asArray(
    decodeSingleCbor(Buffer.from(work, "hex")),
    "script_sources_control",
  );
  return Number(asBigInt(control[9], "script_sources_control.stage"));
};

/**
 * A non-output step's evidence (`VerifyNonOutput`): the auxiliary witness
 * whole and its role, beside which the semantic resolver invokes that
 * role's middle yield.
 */
export const scriptSourcesNonOutputEvidence = ({
  transitionCbor,
  auxiliaryCbor,
}: EvidenceSource): ScriptSourcesYieldEvidence => {
  const auxiliary = auxiliaryOf(auxiliaryCbor);
  const role =
    MIDDLE_ROLES[`${stageOf(transitionCbor).toString()}:${auxiliary.index}`];
  if (role === undefined) return undefined;
  const [index, name] = role;
  const carriageField = ROLE_CARRIAGES[index];
  if (carriageField !== undefined) {
    const carriage = auxiliary.fields[carriageField];
    if (
      !(carriage instanceof Constr) ||
      Number(carriage.index) !== INLINE_CARRIAGE
    )
      return {
        reason: "a scriptSources item of a field delivered by reference",
      };
  }
  return Object.freeze({
    evidenceCbors: Object.freeze([auxiliaryCbor, integer(index)]),
    action: 0,
    itemYield: Object.freeze({ name }),
  });
};

/** `RedeemerScanBeginWitness`'s constructor among the auxiliary
 * witnesses; the other evidence these semantic resolvers take is
 * `RedeemerItemStepWitness`. */
const REDEEMER_SCAN_BEGIN = 10;

/** The fields of a redeemer item control but its traversal, as their CBOR
 * (`narrow_control`). */
const NARROW_CONTROL_FIELDS = 15;

const narrowControl = (fieldCbor: (index: number) => string) =>
  constr(
    0,
    Array.from({ length: NARROW_CONTROL_FIELDS }, (_, index) =>
      fieldCbor(index),
    ),
  );

/**
 * A redeemer descriptor step's evidence at stage ten or twelve: for a scan
 * begin (`VerifyRedeemerScanBegin`), the begin witness's fields; for a
 * descriptor step (`VerifyRedeemerItemStep`), the descriptor step claim the
 * redeemer item-step yield verifies, with the control the step advances to
 * derived again from the step's control and witness. A step that advances
 * to none is not driven.
 */
export const scriptSourcesDescriptorEvidence = ({
  auxiliaryCbor,
}: EvidenceSource): ScriptSourcesYieldEvidence => {
  const auxiliary = auxiliaryOf(auxiliaryCbor);
  const field = (...path: number[]) =>
    plutusConstrFieldCbor(auxiliaryCbor, path);
  if (auxiliary.index === REDEEMER_SCAN_BEGIN)
    return Object.freeze({
      evidenceCbors: Object.freeze(
        auxiliary.fields.map((_, index) => field(index)),
      ),
      action: 1,
    });
  const witness = decodeRedeemerItemWitnessData(auxiliary.fields[2]);
  const next = advanceMidgardRedeemerItemProof({
    control: decodeRedeemerItemControlData(auxiliary.fields[1]),
    witness,
  });
  if (next === null) return undefined;
  const claimedNext = Data.to<unknown>(redeemerItemControlData(next));
  return Object.freeze({
    evidenceCbors: Object.freeze([
      constr(0, [
        narrowControl((index) => field(1, index)),
        constr(witness.action.kind === "openTail" ? 1 : 0, []),
        field(2, 1, 0),
        field(2, 2),
        narrowControl((index) => plutusConstrFieldCbor(claimedNext, [index])),
      ]),
    ]),
    action: 0,
    itemYield: Object.freeze({ name: "scriptSourcesRedeemerDescriptor" }),
  });
};

/** The script-sources semantic resolvers whose evidence is derived here,
 * with every yield each may invoke. */
export const SCRIPT_SOURCES_YIELD_SEMANTICS = Object.freeze({
  scriptSourcesNonOutputSemantic: {
    evidence: scriptSourcesNonOutputEvidence,
    yields: Object.values(MIDDLE_ROLES).map(([, name]) => name),
  },
  scriptSourcesStageTwelveRedeemerSemantic: {
    evidence: scriptSourcesDescriptorEvidence,
    yields: ["scriptSourcesRedeemerDescriptor"],
  },
  scriptSourcesStageTenMismatchSemantic: {
    evidence: scriptSourcesDescriptorEvidence,
    yields: ["scriptSourcesRedeemerDescriptor"],
  },
  scriptSourcesStageTenMatchSemantic: {
    evidence: scriptSourcesDescriptorEvidence,
    yields: ["scriptSourcesRedeemerDescriptor"],
  },
} as const satisfies Readonly<
  Record<
    string,
    Readonly<{
      evidence: (source: EvidenceSource) => ScriptSourcesYieldEvidence;
      yields: readonly ValidationTracePhaseYield[];
    }>
  >
>);

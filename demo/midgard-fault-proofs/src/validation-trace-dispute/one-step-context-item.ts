/** The redeemer item step of a CEK context step our trace routes to the item
 * bind stage, as our trace plans it: the item control and witness the
 * step's auxiliary evidence carries, the control the canonical advance
 * claims next, and where the item return routes the verified step, with
 * the result the hash or finalize on that route derives. A selection scan's
 * step (context stage 0) continues or finishes the scan; a data scan's step
 * (context stage 9) continues, finishes a descriptor or finishes a value. */
import {
  advanceMidgardRedeemerItemProof,
  computeHash32,
  finalizeMidgardCekDataTraverse,
  hashMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { redeemerItemControlData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
} from "../redeemer-item-data.js";
import { CONTEXT_ITEM_ROUTES } from "./one-step-context-item-holders.js";

export type ValidationTraceContextItemRoute =
  (typeof CONTEXT_ITEM_ROUTES)[number];

/** A context step's redeemer item step, each field as Data CBOR. */
export type ValidationTraceContextItem = Readonly<{
  /** `RedeemerItemProofControlV1` the evidence carries. */
  controlCbor: string;
  /** `RedeemerItemProofWitnessV1` the evidence carries. */
  witnessCbor: string;
  /** `blake2b_256` of the witness's serialised Data. */
  witnessHash: string;
  /** The control the canonical advance of the step claims next. */
  claimedNextCbor: string;
  route: ValidationTraceContextItemRoute;
  /**
   * What the route's hash or finalize derives from the next control: its
   * hash on a continuing route, its value's summary on a finishing value;
   * undefined on a route through neither.
   */
  resultCbor: string | undefined;
}>;

/** `ItemAuxiliary` of a context step: constructor 18 of three fields. */
const ITEM_AUXILIARY = 18;

/** `blake2b_256(cbor.serialise(data))` of `data`. */
export const hashItemData = (data: Data): string =>
  computeHash32(Buffer.from(Data.to(data), "hex")).toString("hex");

/**
 * The redeemer item step whose evidence is `auxiliaryCbor`, at the context
 * stage `contextStage` (0, a selection scan, or 9, a data scan).
 */
export const planValidationTraceContextItem = ({
  auxiliaryCbor,
  contextStage,
}: {
  readonly auxiliaryCbor: string;
  readonly contextStage: bigint;
}): ValidationTraceContextItem => {
  const auxiliary = Data.from(auxiliaryCbor);
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== ITEM_AUXILIARY ||
    auxiliary.fields.length !== 3
  )
    throw new Error(
      "validation-trace CEK context item step's evidence is not an item auxiliary",
    );
  const witness = auxiliary.fields[2]!;
  const control = decodeRedeemerItemControlData(auxiliary.fields[1]);
  const next = advanceMidgardRedeemerItemProof({
    control,
    witness: decodeRedeemerItemWitnessData(witness),
  });
  if (next === null)
    throw new Error(
      "validation-trace CEK context item step's witness does not advance its control",
    );
  const terminal = next.stage === MidgardRedeemerItemProofStages.Terminal;
  const route: ValidationTraceContextItemRoute =
    contextStage === 0n
      ? terminal
        ? "contextItemSelectionFinish"
        : "contextItemSelectionHash"
      : !terminal
        ? "contextItemDataHash"
        : next.mode === MidgardRedeemerItemProofModes.Descriptor
          ? "contextItemDataFinishDescriptor"
          : "contextItemFinalize";
  let resultCbor: string | undefined;
  if (route === "contextItemSelectionHash" || route === "contextItemDataHash")
    resultCbor = Data.to(
      hashMidgardRedeemerItemProofControl(next).toString("hex"),
    );
  if (route === "contextItemFinalize") {
    const summary =
      next.traversal === null
        ? null
        : finalizeMidgardCekDataTraverse(next.traversal);
    if (summary === null)
      throw new Error(
        "validation-trace CEK context item step finishes a value with no summary",
      );
    resultCbor = Data.to(
      new Constr(0, [
        Buffer.from(summary.root).toString("hex"),
        summary.cborLength,
        summary.memory,
      ]),
    );
  }
  return Object.freeze({
    controlCbor: plutusConstrFieldCbor(auxiliaryCbor, [1]),
    witnessCbor: plutusConstrFieldCbor(auxiliaryCbor, [2]),
    witnessHash: hashItemData(witness),
    claimedNextCbor: Data.to<unknown>(redeemerItemControlData(next)),
    route,
    resultCbor,
  });
};

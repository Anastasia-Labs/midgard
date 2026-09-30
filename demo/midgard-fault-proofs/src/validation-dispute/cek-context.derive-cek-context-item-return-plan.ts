import {
  advanceMidgardRedeemerItemProof,
  finalizeMidgardRedeemerItemProof,
  hashMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { redeemerItemControlData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
} from "../redeemer-item-data.js";

export type CekContextStageKey = keyof SDK.CekContextStages;

/** Retained from adjacent fresh canonical replay, never from operator authority. */
export type CekContextSuccessorEvidence = {
  readonly successorWorkWitnessCbor: string;
};

export const fields = (value: Data, tag: number, count: number): Data[] => {
  if (
    !(value instanceof Constr) ||
    value.index !== tag ||
    value.fields.length !== count
  )
    throw new Error(`Expected exact CEK context constructor ${tag}/${count}`);
  return value.fields;
};

export const integer = (value: Data): bigint => {
  if (typeof value !== "bigint")
    throw new Error("Expected CEK context integer");
  return value;
};

export const cborArray = SDK.decodeCekContextCborArray;

export const record = (values: Data[]) => new Constr(0, values);

/** Shared-machine handoff and return states for an exact canonical item advance. */
export const deriveCekContextItemReturnPlan = ({
  staged,
  auxiliary,
  verifiedContext,
}: {
  readonly staged: Data;
  readonly auxiliary: Data;
  readonly verifiedContext: Data;
}) => {
  const stagedFields = fields(staged, 0, 4);
  const context = fields(stagedFields[1]!, 0, 25);
  const raw = fields(auxiliary, 18, 3);
  const control = decodeRedeemerItemControlData(raw[1]!);
  const next = advanceMidgardRedeemerItemProof({
    control,
    witness: decodeRedeemerItemWitnessData(raw[2]!),
  });
  if (next === null)
    throw new Error("CEK context item witness does not advance");
  const claimedNext = redeemerItemControlData(next) as Data;
  const pending = record([
    staged,
    raw[1]!,
    SDK.hashCekCoreWitness(raw[2]!),
    claimedNext,
  ]);
  const verified = record([staged, claimedNext]);
  const route: CekContextStageKey[] = ["itemReturn"];
  const states: Data[] = [];
  const terminal = next.stage === MidgardRedeemerItemProofStages.Terminal;
  const selection = integer(context[0]!) === 0n;
  if (!selection && integer(context[0]!) !== 9n)
    throw new Error("CEK context item has invalid context stage");
  if (!terminal) {
    const key = selection ? "itemSelectionHash" : "itemDataHash";
    route.push(key);
    states.push(verified);
    states.push(
      record([
        verified,
        hashMidgardRedeemerItemProofControl(next).toString("hex"),
      ]),
    );
    route.push(selection ? "itemSelectionContinue" : "itemDataContinue");
  } else if (selection) {
    route.push("itemSelectionFinish");
    states.push(verified);
  } else if (next.mode === MidgardRedeemerItemProofModes.Descriptor) {
    route.push("itemDataFinishDescriptor");
    states.push(verified);
  } else {
    const summary = finalizeMidgardRedeemerItemProof(next);
    if (summary === null)
      throw new Error("CEK terminal data item has no canonical summary");
    route.push("itemFinalize");
    states.push(verified);
    route.push("itemDataFinishValue");
    states.push(
      record([
        verified,
        record([
          Buffer.from(summary.root).toString("hex"),
          summary.cborLength,
          summary.memory,
        ]),
      ]),
    );
  }
  states.push(verifiedContext);
  return { pending, claimedNext, witness: raw[2]!, verified, route, states };
};

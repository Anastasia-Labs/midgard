import { advanceMidgardRedeemerItemProof } from "@al-ft/midgard-core";
import { redeemerItemControlData } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
} from "../redeemer-item-data.js";

export const scriptSourcesDescriptorClaim = (
  auxiliary: Constr<unknown>,
): Constr<Data> => {
  if (auxiliary.index !== 18 || auxiliary.fields.length !== 3)
    throw new Error("Descriptor step requires exact redeemer-item evidence");
  const absent = auxiliary.fields[0];
  if (
    !(absent instanceof Constr) ||
    absent.index !== 1 ||
    absent.fields.length !== 0
  )
    throw new Error("Descriptor scan cannot carry a data-mode continuation");
  const current = decodeRedeemerItemControlData(auxiliary.fields[1]);
  const witness = decodeRedeemerItemWitnessData(auxiliary.fields[2]);
  if (
    current.mode !== 0 ||
    current.traversal !== null ||
    (witness.action.kind !== "openHeader" &&
      witness.action.kind !== "openTail") ||
    witness.chunkProof === null
  )
    throw new Error("Descriptor scan requires a header or tail opening");
  const next = advanceMidgardRedeemerItemProof({ control: current, witness });
  if (next === null)
    throw new Error("Descriptor witness does not authenticate an advance");
  const narrow = (control: typeof current): Constr<Data> => {
    const value = Data.from(Data.to<unknown>(redeemerItemControlData(control)));
    if (!(value instanceof Constr) || value.fields.length !== 16)
      throw new Error("Malformed descriptor control");
    return new Constr(0, value.fields.slice(0, 15));
  };
  const rawWitness = auxiliary.fields[2];
  if (!(rawWitness instanceof Constr) || rawWitness.fields.length !== 3)
    throw new Error("Malformed descriptor witness");
  const proof = rawWitness.fields[1];
  if (
    !(proof instanceof Constr) ||
    proof.index !== 0 ||
    proof.fields.length !== 1
  )
    throw new Error("Descriptor step is missing its chunk proof");
  const claim = Data.from(
    Data.to<unknown>(
      new Constr(0, [
        narrow(current),
        new Constr(witness.action.kind === "openTail" ? 1 : 0, []),
        proof.fields[0],
        rawWitness.fields[2],
        narrow(next),
      ]),
    ),
  );
  if (!(claim instanceof Constr)) throw new Error("Malformed descriptor claim");
  return claim;
};

/** The Data CBOR the one-step hops write: their datums and redeemers are
 * assembled from the CBOR of their fields, which keeps each field exactly as
 * our trace or the thread's datum encodes it. */
import { Data } from "@lucid-evolution/lucid";

export const integer = (value: bigint | number) => Data.to(BigInt(value));

export const bytes = (hex: string) => Data.to(hex);

/** A constructor's CBOR, index 0 to 6, from its fields' CBOR. */
export const constr = (index: number, fields: readonly string[]) =>
  `d8${(0x79 + index).toString(16)}${
    fields.length === 0 ? "80" : `9f${fields.join("")}ff`
  }`;

/** `ct.StepDatum { fraud_prover, data: Some(state) }`. */
export const stepDatum = (prover: string, stateCbor: string) =>
  constr(0, [bytes(prover), constr(0, [stateCbor])]);

/** `ct.Continue(action)`. */
export const continued = (actionCbor: string) => constr(1, [actionCbor]);

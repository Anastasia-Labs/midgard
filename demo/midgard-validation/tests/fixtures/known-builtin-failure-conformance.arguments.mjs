/**
 * Argument builders for the known builtin failure conformance corpus
 * (`known-builtin-failure-conformance.cases.mjs`). Each builder returns a
 * direct value witness exactly as the direct failure arm receives it:
 *
 *   * `{ kind: "constant", witness: { typeCbor, payloadCbor } }`, a revealed
 *     constant, encoded by the package's own constant encoder (or, through
 *     `retyped`, a payload under another constant's type);
 *   * `{ kind: "semanticConstant", witness }`, a compacted constant: a type,
 *     a payload summary and a memory figure;
 *   * `{ kind: "opaque", root }` and `{ kind: "blsMillerLoop", expressionRoot }`.
 *
 * From the package this module imports only the `uplc` and `cek-constant`
 * entries, which resolve to `src/` under vitest and to `dist/` on bare
 * `node`, so the generator and the vitest build the same witnesses.
 */

import { midgardCekConstantWitnessFromUplc } from "@al-ft/midgard-validation/cek-constant";
import {
  constT,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  UPLCConst,
} from "@al-ft/midgard-validation/uplc";

export { DataB, DataConstr, DataI, DataList, DataMap };

export const INT64_MIN = -(2n ** 63n);
export const INT64_MAX = 2n ** 63n - 1n;
export const SECP256K1_GROUP_ORDER =
  115792089237316195423570985008687907852837564279074904382605163141518161494337n;
export const SECP256K1_FIELD_PRIME =
  115792089237316195423570985008687907853269984665640564039457584007908834671663n;
export const BLS12_381_FIELD_PRIME =
  4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787n;
/** The x coordinate of the secp256k1 generator. */
export const SECP256K1_GENERATOR_X =
  0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798n;

const constant = (uplc) => ({
  kind: "constant",
  witness: midgardCekConstantWitnessFromUplc(uplc),
});

export const int = (value) => constant(UPLCConst.int(BigInt(value)));
export const bytes = (values) =>
  constant(UPLCConst.byteString(Uint8Array.from(values)));
/** `length` bytes, each `fill`. */
export const filled = (length, fill = 0) =>
  bytes(new Uint8Array(length).fill(fill));
export const str = (text) => constant(UPLCConst.str(text));
export const bool = (value) => constant(UPLCConst.bool(value));
export const unit = constant(UPLCConst.unit);
export const data = (value) => constant(UPLCConst.data(value));
export const intList = (values) =>
  constant(UPLCConst.listOf(constT.int)(values.map(BigInt)));
export const dataList = (values) =>
  constant(UPLCConst.listOf(constT.data)(values));
export const bytesList = (values) =>
  constant(
    UPLCConst.listOf(constT.byteStr)(values.map((v) => Uint8Array.from(v))),
  );

/** `payloadOf`'s payload under `typeOf`'s type: a constant whose payload does not match its type. */
export const retyped = (typeOf, payloadOf) => ({
  kind: "constant",
  witness: {
    typeCbor: typeOf.witness.typeCbor,
    payloadCbor: payloadOf.witness.payloadCbor,
  },
});

/** A compacted constant of `typeOf`'s type with `memory` words. */
export const semantic = (typeOf, memory) => ({
  kind: "semanticConstant",
  witness: {
    typeCbor: typeOf.witness.typeCbor,
    payload: {
      root: new Uint8Array(32).fill(7),
      cborLength: 3n,
      memory: 1n,
    },
    memory: BigInt(memory),
  },
});

export const opaque = { kind: "opaque", root: new Uint8Array(32).fill(9) };
export const millerLoop = {
  kind: "blsMillerLoop",
  expressionRoot: new Uint8Array(32).fill(5),
};

/** `value` as `length` big-endian bytes. */
export const bigEndian = (value, length) => {
  const out = new Uint8Array(length);
  let rest = value;
  for (let index = length - 1; index >= 0; index -= 1) {
    out[index] = Number(rest & 0xffn);
    rest >>= 8n;
  }
  return out;
};

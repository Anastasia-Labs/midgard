/**
 * Cardano's builtins on constant values, evaluated by the pinned evaluator's
 * `eval_builtin_raw` (@lucid-evolution/uplc) exactly as a Plutus V3 script at
 * protocol version 11 evaluates one saturated application: availability,
 * then the arguments' types, then the cost, then the call.
 *
 * Values cross as Flat `(program 1.1.0 (con …))` constants, written and read
 * by Midgard's own Flat codec. Flat has no BLS12-381 points, so a G1 or G2
 * value is held as its compressed point bytes, which is how it crosses; a
 * list of points crosses as their bytes one after the other.
 */
import * as UPLC from "@lucid-evolution/uplc";

import { midgardCekBuiltinArgumentCount } from "./cek-machine.midgard-cek-builtin-argument-count.js";
import { decodeCardanoFlatProgramOrThrow } from "./cek-program.cardano-flat.decode.js";
import { encodeCardanoFlatProgram } from "./cek-program.cardano-flat.encode.js";
import { constT, ConstTyTag, UPLCConst } from "./uplc-constant.js";

/** The protocol version whose builtin semantics Midgard follows. */
const PROTOCOL_MAJOR_VERSION = 11;

/** `BuiltinValueKind` of `eval_builtin_raw`. */
const KIND = Object.freeze({
  flat: 0,
  g1: 1,
  g2: 2,
  g1List: 3,
  g2List: 4,
} as const);

const G1_BYTES = 48;
const G2_BYTES = 96;

/** How a point type crosses, alone and as a list's element. */
const POINTS = new Map<number, { alone: number; list: number; size: number }>([
  [
    ConstTyTag.bls12_381_G1_element,
    { alone: KIND.g1, list: KIND.g1List, size: G1_BYTES },
  ],
  [
    ConstTyTag.bls12_381_G2_element,
    { alone: KIND.g2, list: KIND.g2List, size: G2_BYTES },
  ],
]);

const pointBytes = (value: unknown, size: number): Uint8Array => {
  if (!(value instanceof Uint8Array) || value.length !== size)
    throw new Error("V1 BLS value is not a compressed point");
  return value;
};

/** An argument's bytes and kind. */
const encodeArgument = (
  constant: UPLCConst,
): { readonly bytes: Uint8Array; readonly kind: number } => {
  const [head, element] = constant.type;
  const point = POINTS.get(head);
  if (point !== undefined)
    return { bytes: pointBytes(constant.value, point.size), kind: point.alone };
  const listed = head === ConstTyTag.list ? POINTS.get(element!) : undefined;
  if (listed !== undefined && constant.type.length === 2) {
    const points = (constant.value as readonly unknown[]).map((item) =>
      pointBytes(item, listed.size),
    );
    return { bytes: Buffer.concat(points), kind: listed.list };
  }
  return {
    bytes: encodeCardanoFlatProgram({
      version: { major: 1, minor: 1, patch: 0 },
      body: { tag: 4, type: constant.type, value: constant.value },
    }),
    kind: KIND.flat,
  };
};

const splitPoints = (bytes: Uint8Array, size: number): Uint8Array[] => {
  if (bytes.length % size !== 0)
    throw new Error("pinned evaluator returned a partial point list");
  const points: Uint8Array[] = [];
  for (let offset = 0; offset < bytes.length; offset += size)
    points.push(bytes.slice(offset, offset + size));
  return points;
};

/** The constant a result's bytes and kind stand for. */
const decodeResult = (bytes: Uint8Array, kind: number): UPLCConst => {
  switch (kind) {
    case KIND.flat: {
      const { body } = decodeCardanoFlatProgramOrThrow(bytes);
      if (!(body instanceof UPLCConst))
        throw new Error("pinned evaluator returned a non-constant value");
      return body;
    }
    case KIND.g1:
      return new UPLCConst(constT.bls12_381_G1_element, bytes.slice());
    case KIND.g2:
      return new UPLCConst(constT.bls12_381_G2_element, bytes.slice());
    case KIND.g1List:
      return new UPLCConst(
        constT.listOf(constT.bls12_381_G1_element),
        splitPoints(bytes, G1_BYTES),
      );
    case KIND.g2List:
      return new UPLCConst(
        constT.listOf(constT.bls12_381_G2_element),
        splitPoints(bytes, G2_BYTES),
      );
    default:
      throw new Error(`pinned evaluator returned a value of kind ${kind}`);
  }
};

/** What the pinned evaluator gives for one saturated builtin application. */
export type MidgardCekPinnedBuiltinResult =
  | { readonly kind: "value"; readonly value: UPLCConst }
  | { readonly kind: "failure"; readonly message: string };

/**
 * Evaluates Cardano's builtin `tag` (Flat numbering) on `arguments_`, which
 * must be exactly its arguments. The Miller-loop builtins (68 to 70) are not
 * evaluated here. A builtin that fails gives "failure"; this throws instead
 * when:
 * - the input is not a builtin application: an unknown tag, a Miller-loop
 *   tag, or a wrong number of arguments;
 * - a G1 or G2 argument is not a compressed point of its group, alone or in
 *   a list of points (the evaluator refuses to uncompress it);
 * - an argument or the result holds a BLS point other than alone or in a
 *   list of points (inside a pair, say), which Flat cannot carry;
 * - the Flat decoder refuses the result, or the result is not a constant.
 */
export const evaluatePinnedBuiltin = (
  tag: number,
  arguments_: readonly UPLCConst[],
): MidgardCekPinnedBuiltinResult => {
  if (BigInt(arguments_.length) !== midgardCekBuiltinArgumentCount(BigInt(tag)))
    throw new Error("V1 builtin argument count is incomplete");
  const encoded = arguments_.map(encodeArgument);
  const result = UPLC.eval_builtin_raw(
    tag,
    encoded.map((argument) => argument.bytes),
    Uint8Array.from(encoded.map((argument) => argument.kind)),
    PROTOCOL_MAJOR_VERSION,
  );
  try {
    if (result.ok)
      return {
        kind: "value",
        value: decodeResult(result.result, result.result_kind),
      };
    return { kind: "failure", message: result.error };
  } finally {
    result.free();
  }
};

/** Cardano's builtin `tag` on `arguments_`: its value, or "failure". */
export const runPinnedBuiltin = (
  tag: number,
  arguments_: readonly UPLCConst[],
): UPLCConst | "failure" => {
  const result = evaluatePinnedBuiltin(tag, arguments_);
  return result.kind === "value" ? result.value : "failure";
};

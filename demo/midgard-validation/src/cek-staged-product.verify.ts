/**
 * multiplyInteger and the divisions over staged operands (arm 31, semantics
 * group 9): the TypeScript twin of what `cek_staged_product_v1` verifies
 * over every hop of its column loop.
 *
 * The step carries a source per argument and then the CBOR the loop compares
 * column by column: the result's for multiplyInteger, the quotient's for
 * quotientInteger and divideInteger (whose result it is), and the quotient's
 * and the result's for remainderInteger and modInteger. Both arguments are
 * bound and bounded; the quotient and the result are not bounded. A CBOR
 * source of the result is bound to the result's leaf, and the quotient of
 * remainderInteger and modInteger must be the matching division's, which
 * fixes it. A division by zero has no result, so no step.
 */
import { evaluateMidgardCekBuiltinOnValues } from "./cek-builtin.evaluate-reference-builtin.js";
import type { MidgardCekSemanticConstantWitness } from "./cek-constant.js";
import type { MidgardCekStagedOperandSource } from "./cek-staged-builtin.material.js";
import { UPLCConst } from "./uplc-constant.js";

type Source = MidgardCekStagedOperandSource;
type Value = MidgardCekSemanticConstantWitness;

/** How the staged arms read an integer source. */
export type MidgardCekStagedIntegerReader = {
  /**
   * The integer `source` holds, bound to `value`'s leaf and, when `bounded`,
   * within the integer builtins' argument range; null for a source that
   * binds the leaf without revealing the integer.
   */
  readonly bind: (
    value: Value,
    source: Source,
    bounded: boolean,
  ) => bigint | null;
  /** The integer whose canonical CBOR an inline or staged source holds. */
  readonly read: (source: Source) => bigint;
};

/**
 * The sources a staged step of builtin `tag` with `arguments_` arguments
 * carries.
 */
export const midgardCekStagedSourceCount = (
  tag: bigint,
  arguments_: number,
): number =>
  tag === 5n || tag === 6n
    ? arguments_ + 2
    : tag >= 2n && tag <= 4n
      ? arguments_ + 1
      : arguments_;

const evaluate = (tag: number, a: bigint, b: bigint): bigint | null => {
  const value = evaluateMidgardCekBuiltinOnValues(tag, [
    UPLCConst.int(a),
    UPLCConst.int(b),
  ]);
  return value === "failure" ? null : (value.value as bigint);
};

/**
 * Whether the column loop completes for builtin `tag` (2 to 6) over
 * `arguments_` with `result`, reading `extra`, the sources past the
 * arguments.
 */
export const verifyMidgardCekStagedProduct = (
  tag: bigint,
  arguments_: readonly (readonly [Value, Source])[],
  result: Value,
  extra: readonly Source[],
  reader: MidgardCekStagedIntegerReader,
): boolean => {
  const [left, right] = arguments_;
  if (arguments_.length !== 2 || tag < 2n || tag > 6n) return false;
  const a = reader.bind(left![0], left![1], true);
  const b = reader.bind(right![0], right![1], true);
  if (a === null || b === null) return false;
  const expected = evaluate(Number(tag), a, b);
  if (expected === null) return false;
  const [first, second] = extra;
  if (second === undefined)
    return reader.bind(result, first!, false) === expected;
  return (
    reader.read(first!) === evaluate(tag === 5n ? 4 : 3, a, b) &&
    reader.bind(result, second, false) === expected
  );
};

/**
 * The known builtin failure conformance corpus: the builtin applications the
 * off-chain twin `midgardCekKnownBuiltinFailure`
 * (`src/cek-builtin.known-failure.ts`) and the on-chain
 * `known_builtin_failure_v1` (`onchain/aiken/lib/midgard/cek-builtin-v1.ak`)
 * must judge alike. The off-chain twin must match the on-chain rule.
 *
 * Driven from two sides. The generator
 * (`scripts/generate-known-builtin-failure-conformance-goldens.mjs`) runs the
 * twin from the built `dist/` over every case and writes the JSON fixture and
 * the Aiken module
 * `onchain/aiken/lib/midgard/known-builtin-failure-conformance-golden.test.ak`,
 * which holds `known_builtin_failure_v1` to each verdict. The vitest
 * (`tests/known-builtin-failure-conformance.golden.test.ts`) runs the twin
 * from `src/` over the same cases and compares with the fixture.
 *
 * A verdict is `known` (the twin returns true), `not-known` (false) or
 * `refused` (the twin throws, where the Aiken predicate fails the script).
 *
 * The corpus covers every tag of the known set with every verdict, the size
 * and range limits on both sides, signs, empty and over-long inputs, wrong
 * arities, and each argument kind in each position: revealed constants
 * (well-typed, ill-typed, with a payload that does not match the type),
 * compacted constants (including the dividend memory limit), opaque values
 * and Miller-loop values.
 */

import {
  bool,
  bytes,
  bytesList,
  data,
  dataList,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  BLS12_381_G1_GENERATOR,
  BLS12_381_G2_GENERATOR,
  filled,
  g1Point,
  g2Point,
  int,
  INT64_MAX,
  INT64_MIN,
  intList,
  millerLoop,
  opaque,
  retyped,
  semantic,
  str,
  unit,
} from "./known-builtin-failure-conformance.arguments.mjs";
import { cryptoCases } from "./known-builtin-failure-conformance.crypto-cases.mjs";
import {
  PV11_KIND_SWEEP_BASES,
  PV11_KNOWN_BUILTIN_FAILURE_TAGS,
  pv11Cases,
} from "./known-builtin-failure-conformance.pv11-cases.mjs";

/** The tags `known_builtin_failure_v1` can answer true for. */
export const KNOWN_BUILTIN_FAILURE_TAGS = Object.freeze(
  [
    3, 4, 5, 6, 11, 12, 14, 21, 25, 33, 34, 42, 43, 44, 45, 46, 52, 53, 56, 59,
    60, 63, 66, 67, 73, 79, 80, 81, 82, 83,
  ].concat(PV11_KNOWN_BUILTIN_FAILURE_TAGS),
);

export const VERDICTS = Object.freeze(["known", "not-known", "refused"]);

const semanticBytes = (memory) => semantic(bytes([1]), memory);
const semanticInteger = (memory) => semantic(int(0), memory);
const integerTypeBytesPayload = retyped(int(0), bytes([1]));
const bytesTypeIntegerPayload = retyped(bytes([1]), int(1));
const booleanTypeIntegerPayload = retyped(bool(true), int(1));
const booleanTypeConstrTwo = retyped(bool(true), data(new DataConstr(2n, [])));
const undecodableType = {
  kind: "constant",
  witness: { typeCbor: Uint8Array.of(0xff), payloadCbor: Uint8Array.of(0) },
};

/** divideInteger, quotientInteger, remainderInteger, modInteger (3-6). */
const integerDivisionCases = (add) => {
  for (const tag of [3, 4, 5, 6]) {
    add(tag, "1 by 0", [int(1), int(0)]);
    add(tag, "1 by 1", [int(1), int(1)]);
    add(tag, "-5 by 0", [int(-5), int(0)]);
    add(tag, "0 by -1", [int(0), int(-1)]);
    for (const memory of [0, 1, 4095, 4096, 4097]) {
      add(tag, `compacted dividend of ${memory} words by 0`, [
        semanticInteger(memory),
        int(0),
      ]);
    }
    add(tag, "compacted byte string by 0", [semanticBytes(1), int(0)]);
    add(tag, "byte string by 0", [bytes([1]), int(0)]);
    add(tag, "1 by byte string", [int(1), bytes([0])]);
    add(tag, "1 by compacted integer", [int(1), semanticInteger(1)]);
    add(tag, "opaque by 0", [opaque, int(0)]);
    add(tag, "1 by Miller loop", [int(1), millerLoop]);
    add(tag, "one argument", [int(1)]);
    add(tag, "three arguments", [int(1), int(0), int(0)]);
    add(tag, "ill-typed payload by 0", [integerTypeBytesPayload, int(0)]);
    add(tag, "1 by ill-typed payload", [int(1), integerTypeBytesPayload]);
    add(tag, "undecodable type by 0", [undecodableType, int(0)]);
  }
};

/** consByteString (11), sliceByteString (12), indexByteString (14). */
const byteStringCases = (add) => {
  for (const value of [-1n, 0n, 255n, 256n, 2n ** 64n]) {
    add(11, `cons ${value}`, [int(value), bytes([1, 2])]);
    add(11, `cons ${value} onto compacted bytes`, [
      int(value),
      semanticBytes(5),
    ]);
    add(11, `cons ${value} onto a string`, [int(value), str("a")]);
    add(11, `cons ${value} onto an integer`, [int(value), int(1)]);
  }
  add(11, "cons bytes onto bytes", [bytes([1]), bytes([1])]);
  add(11, "cons opaque", [opaque, bytes([1])]);
  add(11, "cons 256 onto ill-typed payload", [
    int(256),
    bytesTypeIntegerPayload,
  ]);

  const int64Edges = [INT64_MIN - 1n, INT64_MIN, 0n, INT64_MAX, INT64_MAX + 1n];
  for (const start of int64Edges) {
    for (const length of [0n, INT64_MAX + 1n, INT64_MIN - 1n]) {
      add(12, `slice from ${start} length ${length}`, [
        int(start),
        int(length),
        bytes([1, 2, 3]),
      ]);
    }
    add(12, `slice from ${start} of compacted bytes`, [
      int(start),
      int(1),
      semanticBytes(9),
    ]);
    add(12, `slice from ${start} of an integer`, [int(start), int(1), int(3)]);
  }
  add(12, "slice with a byte-string length", [int(0), bytes([1]), bytes([1])]);
  add(12, "slice of empty bytes", [int(INT64_MAX + 1n), int(0), bytes([])]);
  add(12, "slice with a byte-string start", [bytes([1]), int(0), bytes([1])]);
  add(12, "slice from 2^63 with a byte-string length", [
    int(INT64_MAX + 1n),
    bytes([1]),
    bytes([1, 2, 3]),
  ]);
  add(12, "slice of an integer with a byte-string start", [
    bytes([1]),
    int(0),
    int(3),
  ]);
  add(12, "slice of an integer with a byte-string length", [
    int(0),
    bytes([1]),
    int(3),
  ]);

  for (const index of [
    -1n,
    0n,
    2n,
    3n,
    INT64_MAX,
    INT64_MAX + 1n,
    -(2n ** 70n),
  ]) {
    add(14, `index ${index}`, [bytes([1, 2, 3]), int(index)]);
    add(14, `index ${index} of compacted bytes`, [
      semanticBytes(2),
      int(index),
    ]);
    add(14, `index ${index} of a string`, [str("abc"), int(index)]);
  }
  add(14, "index 0 of empty bytes", [bytes([]), int(0)]);
  add(14, "index by bytes", [bytes([1]), bytes([1])]);
};

/** headList (33), tailList (34) and the five unData variants (42-46). */
const listAndDataCases = (add) => {
  for (const tag of [33, 34]) {
    add(tag, "empty integer list", [intList([])]);
    add(tag, "one-integer list", [intList([1n])]);
    add(tag, "empty Data list", [dataList([])]);
    add(tag, "empty byte-string list", [bytesList([])]);
    add(tag, "compacted list", [semantic(intList([]), 0)]);
    add(tag, "byte string", [bytes([])]);
    add(tag, "Data list", [data(new DataList([]))]);
    add(tag, "unit", [unit]);
  }
  const variants = [
    ["Constr", new DataConstr(0n, [])],
    ["Map", new DataMap([])],
    ["List", new DataList([])],
    ["I", new DataI(1n)],
    ["B", new DataB(new Uint8Array([1]))],
  ];
  for (const tag of [42, 43, 44, 45, 46]) {
    for (const [name, value] of variants)
      add(tag, `Data ${name}`, [data(value)]);
    add(tag, "compacted Data", [semantic(data(new DataI(1n)), 4)]);
    add(tag, "integer", [int(1)]);
    add(tag, "two arguments", [data(new DataI(1n)), data(new DataI(1n))]);
  }
};

/** integerToByteString (73) over its size and value limits. */
const integerToBytesCases = (add) => {
  for (const size of [-1n, 0n, 1n, 2n, 8192n, 8193n]) {
    for (const value of [-1n, 0n, 255n, 256n, 65535n, 65536n]) {
      add(73, `size ${size}, value ${value}`, [
        bool(true),
        int(size),
        int(value),
      ]);
    }
  }
  add(73, "size 0, value of 8192 bytes", [
    bool(false),
    int(0),
    int(2n ** 65536n - 1n),
  ]);
  add(73, "size 0, value of 8193 bytes", [
    bool(false),
    int(0),
    int(2n ** 65536n),
  ]);
  add(73, "size 1, value of 1001 bytes", [
    bool(true),
    int(1),
    int(2n ** 8000n),
  ]);
  add(73, "integer endianness", [int(1), int(1), int(1)]);
  add(73, "boolean type, integer payload", [
    booleanTypeIntegerPayload,
    int(1),
    int(1000),
  ]);
  add(73, "boolean type, Constr 2 payload", [
    booleanTypeConstrTwo,
    int(1),
    int(1000),
  ]);
  add(73, "byte-string size", [bool(false), bytes([1]), int(1)]);
  add(73, "byte-string value", [bool(false), int(-1), bytes([1])]);
  add(73, "compacted endianness", [semantic(bool(true), 1), int(-1), int(1)]);
};

/** readBit (79), writeBits (80), replicateByte (81), shift and rotate (82, 83). */
const bitCases = (add) => {
  for (const index of [-1n, 0n, 15n, 16n]) {
    add(79, `read bit ${index}`, [bytes([1, 2]), int(index)]);
    add(79, `read bit ${index} of compacted bytes`, [
      semanticBytes(2),
      int(index),
    ]);
  }
  add(79, "read bit 0 of empty bytes", [bytes([]), int(0)]);

  for (const [name, indices] of [
    ["no indices", intList([])],
    ["indices 0 and 15", intList([0n, 15n])],
    ["index 16", intList([16n])],
    ["index -1", intList([-1n])],
    ["indices 0, 16, -1", intList([0n, 16n, -1n])],
    ["Data index list", dataList([new DataI(16n)])],
    ["byte-string index list", bytesList([[1]])],
    ["compacted index list", semantic(intList([]), 0)],
    ["integer indices", int(1)],
  ]) {
    add(80, `write ${name}`, [bytes([1, 2]), indices, bool(true)]);
    add(80, `write ${name}, integer bit`, [bytes([1, 2]), indices, int(1)]);
    add(80, `write ${name} into compacted bytes`, [
      semanticBytes(2),
      indices,
      bool(true),
    ]);
    add(80, `write ${name}, compacted bit`, [
      bytes([1, 2]),
      indices,
      semantic(bool(true), 1),
    ]);
  }
  add(80, "write index 0 into 4096 bytes", [
    filled(4096),
    intList([0n]),
    bool(false),
  ]);
  add(80, "write index 32768 into 4096 bytes", [
    filled(4096),
    intList([32768n]),
    bool(false),
  ]);
  add(80, "write index 0 into 4097 bytes", [
    filled(4097),
    intList([0n]),
    bool(false),
  ]);
  add(80, "write no indices into 4097 bytes", [
    filled(4097),
    intList([]),
    bool(false),
  ]);
  add(80, "write into empty bytes", [bytes([]), intList([0n]), bool(false)]);

  for (const length of [-1n, 0n, 8192n, 8193n]) {
    for (const byte of [-1n, 0n, 255n, 256n]) {
      add(81, `replicate ${byte} ${length} times`, [int(length), int(byte)]);
    }
  }
  add(81, "replicate with a byte-string length", [bytes([1]), int(0)]);
  add(81, "replicate with a compacted byte", [int(1), semanticInteger(1)]);

  for (const tag of [82, 83]) {
    for (const amount of [
      INT64_MIN - 1n,
      INT64_MIN,
      0n,
      INT64_MAX,
      INT64_MAX + 1n,
    ]) {
      add(tag, `by ${amount}`, [bytes([1, 2]), int(amount)]);
      add(tag, `compacted bytes by ${amount}`, [semanticBytes(2), int(amount)]);
      add(tag, `string by ${amount}`, [str("x"), int(amount)]);
    }
    add(tag, "by a byte string", [bytes([1]), bytes([1])]);
  }
};

/**
 * One known failure per tag, each argument replaced in turn by a value of
 * another kind: an opaque or Miller-loop value, and a compacted constant of
 * the same type.
 */
const KIND_SWEEP_BASES = [
  ...[3, 4, 5, 6].map((tag) => [tag, [int(1), int(0)]]),
  [11, [int(256), bytes([1])]],
  [12, [int(INT64_MAX + 1n), int(0), bytes([1, 2, 3])]],
  [14, [bytes([1, 2, 3]), int(3)]],
  [21, [filled(31), filled(32), filled(64)]],
  [25, [bytes([0xff])]],
  [33, [intList([])]],
  [34, [intList([])]],
  ...[42, 43, 44, 46].map((tag) => [tag, [data(new DataI(1n))]]),
  [45, [data(new DataB(new Uint8Array([1])))]],
  [52, [filled(33, 2), filled(32), filled(64)]],
  [53, [filled(31), filled(7), filled(64)]],
  [56, [int(2n ** 4095n), g1Point(BLS12_381_G1_GENERATOR)]],
  [63, [int(2n ** 4095n), g2Point(BLS12_381_G2_GENERATOR)]],
  [59, [filled(47, 0x80)]],
  [60, [filled(3), filled(256)]],
  [66, [filled(95, 0x80)]],
  [67, [filled(3), filled(256)]],
  [73, [bool(true), int(-1), int(1)]],
  [79, [bytes([1, 2]), int(16)]],
  [80, [bytes([1, 2]), intList([16n]), bool(true)]],
  [81, [int(-1), int(0)]],
  [82, [bytes([1, 2]), int(INT64_MAX + 1n)]],
  [83, [bytes([1, 2]), int(INT64_MAX + 1n)]],
];

const kindSweepCases = (add, bases) => {
  for (const [tag, base] of bases) {
    base.forEach((argument, position) => {
      const replace = (value) =>
        base.map((other, index) => (index === position ? value : other));
      // Opaque and Miller-loop values alternate by position; a one-argument
      // builtin gets both.
      const others =
        base.length === 1
          ? [
              ["opaque", opaque],
              ["Miller-loop", millerLoop],
            ]
          : [
              position % 2 === 0
                ? ["opaque", opaque]
                : ["Miller-loop", millerLoop],
            ];
      for (const [name, other] of others) {
        add(tag, `kind sweep: ${name} argument ${position}`, replace(other), {
          skipRepeat: true,
        });
      }
      add(
        tag,
        `kind sweep: compacted argument ${position}`,
        replace(semantic(argument, 1)),
        { skipRepeat: true },
      );
    });
  }
};

/** Tags outside the known set: never a known failure, whatever the arguments. */
const outsideCases = (add) => {
  for (const tag of [
    -1, 0, 1, 2, 7, 8, 9, 10, 13, 15, 20, 22, 24, 26, 32, 35, 37, 41, 47, 51,
    54, 58, 61, 65, 68, 70, 71, 72, 74, 78, 84, 86, 88, 95, 101,
  ]) {
    add(tag, "1 by 0", [int(1), int(0)]);
  }
  add(37, "constrData index -1", [int(-1), dataList([])]);
  add(37, "constrData index 2^64", [int(2n ** 64n), dataList([])]);
  add(32, "mkCons integer onto byte strings", [int(1), bytesList([])]);
  add(0, "opaque arguments", [opaque, millerLoop]);
};

/**
 * Every case, in corpus order, as `{ tag, name, arguments }`. A case whose tag
 * and arguments repeat an earlier case's is refused, except in the kind
 * sweep, which skips the applications the hand-written cases already hold.
 */
export const knownBuiltinFailureConformanceCases = () => {
  const cases = [];
  const seen = new Map();
  const add = (tag, name, arguments_, { skipRepeat = false } = {}) => {
    const key = JSON.stringify([
      tag,
      arguments_.map(encodeConformanceArgument),
    ]);
    if (seen.has(key)) {
      if (skipRepeat) return;
      throw new Error(`case ${name} (tag ${tag}) repeats ${seen.get(key)}`);
    }
    seen.set(key, `${name} (tag ${tag})`);
    cases.push({ tag, name, arguments: arguments_ });
  };
  integerDivisionCases(add);
  byteStringCases(add);
  listAndDataCases(add);
  cryptoCases(add);
  integerToBytesCases(add);
  bitCases(add);
  kindSweepCases(add, KIND_SWEEP_BASES);
  outsideCases(add);
  pv11Cases(add);
  kindSweepCases(add, PV11_KIND_SWEEP_BASES);
  return cases;
};

const hex = (value) => Buffer.from(value).toString("hex");

/** A witness in the fixture's JSON form. */
export const encodeConformanceArgument = (value) => {
  switch (value.kind) {
    case "constant":
      return {
        kind: "constant",
        type: hex(value.witness.typeCbor),
        payload: hex(value.witness.payloadCbor),
      };
    case "semanticConstant":
      return {
        kind: "semanticConstant",
        type: hex(value.witness.typeCbor),
        root: hex(value.witness.payload.root),
        cborLength: String(value.witness.payload.cborLength),
        payloadMemory: String(value.witness.payload.memory),
        memory: String(value.witness.memory),
      };
    case "opaque":
      return { kind: "opaque", root: hex(value.root) };
    case "blsMillerLoop":
      return { kind: "blsMillerLoop", root: hex(value.expressionRoot) };
    default:
      throw new Error(`no fixture form for a ${String(value.kind)} witness`);
  }
};

/**
 * The fixture rows: each case with its arguments in JSON form and the
 * verdict `knownFailure` (the twin, from `src/` or `dist/`) gives it.
 */
export const buildKnownBuiltinFailureConformanceRows = (knownFailure) =>
  knownBuiltinFailureConformanceCases().map(
    ({ tag, name, arguments: arguments_ }, index) => {
      let verdict;
      try {
        verdict = knownFailure(BigInt(tag), arguments_) ? "known" : "not-known";
      } catch (error) {
        if (!(error instanceof Error)) throw error;
        verdict = "refused";
      }
      return {
        case: index,
        tag,
        name,
        arguments: arguments_.map(encodeConformanceArgument),
        verdict,
      };
    },
  );

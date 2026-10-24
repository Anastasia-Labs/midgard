/**
 * Exact Plutus V3 builtin budgets under the canonical V1 pinned cost model,
 * the TypeScript twin of `onchain/aiken/lib/midgard/cek-cost-v1.ak`: the same
 * cost function shapes and coefficients, tag by tag. The coefficients are the
 * pinned model's (`PLUTUS_V3_CANONICAL_COST_MODEL_VIEW`), which
 * `tests/cek-cost.differential.test.ts` checks;
 * `scripts/generate-cek-cost-v1-aiken-goldens.mjs` holds the Aiken twin to
 * these budgets.
 */

export type MidgardCekBuiltinBudget = {
  readonly cpu: bigint;
  readonly memory: bigint;
};

const MIN_BUILTIN_TAG = 0;
const MAX_BUILTIN_TAG = 86;

type Sizes = readonly bigint[];
type CostFunction = (sizes: Sizes) => bigint;

const max = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;
const min = (left: bigint, right: bigint): bigint =>
  left < right ? left : right;

const constant =
  (value: bigint): CostFunction =>
  () =>
    value;
/** `quote + size * slope` over the argument at `index`. */
const linearIn =
  (index: number, quote: bigint, slope: bigint): CostFunction =>
  (sizes) =>
    quote + sizes[index]! * slope;
const linearAdd =
  (quote: bigint, slope: bigint): CostFunction =>
  ([x, y]) =>
    quote + (x! + y!) * slope;
const linearMul =
  (quote: bigint, slope: bigint): CostFunction =>
  ([x, y]) =>
    quote + x! * y! * slope;
const linearSub =
  (quote: bigint, slope: bigint, minimum: bigint): CostFunction =>
  ([x, y]) =>
    quote + max(minimum, x! - y!) * slope;
const linearMin =
  (quote: bigint, slope: bigint): CostFunction =>
  ([x, y]) =>
    quote + min(x!, y!) * slope;
const linearMax =
  (quote: bigint, slope: bigint): CostFunction =>
  ([x, y]) =>
    quote + max(x!, y!) * slope;
const linearOnEqual =
  (quote: bigint, slope: bigint, otherwise: bigint): CostFunction =>
  ([x, y]) =>
    x === y ? quote + x! * slope : otherwise;
const linearYZ =
  (quote: bigint, ySlope: bigint, zSlope: bigint): CostFunction =>
  ([, y, z]) =>
    quote + y! * ySlope + z! * zSlope;
const linearMaxYZ =
  (quote: bigint, slope: bigint): CostFunction =>
  ([, y, z]) =>
    quote + max(y!, z!) * slope;
/** `x0 + x1 * size + x2 * size * size` over the argument at `index`. */
const quadraticIn =
  (index: number, x0: bigint, x1: bigint, x2: bigint): CostFunction =>
  (sizes) =>
    x0 + x1 * sizes[index]! + x2 * sizes[index]! * sizes[index]!;
const constYOrLinearZ =
  (quote: bigint, slope: bigint): CostFunction =>
  ([, y, z]) =>
    y === 0n ? quote + slope * z! : y!;

/**
 * Plutus V3 variant E: divideInteger and modInteger cost the quadratic over
 * (larger, smaller) sizes on both sides of the diagonal; quotientInteger and
 * remainderInteger stay constant below it.
 */
const divisionCpu =
  (constantBelowDiagonal: boolean): CostFunction =>
  ([x, y]) => {
    if (x! < y! && constantBelowDiagonal) return 85848n;
    const [a, b] = x! < y! ? [y!, x!] : [x!, y!];
    return max(
      85848n,
      123203n +
        1716n * a +
        7305n * b +
        57n * a * a +
        960n * a * b -
        900n * b * b,
    );
  };

/** `[arity, cpu, memory]` for each builtin tag, 0 to 86. */
const COSTS: readonly (readonly [number, CostFunction, CostFunction])[] = [
  /* 0 addInteger */ [2, linearMax(100788n, 420n), linearMax(1n, 1n)],
  /* 1 subtractInteger */ [2, linearMax(100788n, 420n), linearMax(1n, 1n)],
  /* 2 multiplyInteger */ [2, linearMul(90434n, 519n), linearAdd(0n, 1n)],
  /* 3 divideInteger */ [2, divisionCpu(false), linearSub(0n, 1n, 1n)],
  /* 4 quotientInteger */ [2, divisionCpu(true), linearSub(0n, 1n, 1n)],
  /* 5 remainderInteger */ [2, divisionCpu(true), linearIn(1, 0n, 1n)],
  /* 6 modInteger */ [2, divisionCpu(false), linearIn(1, 0n, 1n)],
  /* 7 equalsInteger */ [2, linearMin(51775n, 558n), constant(1n)],
  /* 8 lessThanInteger */ [2, linearMin(44749n, 541n), constant(1n)],
  /* 9 lessThanEqualsInteger */ [2, linearMin(43285n, 552n), constant(1n)],
  /* 10 appendByteString */ [2, linearAdd(1000n, 173n), linearAdd(0n, 1n)],
  /* 11 consByteString */ [2, linearIn(1, 72010n, 178n), linearAdd(0n, 1n)],
  /* 12 sliceByteString */ [3, linearIn(2, 20467n, 1n), linearIn(2, 4n, 0n)],
  /* 13 lengthOfByteString */ [1, constant(22100n), constant(10n)],
  /* 14 indexByteString */ [2, constant(13169n), constant(4n)],
  /* 15 equalsByteString */ [
    2,
    linearOnEqual(28755n, 75n, 30623n),
    constant(1n),
  ],
  /* 16 lessThanByteString */ [2, linearMin(28999n, 74n), constant(1n)],
  /* 17 lessThanEqualsByteString */ [2, linearMin(28999n, 74n), constant(1n)],
  /* 18 sha2_256 */ [1, linearIn(0, 270652n, 22588n), constant(4n)],
  /* 19 sha3_256 */ [1, linearIn(0, 1457325n, 64566n), constant(4n)],
  /* 20 blake2b_256 */ [1, linearIn(0, 201305n, 8356n), constant(4n)],
  /* 21 verifyEd25519Signature */ [
    3,
    linearIn(1, 53384111n, 14333n),
    constant(10n),
  ],
  /* 22 appendString */ [2, linearAdd(1000n, 59957n), linearAdd(4n, 1n)],
  /* 23 equalsString */ [2, linearOnEqual(1000n, 60594n, 39184n), constant(1n)],
  /* 24 encodeUtf8 */ [1, linearIn(0, 1000n, 42921n), linearIn(0, 4n, 2n)],
  /* 25 decodeUtf8 */ [1, linearIn(0, 91189n, 769n), linearIn(0, 4n, 2n)],
  /* 26 ifThenElse */ [3, constant(76049n), constant(1n)],
  /* 27 chooseUnit */ [2, constant(61462n), constant(4n)],
  /* 28 trace */ [2, constant(59498n), constant(32n)],
  /* 29 fstPair */ [1, constant(141895n), constant(32n)],
  /* 30 sndPair */ [1, constant(141992n), constant(32n)],
  /* 31 chooseList */ [3, constant(132994n), constant(32n)],
  /* 32 mkCons */ [2, constant(72362n), constant(32n)],
  /* 33 headList */ [1, constant(83150n), constant(32n)],
  /* 34 tailList */ [1, constant(81663n), constant(32n)],
  /* 35 nullList */ [1, constant(74433n), constant(32n)],
  /* 36 chooseData */ [6, constant(94375n), constant(32n)],
  /* 37 constrData */ [2, constant(22151n), constant(32n)],
  /* 38 mapData */ [1, constant(68246n), constant(32n)],
  /* 39 listData */ [1, constant(33852n), constant(32n)],
  /* 40 iData */ [1, constant(15299n), constant(32n)],
  /* 41 bData */ [1, constant(11183n), constant(32n)],
  /* 42 unConstrData */ [1, constant(24588n), constant(32n)],
  /* 43 unMapData */ [1, constant(24623n), constant(32n)],
  /* 44 unListData */ [1, constant(25933n), constant(32n)],
  /* 45 unIData */ [1, constant(20744n), constant(32n)],
  /* 46 unBData */ [1, constant(20142n), constant(32n)],
  /* 47 equalsData */ [2, linearMin(898148n, 27279n), constant(1n)],
  /* 48 mkPairData */ [2, constant(11546n), constant(32n)],
  /* 49 mkNilData */ [1, constant(7243n), constant(32n)],
  /* 50 mkNilPairData */ [1, constant(7391n), constant(32n)],
  /* 51 serialiseData */ [
    1,
    linearIn(0, 955506n, 213312n),
    linearIn(0, 0n, 2n),
  ],
  /* 52 verifyEcdsaSecp256k1Signature */ [
    3,
    constant(43053543n),
    constant(10n),
  ],
  /* 53 verifySchnorrSecp256k1Signature */ [
    3,
    linearIn(1, 43574283n, 26308n),
    constant(10n),
  ],
  /* 54 bls12_381_G1_add */ [2, constant(962335n), constant(18n)],
  /* 55 bls12_381_G1_neg */ [1, constant(267929n), constant(18n)],
  /* 56 bls12_381_G1_scalarMul */ [
    2,
    linearIn(0, 76433006n, 8868n),
    constant(18n),
  ],
  /* 57 bls12_381_G1_equal */ [2, constant(442008n), constant(1n)],
  /* 58 bls12_381_G1_compress */ [1, constant(2780678n), constant(6n)],
  /* 59 bls12_381_G1_uncompress */ [1, constant(52948122n), constant(18n)],
  /* 60 bls12_381_G1_hashToGroup */ [
    2,
    linearIn(0, 52538055n, 3756n),
    constant(18n),
  ],
  /* 61 bls12_381_G2_add */ [2, constant(1995836n), constant(36n)],
  /* 62 bls12_381_G2_neg */ [1, constant(284546n), constant(36n)],
  /* 63 bls12_381_G2_scalarMul */ [
    2,
    linearIn(0, 158221314n, 26549n),
    constant(36n),
  ],
  /* 64 bls12_381_G2_equal */ [2, constant(901022n), constant(1n)],
  /* 65 bls12_381_G2_compress */ [1, constant(3227919n), constant(12n)],
  /* 66 bls12_381_G2_uncompress */ [1, constant(74698472n), constant(36n)],
  /* 67 bls12_381_G2_hashToGroup */ [
    2,
    linearIn(0, 166917843n, 4307n),
    constant(36n),
  ],
  /* 68 bls12_381_millerLoop */ [2, constant(254006273n), constant(72n)],
  /* 69 bls12_381_mulMlResult */ [2, constant(2174038n), constant(72n)],
  /* 70 bls12_381_finalVerify */ [2, constant(333849714n), constant(1n)],
  /* 71 keccak_256 */ [1, linearIn(0, 2261318n, 64571n), constant(4n)],
  /* 72 blake2b_224 */ [1, linearIn(0, 207616n, 8310n), constant(4n)],
  /* 73 integerToByteString */ [
    3,
    quadraticIn(2, 1293828n, 28716n, 63n),
    constYOrLinearZ(0n, 1n),
  ],
  /* 74 byteStringToInteger */ [
    2,
    quadraticIn(1, 1006041n, 43623n, 251n),
    linearIn(1, 0n, 1n),
  ],
  /* 75 andByteString */ [
    3,
    linearYZ(100181n, 726n, 719n),
    linearMaxYZ(0n, 1n),
  ],
  /* 76 orByteString */ [3, linearYZ(100181n, 726n, 719n), linearMaxYZ(0n, 1n)],
  /* 77 xorByteString */ [
    3,
    linearYZ(100181n, 726n, 719n),
    linearMaxYZ(0n, 1n),
  ],
  /* 78 complementByteString */ [
    1,
    linearIn(0, 107878n, 680n),
    linearIn(0, 0n, 1n),
  ],
  /* 79 readBit */ [2, constant(95336n), constant(1n)],
  /* 80 writeBits */ [3, linearIn(1, 281145n, 18848n), linearIn(0, 0n, 1n)],
  /* 81 replicateByte */ [2, linearIn(0, 180194n, 159n), linearIn(0, 1n, 1n)],
  /* 82 shiftByteString */ [
    2,
    linearIn(0, 158519n, 8942n),
    linearIn(0, 0n, 1n),
  ],
  /* 83 rotateByteString */ [
    2,
    linearIn(0, 159378n, 8813n),
    linearIn(0, 0n, 1n),
  ],
  /* 84 countSetBits */ [1, linearIn(0, 107490n, 3298n), constant(1n)],
  /* 85 findFirstSetBit */ [1, linearIn(0, 106057n, 655n), constant(1n)],
  /* 86 ripemd_160 */ [1, linearIn(0, 1964219n, 24520n), constant(3n)],
];

/**
 * Evaluates the exact pinned Plutus V3 builtin cost function over the sizes
 * supplied by the CEK semantics. `tag` is Cardano's flat tag. Callers size
 * polymorphic, width, count and literal arguments exactly as cardano-node
 * does before calling this helper.
 */
export const computeMidgardCekBuiltinBudget = (
  tag: number,
  costArgumentSizes: readonly bigint[],
): MidgardCekBuiltinBudget => {
  if (
    !Number.isInteger(tag) ||
    tag < MIN_BUILTIN_TAG ||
    tag > MAX_BUILTIN_TAG
  ) {
    throw new Error("V1 builtin tag is outside Plutus V3");
  }
  const [arity, cpu, memory] = COSTS[tag]!;
  if (costArgumentSizes.length !== arity) {
    throw new Error(
      `V1 builtin ${tag.toString(10)} requires ${arity.toString(10)} cost sizes`,
    );
  }
  if (costArgumentSizes.some((size) => size < 0n)) {
    throw new Error("V1 builtin cost size must be non-negative");
  }
  return Object.freeze({
    cpu: cpu(costArgumentSizes),
    memory: memory(costArgumentSizes),
  });
};

import { encodeCbor } from "./codec/cbor.js";

export const MIDGARD_BLAKE2B_224_TRACE_V1_VERSION = 1 as const;
export const MIDGARD_BLAKE2B_BLOCK_BYTES = 128;
export const MIDGARD_BLAKE2B_ROUNDS = 12;
export const MIDGARD_BLAKE2B_224_DIGEST_BYTES = 28;

export const MidgardBlake2b224TraceStagesV1 = Object.freeze({
  Ready: 0,
  Round: 1,
  Finish: 2,
  Terminal: 3,
} as const);

export type MidgardBlake2b224TraceStageV1 =
  (typeof MidgardBlake2b224TraceStagesV1)[keyof typeof MidgardBlake2b224TraceStagesV1];

const WORD_MASK = 0xffff_ffff_ffff_ffffn;
const PARAMETER_BLOCK_V1 = 0x0101_001cn;

const IV = Object.freeze([
  0x6a09_e667_f3bc_c908n,
  0xbb67_ae85_84ca_a73bn,
  0x3c6e_f372_fe94_f82bn,
  0xa54f_f53a_5f1d_36f1n,
  0x510e_527f_ade6_82d1n,
  0x9b05_688c_2b3e_6c1fn,
  0x1f83_d9ab_fb41_bd6bn,
  0x5be0_cd19_137e_2179n,
]);

const SIGMA = Object.freeze([
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
  [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
  [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
  [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
  [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
  [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
  [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
  [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
  [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
  [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
]);

export type MidgardBlake2b224TraceControlV1 = {
  readonly version: typeof MIDGARD_BLAKE2B_224_TRACE_V1_VERSION;
  readonly stage: MidgardBlake2b224TraceStageV1;
  readonly cursor: number;
  readonly totalLength: number;
  readonly chainingValue: Buffer;
  readonly activeBlock: Buffer;
  readonly activeBlockLength: number;
  readonly workingValue: Buffer;
  readonly round: number;
};

export type MidgardBlake2b224TraceStepV1 = {
  readonly control: MidgardBlake2b224TraceControlV1;
  readonly block: Buffer | null;
  readonly next: MidgardBlake2b224TraceControlV1;
};

// This trace hashes a Cardano/Midgard script identity message, which always
// contains at least the one-byte language discriminator before script bytes.
const rotateRight64 = (value: bigint, bits: bigint): bigint =>
  ((value >> bits) | (value << (64n - bits))) & WORD_MASK;

const add64 = (...values: readonly bigint[]): bigint =>
  values.reduce((sum, value) => sum + value, 0n) & WORD_MASK;

const mix = ({
  words,
  a,
  b,
  c,
  d,
  x,
  y,
}: {
  readonly words: bigint[];
  readonly a: number;
  readonly b: number;
  readonly c: number;
  readonly d: number;
  readonly x: bigint;
  readonly y: bigint;
}): void => {
  words[a] = add64(words[a]!, words[b]!, x);
  words[d] = rotateRight64(words[d]! ^ words[a]!, 32n);
  words[c] = add64(words[c]!, words[d]!);
  words[b] = rotateRight64(words[b]! ^ words[c]!, 24n);
  words[a] = add64(words[a]!, words[b]!, y);
  words[d] = rotateRight64(words[d]! ^ words[a]!, 16n);
  words[c] = add64(words[c]!, words[d]!);
  words[b] = rotateRight64(words[b]! ^ words[c]!, 63n);
};

const decodeWordsLe = (
  bytes: Uint8Array,
  expectedWords: number,
  field: string,
): bigint[] => {
  if (bytes.length !== expectedWords * 8) {
    throw new Error(`${field} must contain ${expectedWords.toString()} words`);
  }
  const source = Buffer.from(bytes);
  return Array.from(
    { length: expectedWords },
    (_, index) => source.readBigUInt64LE(index * 8),
  );
};

const encodeWordsLe = (words: readonly bigint[]): Buffer => {
  const encoded = Buffer.alloc(words.length * 8);
  words.forEach((word, index) => {
    encoded.writeBigUInt64LE(word & WORD_MASK, index * 8);
  });
  return encoded;
};

const initialChainingValue = (): Buffer => {
  const words = [...IV];
  words[0] = words[0]! ^ PARAMETER_BLOCK_V1;
  return encodeWordsLe(words);
};

const initializeWorkingValue = ({
  chainingValue,
  bytesCompressed,
  final,
}: {
  readonly chainingValue: Uint8Array;
  readonly bytesCompressed: number;
  readonly final: boolean;
}): Buffer => {
  const h = decodeWordsLe(chainingValue, 8, "BLAKE2b chaining value");
  const words = [...h, ...IV];
  const counter = BigInt(bytesCompressed);
  words[12] = words[12]! ^ (counter & WORD_MASK);
  words[13] = words[13]! ^ (counter >> 64n);
  if (final) words[14] = words[14]! ^ WORD_MASK;
  return encodeWordsLe(words);
};

const applyRound = ({
  workingValue,
  activeBlock,
  round,
}: {
  readonly workingValue: Uint8Array;
  readonly activeBlock: Uint8Array;
  readonly round: number;
}): Buffer => {
  const words = decodeWordsLe(workingValue, 16, "BLAKE2b working value");
  const message = decodeWordsLe(activeBlock, 16, "BLAKE2b active block");
  const sigma = SIGMA[round % SIGMA.length]!;
  mix({
    words,
    a: 0,
    b: 4,
    c: 8,
    d: 12,
    x: message[sigma[0]!]!,
    y: message[sigma[1]!]!,
  });
  mix({
    words,
    a: 1,
    b: 5,
    c: 9,
    d: 13,
    x: message[sigma[2]!]!,
    y: message[sigma[3]!]!,
  });
  mix({
    words,
    a: 2,
    b: 6,
    c: 10,
    d: 14,
    x: message[sigma[4]!]!,
    y: message[sigma[5]!]!,
  });
  mix({
    words,
    a: 3,
    b: 7,
    c: 11,
    d: 15,
    x: message[sigma[6]!]!,
    y: message[sigma[7]!]!,
  });
  mix({
    words,
    a: 0,
    b: 5,
    c: 10,
    d: 15,
    x: message[sigma[8]!]!,
    y: message[sigma[9]!]!,
  });
  mix({
    words,
    a: 1,
    b: 6,
    c: 11,
    d: 12,
    x: message[sigma[10]!]!,
    y: message[sigma[11]!]!,
  });
  mix({
    words,
    a: 2,
    b: 7,
    c: 8,
    d: 13,
    x: message[sigma[12]!]!,
    y: message[sigma[13]!]!,
  });
  mix({
    words,
    a: 3,
    b: 4,
    c: 9,
    d: 14,
    x: message[sigma[14]!]!,
    y: message[sigma[15]!]!,
  });
  return encodeWordsLe(words);
};

const finishBlock = ({
  chainingValue,
  workingValue,
}: {
  readonly chainingValue: Uint8Array;
  readonly workingValue: Uint8Array;
}): Buffer => {
  const h = decodeWordsLe(chainingValue, 8, "BLAKE2b chaining value");
  const words = decodeWordsLe(workingValue, 16, "BLAKE2b working value");
  return encodeWordsLe(
    h.map((word, index) => word ^ words[index]! ^ words[index + 8]!),
  );
};

const emptyActiveState = {
  activeBlock: Buffer.alloc(0),
  activeBlockLength: 0,
  workingValue: Buffer.alloc(0),
  round: 0,
} as const;

const controlIsWellFormed = (
  control: MidgardBlake2b224TraceControlV1,
): boolean => {
  if (
    control.version !== MIDGARD_BLAKE2B_224_TRACE_V1_VERSION ||
    !Number.isSafeInteger(control.stage) ||
    control.stage < MidgardBlake2b224TraceStagesV1.Ready ||
    control.stage > MidgardBlake2b224TraceStagesV1.Terminal ||
    !Number.isSafeInteger(control.cursor) ||
    control.cursor < 0 ||
    !Number.isSafeInteger(control.totalLength) ||
    control.totalLength <= 0 ||
    control.cursor > control.totalLength ||
    control.chainingValue.length !== 64 ||
    !Number.isSafeInteger(control.activeBlockLength) ||
    !Number.isSafeInteger(control.round)
  ) {
    return false;
  }
  if (
    control.cursor === 0 &&
    !control.chainingValue.equals(initialChainingValue())
  ) {
    return false;
  }
  if (
    control.stage === MidgardBlake2b224TraceStagesV1.Ready ||
    control.stage === MidgardBlake2b224TraceStagesV1.Terminal
  ) {
    return (
      control.activeBlock.length === 0 &&
      control.activeBlockLength === 0 &&
      control.workingValue.length === 0 &&
      control.round === 0 &&
      (control.stage === MidgardBlake2b224TraceStagesV1.Ready
        ? control.cursor < control.totalLength &&
          control.cursor % MIDGARD_BLAKE2B_BLOCK_BYTES === 0
        : control.cursor === control.totalLength)
    );
  }
  const expectedLength = Math.min(
    MIDGARD_BLAKE2B_BLOCK_BYTES,
    control.totalLength - control.cursor,
  );
  return (
    control.cursor < control.totalLength &&
    control.cursor % MIDGARD_BLAKE2B_BLOCK_BYTES === 0 &&
    control.activeBlock.length === MIDGARD_BLAKE2B_BLOCK_BYTES &&
    control.activeBlockLength === expectedLength &&
    (control.activeBlockLength === MIDGARD_BLAKE2B_BLOCK_BYTES ||
      control.activeBlock
        .subarray(control.activeBlockLength)
        .every((byte) => byte === 0)) &&
    control.workingValue.length === 128 &&
    (control.stage === MidgardBlake2b224TraceStagesV1.Round
      ? control.round >= 0 && control.round < MIDGARD_BLAKE2B_ROUNDS
      : control.round === MIDGARD_BLAKE2B_ROUNDS)
  );
};

export const initialMidgardBlake2b224TraceControlV1 = (
  totalLength: number,
): MidgardBlake2b224TraceControlV1 => {
  const control = {
    version: MIDGARD_BLAKE2B_224_TRACE_V1_VERSION,
    stage: MidgardBlake2b224TraceStagesV1.Ready,
    cursor: 0,
    totalLength,
    chainingValue: initialChainingValue(),
    ...emptyActiveState,
  } satisfies MidgardBlake2b224TraceControlV1;
  if (!controlIsWellFormed(control)) {
    throw new Error("Invalid V1 BLAKE2b-224 trace total length");
  }
  return control;
};

export const encodeMidgardBlake2b224TraceControlV1 = (
  control: MidgardBlake2b224TraceControlV1,
): Buffer => {
  if (!controlIsWellFormed(control)) {
    throw new Error("Invalid V1 BLAKE2b-224 trace control");
  }
  return encodeCbor([
    1n,
    BigInt(control.stage),
    BigInt(control.cursor),
    BigInt(control.totalLength),
    control.chainingValue,
    control.activeBlock,
    BigInt(control.activeBlockLength),
    control.workingValue,
    BigInt(control.round),
  ]);
};

export const advanceMidgardBlake2b224TraceV1 = ({
  control,
  block,
}: {
  readonly control: MidgardBlake2b224TraceControlV1;
  readonly block?: Uint8Array | null;
}): MidgardBlake2b224TraceControlV1 | null => {
  try {
    if (!controlIsWellFormed(control)) return null;
    if (control.stage === MidgardBlake2b224TraceStagesV1.Ready) {
      const expectedLength = Math.min(
        MIDGARD_BLAKE2B_BLOCK_BYTES,
        control.totalLength - control.cursor,
      );
      if (
        block === null ||
        block === undefined ||
        block.length !== expectedLength
      ) {
        return null;
      }
      const activeBlock = Buffer.alloc(MIDGARD_BLAKE2B_BLOCK_BYTES);
      activeBlock.set(block);
      const bytesCompressed = control.cursor + expectedLength;
      const next = {
        ...control,
        stage: MidgardBlake2b224TraceStagesV1.Round,
        activeBlock,
        activeBlockLength: expectedLength,
        workingValue: initializeWorkingValue({
          chainingValue: control.chainingValue,
          bytesCompressed,
          final: bytesCompressed === control.totalLength,
        }),
        round: 0,
      };
      return controlIsWellFormed(next) ? next : null;
    }
    if (block !== null && block !== undefined) return null;
    if (control.stage === MidgardBlake2b224TraceStagesV1.Round) {
      const round = control.round + 1;
      const next = {
        ...control,
        stage:
          round === MIDGARD_BLAKE2B_ROUNDS
            ? MidgardBlake2b224TraceStagesV1.Finish
            : MidgardBlake2b224TraceStagesV1.Round,
        workingValue: applyRound({
          workingValue: control.workingValue,
          activeBlock: control.activeBlock,
          round: control.round,
        }),
        round,
      };
      return controlIsWellFormed(next) ? next : null;
    }
    if (control.stage === MidgardBlake2b224TraceStagesV1.Finish) {
      const cursor = control.cursor + control.activeBlockLength;
      const next = {
        ...control,
        stage:
          cursor === control.totalLength
            ? MidgardBlake2b224TraceStagesV1.Terminal
            : MidgardBlake2b224TraceStagesV1.Ready,
        cursor,
        chainingValue: finishBlock({
          chainingValue: control.chainingValue,
          workingValue: control.workingValue,
        }),
        ...emptyActiveState,
      };
      return controlIsWellFormed(next) ? next : null;
    }
    return null;
  } catch {
    return null;
  }
};

export const digestMidgardBlake2b224TraceV1 = (
  control: MidgardBlake2b224TraceControlV1,
): Buffer | null =>
  controlIsWellFormed(control) &&
  control.stage === MidgardBlake2b224TraceStagesV1.Terminal
    ? control.chainingValue.subarray(0, MIDGARD_BLAKE2B_224_DIGEST_BYTES)
    : null;

export const buildMidgardBlake2b224TraceV1 = (
  message: Uint8Array,
): readonly MidgardBlake2b224TraceStepV1[] => {
  const bytes = Buffer.from(message);
  let control = initialMidgardBlake2b224TraceControlV1(bytes.length);
  const steps: MidgardBlake2b224TraceStepV1[] = [];
  while (control.stage !== MidgardBlake2b224TraceStagesV1.Terminal) {
    const block =
      control.stage === MidgardBlake2b224TraceStagesV1.Ready
        ? bytes.subarray(
            control.cursor,
            control.cursor + MIDGARD_BLAKE2B_BLOCK_BYTES,
          )
        : null;
    const next = advanceMidgardBlake2b224TraceV1({ control, block });
    if (next === null || !controlIsWellFormed(next)) {
      throw new Error("V1 BLAKE2b-224 trace failed closed");
    }
    steps.push({ control, block, next });
    control = next;
  }
  return steps;
};

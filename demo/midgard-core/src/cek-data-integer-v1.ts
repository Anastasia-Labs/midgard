import { hashMidgardCekDataNodeV1 } from "./cek-semantic.js";
import {
  advanceMidgardCekSourceBlobV1,
  encodeMidgardCekSourceBlobControlV1,
  finalizeMidgardCekSourceBlobV1,
  initialMidgardCekSourceBlobControlV1,
  isWellFormedMidgardCekSourceBlobControlV1,
  type MidgardCekSourceBlobControlV1,
  type MidgardCekSourceBlobSpanV1,
  MidgardCekSourceBlobStagesV1,
  nextMidgardCekSourceBlobSpanV1,
} from "./cek-source-blob-v1.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";

export const MIDGARD_CEK_DATA_INTEGER_V1_VERSION = 1 as const;

export const MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES = 14;

const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MidgardCekDataIntegerStagesV1 = Object.freeze({
  Syntax: 0,
  Blob: 1,
  Terminal: 2,
} as const);

export type MidgardCekDataIntegerStageV1 =
  (typeof MidgardCekDataIntegerStagesV1)[keyof typeof MidgardCekDataIntegerStagesV1];

/**
 * Proves one canonical Cardano Data integer encoding without materializing an
 * unbounded integer in the L1 validator. The parent must authenticate every
 * source span returned by `nextMidgardCekDataIntegerSpanV1`.
 */
export type MidgardCekDataIntegerControlV1 = {
  readonly version: typeof MIDGARD_CEK_DATA_INTEGER_V1_VERSION;
  readonly stage: MidgardCekDataIntegerStageV1;
  readonly sourceStart: number;
  readonly sourceLength: number;
  /** Complete CEK Data memory, including the four-word Data node overhead. */
  readonly memory: bigint;
  readonly blob: MidgardCekSourceBlobControlV1 | null;
};

export type MidgardCekDataIntegerSummaryV1 = {
  readonly root: Buffer;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataIntegerTraceStepV1 = {
  readonly control: MidgardCekDataIntegerControlV1;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekDataIntegerControlV1;
};

export type MidgardCekDataIntegerTraceV1 = {
  readonly initial: MidgardCekDataIntegerControlV1;
  readonly steps: readonly MidgardCekDataIntegerTraceStepV1[];
  readonly terminal: MidgardCekDataIntegerControlV1;
};

type CborArgument = {
  readonly major: number;
  readonly value: bigint;
  readonly nextOffset: number;
};

const exactSourceCoordinate = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 CEK Data integer ${field}`);
  }
  return value;
};

const readCanonicalCborArgument = (
  bytes: Uint8Array,
  offset: number,
): CborArgument | null => {
  if (offset < 0 || offset >= bytes.length) return null;
  const initial = bytes[offset]!;
  const major = initial >>> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return {
      major,
      value: BigInt(additional),
      nextOffset: offset + 1,
    };
  }
  const byteLength =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : null;
  if (byteLength === null || offset + 1 + byteLength > bytes.length) {
    return null;
  }
  let value = 0n;
  for (let index = 0; index < byteLength; index += 1) {
    value = (value << 8n) | BigInt(bytes[offset + 1 + index]!);
  }
  if (
    (additional === 24 && value < 24n) ||
    (additional === 25 && value <= 0xffn) ||
    (additional === 26 && value <= 0xffffn) ||
    (additional === 27 && value <= 0xffff_ffffn)
  ) {
    return null;
  }
  return {
    major,
    value,
    nextOffset: offset + 1 + byteLength,
  };
};

const unsignedByteLength = (value: bigint): bigint => {
  let size = 1n;
  let remaining = value;
  while (remaining >= 256n) {
    size += 1n;
    remaining >>= 8n;
  }
  return size;
};

/**
 * Returns the exact complete Data memory for a canonical integer item.
 * `syntaxBytes` is the authenticated prefix requested by this machine.
 */
export const parseMidgardCekDataIntegerSyntaxV1 = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): bigint | null => {
  try {
    if (
      !Number.isInteger(sourceLength) ||
      sourceLength < 1 ||
      sourceLength > UINT32_MAX ||
      syntaxBytes.length !==
        Math.min(sourceLength, MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES)
    ) {
      return null;
    }
    const first = syntaxBytes[0]!;
    if (first >>> 5 <= 1) {
      const argument = readCanonicalCborArgument(syntaxBytes, 0);
      if (
        argument === null ||
        argument.major > 1 ||
        argument.value > UINT64_MAX ||
        argument.nextOffset !== sourceLength
      ) {
        return null;
      }
      return 4n + unsignedByteLength(argument.value * 2n);
    }
    if (first !== 0xc2 && first !== 0xc3) return null;
    const magnitude = readCanonicalCborArgument(syntaxBytes, 1);
    if (
      magnitude === null ||
      magnitude.major !== 2 ||
      magnitude.value < 9n ||
      magnitude.value > BigInt(UINT32_MAX) ||
      BigInt(magnitude.nextOffset) + magnitude.value !== BigInt(sourceLength) ||
      magnitude.nextOffset >= syntaxBytes.length
    ) {
      return null;
    }
    const firstMagnitudeByte = syntaxBytes[magnitude.nextOffset]!;
    if (firstMagnitudeByte === 0) return null;
    return 4n + magnitude.value + (firstMagnitudeByte >= 0x80 ? 1n : 0n);
  } catch {
    return null;
  }
};

/**
 * Restricts the integer grammar to a canonical nonnegative constructor
 * alternative above 127. Positive bignums are accepted without materializing
 * their value; major-one and tag-three encodings fail closed.
 */
export const parseMidgardCekDataLargeConstructorSyntaxV1 = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): bigint | null => {
  const memory = parseMidgardCekDataIntegerSyntaxV1({
    syntaxBytes,
    sourceLength,
  });
  if (memory === null) return null;
  const first = syntaxBytes[0]!;
  if (first === 0xc2) return memory;
  const argument = readCanonicalCborArgument(syntaxBytes, 0);
  return argument !== null &&
    argument.major === 0 &&
    argument.value > 127n &&
    argument.nextOffset === sourceLength
    ? memory
    : null;
};

export const isWellFormedMidgardCekDataIntegerControlV1 = (
  control: MidgardCekDataIntegerControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_INTEGER_V1_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataIntegerStagesV1.Syntax ||
      control.stage > MidgardCekDataIntegerStagesV1.Terminal ||
      exactSourceCoordinate(control.sourceStart, "source start") !==
        control.sourceStart ||
      !Number.isInteger(control.sourceLength) ||
      control.sourceLength < 1 ||
      control.sourceLength > UINT32_MAX ||
      !Number.isSafeInteger(control.sourceStart + control.sourceLength) ||
      control.memory < 0n ||
      control.memory > UINT64_MAX
    ) {
      return false;
    }
    if (control.stage === MidgardCekDataIntegerStagesV1.Syntax) {
      return control.memory === 0n && control.blob === null;
    }
    if (
      control.memory < 5n ||
      control.blob === null ||
      !isWellFormedMidgardCekSourceBlobControlV1(control.blob) ||
      control.blob.sourceStart !== control.sourceStart ||
      control.blob.sourceLength !== control.sourceLength
    ) {
      return false;
    }
    return (
      control.stage !== MidgardCekDataIntegerStagesV1.Terminal ||
      control.blob.stage === MidgardCekSourceBlobStagesV1.Terminal
    );
  } catch {
    return false;
  }
};

export const initialMidgardCekDataIntegerControlV1 = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataIntegerControlV1 => {
  const control = {
    version: MIDGARD_CEK_DATA_INTEGER_V1_VERSION,
    stage: MidgardCekDataIntegerStagesV1.Syntax,
    sourceStart,
    sourceLength,
    memory: 0n,
    blob: null,
  } satisfies MidgardCekDataIntegerControlV1;
  if (!isWellFormedMidgardCekDataIntegerControlV1(control)) {
    throw new Error("Invalid V1 CEK Data integer range");
  }
  return control;
};

const optionalBlobDataCbor = (
  blob: MidgardCekSourceBlobControlV1 | null,
): Buffer =>
  blob === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekSourceBlobControlV1(blob),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekDataIntegerControlV1 = (
  control: MidgardCekDataIntegerControlV1,
): Buffer => {
  if (!isWellFormedMidgardCekDataIntegerControlV1(control)) {
    throw new Error("Invalid V1 CEK Data integer control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_DATA_INTEGER_V1_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(control.memory),
    optionalBlobDataCbor(control.blob),
  ]);
};

export const nextMidgardCekDataIntegerSpanV1 = (
  control: MidgardCekDataIntegerControlV1,
): MidgardCekSourceBlobSpanV1 | null => {
  if (!isWellFormedMidgardCekDataIntegerControlV1(control)) {
    return null;
  }
  if (control.stage === MidgardCekDataIntegerStagesV1.Syntax) {
    return {
      absoluteStart: control.sourceStart,
      length: Math.min(
        control.sourceLength,
        MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
      ),
    };
  }
  return control.stage === MidgardCekDataIntegerStagesV1.Blob
    ? nextMidgardCekSourceBlobSpanV1(control.blob!)
    : null;
};

export const advanceMidgardCekDataIntegerV1 = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataIntegerControlV1;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekDataIntegerControlV1 | null => {
  try {
    if (!isWellFormedMidgardCekDataIntegerControlV1(control)) {
      return null;
    }
    if (control.stage === MidgardCekDataIntegerStagesV1.Syntax) {
      const span = nextMidgardCekDataIntegerSpanV1(control)!;
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== span.length
      ) {
        return null;
      }
      const memory = parseMidgardCekDataIntegerSyntaxV1({
        syntaxBytes: sourceBytes,
        sourceLength: control.sourceLength,
      });
      if (memory === null) return null;
      const next = {
        ...control,
        stage: MidgardCekDataIntegerStagesV1.Blob,
        memory,
        blob: initialMidgardCekSourceBlobControlV1({
          sourceStart: control.sourceStart,
          sourceLength: control.sourceLength,
        }),
      } satisfies MidgardCekDataIntegerControlV1;
      return isWellFormedMidgardCekDataIntegerControlV1(next) ? next : null;
    }
    if (
      control.stage !== MidgardCekDataIntegerStagesV1.Blob ||
      control.blob === null
    ) {
      return null;
    }
    if (control.blob.stage === MidgardCekSourceBlobStagesV1.Terminal) {
      if (sourceBytes !== null && sourceBytes !== undefined) {
        return null;
      }
      const next = {
        ...control,
        stage: MidgardCekDataIntegerStagesV1.Terminal,
      } satisfies MidgardCekDataIntegerControlV1;
      return isWellFormedMidgardCekDataIntegerControlV1(next) ? next : null;
    }
    const blob = advanceMidgardCekSourceBlobV1({
      control: control.blob,
      sourceBytes,
    });
    if (blob === null) return null;
    const next = { ...control, blob };
    return isWellFormedMidgardCekDataIntegerControlV1(next) ? next : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataIntegerV1 = (
  control: MidgardCekDataIntegerControlV1,
): MidgardCekDataIntegerSummaryV1 | null => {
  if (
    !isWellFormedMidgardCekDataIntegerControlV1(control) ||
    control.stage !== MidgardCekDataIntegerStagesV1.Terminal
  ) {
    return null;
  }
  const cborRoot = finalizeMidgardCekSourceBlobV1(control.blob!);
  if (cborRoot === null) return null;
  return Object.freeze({
    root: Buffer.from(
      hashMidgardCekDataNodeV1({
        kind: "integer",
        cborRoot,
        cborLength: BigInt(control.sourceLength),
        memory: control.memory,
      }),
    ),
    cborLength: BigInt(control.sourceLength),
    memory: control.memory,
  });
};

export const buildMidgardCekDataIntegerTraceV1 = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataIntegerTraceV1 => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekDataIntegerControlV1({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataIntegerTraceStepV1[] = [];
  let control = initial;
  while (control.stage !== MidgardCekDataIntegerStagesV1.Terminal) {
    const span = nextMidgardCekDataIntegerSpanV1(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataIntegerV1({
      control,
      sourceBytes,
    });
    if (next === null || !isWellFormedMidgardCekDataIntegerControlV1(next)) {
      throw new Error("V1 CEK Data integer trace failed closed");
    }
    steps.push({ control, sourceBytes, next });
    control = next;
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};

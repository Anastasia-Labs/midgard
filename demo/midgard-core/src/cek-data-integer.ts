import { hashMidgardCekDataNode } from "./cek-semantic.js";
import {
  advanceMidgardCekSourceBlob,
  encodeMidgardCekSourceBlobControl,
  finalizeMidgardCekSourceBlob,
  initialMidgardCekSourceBlobControl,
  isWellFormedMidgardCekSourceBlobControl,
  type MidgardCekSourceBlobControl,
  type MidgardCekSourceBlobSpan,
  MidgardCekSourceBlobStages,
  nextMidgardCekSourceBlobSpan,
} from "./cek-source-blob.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";

export const MIDGARD_CEK_DATA_INTEGER_VERSION = 1 as const;

export const MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES = 14;

const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MidgardCekDataIntegerStages = Object.freeze({
  Syntax: 0,
  Blob: 1,
  Terminal: 2,
} as const);

export type MidgardCekDataIntegerStage =
  (typeof MidgardCekDataIntegerStages)[keyof typeof MidgardCekDataIntegerStages];

/**
 * Proves one canonical Cardano Data integer encoding without materializing an
 * unbounded integer in the L1 validator. The parent must authenticate every
 * source span returned by `nextMidgardCekDataIntegerSpan`.
 */
export type MidgardCekDataIntegerControl = {
  readonly version: typeof MIDGARD_CEK_DATA_INTEGER_VERSION;
  readonly stage: MidgardCekDataIntegerStage;
  readonly sourceStart: number;
  readonly sourceLength: number;
  /** Complete CEK Data memory, including the four-word Data node overhead. */
  readonly memory: bigint;
  readonly blob: MidgardCekSourceBlobControl | null;
};

export type MidgardCekDataIntegerSummary = {
  readonly root: Buffer;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataIntegerTraceStep = {
  readonly control: MidgardCekDataIntegerControl;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekDataIntegerControl;
};

export type MidgardCekDataIntegerTrace = {
  readonly initial: MidgardCekDataIntegerControl;
  readonly steps: readonly MidgardCekDataIntegerTraceStep[];
  readonly terminal: MidgardCekDataIntegerControl;
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
export const parseMidgardCekDataIntegerSyntax = ({
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
export const parseMidgardCekDataLargeConstructorSyntax = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): bigint | null => {
  const memory = parseMidgardCekDataIntegerSyntax({
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

export const isWellFormedMidgardCekDataIntegerControl = (
  control: MidgardCekDataIntegerControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_INTEGER_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataIntegerStages.Syntax ||
      control.stage > MidgardCekDataIntegerStages.Terminal ||
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
    if (control.stage === MidgardCekDataIntegerStages.Syntax) {
      return control.memory === 0n && control.blob === null;
    }
    if (
      control.memory < 5n ||
      control.blob === null ||
      !isWellFormedMidgardCekSourceBlobControl(control.blob) ||
      control.blob.sourceStart !== control.sourceStart ||
      control.blob.sourceLength !== control.sourceLength
    ) {
      return false;
    }
    return (
      control.stage !== MidgardCekDataIntegerStages.Terminal ||
      control.blob.stage === MidgardCekSourceBlobStages.Terminal
    );
  } catch {
    return false;
  }
};

export const initialMidgardCekDataIntegerControl = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataIntegerControl => {
  const control = {
    version: MIDGARD_CEK_DATA_INTEGER_VERSION,
    stage: MidgardCekDataIntegerStages.Syntax,
    sourceStart,
    sourceLength,
    memory: 0n,
    blob: null,
  } satisfies MidgardCekDataIntegerControl;
  if (!isWellFormedMidgardCekDataIntegerControl(control)) {
    throw new Error("Invalid V1 CEK Data integer range");
  }
  return control;
};

const optionalBlobDataCbor = (
  blob: MidgardCekSourceBlobControl | null,
): Buffer =>
  blob === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekSourceBlobControl(blob),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekDataIntegerControl = (
  control: MidgardCekDataIntegerControl,
): Buffer => {
  if (!isWellFormedMidgardCekDataIntegerControl(control)) {
    throw new Error("Invalid V1 CEK Data integer control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_DATA_INTEGER_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(control.memory),
    optionalBlobDataCbor(control.blob),
  ]);
};

export const nextMidgardCekDataIntegerSpan = (
  control: MidgardCekDataIntegerControl,
): MidgardCekSourceBlobSpan | null => {
  if (!isWellFormedMidgardCekDataIntegerControl(control)) {
    return null;
  }
  if (control.stage === MidgardCekDataIntegerStages.Syntax) {
    return {
      absoluteStart: control.sourceStart,
      length: Math.min(
        control.sourceLength,
        MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
      ),
    };
  }
  return control.stage === MidgardCekDataIntegerStages.Blob
    ? nextMidgardCekSourceBlobSpan(control.blob!)
    : null;
};

export const advanceMidgardCekDataInteger = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataIntegerControl;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekDataIntegerControl | null => {
  try {
    if (!isWellFormedMidgardCekDataIntegerControl(control)) {
      return null;
    }
    if (control.stage === MidgardCekDataIntegerStages.Syntax) {
      const span = nextMidgardCekDataIntegerSpan(control)!;
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== span.length
      ) {
        return null;
      }
      const memory = parseMidgardCekDataIntegerSyntax({
        syntaxBytes: sourceBytes,
        sourceLength: control.sourceLength,
      });
      if (memory === null) return null;
      const next = {
        ...control,
        stage: MidgardCekDataIntegerStages.Blob,
        memory,
        blob: initialMidgardCekSourceBlobControl({
          sourceStart: control.sourceStart,
          sourceLength: control.sourceLength,
        }),
      } satisfies MidgardCekDataIntegerControl;
      return isWellFormedMidgardCekDataIntegerControl(next) ? next : null;
    }
    if (
      control.stage !== MidgardCekDataIntegerStages.Blob ||
      control.blob === null
    ) {
      return null;
    }
    if (control.blob.stage === MidgardCekSourceBlobStages.Terminal) {
      if (sourceBytes !== null && sourceBytes !== undefined) {
        return null;
      }
      const next = {
        ...control,
        stage: MidgardCekDataIntegerStages.Terminal,
      } satisfies MidgardCekDataIntegerControl;
      return isWellFormedMidgardCekDataIntegerControl(next) ? next : null;
    }
    const blob = advanceMidgardCekSourceBlob({
      control: control.blob,
      sourceBytes,
    });
    if (blob === null) return null;
    const next = { ...control, blob };
    return isWellFormedMidgardCekDataIntegerControl(next) ? next : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataInteger = (
  control: MidgardCekDataIntegerControl,
): MidgardCekDataIntegerSummary | null => {
  if (
    !isWellFormedMidgardCekDataIntegerControl(control) ||
    control.stage !== MidgardCekDataIntegerStages.Terminal
  ) {
    return null;
  }
  const cborRoot = finalizeMidgardCekSourceBlob(control.blob!);
  if (cborRoot === null) return null;
  return Object.freeze({
    root: Buffer.from(
      hashMidgardCekDataNode({
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

export const buildMidgardCekDataIntegerTrace = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataIntegerTrace => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekDataIntegerControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataIntegerTraceStep[] = [];
  let control = initial;
  while (control.stage !== MidgardCekDataIntegerStages.Terminal) {
    const span = nextMidgardCekDataIntegerSpan(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataInteger({
      control,
      sourceBytes,
    });
    if (next === null || !isWellFormedMidgardCekDataIntegerControl(next)) {
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

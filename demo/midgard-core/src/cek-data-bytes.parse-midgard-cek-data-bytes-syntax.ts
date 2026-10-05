import { midgardCekDataBytesCborLength } from "./cek-semantic.js";
import {
  encodeMidgardCekSourceBlobControl,
  isWellFormedMidgardCekSourceBlobControl,
  type MidgardCekSourceBlobControl,
  type MidgardCekSourceBlobSpan,
  MidgardCekSourceBlobStages,
} from "./cek-source-blob.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";

export const MIDGARD_CEK_DATA_BYTES_VERSION = 1 as const;

export const MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES = 2;

export const MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN = 132;

const UINT32_MAX = 0xffff_ffff;

export const CARDANO_DATA_BYTES_CHUNK = 64;

/**
 * `Measure` is the indefinite byte string's measuring pass: from the
 * authenticated 0x5f opener each step reads the next chunk header and extends
 * `sourceLength` by that chunk, until the authenticated 0xff break fixes the
 * encoding length. The length is never a witness value.
 */
export const MidgardCekDataBytesStages = Object.freeze({
  Syntax: 0,
  Blob: 1,
  Measure: 2,
  Terminal: 3,
} as const);

export type MidgardCekDataBytesStage =
  (typeof MidgardCekDataBytesStages)[keyof typeof MidgardCekDataBytesStages];

/**
 * Proves one canonical Cardano Data byte-string encoding and commits only its
 * raw byte content into the CEK blob tree. The parent authenticates every raw
 * CBOR span returned by `nextMidgardCekDataBytesSpan`.
 */
export type MidgardCekDataBytesControl = {
  readonly version: typeof MIDGARD_CEK_DATA_BYTES_VERSION;
  readonly stage: MidgardCekDataBytesStage;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly bytesLength: number;
  /** Uses virtual source coordinates 0..bytesLength for extracted content. */
  readonly blob: MidgardCekSourceBlobControl | null;
};

export type MidgardCekDataBytesSummary = {
  readonly root: Buffer;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataBytesTraceStep = {
  readonly control: MidgardCekDataBytesControl;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekDataBytesControl;
};

export type MidgardCekDataBytesTrace = {
  readonly initial: MidgardCekDataBytesControl;
  readonly steps: readonly MidgardCekDataBytesTraceStep[];
  readonly terminal: MidgardCekDataBytesControl;
};

export type ContentSegment = {
  readonly header: Buffer;
  readonly contentLength: number;
};

export type ContentPlan = {
  readonly span: MidgardCekSourceBlobSpan;
  readonly segments: readonly ContentSegment[];
};

const exactSourceCoordinate = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 CEK Data bytes ${field}`);
  }
  return value;
};

export const definiteBytesHeader = (length: number): Buffer => {
  if (
    !Number.isInteger(length) ||
    length < 0 ||
    length > CARDANO_DATA_BYTES_CHUNK
  ) {
    throw new Error("Invalid V1 CEK Data bytes chunk length");
  }
  return length < 24
    ? Buffer.from([0x40 + length])
    : Buffer.from([0x58, length]);
};

export const definiteHeaderLength = (length: number): number =>
  definiteBytesHeader(length).length;

const canonicalCborLength = (bytesLength: number): number => {
  const length = midgardCekDataBytesCborLength(BigInt(bytesLength));
  if (length > BigInt(UINT32_MAX)) {
    throw new Error("V1 CEK Data bytes CBOR length exceeds uint32");
  }
  return Number(length);
};

/**
 * The content length of a canonical indefinite byte string whose complete
 * encoding (0x5f opener through the 0xff break) is `sourceLength` bytes.
 */
export const indefiniteMidgardCekDataBytesLength = (
  sourceLength: number,
): number | null => {
  try {
    if (!Number.isInteger(sourceLength) || sourceLength < 2) return null;
    const framedPayloadLength = sourceLength - 2;
    const fullChunks = Math.floor(
      framedPayloadLength / (CARDANO_DATA_BYTES_CHUNK + 2),
    );
    const encodedRemainder =
      framedPayloadLength % (CARDANO_DATA_BYTES_CHUNK + 2);
    const remainder =
      encodedRemainder === 0
        ? 0
        : encodedRemainder >= 2 && encodedRemainder <= 24
          ? encodedRemainder - 1
          : encodedRemainder >= 26 && encodedRemainder <= 65
            ? encodedRemainder - 2
            : null;
    if (remainder === null) return null;
    const bytesLength = fullChunks * CARDANO_DATA_BYTES_CHUNK + remainder;
    return bytesLength > CARDANO_DATA_BYTES_CHUNK &&
      bytesLength <= UINT32_MAX &&
      canonicalCborLength(bytesLength) === sourceLength
      ? bytesLength
      : null;
  } catch {
    return null;
  }
};

/**
 * The definite byte-string syntax: derives the raw content length from the
 * derived item length and the authenticated framing prefix. An indefinite
 * (0x5f) string never reaches this stage; it is measured instead.
 */
export const parseMidgardCekDataBytesSyntax = ({
  syntaxBytes,
  sourceLength,
}: {
  readonly syntaxBytes: Uint8Array;
  readonly sourceLength: number;
}): number | null => {
  try {
    if (
      !Number.isInteger(sourceLength) ||
      sourceLength < 1 ||
      sourceLength > UINT32_MAX ||
      syntaxBytes.length !==
        Math.min(sourceLength, MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES)
    ) {
      return null;
    }
    const first = syntaxBytes[0]!;
    if (first >= 0x40 && first <= 0x57) {
      const bytesLength = first - 0x40;
      return sourceLength === 1 + bytesLength ? bytesLength : null;
    }
    if (first === 0x58) {
      if (syntaxBytes.length < 2) return null;
      const bytesLength = syntaxBytes[1]!;
      return bytesLength >= 24 &&
        bytesLength <= CARDANO_DATA_BYTES_CHUNK &&
        sourceLength === 2 + bytesLength
        ? bytesLength
        : null;
    }
    return null;
  } catch {
    return null;
  }
};

export const isWellFormedMidgardCekDataBytesControl = (
  control: MidgardCekDataBytesControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_BYTES_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataBytesStages.Syntax ||
      control.stage > MidgardCekDataBytesStages.Terminal ||
      exactSourceCoordinate(control.sourceStart, "source start") !==
        control.sourceStart ||
      !Number.isInteger(control.sourceLength) ||
      control.sourceLength < 1 ||
      control.sourceLength > UINT32_MAX ||
      !Number.isSafeInteger(control.sourceStart + control.sourceLength) ||
      !Number.isInteger(control.bytesLength) ||
      control.bytesLength < 0 ||
      control.bytesLength > UINT32_MAX
    ) {
      return false;
    }
    if (
      control.stage === MidgardCekDataBytesStages.Syntax ||
      control.stage === MidgardCekDataBytesStages.Measure
    ) {
      return control.bytesLength === 0 && control.blob === null;
    }
    if (
      canonicalCborLength(control.bytesLength) !== control.sourceLength ||
      control.blob === null ||
      !isWellFormedMidgardCekSourceBlobControl(control.blob) ||
      control.blob.sourceStart !== 0 ||
      control.blob.sourceLength !== control.bytesLength
    ) {
      return false;
    }
    return (
      control.stage !== MidgardCekDataBytesStages.Terminal ||
      control.blob.stage === MidgardCekSourceBlobStages.Terminal
    );
  } catch {
    return false;
  }
};

export const initialMidgardCekDataBytesControl = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataBytesControl => {
  const control = {
    version: MIDGARD_CEK_DATA_BYTES_VERSION,
    stage: MidgardCekDataBytesStages.Syntax,
    sourceStart,
    sourceLength,
    bytesLength: 0,
    blob: null,
  } satisfies MidgardCekDataBytesControl;
  if (!isWellFormedMidgardCekDataBytesControl(control)) {
    throw new Error("Invalid V1 CEK Data bytes range");
  }
  return control;
};

/** The measuring control of an indefinite byte string whose 0x5f opener sits
 * at `sourceStart`. */
export const initialMidgardCekDataBytesMeasureControl = ({
  sourceStart,
}: {
  readonly sourceStart: number;
}): MidgardCekDataBytesControl => {
  const control = {
    version: MIDGARD_CEK_DATA_BYTES_VERSION,
    stage: MidgardCekDataBytesStages.Measure,
    sourceStart,
    sourceLength: 1,
    bytesLength: 0,
    blob: null,
  } satisfies MidgardCekDataBytesControl;
  if (!isWellFormedMidgardCekDataBytesControl(control)) {
    throw new Error("Invalid V1 CEK Data bytes range");
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

export const encodeMidgardCekDataBytesControl = (
  control: MidgardCekDataBytesControl,
): Buffer => {
  if (!isWellFormedMidgardCekDataBytesControl(control)) {
    throw new Error("Invalid V1 CEK Data bytes control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_DATA_BYTES_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(BigInt(control.bytesLength)),
    optionalBlobDataCbor(control.blob),
  ]);
};

export const rawContentPosition = ({
  control,
  contentOffset,
}: {
  readonly control: MidgardCekDataBytesControl;
  readonly contentOffset: number;
}): number => {
  if (control.bytesLength <= CARDANO_DATA_BYTES_CHUNK) {
    return definiteHeaderLength(control.bytesLength) + contentOffset;
  }
  if (contentOffset === control.bytesLength) {
    return control.sourceLength - 1;
  }
  const chunkIndex = Math.floor(contentOffset / CARDANO_DATA_BYTES_CHUNK);
  const chunkStart = chunkIndex * CARDANO_DATA_BYTES_CHUNK;
  const withinChunk = contentOffset - chunkStart;
  const chunkLength = Math.min(
    CARDANO_DATA_BYTES_CHUNK,
    control.bytesLength - chunkStart,
  );
  const headerStart = 1 + chunkIndex * (CARDANO_DATA_BYTES_CHUNK + 2);
  return headerStart + definiteHeaderLength(chunkLength) + withinChunk;
};

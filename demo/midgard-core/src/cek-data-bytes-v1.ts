import {
  hashMidgardCekDataNodeV1,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataBytesMemoryV1,
} from "./cek-semantic.js";
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

export const MIDGARD_CEK_DATA_BYTES_V1_VERSION = 1 as const;

export const MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES = 2;

export const MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN = 132;

const UINT32_MAX = 0xffff_ffff;
const CARDANO_DATA_BYTES_CHUNK = 64;

export const MidgardCekDataBytesStagesV1 = Object.freeze({
  Syntax: 0,
  Blob: 1,
  Break: 2,
  Terminal: 3,
} as const);

export type MidgardCekDataBytesStageV1 =
  (typeof MidgardCekDataBytesStagesV1)[keyof typeof MidgardCekDataBytesStagesV1];

/**
 * Proves one canonical Cardano Data byte-string encoding and commits only its
 * raw byte content into the CEK blob tree. The parent authenticates every raw
 * CBOR span returned by `nextMidgardCekDataBytesSpanV1`.
 */
export type MidgardCekDataBytesControlV1 = {
  readonly version: typeof MIDGARD_CEK_DATA_BYTES_V1_VERSION;
  readonly stage: MidgardCekDataBytesStageV1;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly bytesLength: number;
  /** Uses virtual source coordinates 0..bytesLength for extracted content. */
  readonly blob: MidgardCekSourceBlobControlV1 | null;
};

export type MidgardCekDataBytesSummaryV1 = {
  readonly root: Buffer;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataBytesTraceStepV1 = {
  readonly control: MidgardCekDataBytesControlV1;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekDataBytesControlV1;
};

export type MidgardCekDataBytesTraceV1 = {
  readonly initial: MidgardCekDataBytesControlV1;
  readonly steps: readonly MidgardCekDataBytesTraceStepV1[];
  readonly terminal: MidgardCekDataBytesControlV1;
};

type ContentSegment = {
  readonly header: Buffer;
  readonly contentLength: number;
};

type ContentPlan = {
  readonly span: MidgardCekSourceBlobSpanV1;
  readonly segments: readonly ContentSegment[];
};

const exactSourceCoordinate = (
  value: number,
  field: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 CEK Data bytes ${field}`);
  }
  return value;
};

const definiteBytesHeader = (length: number): Buffer => {
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

const definiteHeaderLength = (length: number): number =>
  definiteBytesHeader(length).length;

const canonicalCborLength = (bytesLength: number): number => {
  const length = midgardCekDataBytesCborLengthV1(
    BigInt(bytesLength),
  );
  if (length > BigInt(UINT32_MAX)) {
    throw new Error("V1 CEK Data bytes CBOR length exceeds uint32");
  }
  return Number(length);
};

/**
 * Derives the raw content length from the independently committed item length
 * and an authenticated canonical framing prefix.
 */
export const parseMidgardCekDataBytesSyntaxV1 = ({
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
        Math.min(
          sourceLength,
          MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
        )
    ) {
      return null;
    }
    const first = syntaxBytes[0]!;
    if (first >= 0x40 && first <= 0x57) {
      const bytesLength = first - 0x40;
      return sourceLength === 1 + bytesLength
        ? bytesLength
        : null;
    }
    if (first === 0x58) {
      if (syntaxBytes.length < 2) return null;
      const bytesLength = syntaxBytes[1]!;
      return (
        bytesLength >= 24 &&
        bytesLength <= CARDANO_DATA_BYTES_CHUNK &&
        sourceLength === 2 + bytesLength
      )
        ? bytesLength
        : null;
    }
    if (first !== 0x5f || sourceLength < 2) return null;
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
    const bytesLength =
      fullChunks * CARDANO_DATA_BYTES_CHUNK + remainder;
    return (
      bytesLength > CARDANO_DATA_BYTES_CHUNK &&
      bytesLength <= UINT32_MAX &&
      canonicalCborLength(bytesLength) === sourceLength
    )
      ? bytesLength
      : null;
  } catch {
    return null;
  }
};

export const isWellFormedMidgardCekDataBytesControlV1 = (
  control: MidgardCekDataBytesControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_BYTES_V1_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataBytesStagesV1.Syntax ||
      control.stage > MidgardCekDataBytesStagesV1.Terminal ||
      exactSourceCoordinate(
        control.sourceStart,
        "source start",
      ) !== control.sourceStart ||
      !Number.isInteger(control.sourceLength) ||
      control.sourceLength < 1 ||
      control.sourceLength > UINT32_MAX ||
      !Number.isSafeInteger(
        control.sourceStart + control.sourceLength,
      ) ||
      !Number.isInteger(control.bytesLength) ||
      control.bytesLength < 0 ||
      control.bytesLength > UINT32_MAX
    ) {
      return false;
    }
    if (control.stage === MidgardCekDataBytesStagesV1.Syntax) {
      return control.bytesLength === 0 && control.blob === null;
    }
    if (
      canonicalCborLength(control.bytesLength) !==
        control.sourceLength ||
      control.blob === null ||
      !isWellFormedMidgardCekSourceBlobControlV1(control.blob) ||
      control.blob.sourceStart !== 0 ||
      control.blob.sourceLength !== control.bytesLength
    ) {
      return false;
    }
    if (control.stage === MidgardCekDataBytesStagesV1.Break) {
      return (
        control.bytesLength > CARDANO_DATA_BYTES_CHUNK &&
        control.blob.stage === MidgardCekSourceBlobStagesV1.Terminal
      );
    }
    return (
      control.stage !== MidgardCekDataBytesStagesV1.Terminal ||
      control.blob.stage === MidgardCekSourceBlobStagesV1.Terminal
    );
  } catch {
    return false;
  }
};

export const initialMidgardCekDataBytesControlV1 = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekDataBytesControlV1 => {
  const control = {
    version: MIDGARD_CEK_DATA_BYTES_V1_VERSION,
    stage: MidgardCekDataBytesStagesV1.Syntax,
    sourceStart,
    sourceLength,
    bytesLength: 0,
    blob: null,
  } satisfies MidgardCekDataBytesControlV1;
  if (!isWellFormedMidgardCekDataBytesControlV1(control)) {
    throw new Error("Invalid V1 CEK Data bytes range");
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

export const encodeMidgardCekDataBytesControlV1 = (
  control: MidgardCekDataBytesControlV1,
): Buffer => {
  if (!isWellFormedMidgardCekDataBytesControlV1(control)) {
    throw new Error("Invalid V1 CEK Data bytes control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_DATA_BYTES_V1_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(BigInt(control.bytesLength)),
    optionalBlobDataCbor(control.blob),
  ]);
};

const rawContentPosition = ({
  control,
  contentOffset,
}: {
  readonly control: MidgardCekDataBytesControlV1;
  readonly contentOffset: number;
}): number => {
  if (control.bytesLength <= CARDANO_DATA_BYTES_CHUNK) {
    return (
      definiteHeaderLength(control.bytesLength) + contentOffset
    );
  }
  if (contentOffset === control.bytesLength) {
    return control.sourceLength - 1;
  }
  const chunkIndex = Math.floor(
    contentOffset / CARDANO_DATA_BYTES_CHUNK,
  );
  const chunkStart = chunkIndex * CARDANO_DATA_BYTES_CHUNK;
  const withinChunk = contentOffset - chunkStart;
  const chunkLength = Math.min(
    CARDANO_DATA_BYTES_CHUNK,
    control.bytesLength - chunkStart,
  );
  const headerStart =
    1 + chunkIndex * (CARDANO_DATA_BYTES_CHUNK + 2);
  return (
    headerStart +
    definiteHeaderLength(chunkLength) +
    withinChunk
  );
};

const contentPlan = (
  control: MidgardCekDataBytesControlV1,
): ContentPlan | null => {
  if (
    control.stage !== MidgardCekDataBytesStagesV1.Blob ||
    control.blob === null
  ) {
    return null;
  }
  const virtualSpan = nextMidgardCekSourceBlobSpanV1(
    control.blob,
  );
  if (virtualSpan === null) return null;
  const contentStart = virtualSpan.absoluteStart;
  const contentEnd = contentStart + virtualSpan.length;
  if (
    contentStart < 0 ||
    contentEnd > control.bytesLength
  ) {
    return null;
  }
  if (control.bytesLength <= CARDANO_DATA_BYTES_CHUNK) {
    return {
      span: {
        absoluteStart:
          control.sourceStart +
          rawContentPosition({ control, contentOffset: contentStart }),
        length: virtualSpan.length,
      },
      segments: [
        {
          header: Buffer.alloc(0),
          contentLength: virtualSpan.length,
        },
      ],
    };
  }
  if (virtualSpan.length === 0) {
    return {
      span: {
        absoluteStart:
          control.sourceStart +
          rawContentPosition({ control, contentOffset: contentStart }),
        length: 0,
      },
      segments: [],
    };
  }
  const segments: ContentSegment[] = [];
  let cursor = contentStart;
  let remaining = virtualSpan.length;
  while (remaining > 0) {
    const chunkStart =
      Math.floor(cursor / CARDANO_DATA_BYTES_CHUNK) *
      CARDANO_DATA_BYTES_CHUNK;
    const withinChunk = cursor - chunkStart;
    const chunkLength = Math.min(
      CARDANO_DATA_BYTES_CHUNK,
      control.bytesLength - chunkStart,
    );
    const take = Math.min(
      remaining,
      chunkLength - withinChunk,
    );
    if (take <= 0) return null;
    segments.push({
      header:
        withinChunk === 0
          ? definiteBytesHeader(chunkLength)
          : Buffer.alloc(0),
      contentLength: take,
    });
    cursor += take;
    remaining -= take;
  }
  const firstChunkStart =
    Math.floor(contentStart / CARDANO_DATA_BYTES_CHUNK) *
    CARDANO_DATA_BYTES_CHUNK;
  const firstWithinChunk = contentStart - firstChunkStart;
  const firstChunkIndex =
    firstChunkStart / CARDANO_DATA_BYTES_CHUNK;
  const firstChunkLength = Math.min(
    CARDANO_DATA_BYTES_CHUNK,
    control.bytesLength - firstChunkStart,
  );
  const firstHeaderStart =
    1 +
    firstChunkIndex * (CARDANO_DATA_BYTES_CHUNK + 2);
  const relativeStart =
    firstWithinChunk === 0
      ? firstHeaderStart
      : firstHeaderStart +
        definiteHeaderLength(firstChunkLength) +
        firstWithinChunk;
  const rawLength = segments.reduce(
    (length, segment) =>
      length + segment.header.length + segment.contentLength,
    0,
  );
  if (rawLength > MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN) {
    return null;
  }
  return {
    span: {
      absoluteStart: control.sourceStart + relativeStart,
      length: rawLength,
    },
    segments,
  };
};

const extractContent = ({
  plan,
  sourceBytes,
}: {
  readonly plan: ContentPlan;
  readonly sourceBytes: Uint8Array;
}): Buffer | null => {
  if (sourceBytes.length !== plan.span.length) return null;
  const source = Buffer.from(sourceBytes);
  const content: Buffer[] = [];
  let cursor = 0;
  for (const segment of plan.segments) {
    if (
      !source
        .subarray(cursor, cursor + segment.header.length)
        .equals(segment.header)
    ) {
      return null;
    }
    cursor += segment.header.length;
    content.push(
      source.subarray(cursor, cursor + segment.contentLength),
    );
    cursor += segment.contentLength;
  }
  return cursor === source.length ? Buffer.concat(content) : null;
};

export const nextMidgardCekDataBytesSpanV1 = (
  control: MidgardCekDataBytesControlV1,
): MidgardCekSourceBlobSpanV1 | null => {
  if (!isWellFormedMidgardCekDataBytesControlV1(control)) {
    return null;
  }
  if (control.stage === MidgardCekDataBytesStagesV1.Syntax) {
    return {
      absoluteStart: control.sourceStart,
      length: Math.min(
        control.sourceLength,
        MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
      ),
    };
  }
  if (control.stage === MidgardCekDataBytesStagesV1.Break) {
    return {
      absoluteStart:
        control.sourceStart + control.sourceLength - 1,
      length: 1,
    };
  }
  return contentPlan(control)?.span ?? null;
};

export const advanceMidgardCekDataBytesV1 = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataBytesControlV1;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekDataBytesControlV1 | null => {
  try {
    if (!isWellFormedMidgardCekDataBytesControlV1(control)) {
      return null;
    }
    if (control.stage === MidgardCekDataBytesStagesV1.Syntax) {
      const span = nextMidgardCekDataBytesSpanV1(control)!;
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== span.length
      ) {
        return null;
      }
      const bytesLength = parseMidgardCekDataBytesSyntaxV1({
        syntaxBytes: sourceBytes,
        sourceLength: control.sourceLength,
      });
      if (bytesLength === null) return null;
      const next = {
        ...control,
        stage: MidgardCekDataBytesStagesV1.Blob,
        bytesLength,
        blob: initialMidgardCekSourceBlobControlV1({
          sourceStart: 0,
          sourceLength: bytesLength,
        }),
      } satisfies MidgardCekDataBytesControlV1;
      return isWellFormedMidgardCekDataBytesControlV1(next)
        ? next
        : null;
    }
    if (
      control.stage === MidgardCekDataBytesStagesV1.Break
    ) {
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== 1 ||
        sourceBytes[0] !== 0xff
      ) {
        return null;
      }
      const next = {
        ...control,
        stage: MidgardCekDataBytesStagesV1.Terminal,
      } satisfies MidgardCekDataBytesControlV1;
      return isWellFormedMidgardCekDataBytesControlV1(next)
        ? next
        : null;
    }
    if (
      control.stage !== MidgardCekDataBytesStagesV1.Blob ||
      control.blob === null
    ) {
      return null;
    }
    if (
      control.blob.stage === MidgardCekSourceBlobStagesV1.Terminal
    ) {
      if (sourceBytes !== null && sourceBytes !== undefined) {
        return null;
      }
      const next = {
        ...control,
        stage:
          control.bytesLength > CARDANO_DATA_BYTES_CHUNK
            ? MidgardCekDataBytesStagesV1.Break
            : MidgardCekDataBytesStagesV1.Terminal,
      } satisfies MidgardCekDataBytesControlV1;
      return isWellFormedMidgardCekDataBytesControlV1(next)
        ? next
        : null;
    }
    const plan = contentPlan(control);
    const expectsSource = plan !== null;
    if (
      expectsSource !==
      (sourceBytes !== null && sourceBytes !== undefined)
    ) {
      return null;
    }
    const content =
      plan === null
        ? null
        : extractContent({
            plan,
            sourceBytes: sourceBytes!,
          });
    if (plan !== null && content === null) return null;
    const blob = advanceMidgardCekSourceBlobV1({
      control: control.blob,
      sourceBytes: content,
    });
    if (blob === null) return null;
    const next = { ...control, blob };
    return isWellFormedMidgardCekDataBytesControlV1(next)
      ? next
      : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataBytesV1 = (
  control: MidgardCekDataBytesControlV1,
): MidgardCekDataBytesSummaryV1 | null => {
  if (
    !isWellFormedMidgardCekDataBytesControlV1(control) ||
    control.stage !== MidgardCekDataBytesStagesV1.Terminal
  ) {
    return null;
  }
  const bytesRoot = finalizeMidgardCekSourceBlobV1(control.blob!);
  if (bytesRoot === null) return null;
  const memory = midgardCekDataBytesMemoryV1(
    BigInt(control.bytesLength),
  );
  return Object.freeze({
    root: Buffer.from(
      hashMidgardCekDataNodeV1({
        kind: "bytes",
        bytesRoot,
        bytesLength: BigInt(control.bytesLength),
        cborLength: BigInt(control.sourceLength),
        memory,
      }),
    ),
    cborLength: BigInt(control.sourceLength),
    memory,
  });
};

export const buildMidgardCekDataBytesTraceV1 = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekDataBytesTraceV1 => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekDataBytesControlV1({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekDataBytesTraceStepV1[] = [];
  let control = initial;
  while (control.stage !== MidgardCekDataBytesStagesV1.Terminal) {
    const span = nextMidgardCekDataBytesSpanV1(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekDataBytesV1({
      control,
      sourceBytes,
    });
    if (
      next === null ||
      !isWellFormedMidgardCekDataBytesControlV1(next)
    ) {
      throw new Error("V1 CEK Data bytes trace failed closed");
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

import {
  CARDANO_DATA_BYTES_CHUNK,
  type ContentPlan,
  type ContentSegment,
  definiteBytesHeader,
  definiteHeaderLength,
  indefiniteMidgardCekDataBytesLength,
  isWellFormedMidgardCekDataBytesControl,
  MIDGARD_CEK_DATA_BYTES_MAX_SOURCE_SPAN,
  MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
  type MidgardCekDataBytesControl,
  MidgardCekDataBytesStages,
  type MidgardCekDataBytesSummary,
  parseMidgardCekDataBytesSyntax,
  rawContentPosition,
} from "./cek-data-bytes.parse-midgard-cek-data-bytes-syntax.js";
import {
  hashMidgardCekDataNode,
  midgardCekDataBytesMemory,
} from "./cek-semantic.js";
import {
  advanceMidgardCekSourceBlob,
  finalizeMidgardCekSourceBlob,
  initialMidgardCekSourceBlobControl,
  type MidgardCekSourceBlobSpan,
  MidgardCekSourceBlobStages,
  nextMidgardCekSourceBlobSpan,
} from "./cek-source-blob.js";

const contentPlan = (
  control: MidgardCekDataBytesControl,
): ContentPlan | null => {
  if (
    control.stage !== MidgardCekDataBytesStages.Blob ||
    control.blob === null
  ) {
    return null;
  }
  const virtualSpan = nextMidgardCekSourceBlobSpan(control.blob);
  if (virtualSpan === null) return null;
  const contentStart = virtualSpan.absoluteStart;
  const contentEnd = contentStart + virtualSpan.length;
  if (contentStart < 0 || contentEnd > control.bytesLength) {
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
      Math.floor(cursor / CARDANO_DATA_BYTES_CHUNK) * CARDANO_DATA_BYTES_CHUNK;
    const withinChunk = cursor - chunkStart;
    const chunkLength = Math.min(
      CARDANO_DATA_BYTES_CHUNK,
      control.bytesLength - chunkStart,
    );
    const take = Math.min(remaining, chunkLength - withinChunk);
    if (take <= 0) return null;
    segments.push({
      header:
        withinChunk === 0 ? definiteBytesHeader(chunkLength) : Buffer.alloc(0),
      contentLength: take,
    });
    cursor += take;
    remaining -= take;
  }
  const firstChunkStart =
    Math.floor(contentStart / CARDANO_DATA_BYTES_CHUNK) *
    CARDANO_DATA_BYTES_CHUNK;
  const firstWithinChunk = contentStart - firstChunkStart;
  const firstChunkIndex = firstChunkStart / CARDANO_DATA_BYTES_CHUNK;
  const firstChunkLength = Math.min(
    CARDANO_DATA_BYTES_CHUNK,
    control.bytesLength - firstChunkStart,
  );
  const firstHeaderStart = 1 + firstChunkIndex * (CARDANO_DATA_BYTES_CHUNK + 2);
  const relativeStart =
    firstWithinChunk === 0
      ? firstHeaderStart
      : firstHeaderStart +
        definiteHeaderLength(firstChunkLength) +
        firstWithinChunk;
  const rawLength = segments.reduce(
    (length, segment) => length + segment.header.length + segment.contentLength,
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
    content.push(source.subarray(cursor, cursor + segment.contentLength));
    cursor += segment.contentLength;
  }
  return cursor === source.length ? Buffer.concat(content) : null;
};

/**
 * The next chunk-header window of a measuring control: two bytes (a chunk
 * header, or the 0xff break and whatever follows it), or the single last byte
 * of the enclosing source.
 */
const measureSpan = (
  control: MidgardCekDataBytesControl,
  sourceEnd: number,
): MidgardCekSourceBlobSpan | null => {
  const absoluteStart = control.sourceStart + control.sourceLength;
  return absoluteStart < sourceEnd
    ? {
        absoluteStart,
        length: Math.min(
          MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
          sourceEnd - absoluteStart,
        ),
      }
    : null;
};

/**
 * `sourceEnd` is the absolute end of the enclosing authenticated source; only
 * the measuring stage reads it, to bound its chunk-header window.
 */
export const nextMidgardCekDataBytesSpan = (
  control: MidgardCekDataBytesControl,
  sourceEnd: number,
): MidgardCekSourceBlobSpan | null => {
  if (!isWellFormedMidgardCekDataBytesControl(control)) {
    return null;
  }
  if (control.stage === MidgardCekDataBytesStages.Syntax) {
    return {
      absoluteStart: control.sourceStart,
      length: Math.min(
        control.sourceLength,
        MIDGARD_CEK_DATA_BYTES_SYNTAX_BYTES,
      ),
    };
  }
  if (control.stage === MidgardCekDataBytesStages.Measure) {
    return measureSpan(control, sourceEnd);
  }
  return contentPlan(control)?.span ?? null;
};

/**
 * One measuring step: the authenticated window at the measured end is either
 * the 0xff break, which fixes the encoding length and starts the content pass,
 * or one definite chunk header, which extends the measured length by that
 * whole chunk. The chunk layout itself is checked by the content pass.
 */
const advanceMeasure = (
  control: MidgardCekDataBytesControl,
  sourceEnd: number,
  sourceBytes: Uint8Array | null | undefined,
): MidgardCekDataBytesControl | null => {
  const span = measureSpan(control, sourceEnd);
  if (
    span === null ||
    sourceBytes === null ||
    sourceBytes === undefined ||
    sourceBytes.length !== span.length
  ) {
    return null;
  }
  const first = sourceBytes[0]!;
  if (first === 0xff) {
    const sourceLength = control.sourceLength + 1;
    const bytesLength = indefiniteMidgardCekDataBytesLength(sourceLength);
    if (bytesLength === null) return null;
    const next = {
      ...control,
      stage: MidgardCekDataBytesStages.Blob,
      sourceLength,
      bytesLength,
      blob: initialMidgardCekSourceBlobControl({
        sourceStart: 0,
        sourceLength: bytesLength,
      }),
    } satisfies MidgardCekDataBytesControl;
    return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
  }
  const chunkLength =
    first >= 0x41 && first <= 0x57
      ? first - 0x3f
      : first === 0x58 &&
          span.length === 2 &&
          sourceBytes[1]! >= 24 &&
          sourceBytes[1]! <= CARDANO_DATA_BYTES_CHUNK
        ? sourceBytes[1]! + 2
        : null;
  if (chunkLength === null || span.absoluteStart + chunkLength >= sourceEnd) {
    return null;
  }
  const next = {
    ...control,
    sourceLength: control.sourceLength + chunkLength,
  } satisfies MidgardCekDataBytesControl;
  return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
};

export const advanceMidgardCekDataBytes = ({
  control,
  sourceBytes,
  sourceEnd,
}: {
  readonly control: MidgardCekDataBytesControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly sourceEnd: number;
}): MidgardCekDataBytesControl | null => {
  try {
    if (!isWellFormedMidgardCekDataBytesControl(control)) {
      return null;
    }
    if (control.stage === MidgardCekDataBytesStages.Syntax) {
      const span = nextMidgardCekDataBytesSpan(control, sourceEnd)!;
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== span.length
      ) {
        return null;
      }
      const bytesLength = parseMidgardCekDataBytesSyntax({
        syntaxBytes: sourceBytes,
        sourceLength: control.sourceLength,
      });
      if (bytesLength === null) return null;
      const next = {
        ...control,
        stage: MidgardCekDataBytesStages.Blob,
        bytesLength,
        blob: initialMidgardCekSourceBlobControl({
          sourceStart: 0,
          sourceLength: bytesLength,
        }),
      } satisfies MidgardCekDataBytesControl;
      return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
    }
    if (control.stage === MidgardCekDataBytesStages.Measure) {
      return advanceMeasure(control, sourceEnd, sourceBytes);
    }
    if (
      control.stage !== MidgardCekDataBytesStages.Blob ||
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
        stage: MidgardCekDataBytesStages.Terminal,
      } satisfies MidgardCekDataBytesControl;
      return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
    }
    const plan = contentPlan(control);
    const expectsSource = plan !== null;
    if (expectsSource !== (sourceBytes !== null && sourceBytes !== undefined)) {
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
    const blob = advanceMidgardCekSourceBlob({
      control: control.blob,
      sourceBytes: content,
    });
    if (blob === null) return null;
    const next = { ...control, blob };
    return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataBytes = (
  control: MidgardCekDataBytesControl,
): MidgardCekDataBytesSummary | null => {
  if (
    !isWellFormedMidgardCekDataBytesControl(control) ||
    control.stage !== MidgardCekDataBytesStages.Terminal
  ) {
    return null;
  }
  const bytesRoot = finalizeMidgardCekSourceBlob(control.blob!);
  if (bytesRoot === null) return null;
  const memory = midgardCekDataBytesMemory(BigInt(control.bytesLength));
  return Object.freeze({
    root: Buffer.from(
      hashMidgardCekDataNode({
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

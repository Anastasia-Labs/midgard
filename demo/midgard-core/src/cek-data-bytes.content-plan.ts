import {
  CARDANO_DATA_BYTES_CHUNK,
  type ContentPlan,
  type ContentSegment,
  definiteBytesHeader,
  definiteHeaderLength,
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

export const nextMidgardCekDataBytesSpan = (
  control: MidgardCekDataBytesControl,
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
  if (control.stage === MidgardCekDataBytesStages.Break) {
    return {
      absoluteStart: control.sourceStart + control.sourceLength - 1,
      length: 1,
    };
  }
  return contentPlan(control)?.span ?? null;
};

export const advanceMidgardCekDataBytes = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekDataBytesControl;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekDataBytesControl | null => {
  try {
    if (!isWellFormedMidgardCekDataBytesControl(control)) {
      return null;
    }
    if (control.stage === MidgardCekDataBytesStages.Syntax) {
      const span = nextMidgardCekDataBytesSpan(control)!;
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
    if (control.stage === MidgardCekDataBytesStages.Break) {
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
        stage: MidgardCekDataBytesStages.Terminal,
      } satisfies MidgardCekDataBytesControl;
      return isWellFormedMidgardCekDataBytesControl(next) ? next : null;
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
        stage:
          control.bytesLength > CARDANO_DATA_BYTES_CHUNK
            ? MidgardCekDataBytesStages.Break
            : MidgardCekDataBytesStages.Terminal,
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

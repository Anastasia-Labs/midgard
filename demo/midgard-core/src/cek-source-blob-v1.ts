import {
  advanceMidgardBlake2b256TraceV1,
  digestMidgardBlake2b256TraceV1,
  encodeMidgardBlake2b256TraceControlV1,
  initialMidgardBlake2b256TraceControlV1,
  isWellFormedMidgardBlake2b256TraceControlV1,
  MIDGARD_BLAKE2B_256_BLOCK_BYTES,
  type MidgardBlake2b256TraceControlV1,
  MidgardBlake2b256TraceStagesV1,
} from "./blake2b-256-trace-v1.js";
import {
  appendMidgardCekBlobFrontierChunkRootV1,
  emptyMidgardCekBlobFrontierV1,
  encodeMidgardCekBlobFrontierV1,
  finalizeMidgardCekBlobFrontierV1,
  type MidgardCekBlobFrontierV1,
  validateMidgardCekBlobFrontierV1,
} from "./cek-blob-frontier-v1.js";
import { MIDGARD_CEK_BLOB_CHUNK_BYTES } from "./cek-proof.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT } from "./validation-merkle.js";

export const MIDGARD_CEK_SOURCE_BLOB_V1_VERSION = 1 as const;

export const MidgardCekSourceBlobStagesV1 = Object.freeze({
  Active: 0,
  Terminal: 1,
} as const);

export type MidgardCekSourceBlobStageV1 =
  (typeof MidgardCekSourceBlobStagesV1)[keyof typeof MidgardCekSourceBlobStagesV1];

/**
 * Hashes one contiguous span supplied by a parent authenticated-source
 * machine into the canonical CEK blob tree. Source bytes are accepted only
 * while the nested BLAKE2b trace is ready for its next block.
 */
export type MidgardCekSourceBlobControlV1 = {
  readonly version: typeof MIDGARD_CEK_SOURCE_BLOB_V1_VERSION;
  readonly stage: MidgardCekSourceBlobStageV1;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly frontier: MidgardCekBlobFrontierV1;
  readonly activeHash: MidgardBlake2b256TraceControlV1 | null;
};

export type MidgardCekSourceBlobSpanV1 = {
  readonly absoluteStart: number;
  readonly length: number;
};

export type MidgardCekSourceBlobTraceStepV1 = {
  readonly control: MidgardCekSourceBlobControlV1;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekSourceBlobControlV1;
};

export type MidgardCekSourceBlobTraceV1 = {
  readonly initial: MidgardCekSourceBlobControlV1;
  readonly steps: readonly MidgardCekSourceBlobTraceStepV1[];
  readonly terminal: MidgardCekSourceBlobControlV1;
};

const BLOB_CHUNK_DOMAIN = Buffer.from(
  "MidgardCekBlobChunkV1",
  "ascii",
);

const exactNonNegativeSafeInteger = (
  value: number,
  field: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 CEK source blob ${field}`);
  }
  return value;
};

const definiteBytesHeader = (length: number): Buffer => {
  if (
    !Number.isSafeInteger(length) ||
    length < 0 ||
    length > MIDGARD_CEK_BLOB_CHUNK_BYTES
  ) {
    throw new Error("Invalid V1 CEK source blob chunk length");
  }
  if (length < 24) return Buffer.from([0x40 + length]);
  if (length <= 0xff) return Buffer.from([0x58, length]);
  const header = Buffer.alloc(3);
  header[0] = 0x59;
  header.writeUInt16BE(length, 1);
  return header;
};

const chunkPrefix = (length: number): Buffer =>
  Buffer.concat([BLOB_CHUNK_DOMAIN, definiteBytesHeader(length)]);

const expectedChunkCount = (sourceLength: number): number =>
  Math.max(
    1,
    Math.ceil(sourceLength / MIDGARD_CEK_BLOB_CHUNK_BYTES),
  );

const expectedFrontierByteLength = ({
  sourceLength,
  count,
}: {
  readonly sourceLength: number;
  readonly count: number;
}): bigint =>
  BigInt(
    Math.min(
      sourceLength,
      count * MIDGARD_CEK_BLOB_CHUNK_BYTES,
    ),
  );

const activeChunkLength = (
  control: MidgardCekSourceBlobControlV1,
): number =>
  control.sourceLength === 0
    ? 0
    : Math.min(
        MIDGARD_CEK_BLOB_CHUNK_BYTES,
        control.sourceLength - Number(control.frontier.byteLength),
      );

const initialActiveHash = (
  chunkLength: number,
): MidgardBlake2b256TraceControlV1 =>
  initialMidgardBlake2b256TraceControlV1(
    chunkPrefix(chunkLength).length + chunkLength,
  );

export const isWellFormedMidgardCekSourceBlobControlV1 = (
  control: MidgardCekSourceBlobControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_SOURCE_BLOB_V1_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardCekSourceBlobStagesV1.Active ||
      control.stage > MidgardCekSourceBlobStagesV1.Terminal ||
      exactNonNegativeSafeInteger(
        control.sourceStart,
        "source start",
      ) !== control.sourceStart ||
      exactNonNegativeSafeInteger(
        control.sourceLength,
        "source length",
      ) !== control.sourceLength ||
      !Number.isSafeInteger(
        control.sourceStart + control.sourceLength,
      )
    ) {
      return false;
    }
    validateMidgardCekBlobFrontierV1(control.frontier);
    const chunkCount = expectedChunkCount(control.sourceLength);
    if (
      chunkCount > MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT ||
      control.frontier.count > chunkCount ||
      control.frontier.byteLength !==
        expectedFrontierByteLength({
          sourceLength: control.sourceLength,
          count: control.frontier.count,
        })
    ) {
      return false;
    }
    if (control.stage === MidgardCekSourceBlobStagesV1.Terminal) {
      return (
        control.frontier.count === chunkCount &&
        control.activeHash === null
      );
    }
    if (
      control.frontier.count >= chunkCount ||
      control.activeHash === null ||
      !isWellFormedMidgardBlake2b256TraceControlV1(
        control.activeHash,
      )
    ) {
      return false;
    }
    const chunkLength = activeChunkLength(control);
    return (
      chunkLength >= 0 &&
      control.activeHash.totalLength ===
        chunkPrefix(chunkLength).length + chunkLength
    );
  } catch {
    return false;
  }
};

export const initialMidgardCekSourceBlobControlV1 = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekSourceBlobControlV1 => {
  exactNonNegativeSafeInteger(sourceStart, "source start");
  exactNonNegativeSafeInteger(sourceLength, "source length");
  const control = {
    version: MIDGARD_CEK_SOURCE_BLOB_V1_VERSION,
    stage: MidgardCekSourceBlobStagesV1.Active,
    sourceStart,
    sourceLength,
    frontier: emptyMidgardCekBlobFrontierV1(),
    activeHash: initialActiveHash(
      Math.min(sourceLength, MIDGARD_CEK_BLOB_CHUNK_BYTES),
    ),
  } satisfies MidgardCekSourceBlobControlV1;
  if (!isWellFormedMidgardCekSourceBlobControlV1(control)) {
    throw new Error("Invalid V1 CEK source blob range");
  }
  return control;
};

const optionalActiveHashDataCbor = (
  control: MidgardBlake2b256TraceControlV1 | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardBlake2b256TraceControlV1(control),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekSourceBlobControlV1 = (
  control: MidgardCekSourceBlobControlV1,
): Buffer => {
  if (!isWellFormedMidgardCekSourceBlobControlV1(control)) {
    throw new Error("Invalid V1 CEK source blob control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_SOURCE_BLOB_V1_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeMidgardCekBlobFrontierV1(control.frontier),
    optionalActiveHashDataCbor(control.activeHash),
  ]);
};

export const nextMidgardCekSourceBlobSpanV1 = (
  control: MidgardCekSourceBlobControlV1,
): MidgardCekSourceBlobSpanV1 | null => {
  if (
    !isWellFormedMidgardCekSourceBlobControlV1(control) ||
    control.stage !== MidgardCekSourceBlobStagesV1.Active ||
    control.activeHash!.stage !==
      MidgardBlake2b256TraceStagesV1.Ready
  ) {
    return null;
  }
  const hashControl = control.activeHash!;
  const prefix = chunkPrefix(activeChunkLength(control));
  const blockLength = Math.min(
    MIDGARD_BLAKE2B_256_BLOCK_BYTES,
    hashControl.totalLength - hashControl.cursor,
  );
  const prefixStart = Math.min(hashControl.cursor, prefix.length);
  const prefixEnd = Math.min(
    hashControl.cursor + blockLength,
    prefix.length,
  );
  const prefixLength = prefixEnd - prefixStart;
  return {
    absoluteStart:
      control.sourceStart +
      Number(control.frontier.byteLength) +
      Math.max(hashControl.cursor - prefix.length, 0),
    length: blockLength - prefixLength,
  };
};

const activeMessageBlock = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekSourceBlobControlV1;
  readonly sourceBytes: Uint8Array;
}): Buffer | null => {
  const span = nextMidgardCekSourceBlobSpanV1(control);
  if (span === null || sourceBytes.length !== span.length) return null;
  const hashControl = control.activeHash!;
  const prefix = chunkPrefix(activeChunkLength(control));
  const blockLength = Math.min(
    MIDGARD_BLAKE2B_256_BLOCK_BYTES,
    hashControl.totalLength - hashControl.cursor,
  );
  return Buffer.concat([
    prefix.subarray(
      hashControl.cursor,
      Math.min(hashControl.cursor + blockLength, prefix.length),
    ),
    Buffer.from(sourceBytes),
  ]);
};

export const advanceMidgardCekSourceBlobV1 = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekSourceBlobControlV1;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekSourceBlobControlV1 | null => {
  try {
    if (
      !isWellFormedMidgardCekSourceBlobControlV1(control) ||
      control.stage !== MidgardCekSourceBlobStagesV1.Active
    ) {
      return null;
    }
    const hashControl = control.activeHash!;
    if (
      hashControl.stage ===
      MidgardBlake2b256TraceStagesV1.Terminal
    ) {
      if (sourceBytes !== null && sourceBytes !== undefined) return null;
      const root = digestMidgardBlake2b256TraceV1(hashControl);
      if (root === null) return null;
      const frontier = appendMidgardCekBlobFrontierChunkRootV1(
        control.frontier,
        {
          root,
          byteLength: activeChunkLength(control),
        },
      );
      const terminal =
        frontier.count === expectedChunkCount(control.sourceLength);
      const next = {
        ...control,
        stage: terminal
          ? MidgardCekSourceBlobStagesV1.Terminal
          : MidgardCekSourceBlobStagesV1.Active,
        frontier,
        activeHash: terminal
          ? null
          : initialActiveHash(
              Math.min(
                MIDGARD_CEK_BLOB_CHUNK_BYTES,
                control.sourceLength - Number(frontier.byteLength),
              ),
            ),
      } satisfies MidgardCekSourceBlobControlV1;
      return isWellFormedMidgardCekSourceBlobControlV1(next)
        ? next
        : null;
    }
    const ready =
      hashControl.stage === MidgardBlake2b256TraceStagesV1.Ready;
    if (
      ready !==
      (sourceBytes !== null && sourceBytes !== undefined)
    ) {
      return null;
    }
    const block = ready
      ? activeMessageBlock({
          control,
          sourceBytes: sourceBytes!,
        })
      : null;
    if (ready && block === null) return null;
    const activeHash = advanceMidgardBlake2b256TraceV1({
      control: hashControl,
      block,
    });
    if (activeHash === null) return null;
    const next = { ...control, activeHash };
    return isWellFormedMidgardCekSourceBlobControlV1(next)
      ? next
      : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekSourceBlobV1 = (
  control: MidgardCekSourceBlobControlV1,
): Buffer | null =>
  isWellFormedMidgardCekSourceBlobControlV1(control) &&
  control.stage === MidgardCekSourceBlobStagesV1.Terminal
    ? finalizeMidgardCekBlobFrontierV1(control.frontier)
    : null;

export const buildMidgardCekSourceBlobTraceV1 = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekSourceBlobTraceV1 => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekSourceBlobControlV1({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekSourceBlobTraceStepV1[] = [];
  let control = initial;
  while (control.stage !== MidgardCekSourceBlobStagesV1.Terminal) {
    const span = nextMidgardCekSourceBlobSpanV1(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceMidgardCekSourceBlobV1({
      control,
      sourceBytes,
    });
    if (
      next === null ||
      !isWellFormedMidgardCekSourceBlobControlV1(next)
    ) {
      throw new Error("V1 CEK source blob trace failed closed");
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

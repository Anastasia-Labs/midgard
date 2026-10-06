import {
  advanceMidgardBlake2b256Trace,
  digestMidgardBlake2b256Trace,
  encodeMidgardBlake2b256TraceControl,
  initialMidgardBlake2b256TraceControl,
  isWellFormedMidgardBlake2b256TraceControl,
  MIDGARD_BLAKE2B_256_BLOCK_BYTES,
  type MidgardBlake2b256TraceControl,
  MidgardBlake2b256TraceStages,
} from "./blake2b-256-trace.js";
import {
  appendMidgardCekBlobFrontierChunkRoot,
  emptyMidgardCekBlobFrontier,
  encodeMidgardCekBlobFrontier,
  finalizeMidgardCekBlobFrontier,
  type MidgardCekBlobFrontier,
  validateMidgardCekBlobFrontier,
} from "./cek-blob-frontier.js";
import { MIDGARD_CEK_BLOB_CHUNK_BYTES } from "./cek-proof.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT } from "./validation-merkle.js";

export const MIDGARD_CEK_SOURCE_BLOB_VERSION = 1 as const;

export const MidgardCekSourceBlobStages = Object.freeze({
  Active: 0,
  Terminal: 1,
} as const);

export type MidgardCekSourceBlobStage =
  (typeof MidgardCekSourceBlobStages)[keyof typeof MidgardCekSourceBlobStages];

/**
 * Hashes one contiguous span supplied by a parent authenticated-source
 * machine into the canonical CEK blob tree. Source bytes are accepted only
 * while the nested BLAKE2b trace is ready for its next block.
 */
export type MidgardCekSourceBlobControl = {
  readonly version: typeof MIDGARD_CEK_SOURCE_BLOB_VERSION;
  readonly stage: MidgardCekSourceBlobStage;
  readonly sourceStart: number;
  readonly sourceLength: number;
  readonly frontier: MidgardCekBlobFrontier;
  readonly activeHash: MidgardBlake2b256TraceControl | null;
};

export type MidgardCekSourceBlobSpan = {
  readonly absoluteStart: number;
  readonly length: number;
};

export type MidgardCekSourceBlobTraceStep = {
  readonly control: MidgardCekSourceBlobControl;
  readonly sourceBytes: Buffer | null;
  readonly next: MidgardCekSourceBlobControl;
};

export type MidgardCekSourceBlobTrace = {
  readonly initial: MidgardCekSourceBlobControl;
  readonly steps: readonly MidgardCekSourceBlobTraceStep[];
  readonly terminal: MidgardCekSourceBlobControl;
};

const BLOB_CHUNK_DOMAIN = Buffer.from("MidgardCekBlobChunkV1", "ascii");

const exactNonNegativeSafeInteger = (value: number, field: string): number => {
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
  Math.max(1, Math.ceil(sourceLength / MIDGARD_CEK_BLOB_CHUNK_BYTES));

const expectedFrontierByteLength = ({
  sourceLength,
  count,
}: {
  readonly sourceLength: number;
  readonly count: number;
}): bigint =>
  BigInt(Math.min(sourceLength, count * MIDGARD_CEK_BLOB_CHUNK_BYTES));

const activeChunkLength = (control: MidgardCekSourceBlobControl): number =>
  control.sourceLength === 0
    ? 0
    : Math.min(
        MIDGARD_CEK_BLOB_CHUNK_BYTES,
        control.sourceLength - Number(control.frontier.byteLength),
      );

const initialActiveHash = (
  chunkLength: number,
): MidgardBlake2b256TraceControl =>
  initialMidgardBlake2b256TraceControl(
    chunkPrefix(chunkLength).length + chunkLength,
  );

/**
 * `hashValidated` is true only inside this package's own call chains, when the
 * active BLAKE2b trace was just validated by its own machine (an entry check
 * of this control, an initial trace, or the trace's exit check). Every
 * exported entry point validates with it false.
 */
const wellFormedSourceBlob = (
  control: MidgardCekSourceBlobControl,
  hashValidated: boolean,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_SOURCE_BLOB_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardCekSourceBlobStages.Active ||
      control.stage > MidgardCekSourceBlobStages.Terminal ||
      exactNonNegativeSafeInteger(control.sourceStart, "source start") !==
        control.sourceStart ||
      exactNonNegativeSafeInteger(control.sourceLength, "source length") !==
        control.sourceLength ||
      !Number.isSafeInteger(control.sourceStart + control.sourceLength)
    ) {
      return false;
    }
    validateMidgardCekBlobFrontier(control.frontier);
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
    if (control.stage === MidgardCekSourceBlobStages.Terminal) {
      return (
        control.frontier.count === chunkCount && control.activeHash === null
      );
    }
    if (
      control.frontier.count >= chunkCount ||
      control.activeHash === null ||
      (!hashValidated &&
        !isWellFormedMidgardBlake2b256TraceControl(control.activeHash))
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

export const isWellFormedMidgardCekSourceBlobControl = (
  control: MidgardCekSourceBlobControl,
): boolean => wellFormedSourceBlob(control, false);

export const initialMidgardCekSourceBlobControl = ({
  sourceStart,
  sourceLength,
}: {
  readonly sourceStart: number;
  readonly sourceLength: number;
}): MidgardCekSourceBlobControl => {
  exactNonNegativeSafeInteger(sourceStart, "source start");
  exactNonNegativeSafeInteger(sourceLength, "source length");
  const control = {
    version: MIDGARD_CEK_SOURCE_BLOB_VERSION,
    stage: MidgardCekSourceBlobStages.Active,
    sourceStart,
    sourceLength,
    frontier: emptyMidgardCekBlobFrontier(),
    activeHash: initialActiveHash(
      Math.min(sourceLength, MIDGARD_CEK_BLOB_CHUNK_BYTES),
    ),
  } satisfies MidgardCekSourceBlobControl;
  if (!isWellFormedMidgardCekSourceBlobControl(control)) {
    throw new Error("Invalid V1 CEK source blob range");
  }
  return control;
};

const optionalActiveHashDataCbor = (
  control: MidgardBlake2b256TraceControl | null,
): Buffer =>
  control === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardBlake2b256TraceControl(control),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekSourceBlobControl = (
  control: MidgardCekSourceBlobControl,
): Buffer => {
  if (!isWellFormedMidgardCekSourceBlobControl(control)) {
    throw new Error("Invalid V1 CEK source blob control");
  }
  return encodeValidatedMidgardCekSourceBlobControl(control);
};

/**
 * Package-internal (not re-exported by the package index): for a control the
 * caller has already validated in the same synchronous call chain, either
 * directly or as the nested child of a validated parent control.
 */
export const encodeValidatedMidgardCekSourceBlobControl = (
  control: MidgardCekSourceBlobControl,
): Buffer =>
  encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_SOURCE_BLOB_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeMidgardCekBlobFrontier(control.frontier),
    optionalActiveHashDataCbor(control.activeHash),
  ]);

export const nextMidgardCekSourceBlobSpan = (
  control: MidgardCekSourceBlobControl,
): MidgardCekSourceBlobSpan | null =>
  isWellFormedMidgardCekSourceBlobControl(control)
    ? nextValidatedMidgardCekSourceBlobSpan(control)
    : null;

/** Package-internal; see `encodeValidatedMidgardCekSourceBlobControl`. */
export const nextValidatedMidgardCekSourceBlobSpan = (
  control: MidgardCekSourceBlobControl,
): MidgardCekSourceBlobSpan | null => {
  if (
    control.stage !== MidgardCekSourceBlobStages.Active ||
    control.activeHash!.stage !== MidgardBlake2b256TraceStages.Ready
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
  const prefixEnd = Math.min(hashControl.cursor + blockLength, prefix.length);
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
  readonly control: MidgardCekSourceBlobControl;
  readonly sourceBytes: Uint8Array;
}): Buffer | null => {
  const span = nextValidatedMidgardCekSourceBlobSpan(control);
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

export const advanceMidgardCekSourceBlob = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekSourceBlobControl;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekSourceBlobControl | null =>
  isWellFormedMidgardCekSourceBlobControl(control)
    ? advanceValidatedMidgardCekSourceBlob({ control, sourceBytes })
    : null;

/**
 * Package-internal; see `encodeValidatedMidgardCekSourceBlobControl`. The
 * successor is still checked before it is returned; only its active trace,
 * which its own machine has just validated, is not re-validated.
 */
export const advanceValidatedMidgardCekSourceBlob = ({
  control,
  sourceBytes,
}: {
  readonly control: MidgardCekSourceBlobControl;
  readonly sourceBytes?: Uint8Array | null;
}): MidgardCekSourceBlobControl | null => {
  try {
    if (control.stage !== MidgardCekSourceBlobStages.Active) {
      return null;
    }
    const hashControl = control.activeHash!;
    if (hashControl.stage === MidgardBlake2b256TraceStages.Terminal) {
      if (sourceBytes !== null && sourceBytes !== undefined) return null;
      const root = digestMidgardBlake2b256Trace(hashControl);
      if (root === null) return null;
      const frontier = appendMidgardCekBlobFrontierChunkRoot(control.frontier, {
        root,
        byteLength: activeChunkLength(control),
      });
      const terminal =
        frontier.count === expectedChunkCount(control.sourceLength);
      const next = {
        ...control,
        stage: terminal
          ? MidgardCekSourceBlobStages.Terminal
          : MidgardCekSourceBlobStages.Active,
        frontier,
        activeHash: terminal
          ? null
          : initialActiveHash(
              Math.min(
                MIDGARD_CEK_BLOB_CHUNK_BYTES,
                control.sourceLength - Number(frontier.byteLength),
              ),
            ),
      } satisfies MidgardCekSourceBlobControl;
      // The new trace is null or an initial trace, which validates itself.
      return wellFormedSourceBlob(next, true) ? next : null;
    }
    const ready = hashControl.stage === MidgardBlake2b256TraceStages.Ready;
    if (ready !== (sourceBytes !== null && sourceBytes !== undefined)) {
      return null;
    }
    const block = ready
      ? activeMessageBlock({
          control,
          sourceBytes: sourceBytes!,
        })
      : null;
    if (ready && block === null) return null;
    const activeHash = advanceMidgardBlake2b256Trace({
      control: hashControl,
      block,
    });
    if (activeHash === null) return null;
    const next = { ...control, activeHash };
    // A non-null trace successor passed the trace machine's exit check.
    return wellFormedSourceBlob(next, true) ? next : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekSourceBlob = (
  control: MidgardCekSourceBlobControl,
): Buffer | null =>
  isWellFormedMidgardCekSourceBlobControl(control)
    ? finalizeValidatedMidgardCekSourceBlob(control)
    : null;

/** Package-internal; see `encodeValidatedMidgardCekSourceBlobControl`. */
export const finalizeValidatedMidgardCekSourceBlob = (
  control: MidgardCekSourceBlobControl,
): Buffer | null =>
  control.stage === MidgardCekSourceBlobStages.Terminal
    ? finalizeMidgardCekBlobFrontier(control.frontier)
    : null;

export const buildMidgardCekSourceBlobTrace = ({
  sourceStart,
  source,
}: {
  readonly sourceStart: number;
  readonly source: Uint8Array;
}): MidgardCekSourceBlobTrace => {
  const bytes = Buffer.from(source);
  const initial = initialMidgardCekSourceBlobControl({
    sourceStart,
    sourceLength: bytes.length,
  });
  const steps: MidgardCekSourceBlobTraceStep[] = [];
  let control = initial;
  // Every control here is the validated initial control or a successor that
  // passed the machine's exit check.
  while (control.stage !== MidgardCekSourceBlobStages.Terminal) {
    const span = nextValidatedMidgardCekSourceBlobSpan(control);
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceValidatedMidgardCekSourceBlob({
      control,
      sourceBytes,
    });
    if (next === null) {
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

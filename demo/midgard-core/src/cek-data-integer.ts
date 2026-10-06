import {
  advanceChunkedMagnitude,
  chunkedMagnitudeSpan,
  initialMidgardCekDataIntegerMeasureControl,
  measuredMagnitudeLength,
} from "./cek-data-integer.chunked-magnitude.js";
import {
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  parseMidgardCekDataIntegerSyntax,
  UINT32_MAX,
  UINT64_MAX,
} from "./cek-data-integer.syntax.js";
import { hashMidgardCekDataNode } from "./cek-semantic.js";
import {
  advanceValidatedMidgardCekSourceBlob,
  encodeValidatedMidgardCekSourceBlobControl,
  finalizeValidatedMidgardCekSourceBlob,
  initialMidgardCekSourceBlobControl,
  isWellFormedMidgardCekSourceBlobControl,
  type MidgardCekSourceBlobControl,
  type MidgardCekSourceBlobSpan,
  MidgardCekSourceBlobStages,
  nextValidatedMidgardCekSourceBlobSpan,
} from "./cek-source-blob.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";

export const MIDGARD_CEK_DATA_INTEGER_VERSION = 1 as const;

export {
  MIDGARD_CEK_DATA_INTEGER_SYNTAX_BYTES,
  parseMidgardCekDataIntegerSyntax,
  parseMidgardCekDataLargeConstructorSyntax,
} from "./cek-data-integer.syntax.js";

export const MidgardCekDataIntegerStages = Object.freeze({
  Syntax: 0,
  Blob: 1,
  Terminal: 2,
  Measure: 3,
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

const exactSourceCoordinate = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`Invalid V1 CEK Data integer ${field}`);
  }
  return value;
};

/**
 * `blobValidated` is true only inside this package's own call chains, when the
 * source-blob child was just validated by its own machine (an entry check of
 * this control, an initial blob, or the blob's exit check). Every exported
 * entry point validates with it false.
 */
const wellFormedInteger = (
  control: MidgardCekDataIntegerControl,
  blobValidated: boolean,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_CEK_DATA_INTEGER_VERSION ||
      !Number.isInteger(control.stage) ||
      control.stage < MidgardCekDataIntegerStages.Syntax ||
      control.stage > MidgardCekDataIntegerStages.Measure ||
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
    if (control.stage === MidgardCekDataIntegerStages.Measure) {
      const length = measuredMagnitudeLength(control.sourceLength);
      return (
        control.blob === null &&
        (control.sourceLength === 2
          ? control.memory === 0n
          : length !== null &&
            (control.memory === 4n + BigInt(length) ||
              control.memory === 5n + BigInt(length)))
      );
    }
    if (
      control.memory < 5n ||
      control.blob === null ||
      (!blobValidated &&
        !isWellFormedMidgardCekSourceBlobControl(control.blob)) ||
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

export const isWellFormedMidgardCekDataIntegerControl = (
  control: MidgardCekDataIntegerControl,
): boolean => wellFormedInteger(control, false);

/**
 * Package-internal (not re-exported by the package index): the integer checks
 * of `isWellFormedMidgardCekDataIntegerControl` for a control whose blob child
 * the caller has just validated in the same synchronous call chain.
 */
export const isWellFormedMidgardCekDataIntegerControlWithValidatedBlob = (
  control: MidgardCekDataIntegerControl,
): boolean => wellFormedInteger(control, true);

export { initialMidgardCekDataIntegerMeasureControl } from "./cek-data-integer.chunked-magnitude.js";
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
        encodeValidatedMidgardCekSourceBlobControl(blob),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardCekDataIntegerControl = (
  control: MidgardCekDataIntegerControl,
): Buffer => {
  if (!isWellFormedMidgardCekDataIntegerControl(control)) {
    throw new Error("Invalid V1 CEK Data integer control");
  }
  return encodeValidatedMidgardCekDataIntegerControl(control);
};

/**
 * Package-internal (not re-exported by the package index): for a control the
 * caller has already validated in the same synchronous call chain, either
 * directly or as the nested child of a validated parent control.
 */
export const encodeValidatedMidgardCekDataIntegerControl = (
  control: MidgardCekDataIntegerControl,
): Buffer =>
  encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_CEK_DATA_INTEGER_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.sourceStart)),
    encodeCbor(BigInt(control.sourceLength)),
    encodeCbor(control.memory),
    optionalBlobDataCbor(control.blob),
  ]);

export const nextMidgardCekDataIntegerSpan = (
  control: MidgardCekDataIntegerControl,
  sourceEnd: number,
): MidgardCekSourceBlobSpan | null =>
  isWellFormedMidgardCekDataIntegerControl(control)
    ? nextValidatedMidgardCekDataIntegerSpan(control, sourceEnd)
    : null;

/** Package-internal; see `encodeValidatedMidgardCekDataIntegerControl`. */
export const nextValidatedMidgardCekDataIntegerSpan = (
  control: MidgardCekDataIntegerControl,
  sourceEnd: number,
): MidgardCekSourceBlobSpan | null => {
  if (control.stage === MidgardCekDataIntegerStages.Measure) {
    return chunkedMagnitudeSpan(control, sourceEnd);
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
    ? nextValidatedMidgardCekSourceBlobSpan(control.blob!)
    : null;
};

export const advanceMidgardCekDataInteger = ({
  control,
  sourceBytes,
  sourceEnd,
}: {
  readonly control: MidgardCekDataIntegerControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly sourceEnd: number;
}): MidgardCekDataIntegerControl | null =>
  isWellFormedMidgardCekDataIntegerControl(control)
    ? advanceValidatedMidgardCekDataInteger({ control, sourceBytes, sourceEnd })
    : null;

/**
 * Package-internal; see `encodeValidatedMidgardCekDataIntegerControl`. The
 * successor is still checked before it is returned; only its blob child,
 * which is an initial blob or its own machine's exit-checked successor, is not
 * re-validated.
 */
export const advanceValidatedMidgardCekDataInteger = ({
  control,
  sourceBytes,
  sourceEnd,
}: {
  readonly control: MidgardCekDataIntegerControl;
  readonly sourceBytes?: Uint8Array | null;
  readonly sourceEnd: number;
}): MidgardCekDataIntegerControl | null => {
  try {
    if (control.stage === MidgardCekDataIntegerStages.Syntax) {
      const span = nextValidatedMidgardCekDataIntegerSpan(control, sourceEnd)!;
      if (
        sourceBytes === null ||
        sourceBytes === undefined ||
        sourceBytes.length !== span.length
      ) {
        return null;
      }
      if (
        (sourceBytes[0] === 0xc2 || sourceBytes[0] === 0xc3) &&
        sourceBytes[1] === 0x5f
      ) {
        return initialMidgardCekDataIntegerMeasureControl({
          sourceStart: control.sourceStart,
        });
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
      return wellFormedInteger(next, true) ? next : null;
    }
    if (control.stage === MidgardCekDataIntegerStages.Measure) {
      return advanceChunkedMagnitude(
        control,
        sourceEnd,
        sourceBytes,
        nextValidatedMidgardCekDataIntegerSpan(control, sourceEnd),
      );
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
      // The blob child is unchanged and was validated with this control.
      return wellFormedInteger(next, true) ? next : null;
    }
    const blob = advanceValidatedMidgardCekSourceBlob({
      control: control.blob,
      sourceBytes,
    });
    if (blob === null) return null;
    const next = { ...control, blob };
    // A non-null blob successor passed the blob machine's exit check.
    return wellFormedInteger(next, true) ? next : null;
  } catch {
    return null;
  }
};

export const finalizeMidgardCekDataInteger = (
  control: MidgardCekDataIntegerControl,
): MidgardCekDataIntegerSummary | null =>
  isWellFormedMidgardCekDataIntegerControl(control)
    ? finalizeValidatedMidgardCekDataInteger(control)
    : null;

/** Package-internal; see `encodeValidatedMidgardCekDataIntegerControl`. */
export const finalizeValidatedMidgardCekDataInteger = (
  control: MidgardCekDataIntegerControl,
): MidgardCekDataIntegerSummary | null => {
  if (control.stage !== MidgardCekDataIntegerStages.Terminal) {
    return null;
  }
  const cborRoot = finalizeValidatedMidgardCekSourceBlob(control.blob!);
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
  // Every control here is the validated initial control or a successor that
  // passed the check below.
  while (control.stage !== MidgardCekDataIntegerStages.Terminal) {
    const span = nextValidatedMidgardCekDataIntegerSpan(
      control,
      sourceStart + bytes.length,
    );
    const sourceBytes =
      span === null
        ? null
        : bytes.subarray(
            span.absoluteStart - sourceStart,
            span.absoluteStart - sourceStart + span.length,
          );
    const next = advanceValidatedMidgardCekDataInteger({
      control,
      sourceBytes,
      sourceEnd: sourceStart + bytes.length,
    });
    // A Measure-stage successor is returned unchecked by the machine, so the
    // integer fields are re-checked here; every successor's blob is an initial
    // blob, unchanged, or the blob machine's exit-checked successor.
    if (
      next === null ||
      !isWellFormedMidgardCekDataIntegerControlWithValidatedBlob(next)
    ) {
      throw new Error("V1 CEK Data integer trace failed closed");
    }
    steps.push({ control, sourceBytes, next });
    control = next;
  }
  if (control.sourceLength !== bytes.length)
    throw new Error("V1 CEK Data integer trace failed closed: trailing bytes");
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};

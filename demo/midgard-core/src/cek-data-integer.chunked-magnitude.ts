import {
  isWellFormedMidgardCekDataIntegerControl,
  MIDGARD_CEK_DATA_INTEGER_VERSION,
  type MidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import {
  initialMidgardCekSourceBlobControl,
  type MidgardCekSourceBlobSpan,
} from "./cek-source-blob.js";

export const measuredMagnitudeLength = (
  sourceLength: number,
): number | null => {
  if (sourceLength < 68) return null;
  const encoded = sourceLength - 2;
  const full = Math.floor(encoded / 66);
  const remainder = encoded % 66;
  if (remainder === 0) return full * 64;
  if (remainder >= 2 && remainder <= 24) return full * 64 + remainder - 1;
  if (remainder >= 26 && remainder <= 65) return full * 64 + remainder - 2;
  return null;
};

export const parseChunkedMagnitudeSyntax = (
  syntaxBytes: Uint8Array,
  sourceLength: number,
): bigint | null => {
  const length = measuredMagnitudeLength(sourceLength - 1);
  if (
    length === null ||
    length <= 64 ||
    syntaxBytes[2] !== 0x58 ||
    syntaxBytes[3] !== 64 ||
    syntaxBytes[4] === undefined ||
    syntaxBytes[4] === 0
  )
    return null;
  return 4n + BigInt(length) + (syntaxBytes[4] >= 128 ? 1n : 0n);
};

/** A measuring step consumes one authenticated header window. */
export const advanceChunkedMagnitude = (
  control: MidgardCekDataIntegerControl,
  sourceEnd: number,
  sourceBytes: Uint8Array | null | undefined,
  span: MidgardCekSourceBlobSpan | null,
): MidgardCekDataIntegerControl | null => {
  if (
    sourceBytes == null ||
    span === null ||
    sourceBytes.length !== span.length
  )
    return null;
  const first = sourceBytes[0]!;
  if (first === 0xff) {
    const magnitudeLength = measuredMagnitudeLength(control.sourceLength);
    if (magnitudeLength === null || magnitudeLength <= 64) return null;
    const sourceLength = control.sourceLength + 1;
    return {
      ...control,
      stage: MidgardCekDataIntegerStages.Blob,
      sourceLength,
      blob: initialMidgardCekSourceBlobControl({
        sourceStart: control.sourceStart,
        sourceLength,
      }),
    };
  }
  if ((control.sourceLength - 2) % 66 !== 0) return null;
  const headerLength = first >= 0x41 && first <= 0x57 ? 1 : 2;
  const length =
    headerLength === 1
      ? first - 0x40
      : first === 0x58 &&
          span.length >= 2 &&
          sourceBytes[1]! >= 24 &&
          sourceBytes[1]! <= 64
        ? sourceBytes[1]!
        : -1;
  const isFirst = control.sourceLength === 2;
  if (
    length < 1 ||
    span.absoluteStart + headerLength + length >= sourceEnd ||
    (isFirst && (length !== 64 || span.length < 3 || sourceBytes[2] === 0))
  )
    return null;
  return {
    ...control,
    sourceLength: control.sourceLength + headerLength + length,
    memory: isFirst
      ? 4n + BigInt(length) + (sourceBytes[2]! >= 128 ? 1n : 0n)
      : control.memory + BigInt(length),
  };
};

/** The tag/opener have already been authenticated by the enclosing head. */
export const initialMidgardCekDataIntegerMeasureControl = ({
  sourceStart,
}: {
  readonly sourceStart: number;
}): MidgardCekDataIntegerControl => {
  const control = {
    version: MIDGARD_CEK_DATA_INTEGER_VERSION,
    stage: MidgardCekDataIntegerStages.Measure,
    sourceStart,
    sourceLength: 2,
    memory: 0n,
    blob: null,
  } satisfies MidgardCekDataIntegerControl;
  if (!isWellFormedMidgardCekDataIntegerControl(control))
    throw new Error("Invalid V1 CEK Data integer range");
  return control;
};

export const chunkedMagnitudeSpan = (
  control: MidgardCekDataIntegerControl,
  sourceEnd: number,
): MidgardCekSourceBlobSpan | null => {
  const absoluteStart = control.sourceStart + control.sourceLength;
  return absoluteStart < sourceEnd
    ? { absoluteStart, length: Math.min(3, sourceEnd - absoluteStart) }
    : null;
};

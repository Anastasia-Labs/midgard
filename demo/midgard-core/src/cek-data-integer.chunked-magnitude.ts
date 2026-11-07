import {
  isWellFormedMidgardCekDataIntegerControl,
  MIDGARD_CEK_DATA_INTEGER_VERSION,
  type MidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
} from "./cek-data-integer.js";
import { midgardCekBytesMemoryWords } from "./cek-memory-size.js";
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

/** The widest measured chunk: a two-byte head and 64 bytes. */
export const MIDGARD_CEK_DATA_INTEGER_MEASURE_BYTES = 66;

const isAllOnes = (bytes: Uint8Array): boolean =>
  bytes.every((byte) => byte === 0xff);

/**
 * Head check for an indefinite magnitude: the extent decodes to more than 64
 * bytes and the first chunk is a full 64-byte chunk with a nonzero leading
 * byte. Its memory comes from the measure stage, chunk by chunk.
 */
export const isCanonicalChunkedMagnitudeHead = (
  syntaxBytes: Uint8Array,
  sourceLength: number,
): boolean => {
  const length = measuredMagnitudeLength(sourceLength - 1);
  return (
    length !== null &&
    length > 64 &&
    syntaxBytes[2] === 0x58 &&
    syntaxBytes[3] === 64 &&
    syntaxBytes[4] !== undefined &&
    syntaxBytes[4] !== 0
  );
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
    // While a tag 3 magnitude stays all one bits, |n| = magnitude + 1 gains a
    // word exactly when the magnitude fills whole words.
    const carry = control.memory - 4n - BigInt(magnitudeLength);
    return {
      ...control,
      stage: MidgardCekDataIntegerStages.Blob,
      sourceLength,
      memory:
        4n +
        midgardCekBytesMemoryWords(magnitudeLength) +
        (carry === 1n && magnitudeLength % 8 === 0 ? 1n : 0n),
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
  const measured = BigInt(Math.floor((control.sourceLength - 2) / 66) * 64);
  const carry = isFirst ? control.memory : control.memory - 4n - measured;
  const stillOnes =
    carry === 1n &&
    isAllOnes(sourceBytes.subarray(headerLength, headerLength + length));
  return {
    ...control,
    sourceLength: control.sourceLength + headerLength + length,
    memory: 4n + measured + BigInt(length) + (stillOnes ? 1n : 0n),
  };
};

/**
 * The tag/opener have already been authenticated by the enclosing head.
 * Before the first chunk the memory field holds the sign (1 for tag 3).
 */
export const initialMidgardCekDataIntegerMeasureControl = ({
  sourceStart,
  negative,
}: {
  readonly sourceStart: number;
  readonly negative: boolean;
}): MidgardCekDataIntegerControl => {
  const control = {
    version: MIDGARD_CEK_DATA_INTEGER_VERSION,
    stage: MidgardCekDataIntegerStages.Measure,
    sourceStart,
    sourceLength: 2,
    memory: negative ? 1n : 0n,
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
    ? {
        absoluteStart,
        length: Math.min(
          MIDGARD_CEK_DATA_INTEGER_MEASURE_BYTES,
          sourceEnd - absoluteStart,
        ),
      }
    : null;
};

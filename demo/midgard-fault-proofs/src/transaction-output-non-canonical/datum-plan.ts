import {
  type CanonicalDataStreamPlan,
  type CanonicalDataStreamStep,
  planCanonicalDataStream,
} from "../canonical-data-stream-plan.js";

/** Item chunk width shared with the envelope scan (`scan_chunk_bytes`). */
const SCAN_CHUNK_BYTES = 4_095;

/**
 * Execution memory a datum step spends besides its nodes: the thread's spend
 * and successor checks and the admission of one or two item chunks against
 * the stored chunk hashes, measured with margin. The family's datum lifecycle
 * suite submits the costliest step, and the emulator submit guard holds it to
 * the Van Rossem limits.
 */
export const TRANSACTION_OUTPUT_DATUM_STEP_FIXED_MEMORY = 500_000;

/** One datum-step transaction: the state it spends and what it reveals. */
export type TransactionOutputDatumStep = CanonicalDataStreamStep;

export type TransactionOutputDatumPlan = CanonicalDataStreamPlan;

const minimum = (left: number, right: number): number =>
  left < right ? left : right;

/** The window `advance_datum_v1` authenticates for a cursor. */
export const transactionOutputDatumWindow = ({
  itemLength,
  datumEnd,
  cursor,
}: {
  readonly itemLength: number;
  readonly datumEnd: number;
  readonly cursor: number;
}): { readonly start: number; readonly length: number } => {
  const chunkIndex = Math.floor(
    minimum(cursor, itemLength - 1) / SCAN_CHUNK_BYTES,
  );
  const base = chunkIndex * SCAN_CHUNK_BYTES;
  const currentLength = minimum(SCAN_CHUNK_BYTES, itemLength - base);
  const nextLength =
    datumEnd > base + currentLength
      ? minimum(SCAN_CHUNK_BYTES, itemLength - base - currentLength)
      : 0;
  return { start: base, length: currentLength + nextLength };
};

/**
 * The verdict and honest datum-step sequence for an output whose envelope
 * is canonical, each step reading as many nodes as fit one transaction over
 * the authenticated window.
 */
export const planTransactionOutputDatum = ({
  item,
  datumOffset,
  datumLength,
  fixedMemory = TRANSACTION_OUTPUT_DATUM_STEP_FIXED_MEMORY,
  target,
}: {
  readonly item: Uint8Array;
  readonly datumOffset: number;
  readonly datumLength: number;
  readonly fixedMemory?: number;
  readonly target?: number;
}): TransactionOutputDatumPlan => {
  if (
    !Number.isSafeInteger(datumOffset) ||
    !Number.isSafeInteger(datumLength) ||
    datumOffset < 0 ||
    datumLength <= 0 ||
    datumOffset + datumLength > item.length
  )
    throw new Error("transaction-output-non-canonical: datum span is invalid");
  const datumEnd = datumOffset + datumLength;
  return planCanonicalDataStream({
    spanStart: datumOffset,
    windowAt: (cursor) => {
      const window = transactionOutputDatumWindow({
        itemLength: item.length,
        datumEnd,
        cursor,
      });
      const full = window.start + window.length >= datumEnd;
      return {
        ...window,
        bytes: item.subarray(
          window.start,
          full ? datumEnd : window.start + window.length,
        ),
        full,
      };
    },
    fixedMemory,
    ...(target === undefined ? {} : { target }),
    label: "transaction-output-non-canonical",
  });
};

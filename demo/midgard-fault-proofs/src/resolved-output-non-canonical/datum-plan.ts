import {
  type CanonicalDataStreamPlan,
  planCanonicalDataStream,
} from "../canonical-data-stream-plan.js";

/** `bounded_item_v1.chunk_bytes`: the output chunk width. */
export const RESOLVED_OUTPUT_DATUM_CHUNK_BYTES = 4_095;

/**
 * Execution memory a datum step spends besides its nodes: the thread's spend
 * and successor checks and the admission of one or two output chunks against
 * the item commitment, measured with margin. The family's datum lifecycle
 * suite submits the costliest step, and the emulator submit guard holds it to
 * the Van Rossem limits.
 */
export const RESOLVED_OUTPUT_DATUM_STEP_FIXED_MEMORY = 1_100_000;

/** The output window `advance_datum_v1` authenticates at a cursor. */
export const resolvedOutputDatumWindow = ({
  output,
  datumEnd,
  cursor,
}: {
  readonly output: Uint8Array;
  readonly datumEnd: number;
  readonly cursor: number;
}) => {
  const chunkIndex = Math.floor(
    Math.min(cursor, output.length - 1) / RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
  );
  const start = chunkIndex * RESOLVED_OUTPUT_DATUM_CHUNK_BYTES;
  const currentLength = Math.min(
    RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
    output.length - start,
  );
  const nextLength =
    datumEnd > start + currentLength
      ? Math.min(
          RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
          output.length - start - currentLength,
        )
      : 0;
  const length = currentLength + nextLength;
  const full = start + length >= datumEnd;
  return {
    chunkIndex,
    start,
    length,
    bytes: output.subarray(start, full ? datumEnd : start + length),
    full,
  };
};

export type ResolvedOutputDatumPlan = CanonicalDataStreamPlan & {
  readonly datumStart: number;
  readonly datumEnd: number;
};

/**
 * The verdict and honest datum-step sequence for the inline datum at
 * `[datumOffset, datumOffset + datumLength)` of an output whose envelope is
 * canonical.
 */
export const planResolvedOutputDatum = ({
  output,
  datumOffset,
  datumLength,
  fixedMemory = RESOLVED_OUTPUT_DATUM_STEP_FIXED_MEMORY,
  target,
}: {
  readonly output: Uint8Array;
  readonly datumOffset: number;
  readonly datumLength: number;
  readonly fixedMemory?: number;
  readonly target?: number;
}): ResolvedOutputDatumPlan => {
  const datumEnd = datumOffset + datumLength;
  if (
    !Number.isSafeInteger(datumOffset) ||
    !Number.isSafeInteger(datumLength) ||
    datumOffset < 0 ||
    datumLength <= 0 ||
    datumEnd > output.length
  )
    throw new Error("resolvedOutputNonCanonical: datum span is invalid");
  const plan = planCanonicalDataStream({
    spanStart: datumOffset,
    windowAt: (cursor) =>
      resolvedOutputDatumWindow({ output, datumEnd, cursor }),
    fixedMemory,
    ...(target === undefined ? {} : { target }),
    label: "resolvedOutputNonCanonical",
  });
  return Object.freeze({ ...plan, datumStart: datumOffset, datumEnd });
};

/** The datum step's state at a checkpoint (the opening one by default). */
export const resolvedOutputDatumState = (
  subject: unknown,
  itemCommitment: string,
  plan: ResolvedOutputDatumPlan,
  checkpoint: Readonly<{ cursor: number; stackRoot: string }> = {
    cursor: plan.datumStart,
    stackRoot: "",
  },
) => ({
  subject,
  item_commitment: itemCommitment,
  datum_start: BigInt(plan.datumStart),
  datum_end: BigInt(plan.datumEnd),
  cursor: BigInt(checkpoint.cursor),
  stack_root: checkpoint.stackRoot,
});

import { computeHash32 } from "@al-ft/midgard-core";

import {
  canonicalDataStreamFrameData,
  type CanonicalDataStreamPlan,
  type CanonicalDataStreamStep,
  planCanonicalDataStream,
} from "../canonical-data-stream-plan.js";

/** Span chunk width (`rule.data_chunk_bytes`). */
export const REDEEMER_DATA_CHUNK_BYTES = 4_095;

/**
 * Execution memory a data step spends besides its nodes: the thread's spend
 * and successor checks, the window's chunk hashes and the stored stack root,
 * measured with margin. `redeemer_canonicity_w1_step_max` in the family's fit ledger measures the
 * costliest step against it.
 */
export const REDEEMER_DATA_STEP_FIXED_MEMORY = 400_000;

const SUPPORTED_PURPOSES = new Set([0n, 1n, 3n, 6n]);

type Head = { readonly value: bigint; readonly next: number };

/** One definite, shortest-form head of `major` (`head_at_v1`). */
const headAt = (
  bytes: Uint8Array,
  offset: number,
  major: number,
): Head | null => {
  const first = bytes[offset];
  if (first === undefined || first >> 5 !== major) return null;
  const additional = first & 31;
  if (additional < 24) return { value: BigInt(additional), next: offset + 1 };
  const width =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : 0;
  if (width === 0 || offset + width >= bytes.length) return null;
  let value = 0n;
  for (let index = offset + 1; index <= offset + width; index += 1)
    value = (value << 8n) | BigInt(bytes[index]!);
  const least = width === 1 ? 24n : 1n << BigInt(4 * width);
  return value >= least ? { value, next: offset + 1 + width } : null;
};

/**
 * The data span `(start, length)` of a field-8 item whose envelope
 * `[purpose, index, bytes(data), [mem, steps]]` is canonical and ends at the
 * item's end (`data_span_v1`); `null` for any other item.
 */
export const redeemerDataSpan = (
  item: Uint8Array,
): { readonly start: number; readonly length: number } | null => {
  const outer = headAt(item, 0, 4);
  if (outer === null || outer.value !== 4n) return null;
  const purpose = headAt(item, outer.next, 0);
  if (purpose === null || !SUPPORTED_PURPOSES.has(purpose.value)) return null;
  const pointer = headAt(item, purpose.next, 0);
  if (pointer === null) return null;
  const data = headAt(item, pointer.next, 2);
  if (data === null) return null;
  const dataEnd = data.next + Number(data.value);
  if (data.value > BigInt(item.length)) return null;
  const exUnits = headAt(item, dataEnd, 4);
  if (exUnits === null || exUnits.value !== 2n) return null;
  const memory = headAt(item, exUnits.next, 0);
  if (memory === null) return null;
  const steps = headAt(item, memory.next, 0);
  if (steps === null || steps.next !== item.length) return null;
  return { start: data.next, length: Number(data.value) };
};

/** The span chunks' hashes the data step's state stores. */
export const redeemerDataChunkHashes = (
  span: Uint8Array,
): readonly string[] => {
  const hashes: string[] = [];
  for (
    let offset = 0;
    offset < span.length;
    offset += REDEEMER_DATA_CHUNK_BYTES
  )
    hashes.push(
      computeHash32(
        span.subarray(offset, offset + REDEEMER_DATA_CHUNK_BYTES),
      ).toString("hex"),
    );
  return Object.freeze(hashes);
};

/** The span window the data step authenticates at a cursor. */
export const redeemerDataWindow = (span: Uint8Array, cursor: number) => {
  const chunkIndex = Math.floor(
    Math.min(cursor, span.length - 1) / REDEEMER_DATA_CHUNK_BYTES,
  );
  const start = chunkIndex * REDEEMER_DATA_CHUNK_BYTES;
  const currentLength = Math.min(
    REDEEMER_DATA_CHUNK_BYTES,
    span.length - start,
  );
  const rest = span.length - start - currentLength;
  const nextLength = Math.min(REDEEMER_DATA_CHUNK_BYTES, rest);
  const length = currentLength + nextLength;
  return {
    start,
    length,
    bytes: span.subarray(start, start + length),
    full: nextLength === rest,
  };
};

export type RedeemerDataPlan = CanonicalDataStreamPlan & {
  readonly span: Uint8Array;
  readonly chunkHashes: readonly string[];
};

/**
 * The verdict and honest data-step sequence for a field-8 item: `null` when
 * the envelope is malformed or the data span is empty, which step 02 decides
 * without a data step.
 */
export const planRedeemerData = (
  item: Uint8Array,
  {
    fixedMemory = REDEEMER_DATA_STEP_FIXED_MEMORY,
    target,
  }: { readonly fixedMemory?: number; readonly target?: number } = {},
): RedeemerDataPlan | null => {
  const located = redeemerDataSpan(item);
  if (located === null || located.length === 0) return null;
  const span = item.subarray(located.start, located.start + located.length);
  const plan = planCanonicalDataStream({
    spanStart: 0,
    windowAt: (cursor) => redeemerDataWindow(span, cursor),
    fixedMemory,
    ...(target === undefined ? {} : { target }),
    label: "redeemer-canonicity",
  });
  return Object.freeze({
    ...plan,
    span,
    chunkHashes: redeemerDataChunkHashes(span),
  });
};

/** The data step's spend arguments for one planned step. */
export const redeemerDataStepArgs = (
  plan: RedeemerDataPlan,
  step: CanonicalDataStreamStep,
) => ({
  window: Buffer.from(
    plan.span.subarray(step.windowStart, step.windowStart + step.windowLength),
  ).toString("hex"),
  revealed: step.stack
    .slice(0, step.revealedCount)
    .map(canonicalDataStreamFrameData),
  tail_root: step.tailRoot,
  node_budget: BigInt(step.nodeBudget),
});

/** The data step's opening state: the span's chunk hashes at cursor zero. */
export const redeemerDataScanState = (
  bound: Readonly<{
    subject: unknown;
    witness_set_hash: string;
    redeemer_index: bigint;
  }>,
  plan: RedeemerDataPlan,
  checkpoint: Readonly<{ cursor: number; stackRoot: string }> = {
    cursor: 0,
    stackRoot: "",
  },
) => ({
  bound,
  data_length: BigInt(plan.span.length),
  chunk_hashes: [...plan.chunkHashes],
  cursor: BigInt(checkpoint.cursor),
  stack_root: checkpoint.stackRoot,
});

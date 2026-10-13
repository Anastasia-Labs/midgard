import {
  MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
  type MidgardDataStreamFrame,
  midgardDataStreamStackRoot,
  runMidgardDataStream,
  runMidgardDataStreamTracked,
} from "@al-ft/midgard-core";

/** Item chunk width shared with the envelope scan (`scan_chunk_bytes`). */
const SCAN_CHUNK_BYTES = 4_095;

/**
 * Nodes the datum step reads per transaction. The on-chain step takes any
 * budget; this one keeps the costliest node shape (a generic constructor
 * opening with an eight-byte alternative, which also hashes its new frame)
 * inside one transaction's execution budget with margin.
 * `tonc_datum_verdict_step_max` in the family's fit ledger measures it.
 */
export const TRANSACTION_OUTPUT_DATUM_NODE_BUDGET = 220;

/** One datum-step transaction: the state it spends and what it reveals. */
export type TransactionOutputDatumStep = {
  /** Item offset of the next node. */
  readonly cursor: number;
  /** Every open container, top first; the stored root is their hash chain. */
  readonly stack: readonly MidgardDataStreamFrame[];
  readonly stackRoot: string;
  /** Item offset of the window's first byte. */
  readonly windowStart: number;
  readonly windowLength: number;
  /** How many of `stack`'s top frames the redeemer reveals. */
  readonly revealedCount: number;
  readonly tailRoot: string;
  readonly nodeBudget: number;
  /** The state the step continues to, or the verdict it finishes with. */
  readonly next:
    | {
        readonly kind: "continue";
        readonly cursor: number;
        readonly stack: readonly MidgardDataStreamFrame[];
        readonly stackRoot: string;
      }
    | { readonly kind: "finish"; readonly canonical: boolean };
};

export type TransactionOutputDatumPlan = {
  readonly canonical: boolean;
  readonly steps: readonly TransactionOutputDatumStep[];
};

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
 * The honest datum-step sequence for an output whose envelope is canonical:
 * each step reads `nodeBudget` nodes over the authenticated window and
 * reveals one frame more than it pops, so its run equals a run with the
 * whole stack revealed.
 */
export const planTransactionOutputDatum = ({
  item,
  datumOffset,
  datumLength,
  nodeBudget = TRANSACTION_OUTPUT_DATUM_NODE_BUDGET,
}: {
  readonly item: Uint8Array;
  readonly datumOffset: number;
  readonly datumLength: number;
  readonly nodeBudget?: number;
}): TransactionOutputDatumPlan => {
  if (
    !Number.isSafeInteger(datumOffset) ||
    !Number.isSafeInteger(datumLength) ||
    datumOffset < 0 ||
    datumLength <= 0 ||
    datumOffset + datumLength > item.length
  )
    throw new Error("transaction-output-non-canonical: datum span is invalid");
  if (!Number.isSafeInteger(nodeBudget) || nodeBudget < 1)
    throw new Error("transaction-output-non-canonical: node budget is invalid");
  const datumEnd = datumOffset + datumLength;
  const steps: TransactionOutputDatumStep[] = [];
  let cursor = datumOffset;
  let stack: readonly MidgardDataStreamFrame[] = [];
  for (;;) {
    const window = transactionOutputDatumWindow({
      itemLength: item.length,
      datumEnd,
      cursor,
    });
    const full = window.start + window.length >= datumEnd;
    const bytes = item.subarray(
      window.start,
      full ? datumEnd : window.start + window.length,
    );
    const common = {
      bytes,
      full,
      rootStart: datumOffset - window.start,
      offset: cursor - window.start,
      budget: nodeBudget,
    };
    const { consumed } = runMidgardDataStreamTracked({
      ...common,
      frames: stack,
      tailRoot: MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    });
    const revealedCount = minimum(stack.length, consumed + 1);
    const revealed = stack.slice(0, revealedCount);
    const hidden = stack.slice(revealedCount);
    const tailRoot = midgardDataStreamStackRoot(
      hidden,
      MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    );
    const outcome = runMidgardDataStream({
      ...common,
      frames: revealed,
      tailRoot,
    });
    const step = {
      cursor,
      stack,
      stackRoot: midgardDataStreamStackRoot(revealed, tailRoot).toString("hex"),
      windowStart: window.start,
      windowLength: window.length,
      revealedCount,
      tailRoot: tailRoot.toString("hex"),
      nodeBudget,
    };
    if (outcome.kind !== "paused") {
      const canonical = outcome.kind === "canonical";
      steps.push(
        Object.freeze({
          ...step,
          next: Object.freeze({ kind: "finish" as const, canonical }),
        }),
      );
      return Object.freeze({ canonical, steps: Object.freeze(steps) });
    }
    if (outcome.nodes === 0)
      throw new Error(
        "transaction-output-non-canonical: datum step made no progress",
      );
    const nextStack = Object.freeze([...outcome.frames, ...hidden]);
    const nextCursor = window.start + outcome.offset;
    steps.push(
      Object.freeze({
        ...step,
        next: Object.freeze({
          kind: "continue" as const,
          cursor: nextCursor,
          stack: nextStack,
          stackRoot: midgardDataStreamStackRoot(
            outcome.frames,
            tailRoot,
          ).toString("hex"),
        }),
      }),
    );
    cursor = nextCursor;
    stack = nextStack;
  }
};

/** A frame as the `StreamFrameV1` Data the step redeemer carries. */
export const transactionOutputDatumFrameData = (
  frame: MidgardDataStreamFrame,
) => {
  switch (frame.kind) {
    case "list":
      return { ListFrame: { started: frame.started } };
    case "map":
      return { MapFrame: { remaining: frame.remaining } };
    case "chunks":
      return {
        ChunksFrame: { seen: BigInt(frame.seen), leading: frame.leading },
      };
  }
};

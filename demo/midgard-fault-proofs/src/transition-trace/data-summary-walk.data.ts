/**
 * The Data encodings of the data summary walk's state, in the on-chain types'
 * constructor order, and the hash-chained stack they commit.
 */
import type {
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { Constr, Data } from "@lucid-evolution/lucid";

import type {
  DataSummaryWalkEntry,
  DataSummaryWalkFrame,
  DataSummaryWalkPoint,
  DataSummaryWalkStack,
} from "./data-summary-walk.types.js";

export const EMPTY_STACK_ROOT = Buffer.alloc(0);

const sequenceData = (sequence: MidgardCekDataSequenceSummary): Data =>
  new Constr(0, [
    Buffer.from(sequence.root).toString("hex"),
    sequence.length,
    sequence.payloadCborLength,
    sequence.memory,
  ]);

const summaryData = (summary: MidgardCekDataSummary): Data =>
  new Constr(0, [
    Buffer.from(summary.root).toString("hex"),
    summary.cborLength,
    summary.memory,
  ]);

export const dataSummaryWalkFrameData = (frame: DataSummaryWalkFrame): Data => {
  switch (frame.kind) {
    case "root":
      return new Constr(0, []);
    case "list":
      return new Constr(1, []);
    case "constr":
      return new Constr(2, [frame.constructor]);
    case "largeConstr":
      return new Constr(3, [
        Buffer.from(frame.indexRoot).toString("hex"),
        frame.indexCborLength,
        frame.indexMemory,
      ]);
    case "map":
      return new Constr(4, [frame.remaining]);
    case "folding":
      return new Constr(5, [
        dataSummaryWalkFrameData(frame.open),
        sequenceData(frame.folded),
      ]);
  }
};

export const dataSummaryWalkEntryData = (entry: DataSummaryWalkEntry): Data =>
  entry.kind === "item"
    ? new Constr(0, [summaryData(entry.summary)])
    : new Constr(1, [dataSummaryWalkFrameData(entry.frame)]);

export const pushDataSummaryWalkEntry = (
  entry: DataSummaryWalkEntry,
  below: DataSummaryWalkStack,
): DataSummaryWalkStack => ({
  entry,
  below,
  depth: (below?.depth ?? 0) + 1,
  chain: computeHash32(
    Buffer.concat([
      Buffer.from(Data.to(dataSummaryWalkEntryData(entry)), "hex"),
      below?.chain ?? EMPTY_STACK_ROOT,
    ]),
  ),
});

/** The on-chain `Walk { offset, frame, stack_root }` of a checkpoint. */
export const dataSummaryWalkData = (point: DataSummaryWalkPoint): Data =>
  new Constr(0, [
    BigInt(point.at),
    dataSummaryWalkFrameData(point.frame),
    (point.stack?.chain ?? EMPTY_STACK_ROOT).toString("hex"),
  ]);

/** The on-chain `WalkWitness { restore, base, steps }`. */
export const dataSummaryWalkWitnessData = (transaction: {
  readonly restore: readonly DataSummaryWalkEntry[];
  readonly base: Buffer;
  readonly steps: number;
}): Data =>
  new Constr(0, [
    transaction.restore.map(dataSummaryWalkEntryData),
    transaction.base.toString("hex"),
    BigInt(transaction.steps),
  ]);

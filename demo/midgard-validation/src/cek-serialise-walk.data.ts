/**
 * The Data encodings of the serialise reader's state, in the on-chain types'
 * constructor order (`data_serialise_walk_v1`), and the hash-chained stack
 * they commit.
 */
import type {
  MidgardCekBlobFrontier,
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { Constr, type Data } from "@lucid-evolution/lucid";

import type {
  MidgardCekSerialiseEntry,
  MidgardCekSerialiseFrame,
  MidgardCekSerialiseStack,
} from "./cek-serialise-walk.types.js";

export const MIDGARD_CEK_SERIALISE_EMPTY_STACK_ROOT = Buffer.alloc(0);

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const sequenceData = (sequence: MidgardCekDataSequenceSummary): Data =>
  new Constr(0, [
    hex(sequence.root),
    sequence.length,
    sequence.payloadCborLength,
    sequence.memory,
  ]);

const summaryData = (summary: MidgardCekDataSummary): Data =>
  new Constr(0, [hex(summary.root), summary.cborLength, summary.memory]);

const booleanData = (value: boolean): Data => new Constr(value ? 1 : 0, []);

/** `FrontierV1 { count, length, peaks }`, peaks smallest first. */
export const midgardCekSerialiseFrontierData = (
  frontier: MidgardCekBlobFrontier,
): Data =>
  new Constr(0, [
    BigInt(frontier.count),
    frontier.byteLength,
    frontier.peaks.map(
      (peak) =>
        new Constr(0, [BigInt(peak.height), hex(peak.root), peak.byteLength]),
    ),
  ]);

export const midgardCekSerialiseFrameData = (
  frame: MidgardCekSerialiseFrame,
): Data => {
  switch (frame.kind) {
    case "root":
      return new Constr(0, []);
    case "list":
      return new Constr(1, []);
    case "constr":
      return new Constr(2, [frame.constructor]);
    case "largeConstr":
      return new Constr(3, [
        hex(frame.indexRoot),
        frame.indexCborLength,
        frame.indexMemory,
      ]);
    case "map":
      return new Constr(4, [frame.remaining]);
    case "folding":
      return new Constr(5, [
        midgardCekSerialiseFrameData(frame.open),
        sequenceData(frame.folded),
      ]);
    case "leaf":
      return new Constr(6, [
        midgardCekSerialiseFrameData(frame.parent),
        BigInt(frame.tag),
        midgardCekSerialiseFrontierData(frame.leaf),
        hex(frame.pending),
        frame.length,
        frame.cborLength,
        booleanData(frame.allFf),
      ]);
  }
};

export const midgardCekSerialiseEntryData = (
  entry: MidgardCekSerialiseEntry,
): Data =>
  entry.kind === "item"
    ? new Constr(0, [summaryData(entry.summary)])
    : new Constr(1, [midgardCekSerialiseFrameData(entry.frame)]);

const link = (entry: MidgardCekSerialiseEntry, below: Buffer): Buffer =>
  computeHash32(
    Buffer.concat([
      lucidDataToCborIterative(midgardCekSerialiseEntryData(entry)),
      below,
    ]),
  );

export const pushMidgardCekSerialiseEntry = (
  entry: MidgardCekSerialiseEntry,
  below: MidgardCekSerialiseStack,
): MidgardCekSerialiseStack => ({
  entry,
  below,
  depth: (below?.depth ?? 0) + 1,
  chain: link(entry, below?.chain ?? MIDGARD_CEK_SERIALISE_EMPTY_STACK_ROOT),
});

/** The hash chain of `entries` (top first) above `base`. */
export const midgardCekSerialiseChain = (
  entries: readonly MidgardCekSerialiseEntry[],
  base: Buffer,
): Buffer => entries.reduceRight((below, entry) => link(entry, below), base);

/**
 * The hash chain of `stack` above `base`, where the walk left `stack` over
 * `restored` (a stack built above nothing): nodes `stack` still shares with
 * `restored` keep their place, so only the entries pushed above them are
 * linked again.
 */
export const midgardCekSerialiseChainOver = (
  stack: MidgardCekSerialiseStack,
  restored: MidgardCekSerialiseStack,
  base: Buffer,
): Buffer => {
  const shared: NonNullable<MidgardCekSerialiseStack>[] = [];
  for (let node = restored; node !== null; node = node.below) shared.push(node);
  const over = new Map<MidgardCekSerialiseStack, Buffer>([[null, base]]);
  let below = base;
  for (const node of shared.reverse()) {
    below = link(node.entry, below);
    over.set(node, below);
  }
  const pushed: MidgardCekSerialiseEntry[] = [];
  let node = stack;
  while (!over.has(node)) {
    pushed.push(node!.entry);
    node = node!.below;
  }
  return midgardCekSerialiseChain(pushed, over.get(node)!);
};

/** The chain root of `stack`. */
export const midgardCekSerialiseStackRoot = (
  stack: MidgardCekSerialiseStack,
): Buffer => stack?.chain ?? MIDGARD_CEK_SERIALISE_EMPTY_STACK_ROOT;

/** The top `count` entries of `stack`, top first, and the stack below them. */
export const takeMidgardCekSerialiseEntries = (
  stack: MidgardCekSerialiseStack,
  count: number,
): {
  readonly entries: readonly MidgardCekSerialiseEntry[];
  readonly below: MidgardCekSerialiseStack;
} => {
  const entries: MidgardCekSerialiseEntry[] = [];
  let below = stack;
  for (let taken = 0; taken < count; taken += 1) {
    if (below === null) throw new Error("the stack has fewer entries");
    entries.push(below.entry);
    below = below.below;
  }
  return { entries, below };
};

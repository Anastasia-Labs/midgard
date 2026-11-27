import {
  buildMidgardCekDataTraverseTrace,
  type MidgardCekStagedOutRef,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";

import {
  MIDGARD_CEK_SERIALISE_START,
  type MidgardCekSerialiseLoop,
  type MidgardCekSerialisePlannedHop,
  type MidgardCekSerialisePoint,
  planMidgardCekSerialiseHop,
} from "../../src/cek-serialise-loop.hops.js";
import {
  advanceMidgardCekSerialiseHop,
  type MidgardCekSerialiseFacts,
  midgardCekSerialiseFactsOf,
} from "../../src/cek-serialise-loop.js";

/**
 * Loops of serialiseData for the serialise loop's off-chain twin tests: R
 * staged in runs of three chunks on the staging outputs the Aiken fixture
 * names, and the hops an honest prover plans over them.
 */

export const summaryOf = (source: Buffer) => {
  const summary = buildMidgardCekDataTraverseTrace({ sourceStart: 0, source })
    .terminal.result;
  if (summary === null || summary === undefined)
    throw new Error("not canonical Data");
  return summary;
};

/** The staging output of run `index`, as the Aiken fixture names it. */
export const refOf = (index: number): MidgardCekStagedOutRef => ({
  transactionId: computeHash32(Buffer.from([10 + index])),
  outputIndex: 10 + index,
});

export const runsOf = (chunks: readonly Buffer[]): Buffer[][] => {
  const runs: Buffer[][] = [];
  for (let index = 0; index < chunks.length; index += 3)
    runs.push(chunks.slice(index, index + 3));
  return runs;
};

/** The loop of serialiseData over `argument`, its result staged as `staged`. */
export const loopOf = (
  argument: Buffer,
  staged: Buffer = argument,
): MidgardCekSerialiseLoop => {
  const { facts, chunks } = midgardCekSerialiseFactsOf(
    summaryOf(argument),
    staged,
  );
  const runs = runsOf(chunks);
  return { facts, runs, refs: runs.map((_, index) => refOf(index)) };
};

export const hexLoop = (hex: string) => loopOf(Buffer.from(hex, "hex"));

/** Plans and runs every hop at most `limit` steps long; the hops it ran. */
export const drive = (
  loop: MidgardCekSerialiseLoop,
  limit?: number,
): MidgardCekSerialisePlannedHop[] => {
  const hops: MidgardCekSerialisePlannedHop[] = [];
  let point: MidgardCekSerialisePoint | null = MIDGARD_CEK_SERIALISE_START;
  while (point !== null) {
    const planned = planMidgardCekSerialiseHop(loop, point, { limit });
    hops.push(planned);
    point = planned.point;
  }
  return hops;
};

export const advance = (
  loop: { readonly facts: MidgardCekSerialiseFacts },
  planned: MidgardCekSerialisePlannedHop,
  edit: Partial<MidgardCekSerialisePlannedHop> = {},
) => {
  const hop = { ...planned, ...edit };
  return advanceMidgardCekSerialiseHop(
    loop.facts,
    hop.hop,
    hop.referenceInputs,
    hop.from,
    hop.end,
  );
};

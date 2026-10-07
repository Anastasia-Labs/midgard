import {
  type BlockPoint,
  chainPoint,
  type ChainSyncEvent,
} from "@al-ft/l1-node-transport";

import { BlockDecodeError, decodeBlock } from "../decode/block.js";
import type {
  ApplyResult,
  FactStore,
  RewindResult,
} from "../store/fact-store.js";
import type { Point } from "../types.js";

/** A transport block point as a store point. */
export const storePoint = (point: BlockPoint): Point => ({
  slot: Number(point.slot),
  hash: Buffer.from(point.hash, "hex"),
});

/** A store point as a transport block point. */
export const transportPoint = (point: Point): BlockPoint =>
  chainPoint(BigInt(point.slot), point.hash.toString("hex"));

/** A raw block the decoder refused: the follower cannot advance past it. */
export type BlockUndecodable = Readonly<{
  kind: "block_undecodable";
  point: BlockPoint;
  detail: string;
}>;

/**
 * What one chain-sync event did to the store. `store_locked` (the writer
 * lease was lost or the store fenced by a newer writer) changed nothing and
 * is transient: the caller stops applying, acknowledges nothing and calls
 * `start()` again, then reopens chain-sync from the store's cursor.
 */
export type FollowStep =
  | Readonly<{ event: "roll_forward"; result: ApplyResult | BlockUndecodable }>
  | Readonly<{ event: "roll_backward"; result: RewindResult }>;

/**
 * The sequential writer's step for one transport event (plan §4.1, S1 with
 * F6 deferred): a block is decoded and applied in one transaction, a
 * rollback rewinds the store to its point. A rollback to the chain's genesis
 * lies below every origin, so it is the R1 case without touching the store.
 * The caller acknowledges `event.seq` only after a result that changed or
 * confirmed the store (`applied`, `rewound`, `noop`).
 */
export const applyChainSyncEvent = async (
  store: FactStore,
  event: ChainSyncEvent,
): Promise<FollowStep> => {
  if (event.kind === "roll_backward") {
    if (event.point.kind === "origin")
      return {
        event: "roll_backward",
        result: {
          kind: "intervention",
          reason: "rollback_beyond_k",
          detail: "roll_backward to the chain genesis, below the origin",
        },
      };
    return {
      event: "roll_backward",
      result: await store.rewind(storePoint(event.point)),
    };
  }
  let block;
  try {
    block = decodeBlock(event.block);
  } catch (error) {
    if (!(error instanceof BlockDecodeError)) throw error;
    return {
      event: "roll_forward",
      result: {
        kind: "block_undecodable",
        point: event.point,
        detail: error.message,
      },
    };
  }
  return { event: "roll_forward", result: await store.applyBlock(block) };
};

/**
 * Whether a step left the store on the event's point (safe to acknowledge).
 * Never true for `store_locked`, `error`, an intervention or an undecodable
 * block.
 */
export const stepSettled = (step: FollowStep): boolean =>
  step.result.kind === "applied" ||
  step.result.kind === "rewound" ||
  step.result.kind === "noop";

/**
 * Whether a step found the store locked: another writer holds or took over
 * its lease. Transient, never an intervention: stop applying and call
 * `start()` again.
 */
export const stepLocked = (step: FollowStep): boolean =>
  step.result.kind === "store_locked";

const RECENT_POINTS = 64;
const MAX_POINTS = 256;

/**
 * The intersection candidates for (re)opening chain-sync from the store
 * (§4.2), best first: the cursor and the 63 blocks below it, then blocks at
 * exponentially growing distances, then the origin. Empty when the store has
 * no cursor (it was never initialized).
 */
export const intersectionPoints = async (
  store: FactStore,
): Promise<BlockPoint[]> => {
  const cursor = await store.cursor();
  if (cursor === null) return [];
  const origin = await store.blockByHash(cursor.origin.hash);
  const floor = origin?.height ?? 0;
  const heights: number[] = [];
  for (let i = 0; i < RECENT_POINTS; i += 1) heights.push(cursor.height - i);
  for (let gap = RECENT_POINTS * 2; cursor.height - gap > floor; gap *= 2)
    heights.push(cursor.height - gap);
  const points: BlockPoint[] = [];
  for (const height of heights) {
    if (height <= floor || points.length >= MAX_POINTS - 1) break;
    const block = await store.blockAtHeight(height);
    if (block !== null)
      points.push(transportPoint({ slot: block.slot, hash: block.hash }));
  }
  points.push(transportPoint(cursor.origin));
  return points;
};

import type {
  FollowerBoundary,
  FollowerPoint,
} from "../../src/l1/follower/availability-reads.js";

/** The follower boundary at `point` under view `generation`. */
export const followerBoundary = (
  point: FollowerPoint,
  generation = 0,
): FollowerBoundary => ({
  ...point,
  pointId: `${point.slot.toString()}:${point.blockHash}`,
  generation,
  view: {
    generation,
    point: { slot: point.slot, hash: Buffer.from(point.blockHash, "hex") },
    height: point.blockNo,
  },
});

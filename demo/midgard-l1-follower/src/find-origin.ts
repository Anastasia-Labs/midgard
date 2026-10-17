import {
  type ChainPoint,
  type ChainSyncStream,
  IntersectNotFoundError,
  type L1NodeTransport,
  ORIGIN,
  samePoint,
} from "@al-ft/l1-node-transport";
import {
  checkL1OriginBeforeHubOracleNonceBlock,
  type L1Origin,
} from "@al-ft/midgard-core/l1-origin";

import { decodeBlock } from "./decode/block.js";

/** N2C block types 0 and 1 are Byron blocks, which hold no Midgard tx. */
const FIRST_SHELLEY_BLOCK_TYPE = 2;
const SCAN_CREDIT = 50;

export type FindOriginOptions = Readonly<{
  transport: Pick<L1NodeTransport, "openChainSync">;
  /** The `prepareHubOracleNonce` tx id, 64 lowercase hex. */
  txHash: string;
  /** A known point before the tx's block; the scan starts after it. Default: genesis. */
  from?: L1Origin;
  /** Called with each scanned block and the node tip's height, for progress output. */
  onBlock?: (
    block: L1Origin & Readonly<{ height: number }>,
    tipHeight: number,
  ) => void;
}>;

export type FindOriginResult =
  | Readonly<{
      kind: "found";
      /** The point immediately before the block holding the tx: `l1Origin`. */
      origin: L1Origin;
      nonceBlock: L1Origin & Readonly<{ height: number }>;
      txIndex: number;
      /** Blocks from the nonce block to the node tip, the nonce block counted. */
      depth: number;
    }>
  /** The scan reached the node tip without the tx (or `from` is after its block). */
  | Readonly<{ kind: "not_found"; scannedTo: L1Origin | "genesis" }>
  | Readonly<{ kind: "from_not_on_chain" }>
  /** The tx is in the chain's first block, which has no preceding block point. */
  | Readonly<{ kind: "no_preceding_point" }>;

const asOrigin = (point: ChainPoint): L1Origin | "genesis" =>
  point.kind === "origin"
    ? "genesis"
    : { slot: Number(point.slot), blockHash: point.hash };

/**
 * Finds the deployment origin (l1-architecture-plan §5.3, §14 item 4): from
 * `from` (or genesis) it follows the node's chain to the block holding
 * `txHash` and returns the point immediately before that block. It stops
 * with `not_found` at the node tip. A one-time step for an existing
 * deployment; the origin is never guessed.
 */
export const findOrigin = async (
  options: FindOriginOptions,
): Promise<FindOriginResult> => {
  if (!/^[0-9a-f]{64}$/u.test(options.txHash))
    throw new TypeError("the tx id must be 64 lowercase hex characters");
  const start: ChainPoint =
    options.from === undefined
      ? ORIGIN
      : {
          kind: "point",
          slot: BigInt(options.from.slot),
          hash: options.from.blockHash,
        };
  const stream: ChainSyncStream = options.transport.openChainSync({
    points: [start],
    credit: SCAN_CREDIT,
  });
  try {
    let opened;
    try {
      opened = await stream.opened;
    } catch (error) {
      if (error instanceof IntersectNotFoundError)
        return { kind: "from_not_on_chain" };
      throw error;
    }
    let previous: ChainPoint = opened.intersection;
    if (samePoint(previous, opened.tip.point))
      return { kind: "not_found", scannedTo: asOrigin(previous) };
    for (;;) {
      const event = await stream.next();
      if (event === undefined)
        throw new Error("the chain-sync stream ended before the node tip");
      stream.ack(event.seq);
      if (event.kind === "roll_backward") {
        previous = event.point;
        continue;
      }
      options.onBlock?.(
        {
          slot: Number(event.point.slot),
          blockHash: event.point.hash,
          height: Number(event.blockNo),
        },
        Number(event.tip.blockNo),
      );
      const index =
        event.blockType < FIRST_SHELLEY_BLOCK_TYPE
          ? -1
          : decodeBlock(event.block).txs.findIndex(
              (tx) => tx.hash.toString("hex") === options.txHash,
            );
      if (index >= 0) {
        if (previous.kind === "origin") return { kind: "no_preceding_point" };
        if (event.prevHash !== previous.hash)
          throw new Error(
            `block ${event.point.slot}.${event.point.hash} does not extend the previous point ${previous.slot}.${previous.hash}`,
          );
        const origin = {
          slot: Number(previous.slot),
          blockHash: previous.hash,
        };
        const nonceBlock = {
          slot: Number(event.point.slot),
          blockHash: event.point.hash,
          height: Number(event.blockNo),
        };
        const order = checkL1OriginBeforeHubOracleNonceBlock(
          origin,
          nonceBlock,
        );
        if (!order.ok) throw new Error(order.reason);
        return {
          kind: "found",
          origin,
          nonceBlock,
          txIndex: index,
          depth: Number(event.tip.blockNo - event.blockNo) + 1,
        };
      }
      previous = event.point;
      if (samePoint(event.point, event.tip.point))
        return { kind: "not_found", scannedTo: asOrigin(event.point) };
    }
  } finally {
    await stream.close();
  }
};

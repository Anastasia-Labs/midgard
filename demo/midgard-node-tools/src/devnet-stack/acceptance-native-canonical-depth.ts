/**
 * The acceptance harness's selected-chain depth of one block, read from the
 * devnet's Ogmios (the harness's own reader; the node reads its follower).
 */
import {
  normalizeOgmiosWebSocketUrl,
  ogmiosJsonRpcAnswerCode,
  openOgmiosSession,
  type WebSocketFactory,
} from "midgard-node/l1-external/kupmios-history";

const HEX_32 = /^[0-9a-f]{64}$/u;

/**
 * Proves this exact block belongs to the selected chain at the response's tip.
 * Its depth includes inclusion; `null` when the block is not on the selected
 * chain. Kupo's spend plus an independent tip height cannot prove ancestry:
 * Kupo may still name a shallow orphan from an old fork.
 */
export const canonicalOgmiosBlockDepth = async (args: {
  readonly ogmiosUrl: string;
  readonly blockHash: string;
  readonly slot: number;
  readonly blockNo: bigint;
  readonly timeoutMs: number;
  readonly webSocketFactory: WebSocketFactory;
}): Promise<bigint | null> => {
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(args.ogmiosUrl),
    timeoutMs: args.timeoutMs,
    webSocketFactory: args.webSocketFactory,
  });
  try {
    let result: { intersection?: unknown; tip?: unknown };
    try {
      result = (await session.request("findIntersection", {
        points: [{ slot: args.slot, id: args.blockHash }],
      })) as typeof result;
    } catch (error) {
      if (ogmiosJsonRpcAnswerCode(error) === 1000) return null;
      throw error;
    }
    const point = result.intersection as
      | { slot?: unknown; id?: unknown }
      | undefined;
    const tip = result.tip as
      | { slot?: unknown; id?: unknown; height?: unknown }
      | undefined;
    if (
      point?.slot !== args.slot ||
      point.id !== args.blockHash ||
      typeof tip?.id !== "string" ||
      !HEX_32.test(tip.id) ||
      typeof tip.slot !== "number" ||
      !Number.isSafeInteger(tip.slot) ||
      tip.slot < args.slot ||
      typeof tip.height !== "number" ||
      !Number.isSafeInteger(tip.height) ||
      BigInt(tip.height) < args.blockNo ||
      (BigInt(tip.height) === args.blockNo
        ? tip.id !== args.blockHash || tip.slot !== args.slot
        : tip.id === args.blockHash || tip.slot <= args.slot)
    ) {
      throw new Error(
        "Ogmios canonical block depth lacks an exact intersection and coherent selected-chain tip",
      );
    }
    return BigInt(tip.height) - args.blockNo + 1n;
  } finally {
    session.close();
  }
};

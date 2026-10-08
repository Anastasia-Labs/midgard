import {
  BASE16_BYTES,
  defaultWebSocketFactory,
} from "./l1-kupmios.fetch-kupo-spend.js";
import {
  DEFAULT_L1_BLOCK_SCAN_LIMIT,
  DEFAULT_L1_READ_TIMEOUT_MS,
  exactHeaderHash,
  exactSlot,
  type L1ChainPoint,
  normalizeOgmiosWebSocketUrl,
  type ObservedL1TransactionAtPoint,
  type WebSocketFactory,
} from "./l1-kupmios.l1-chain-point.js";
import {
  openOgmiosSession,
  parseObservedTransaction,
} from "./l1-kupmios.open-ogmios-session.js";

/**
 * Rolls chain-sync from `intersection` forward to `blockPoint` and returns the
 * named transaction out of that block.
 *
 * **Rollbacks fail the read; they never widen it.** The first `nextBlock` after
 * an intersection is always a roll *backward* to the intersection itself, and
 * that one is expected. A later backward roll means the chain moved under the
 * scan, so the block Kupo named may no longer be on it — the read refuses and
 * the caller's next pass sees whatever the chain settled on.
 */
export const readOgmiosBlockTransaction = async ({
  ogmiosUrl,
  intersection,
  blockPoint,
  txHash,
  webSocketFactory = defaultWebSocketFactory,
  timeoutMs = DEFAULT_L1_READ_TIMEOUT_MS,
  blockScanLimit = DEFAULT_L1_BLOCK_SCAN_LIMIT,
}: {
  readonly ogmiosUrl: string;
  readonly intersection: L1ChainPoint;
  readonly blockPoint: L1ChainPoint;
  readonly txHash: string;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Promise<ObservedL1TransactionAtPoint> => {
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(ogmiosUrl),
    timeoutMs,
    webSocketFactory,
  });
  try {
    const found = (await session.request("findIntersection", {
      points: [{ slot: intersection.slot, id: intersection.headerHash }],
    })) as { intersection?: unknown };
    if (found.intersection === undefined) {
      throw new Error(
        `Ogmios found no intersection at slot ${intersection.slot.toString()}`,
      );
    }
    let rolledBack = false;
    for (let scanned = 0; scanned < blockScanLimit; scanned += 1) {
      const next = (await session.request("nextBlock", {})) as {
        direction?: unknown;
        block?: unknown;
        tip?: unknown;
      };
      if (next.direction === "backward") {
        if (rolledBack) {
          throw new Error(
            "the chain rolled back while reading the order's creating block",
          );
        }
        rolledBack = true;
        // The intersection acknowledgement is not a scanned block.
        scanned -= 1;
        continue;
      }
      if (next.direction !== "forward") {
        throw new Error("Ogmios nextBlock answered with no direction");
      }
      const block = next.block as {
        id?: unknown;
        slot?: unknown;
        height?: unknown;
        transactions?: unknown;
      };
      const blockId = block.id;
      if (typeof blockId !== "string") {
        throw new Error("Ogmios nextBlock answered with an unidentified block");
      }
      if (blockId !== blockPoint.headerHash) {
        if (typeof block.slot === "number" && block.slot > blockPoint.slot) {
          throw new Error(
            `chain-sync passed slot ${blockPoint.slot.toString()} without ` +
              `reaching block ${blockPoint.headerHash}`,
          );
        }
        continue;
      }
      const transactions = Array.isArray(block.transactions)
        ? block.transactions
        : [];
      const index = transactions.findIndex(
        (transaction) => (transaction as { id?: unknown }).id === txHash,
      );
      if (index === -1) {
        throw new Error(
          `block ${blockPoint.headerHash} does not contain transaction ${txHash}`,
        );
      }
      const blockNo = exactSlot(block.height, "ogmios.block.height");
      const transactionCbor = (transactions[index] as { cbor?: unknown }).cbor;
      return {
        ...parseObservedTransaction(
          transactions[index],
          `ogmios.block(${blockPoint.headerHash}).transactions[${index.toString()}]`,
        ),
        blockPoint: {
          slot: exactSlot(block.slot, "ogmios.block.slot"),
          headerHash: exactHeaderHash(block.id, "ogmios.block.id"),
          blockNo,
        },
        transactionIndex: index,
        ...(next.tip !== undefined && next.tip !== "origin"
          ? {
              selectedChainTip: {
                id: exactHeaderHash(
                  (next.tip as { id?: unknown }).id,
                  "ogmios.nextBlock.tip.id",
                ),
                slot: exactSlot(
                  (next.tip as { slot?: unknown }).slot,
                  "ogmios.nextBlock.tip.slot",
                ),
                height: exactSlot(
                  (next.tip as { height?: unknown }).height,
                  "ogmios.nextBlock.tip.height",
                ),
              },
            }
          : {}),
        ...(typeof transactionCbor === "string" &&
        BASE16_BYTES.test(transactionCbor)
          ? { transactionCbor }
          : {}),
      };
    }
    throw new Error(
      `chain-sync did not reach block ${blockPoint.headerHash} within ` +
        `${blockScanLimit.toString()} blocks of its Kupo checkpoint ancestor`,
    );
  } finally {
    session.close();
  }
};

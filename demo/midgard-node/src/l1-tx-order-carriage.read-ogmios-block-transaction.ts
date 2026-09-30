import {
  type MidgardFieldCarriage,
  type MidgardFieldPreimageCertificate,
  type ResolvedCarriageReferenceInput,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  BASE16_BYTES,
  defaultWebSocketFactory,
} from "./l1-tx-order-carriage.fetch-kupo-spend.js";
import {
  DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
  DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  exactHeaderHash,
  exactSlot,
  type L1ChainPoint,
  normalizeOgmiosWebSocketUrl,
  type ObservedL1Transaction,
  type ObservedL1TransactionAtPoint,
  type WebSocketFactory,
} from "./l1-tx-order-carriage.l1-chain-point.js";
import {
  openOgmiosSession,
  parseObservedTransaction,
} from "./l1-tx-order-carriage.open-ogmios-session.js";

/**
 * Rolls chain-sync from `intersection` forward to `blockPoint` and returns the
 * named transaction out of that block.
 *
 * **Rollbacks fail the read; they never widen it.** The first `nextBlock` after
 * an intersection is always a roll *backward* to the intersection itself, and
 * that one is expected. A later backward roll means the chain moved under the
 * scan, so the block Kupo named may no longer be on it — the read refuses, the
 * order is not ingested this pass, and the next reconciliation tick sees whatever
 * the chain settled on. That is the same exposure ingestion already had: an
 * order's *existence* is decided by the `utxosAt` view the walk starts from, and
 * this read only supplies bytes that must hash to that order's own committed
 * field hashes.
 */
export const readOgmiosBlockTransaction = async ({
  ogmiosUrl,
  intersection,
  blockPoint,
  txHash,
  webSocketFactory = defaultWebSocketFactory,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  blockScanLimit = DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
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

/**
 * The redeemer the tx-order policy ran, out of an observed transaction.
 *
 * **The index is positional over the mint's policy ids in ascending order**, which
 * is how the ledger builds a minting redeemer's pointer: the mint field is a map
 * keyed by policy id, and the purpose index is that key's position in it. So the
 * selection is "the redeemer whose pointer names *this* policy", never "the
 * transaction's mint redeemer" — an order transaction that mints a second policy
 * below the tx-order one puts the tx-order redeemer at index 1, and either
 * shortcut would read a pointer that belongs to something else.
 *
 * Both refusals are fail-closed and neither is recoverable by guessing: a
 * transaction that mints nothing under the policy is not the order's creating
 * transaction, and a mint under the policy with no redeemer for it could not have
 * run the tx-order validator at all.
 */
export const txOrderMintRedeemer = (
  transaction: ObservedL1Transaction,
  txOrderPolicyId: string,
): string => {
  const policyIndex = transaction.mintPolicyIds.indexOf(txOrderPolicyId);
  if (policyIndex === -1) {
    throw new Error(
      `transaction ${transaction.txHash} mints nothing under the tx-order ` +
        `policy ${txOrderPolicyId}`,
    );
  }
  const mintRedeemer = transaction.redeemers.find(
    (redeemer) => redeemer.purpose === "mint" && redeemer.index === policyIndex,
  );
  if (mintRedeemer === undefined) {
    throw new Error(
      `transaction ${transaction.txHash} carries no mint redeemer for the ` +
        "tx-order policy",
    );
  }
  return mintRedeemer.redeemer;
};

/**
 * The order's §8 carriage vector, out of its own mint redeemer.
 *
 * Mirrors `midgard-watcher`'s `decodeMintRedeemer` for the `forced_order` case:
 * the tx-order policy does not take `user_events.MintRedeemer` bare — #594 gave
 * it its own `MintRedeemer`, wrapping that enum beside the §8 vector — so the
 * decode is against `TxOrderMintRedeemer` and a bare-enum redeemer at this
 * policy is a failure rather than an empty carriage. The schema itself is the
 * SDK's, so the two packages cannot drift apart on the wire format.
 */
export const txOrderMintCarriageVector = (
  redeemerCbor: string,
): readonly MidgardFieldCarriage[] => {
  const decoded = Data.from(
    redeemerCbor,
    SDK.TxOrderMintRedeemer,
  ) as SDK.TxOrderMintRedeemer;
  return decoded.material_carriage.map((entry): MidgardFieldCarriage => {
    if ("Inline" in entry) {
      return {
        carriage: "Inline",
        preimage: Buffer.from(entry.Inline.preimage, "hex"),
      };
    }
    if ("RawUtxo" in entry) {
      return {
        carriage: "RawUtxo",
        refInputIndex: exactRefInputIndex(entry.RawUtxo.ref_input_index),
      };
    }
    return {
      carriage: "Certified",
      certRefInputIndex: exactRefInputIndex(
        entry.Certified.cert_ref_input_index,
      ),
      chunkRefInputIndices:
        entry.Certified.chunk_ref_input_indices.map(exactRefInputIndex),
    };
  });
};

const exactRefInputIndex = (value: bigint): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(
      `reference-input index ${value.toString()} is out of range`,
    );
  }
  return Number(value);
};

/**
 * Turns one resolved carriage UTxO into what the §8.8 door reads off it.
 *
 * Both shapes are recognised by decoding, never by address or by position: §8.5
 * raw carriage is a nothing-but-bytes inline datum, and an §8.6 manifest is the
 * certificate record. A reference input that is neither — the hub oracle, a
 * reference script, anything else the order transaction reads — resolves to an
 * empty entry, which keeps every index in the vector pointing at the same input
 * the mint saw while giving the door nothing to open there.
 */
export const resolveCarriageReferenceInput = (
  datumCbor: string | null,
): ResolvedCarriageReferenceInput => {
  if (datumCbor === null) {
    return {};
  }
  try {
    return { inlineDatumBytes: SDK.fieldPreimagePublicationBytes(datumCbor) };
  } catch {
    // Not raw carriage. The one other thing a carriage index can name is a
    // manifest, so that is what is tried next.
  }
  try {
    const certificate = Data.from(
      datumCbor,
      SDK.FieldPreimageCertificate,
    ) as SDK.FieldPreimageCertificate;
    return {
      certificate: {
        owner: Buffer.from(certificate.owner, "hex"),
        txId: Buffer.from(certificate.tx_id, "hex"),
        fieldIndex: Number(certificate.field_index),
        fieldHash: Buffer.from(certificate.field_hash, "hex"),
        totalLength: Number(certificate.total_length),
        chunkDigests: certificate.chunk_digests.map((digest) =>
          Buffer.from(digest, "hex"),
        ),
      } satisfies MidgardFieldPreimageCertificate,
    };
  } catch {
    return {};
  }
};

import { type ResolvedCarriageReferenceInput } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { compareOutRefs, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  defaultWebSocketFactory,
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  fetchKupoMatch,
} from "./l1-tx-order-carriage.fetch-kupo-spend.js";
import {
  DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
  DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  type FetchLike,
  type TxOrderMaterialCarriage,
  type WebSocketFactory,
} from "./l1-tx-order-carriage.l1-chain-point.js";
import {
  readOgmiosBlockTransaction,
  resolveCarriageReferenceInput,
  txOrderMintCarriageVector,
  txOrderMintRedeemer,
} from "./l1-tx-order-carriage.read-ogmios-block-transaction.js";

/**
 * Resolves an observed transaction's reference inputs into the positional list
 * the redeemer's indices point into.
 *
 * **The order is the ledger's, re-derived rather than trusted.** Reference inputs
 * are a set, and what the validator was handed is that set in canonical
 * `(txHash, outputIndex)` order — which is the same discipline every positional
 * redeemer in the SDK keeps (`resolveChunkReferenceIndicesV1` sorts for exactly
 * this reason). Sorting here rather than taking the observation's order means a
 * provider that enumerates a set differently cannot shift an index.
 */
export const resolveCarriageReferenceInputs = async ({
  kupoUrl,
  referenceInputs,
  fetchImpl = fetch,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
}: {
  readonly kupoUrl: string;
  readonly referenceInputs: readonly OutRefLike[];
  readonly fetchImpl?: FetchLike;
  readonly timeoutMs?: number;
}): Promise<readonly ResolvedCarriageReferenceInput[]> => {
  const ordered = [...referenceInputs].sort(compareOutRefs);
  const resolved: ResolvedCarriageReferenceInput[] = [];
  for (const outRef of ordered) {
    const match = await fetchKupoMatch({
      kupoUrl,
      outRef,
      fetchImpl,
      timeoutMs,
    });
    // §8.5 requires an inline datum, and `datum_type` is the only thing that
    // distinguishes one: an output that merely *referenced* a datum is not
    // carriage even when Kupo happens to hold the preimage, so it resolves to
    // nothing rather than to bytes the ledger never put in the output. An output
    // with no datum at all omits the field entirely, which lands here too.
    if (match.datum_type !== "inline" || typeof match.datum_hash !== "string") {
      resolved.push(resolveCarriageReferenceInput(null));
      continue;
    }
    // The output has told us it carries an inline datum, so anything other than
    // bytes here is a Kupo that could not produce them — `null` is its documented
    // answer for a datum it does not hold. That is a *failure*, never an empty
    // resolution: an emptied carriage index reads as "this input carries no
    // carriage", which silently turns a readable order into an unreadable one
    // instead of into a read the next reconciliation tick retries.
    if (typeof match.datum !== "string") {
      throw new Error(
        `Kupo resolved no datum for hash ${match.datum_hash} on ` +
          `${outRef.txHash}#${outRef.outputIndex.toString()}`,
      );
    }
    resolved.push(resolveCarriageReferenceInput(match.datum));
  }
  return Object.freeze(resolved);
};

/**
 * The whole read: from the order's own UTxO to the §8 carriage its mint
 * authenticated.
 */
export const observeTxOrderMaterialCarriage = async ({
  ogmiosUrl,
  kupoUrl,
  txOrderOutRef,
  txOrderPolicyId,
  fetchImpl = fetch,
  webSocketFactory = defaultWebSocketFactory,
  timeoutMs = DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  blockScanLimit = DEFAULT_TX_ORDER_CARRIAGE_BLOCK_SCAN_LIMIT,
}: {
  readonly ogmiosUrl: string;
  readonly kupoUrl: string;
  readonly txOrderOutRef: OutRefLike;
  readonly txOrderPolicyId: string;
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Promise<TxOrderMaterialCarriage> => {
  const createdAt = await fetchKupoCreationPoint({
    kupoUrl,
    outRef: txOrderOutRef,
    fetchImpl,
    timeoutMs,
  });
  const ancestor = await fetchKupoAncestorPoint({
    kupoUrl,
    slot: createdAt.slot,
    fetchImpl,
    timeoutMs,
  });
  const transaction = await readOgmiosBlockTransaction({
    ogmiosUrl,
    intersection: ancestor,
    blockPoint: createdAt,
    txHash: txOrderOutRef.txHash,
    webSocketFactory,
    timeoutMs,
    blockScanLimit,
  });
  const carriage = txOrderMintCarriageVector(
    txOrderMintRedeemer(transaction, txOrderPolicyId),
  );
  // Tier 1 names no reference input, so an all-inline order needs no Kupo
  // resolution at all and does not pay for one.
  const referenceInputs = carriage.every((entry) => entry.carriage === "Inline")
    ? []
    : await resolveCarriageReferenceInputs({
        kupoUrl,
        referenceInputs: transaction.referenceInputs,
        fetchImpl,
        timeoutMs,
      });
  return { carriage, referenceInputs };
};

/**
 * The Effect wrapper the ingestion walk calls. Every failure — a missing Kupo
 * match, a pruned checkpoint, a rollback mid-scan, an undecodable redeemer —
 * arrives as the one `LucidError` the walk already fails with, because from the
 * walk's side they are the same event: this order's material could not be read
 * this pass.
 */
export const observeTxOrderMaterialCarriageProgram = (options: {
  readonly ogmiosUrl: string;
  readonly kupoUrl: string;
  readonly txOrderOutRef: OutRefLike;
  readonly txOrderPolicyId: string;
  readonly fetchImpl?: FetchLike;
  readonly webSocketFactory?: WebSocketFactory;
  readonly timeoutMs?: number;
  readonly blockScanLimit?: number;
}): Effect.Effect<TxOrderMaterialCarriage, SDK.LucidError> =>
  Effect.tryPromise({
    try: () => observeTxOrderMaterialCarriage(options),
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
        cause,
      }),
  });

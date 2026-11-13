/**
 * A forced order's `forced_transaction_utxos` row, from the order and the
 * nine field preimages its carriage supplied, and the admission screen
 * every row the node writes goes through (`screenForcedAdmissions`).
 */
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";

import { ForcedTransactionsDB } from "../database/index.js";
import type { DatabaseError } from "../database/utils/common.js";
import {
  FORCED_ADMISSION_DIVERGENCE_EVENT,
  forcedAdmissionDivergenceCounter,
  type ForcedAdmissionDivergences,
  forcedAdmissionRefusal,
} from "./admission-divergence.js";
import { reconstructTxOrderMaterial } from "./carriage.js";
import { outRefLabel } from "./derive.js";

const lucidError = (message: string, cause: unknown) =>
  new SDK.LucidError({ message, cause });

/**
 * The row for `order`, whose canonical transaction is rebuilt from
 * `fieldPreimages`. The two identity columns are recomputed from the
 * rebuilt bytes and checked against the datum's, so a reconstruction that
 * lost identity is never persisted under the datum's name. Identity is read
 * from the §5.1 envelope the tx-order mint authenticated, so an order whose
 * items do not decode still gets its row; classification gives its verdict.
 */
export const forcedOrderEntry = (input: {
  readonly order: SDK.TxOrderUTxOV1;
  readonly fieldPreimages: readonly Uint8Array[];
  readonly consensusProfile: MidgardConsensusProfile;
}): Effect.Effect<ForcedTransactionsDB.Entry, SDK.LucidError | DatabaseError> =>
  Effect.gen(function* () {
    const { order, consensusProfile } = input;
    const payload = order.datum.event.tx;
    const datum = order.utxo.datum;
    if (datum == null)
      return yield* Effect.fail(
        lucidError("Missing inline datum for a tx order", order.utxo.txHash),
      );
    const nativeTxCbor = yield* Effect.try({
      try: () =>
        reconstructTxOrderMaterial({
          payload,
          fieldPreimages: input.fieldPreimages,
        }),
      catch: (cause) =>
        lucidError(
          "Failed to reconstruct the authenticated V1 tx-order material from its field preimages",
          cause,
        ),
    });
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile,
    });
    if (payload.tx_id !== encoded.txId.toString("hex"))
      return yield* Effect.fail(
        lucidError(
          "V1 tx-order transaction id does not match its canonical transaction",
          `datum=${payload.tx_id},derived=${encoded.txId.toString("hex")}`,
        ),
      );
    if (
      payload.transaction_commitment !==
      encoded.transactionCommitment.toString("hex")
    )
      return yield* Effect.fail(
        lucidError(
          "V1 tx-order transaction commitment does not match its canonical transaction",
          `datum=${payload.transaction_commitment},derived=${encoded.transactionCommitment.toString("hex")}`,
        ),
      );
    const C = ForcedTransactionsDB.Columns;
    return {
      [C.TX_ORDER_ID]: Buffer.from(order.idCbor),
      [C.TX_ORDER_L1_TX_HASH]: Buffer.from(order.utxo.txHash, "hex"),
      [C.TX_ORDER_L1_OUTPUT_INDEX]: order.utxo.outputIndex,
      [C.ASSET_NAME]: Buffer.from(order.assetName, "hex"),
      [C.RAW_DATUM]: Buffer.from(datum, "hex"),
      [C.TX_ID]: encoded.txId,
      [C.TX_COMPACT]: encoded.txCompact,
      [C.FORCED_INCLUSION_VALUE]: encoded.value,
      [C.CONSENSUS_PROFILE_ID]: consensusProfile.profileId,
      [C.NATIVE_TX_CBOR]: nativeTxCbor,
      [C.TRANSACTION_COMMITMENT]: encoded.transactionCommitment,
      [C.INCLUSION_TIME]: order.inclusionTime,
      [C.PROJECTED_HEADER_HASH]: null,
      [C.STATUS]: ForcedTransactionsDB.Status.Awaiting,
    };
  });

/**
 * Screens each written row with the off-chain twin of the tx-order mint's
 * admission check (`admission-divergence.ts`), whichever write wrote it:
 * the forced-order hook's, or the processing of a landed block that names
 * the order. A refusal is an alarm only: the row is written and is
 * classified as any other, and the node stays ready. An order `divergences`
 * already names raises nothing again; without it every refusal raises.
 */
export const screenForcedAdmissions = (
  entries: readonly ForcedTransactionsDB.Entry[],
  divergences?: ForcedAdmissionDivergences,
) =>
  Effect.forEach(
    entries,
    (entry) => {
      const C = ForcedTransactionsDB.Columns;
      const refusal = forcedAdmissionRefusal(entry[C.NATIVE_TX_CBOR]);
      if (refusal === null) return Effect.void;
      const orderId = entry[C.TX_ORDER_ID].toString("hex");
      if (
        divergences !== undefined &&
        !divergences.record(orderId, refusal.refusalCode)
      )
        return Effect.void;
      return Effect.logError(FORCED_ADMISSION_DIVERGENCE_EVENT).pipe(
        Effect.zipRight(
          Metric.increment(
            Metric.tagged(
              forcedAdmissionDivergenceCounter,
              "refusal_code",
              refusal.refusalCode,
            ),
          ),
        ),
        Effect.annotateLogs({
          event: FORCED_ADMISSION_DIVERGENCE_EVENT,
          orderId,
          orderOutRef: outRefLabel({
            txHash: entry[C.TX_ORDER_L1_TX_HASH],
            index: entry[C.TX_ORDER_L1_OUTPUT_INDEX],
          }),
          txId: entry[C.TX_ID].toString("hex"),
          refusalCode: refusal.refusalCode,
          refusalDetail: refusal.detail,
        }),
      );
    },
    { discard: true },
  );

import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  DepositEvent,
  ForcedInclusionTxV1,
  OutputReference,
  TxOrderEvent,
  WithdrawalEvent,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  watcherForcedOperatorVerdict,
  type WatcherIndexedUserEvent,
} from "../indexers/user-event-indexer.js";

/** Raw canonical entries copied from a freshly authenticated DA reconstruction.
 * This structural value does not grant authority by itself.
 */
export type WatcherCommittedEventClaim = Readonly<{
  phase: "Deposit" | "Withdrawal" | "ForcedTransaction";
  eventIdCborHex: string;
  valueCborHex: string;
  canonicalNativeTxCborHex: string | null;
}>;

/** Binds a separately authenticated originating event to the operator's DA
 * claim. Withdrawal validity and forced verdict are claims to be adjudicated,
 * never facts inferred from subsequent L1 settlement.
 */
export const bindWatcherOriginEventClaim = (
  event: WatcherIndexedUserEvent,
  claim: WatcherCommittedEventClaim,
) => {
  if (claim.eventIdCborHex !== event.eventId) {
    throw new Error("committed event key differs from its originating event");
  }
  if (claim.phase === "Deposit" && event.kind === "deposit") {
    const origin = Data.from(event.eventCborHex, DepositEvent);
    if (
      Data.to(origin.id, OutputReference) !== claim.eventIdCborHex ||
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        plutusConstrFieldCbor(event.eventCborHex, [1]),
      ) !== claim.valueCborHex
    ) {
      throw new Error("committed deposit differs from its originating event");
    }
    return { phase: "Deposit" as const, origin };
  }
  if (claim.phase === "Withdrawal" && event.kind === "withdrawal") {
    const origin = Data.from(event.eventCborHex, WithdrawalEvent);
    const committed = Data.from(claim.valueCborHex, WithdrawalInfo);
    if (
      Data.to(origin.id, OutputReference) !== claim.eventIdCborHex ||
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        replacePlutusConstrFieldCbor(
          plutusConstrFieldCbor(event.eventCborHex, [1]),
          [2],
          plutusConstrFieldCbor(claim.valueCborHex, [2]),
        ),
      ) !== claim.valueCborHex
    ) {
      throw new Error(
        "committed withdrawal differs from its originating event",
      );
    }
    return { phase: "Withdrawal" as const, origin, committed };
  }
  if (
    claim.phase === "ForcedTransaction" &&
    event.kind === "forced_order" &&
    claim.canonicalNativeTxCborHex !== null
  ) {
    const origin = Data.from(event.eventCborHex, TxOrderEvent);
    const committed = Data.from(claim.valueCborHex, ForcedInclusionTxV1);
    const bytes = Buffer.from(claim.canonicalNativeTxCborHex, "hex");
    const native = decodeMidgardForcedTxFullFromCanonicalCbor(bytes);
    const submitted = deriveMidgardForcedTxProofSourceFromCanonicalCbor(bytes);
    const operatorValidity = watcherForcedOperatorVerdict(committed.verdict);
    if (
      Data.to(origin.id, OutputReference) !== claim.eventIdCborHex ||
      operatorValidity === null ||
      origin.tx.tx_id !==
        computeMidgardNativeTxId(native.compact).toString("hex") ||
      committed.tx_id !== origin.tx.tx_id ||
      origin.tx.transaction_commitment !==
        computeMidgardForcedTxProofCommitment(submitted).toString("hex") ||
      origin.tx.submitted_source.compact_cbor !==
        submitted.compactCbor.toString("hex") ||
      origin.tx.submitted_source.witness_set_compact_cbor !==
        submitted.witnessSetCompactCbor.toString("hex") ||
      origin.tx.submitted_source.field_preimage_lengths_cbor !==
        submitted.fieldPreimageLengthsCbor.toString("hex") ||
      committed.submitted_source.compact_cbor !==
        submitted.compactCbor.toString("hex") ||
      committed.submitted_source.witness_set_compact_cbor !==
        submitted.witnessSetCompactCbor.toString("hex") ||
      committed.submitted_source.field_preimage_lengths_cbor !==
        submitted.fieldPreimageLengthsCbor.toString("hex")
    ) {
      throw new Error(
        "committed forced source differs from its originating order",
      );
    }
    return {
      phase: "ForcedTransaction" as const,
      origin,
      committed,
      operatorValidity,
    };
  }
  throw new Error("committed event phase differs from its originating event");
};

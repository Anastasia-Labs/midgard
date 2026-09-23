import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { type Assets, Data } from "@lucid-evolution/lucid";

import { Value } from "../common.js";
import { CardanoDatum } from "../ledger-state.js";
import { PayoutDatum } from "../payout.js";
import { assetsToValue, valueToAssets } from "../reserve-payout/assets.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
  type EventHistoryPayload,
} from "./history.js";

const bytes = (cbor: string) =>
  BigInt(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor).length / 2);

/** Conservative envelope shared with event_history/funding.ak. It includes a
 * worst-case Shelley address and datum framing; reference scripts are forbidden. */
export const eventHistoryMinimumOutputLovelace = (
  assets: Assets,
  datum: CardanoDatum,
  addedNft = false,
): bigint =>
  BigInt(MIDGARD_CONSENSUS_LIMITS.coinsPerUtxoByte) *
  (160n +
    96n +
    bytes(Data.to(assetsToValue(assets), Value)) +
    bytes(Data.to(datum, CardanoDatum)) +
    (addedNft ? 80n : 0n));

export const eventHistoryMinimumNodeLovelace = (
  assets: Assets,
  node: EventHistoryNode,
): bigint =>
  eventHistoryMinimumOutputLovelace(assets, {
    InlineDatum: {
      data: Data.from(
        Data.to(
          {
            ...node,
            next: "ff".repeat(32),
            protected_until: EVENT_HISTORY_MAX_PROTECTION_TIME,
          },
          EventHistoryNode,
        ),
      ),
    },
  });

export const eventHistoryWithdrawalFunding = (
  payload: Extract<EventHistoryPayload, { WithdrawalPayload: unknown }>,
  lockedLovelace: bigint,
) => {
  const { event, refund_datum } = payload.WithdrawalPayload;
  const body = event.info.body;
  const payout: CardanoDatum = {
    InlineDatum: {
      data: Data.from(
        Data.to(
          {
            l2_value: body.l2_value,
            l1_address: body.l1_address,
            l1_datum: body.l1_datum,
          },
          PayoutDatum,
        ),
      ),
    },
  };
  const target = valueToAssets(body.l2_value);
  const locked = { lovelace: lockedLovelace };
  return {
    refundMinimum: eventHistoryMinimumOutputLovelace(locked, refund_datum),
    payoutMinimum: eventHistoryMinimumOutputLovelace(locked, payout, true),
    targetMinimum: eventHistoryMinimumOutputLovelace(target, payout, true),
    targetLovelace: target.lovelace ?? 0n,
  };
};

/** Do not turn extra funding into deposited Value. Callers specify structural
 * ADA separately and must resolve an underfunded request before admission. */
export const assertEventHistoryAdmissionFunding = (
  node: EventHistoryNode,
  assetsWithNft: Assets,
  policyId: string,
  payload: EventHistoryPayload,
  structuralLovelace: bigint,
) => {
  if (node.position === "Root")
    throw new Error("Order funding requires an event key");
  if (node.protected_until > EVENT_HISTORY_MAX_PROTECTION_TIME)
    throw new Error(
      "History protection timestamp exceeds its funded encoding width",
    );
  const { [policyId + node.position.Key[0]]: nft, ...locked } = assetsWithNft;
  if (
    nft !== 1n ||
    structuralLovelace < 0n ||
    structuralLovelace > locked.lovelace
  ) {
    throw new Error("Invalid event history structural funding");
  }
  const nodeMinimum = eventHistoryMinimumNodeLovelace(assetsWithNft, node);
  if (locked.lovelace < nodeMinimum)
    throw new Error(
      `History node requires at least ${nodeMinimum} lovelace for future pointer changes`,
    );
  if ("DepositPayload" in payload) {
    const original = {
      ...locked,
      lovelace: locked.lovelace - structuralLovelace,
    };
    if (
      original.lovelace < eventHistoryMinimumOutputLovelace(original, "NoDatum")
    ) {
      throw new Error(
        "Original deposit Value cannot fund its future reserve output",
      );
    }
  } else {
    const minimum = eventHistoryWithdrawalFunding(payload, locked.lovelace);
    if (
      structuralLovelace !== 0n ||
      locked.lovelace > minimum.targetLovelace ||
      locked.lovelace < minimum.refundMinimum ||
      locked.lovelace < minimum.payoutMinimum ||
      minimum.targetLovelace < minimum.targetMinimum
    ) {
      throw new Error(
        "Withdrawal funding cannot complete both payout and refund retirement paths",
      );
    }
  }
};

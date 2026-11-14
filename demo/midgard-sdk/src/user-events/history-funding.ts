import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { type Assets, Constr, Data } from "@lucid-evolution/lucid";

import { Value } from "../common.js";
import { CardanoDatum } from "../ledger-state.js";
import { PayoutDatum } from "../payout.js";
import { assetsToValue, valueToAssets } from "../reserve-payout/assets.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
  EventHistoryPayload,
} from "./history.js";

const bytes = (cbor: string) =>
  BigInt(aikenSerialisedPlutusDataCborPreservingMapOrder(cbor).length / 2);

/** Conservative envelope shared with event_history/funding.ak. It includes a
 * worst-case Shelley address and datum framing; reference scripts are forbidden. */
export const eventHistoryMinimumOutputLovelace = (
  assets: Assets,
  datum: CardanoDatum | string,
  addedNft = false,
): bigint =>
  BigInt(MIDGARD_CONSENSUS_LIMITS.coinsPerUtxoByte) *
  (160n +
    96n +
    bytes(Data.to(assetsToValue(assets), Value)) +
    bytes(
      typeof datum === "string" && datum !== "NoDatum"
        ? datum
        : Data.to(datum, CardanoDatum),
    ) +
    (addedNft ? 80n : 0n));

const inlineDatumCbor = (dataCbor: string): string =>
  replacePlutusConstrFieldCbor(
    Data.to({ InlineDatum: { data: 0n } }, CardanoDatum),
    [0],
    dataCbor,
  );

export const eventHistoryMinimumNodeLovelace = (
  assets: Assets,
  node: EventHistoryNode | string,
): bigint => {
  const nodeCbor =
    typeof node === "string" ? node : Data.to(node, EventHistoryNode);
  const maximumNode = replacePlutusConstrFieldCbor(
    replacePlutusConstrFieldCbor(
      nodeCbor,
      [1],
      Data.to(new Constr(0, ["ff".repeat(32)])),
    ),
    [2],
    Data.to(EVENT_HISTORY_MAX_PROTECTION_TIME),
  );
  return eventHistoryMinimumOutputLovelace(
    assets,
    inlineDatumCbor(maximumNode),
  );
};

export const eventHistoryWithdrawalFunding = (
  payload:
    | Extract<EventHistoryPayload, { WithdrawalPayload: unknown }>
    | string,
  lockedLovelace: bigint,
) => {
  const payloadCbor =
    typeof payload === "string"
      ? payload
      : Data.to(payload, EventHistoryPayload);
  const decoded = Data.from(payloadCbor, EventHistoryPayload);
  if (!("WithdrawalPayload" in decoded))
    throw new Error("Withdrawal funding requires a withdrawal payload");
  const body = decoded.WithdrawalPayload.event.info.body;
  const bodyCbor = plutusConstrFieldCbor(payloadCbor, [0, 1, 0]);
  let payoutCbor = Data.to(
    {
      l2_value: body.l2_value,
      l1_address: body.l1_address,
      l1_datum: body.l1_datum,
    },
    PayoutDatum,
  );
  for (let field = 0; field < 3; field++)
    payoutCbor = replacePlutusConstrFieldCbor(
      payoutCbor,
      [field],
      plutusConstrFieldCbor(bodyCbor, [field + 2]),
    );
  const payout = inlineDatumCbor(payoutCbor);
  const refund = plutusConstrFieldCbor(payloadCbor, [2]);
  const target = valueToAssets(body.l2_value);
  const locked = { lovelace: lockedLovelace };
  return {
    refundMinimum: eventHistoryMinimumOutputLovelace(locked, refund),
    payoutMinimum: eventHistoryMinimumOutputLovelace(locked, payout, true),
    targetMinimum: eventHistoryMinimumOutputLovelace(target, payout, true),
    targetLovelace: target.lovelace ?? 0n,
  };
};

/** Do not turn extra funding into deposited Value. Callers specify structural
 * ADA separately and must resolve an underfunded request before admission. */
export const assertEventHistoryAdmissionFunding = (
  node: EventHistoryNode | string,
  assetsWithNft: Assets,
  policyId: string,
  payload: EventHistoryPayload | string,
  structuralLovelace: bigint,
) => {
  const nodeView =
    typeof node === "string" ? Data.from(node, EventHistoryNode) : node;
  const payloadView =
    typeof payload === "string"
      ? Data.from(payload, EventHistoryPayload)
      : payload;
  if (nodeView.position === "Root")
    throw new Error("Order funding requires an event key");
  if (nodeView.protected_until > EVENT_HISTORY_MAX_PROTECTION_TIME)
    throw new Error(
      "History protection timestamp exceeds its funded encoding width",
    );
  const { [policyId + nodeView.position.Key[0]]: nft, ...locked } =
    assetsWithNft;
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
  if ("DepositPayload" in payloadView) {
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
    const minimum = eventHistoryWithdrawalFunding(
      typeof payload === "string"
        ? payload
        : Data.to(payload, EventHistoryPayload),
      locked.lovelace,
    );
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

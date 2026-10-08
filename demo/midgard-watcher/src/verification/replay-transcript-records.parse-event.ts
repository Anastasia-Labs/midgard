import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type WatcherCommittedEventClaim } from "./event-claims.js";
import {
  ADMISSION_KEYS,
  EVENT_KEYS,
} from "./replay-transcript-records.parse-w25.js";
import {
  equal,
  HEX,
  HEX28,
  HEX32,
  NATURAL,
  nullableText,
  record,
  requireCondition,
  stringFields,
  text,
} from "./replay-transcript-records.w25-keys.js";
import type { WatcherUserEvent } from "./user-event.js";

/** A transcript's descriptive user event, as the follower read it (W3). */
export const parseEvent = (value: unknown): WatcherUserEvent => {
  const r = record(value, EVENT_KEYS, "event");
  requireCondition(
    r.kind === "deposit" ||
      r.kind === "withdrawal" ||
      r.kind === "forced_order",
    "event.kind",
  );
  stringFields(r, ["eventId", "assetNameHex", "eventCborHex"], "event", HEX);
  text(r.policyId, "event.policyId", HEX28);
  text(r.inclusionTime, "event.inclusionTime", NATURAL);
  nullableText(r.originalAssetsCborHex, "event.originalAssetsCborHex", HEX);
  equal(
    r.originalAssetsCborHex !== null,
    r.kind === "deposit",
    "event original assets",
  );
  const admission = record(r.admission, ADMISSION_KEYS, "event.admission");
  stringFields(
    admission,
    ["blockHash", "transactionHash"],
    "event.admission",
    HEX32,
  );
  stringFields(
    admission,
    ["slot", "blockNo", "transactionIndex", "outputIndex"],
    "event.admission",
    NATURAL,
  );
  const id = Data.from(text(r.eventId, "event.eventId"), SDK.OutputReference);
  equal(Data.to(id, SDK.OutputReference), r.eventId, "event id CBOR");
  equal(
    `${id.transactionId}#${id.outputIndex.toString()}`,
    r.nonceOutRef,
    "event nonce",
  );
  return r as unknown as WatcherUserEvent;
};

export const eventKeyFor = (
  event: Pick<WatcherUserEvent, "kind" | "eventId">,
): SDK.EventKey => {
  const id = Data.from(event.eventId, SDK.OutputReference);
  if (event.kind === "deposit") return { DepositEventKey: { deposit_id: id } };
  if (event.kind === "withdrawal")
    return { WithdrawalEventKey: { withdrawal_id: id } };
  return { ForcedTransactionEventKey: { tx_order_id: id } };
};

export const phaseFor = (
  event: Pick<WatcherUserEvent, "kind">,
): WatcherCommittedEventClaim["phase"] =>
  event.kind === "deposit"
    ? "Deposit"
    : event.kind === "withdrawal"
      ? "Withdrawal"
      : "ForcedTransaction";

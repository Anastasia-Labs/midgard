import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  WatcherIndexedUserEvent,
  WatcherTerminalUserEvent,
} from "../indexers/user-event-indexer.js";
import { type WatcherCommittedEventClaim } from "./event-claims.js";
import { EVENT_KEYS } from "./replay-transcript-records.parse-w25.js";
import {
  equal,
  HEX,
  HEX28,
  HEX32,
  NATURAL,
  OUT_REF,
  record,
  requireCondition,
  sha256,
  stringFields,
  text,
} from "./replay-transcript-records.w25-keys.js";

export const TERMINAL_KEYS = [
  "terminalStatus",
  "terminalTransactionHash",
  "terminalPointDigest",
  "terminalBlockHash",
  "terminalSlot",
  "terminalBlockNo",
  "terminalFinalityStatus",
] as const;

export const parseEvent = (
  value: unknown,
): WatcherIndexedUserEvent | WatcherTerminalUserEvent => {
  requireCondition(typeof value === "object" && value !== null, "event");
  const terminal = Object.prototype.hasOwnProperty.call(
    value,
    "terminalStatus",
  );
  const classified = Object.prototype.hasOwnProperty.call(
    value,
    "terminalClassification",
  );
  const history =
    "kind" in value &&
    (value.kind === "deposit" || value.kind === "withdrawal");
  const keys = [
    ...EVENT_KEYS,
    ...(history ? ["historyPayloadCborHex"] : ["witnessScriptHash"]),
    ...(terminal ? TERMINAL_KEYS : []),
    ...(classified ? ["terminalClassification"] : []),
  ];
  const r = record(value, keys, "event");
  requireCondition(
    r.kind === "deposit" ||
      r.kind === "withdrawal" ||
      r.kind === "forced_order",
    "event.kind",
  );
  stringFields(
    r,
    [
      "eventId",
      "addressHex",
      "assetNameHex",
      "eventCborHex",
      "datumCborHex",
      "outputCborHex",
    ],
    "event",
    HEX,
  );
  stringFields(
    r,
    [
      "transactionHash",
      "eventContentDigest",
      "datumDigest",
      "outputDigest",
      "originPointDigest",
      "originChainPointId",
      "originBlockHash",
    ],
    "event",
    HEX32,
  );
  stringFields(
    r,
    ["policyId", "spendScriptHash", ...(history ? [] : ["witnessScriptHash"])],
    "event",
    HEX28,
  );
  if (history) {
    const payload = Data.from(
      text(r.historyPayloadCborHex, "event.historyPayloadCborHex", HEX),
      SDK.EventHistoryPayload,
    );
    const serializedEvent =
      r.kind === "deposit" && "DepositPayload" in payload
        ? aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(String(r.historyPayloadCborHex), [0]),
          )
        : r.kind === "withdrawal" && "WithdrawalPayload" in payload
          ? aikenSerialisedPlutusDataCborPreservingMapOrder(
              plutusConstrFieldCbor(String(r.historyPayloadCborHex), [0]),
            )
          : null;
    equal(serializedEvent, r.eventCborHex, "event history payload");
  }
  stringFields(
    r,
    ["outputIndex", "inclusionTime", "originSlot", "originBlockNo"],
    "event",
    NATURAL,
  );
  stringFields(r, ["outRef", "nonceOutRef"], "event", OUT_REF);
  equal(
    r.outRef,
    `${String(r.transactionHash)}#${String(r.outputIndex)}`,
    "event outRef",
  );
  equal(r.finalityStatus, "final", "event finality");
  for (const [bytes, digest] of [
    ["eventCborHex", "eventContentDigest"],
    ["datumCborHex", "datumDigest"],
    ["outputCborHex", "outputDigest"],
  ])
    equal(
      sha256(Buffer.from(text(r[bytes!], `event.${bytes}`, HEX), "hex")),
      r[digest!],
      `event.${digest}`,
    );
  const id = Data.from(text(r.eventId, "event.eventId"), SDK.OutputReference);
  equal(Data.to(id, SDK.OutputReference), r.eventId, "event id CBOR");
  equal(
    `${id.transactionId}#${id.outputIndex.toString()}`,
    r.nonceOutRef,
    "event nonce",
  );
  if (terminal) {
    text(r.terminalStatus, "event.terminalStatus");
    stringFields(
      r,
      ["terminalTransactionHash", "terminalPointDigest", "terminalBlockHash"],
      "event",
      HEX32,
    );
    stringFields(r, ["terminalSlot", "terminalBlockNo"], "event", NATURAL);
    equal(r.terminalFinalityStatus, "final", "event terminal finality");
  }
  if (classified) {
    requireCondition(
      terminal && r.kind === "forced_order",
      "event terminal classification kind",
    );
    const item = record(
      r.terminalClassification,
      [
        "schemaVersion",
        "operatorValidity",
        "terminalTransactionHash",
        "terminalPointDigest",
      ],
      "event.terminalClassification",
    );
    stringFields(
      item,
      ["schemaVersion", "operatorValidity"],
      "event.terminalClassification",
    );
    equal(
      item.terminalTransactionHash,
      r.terminalTransactionHash,
      "terminal transaction",
    );
    equal(item.terminalPointDigest, r.terminalPointDigest, "terminal point");
  }
  return r as unknown as WatcherIndexedUserEvent | WatcherTerminalUserEvent;
};

export const eventKeyFor = (event: WatcherIndexedUserEvent): SDK.EventKey => {
  const id = Data.from(event.eventId, SDK.OutputReference);
  if (event.kind === "deposit") return { DepositEventKey: { deposit_id: id } };
  if (event.kind === "withdrawal")
    return { WithdrawalEventKey: { withdrawal_id: id } };
  return { ForcedTransactionEventKey: { tx_order_id: id } };
};

export const phaseFor = (
  event: WatcherIndexedUserEvent,
): WatcherCommittedEventClaim["phase"] =>
  event.kind === "deposit"
    ? "Deposit"
    : event.kind === "withdrawal"
      ? "Withdrawal"
      : "ForcedTransaction";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type HistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import {
  detectTransitionTraceFaults,
  type TransitionTraceDetectionEvidence,
} from "./detect.js";
import {
  requireTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "./l1-events.js";
import { eventKeyFingerprint } from "./reconstruct.js";
import { deriveTransitionTraceReplayEvidence } from "./replay.js";
import type {
  OmittedDueL1EventEvidence,
  OutOfWindowSourceEventEvidence,
} from "./witnesses.js";

/** This entry point requires the opaque, freshly admitted history and raw L1
 * handles. A journal copy, supplied proof, or supplied replay verdict cannot
 * recreate that authority. */
export const replayTransitionTraceFromRetainedHistory = async ({
  evidence,
  corpus,
  l1Events,
}: {
  evidence: CanonicalBlockEvidence;
  corpus: HistoricalNativeScriptCorpus;
  l1Events: TransitionTraceL1Events;
}) => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (
    history.currentEvidence !== evidence ||
    l1Events.headerHash !== evidence.headerHash
  )
    throw new Error(
      "Transition replay authority targets another canonical block",
    );
  const l1 = requireTransitionTraceL1Events(l1Events);
  const current = evidence.reconstruction;
  const omitted: OmittedDueL1EventEvidence[] = [];
  const outside: OutOfWindowSourceEventEvidence[] = [];
  const referencesByEvent = new Map<string, (typeof l1.events)[number]>();
  for (const entry of l1.events) {
    const common = { eventRefInputIndex: 0n, eventAssetName: entry.assetName };
    let eventKey: SDK.EventKey;
    let due: boolean;
    let omittedItem: OmittedDueL1EventEvidence;
    let outsideItem: OutOfWindowSourceEventEvidence;
    if (entry.kind === "deposit") {
      const datum = Data.from(entry.utxo.datum!, SDK.DepositDatum);
      eventKey = { DepositEventKey: { deposit_id: datum.event.id } };
      due =
        current.header.startTime < datum.inclusion_time &&
        datum.inclusion_time <= current.header.endTime;
      omittedItem = { ...common, kind: "deposit", depositId: datum.event.id };
      outsideItem = omittedItem;
    } else if (entry.kind === "withdrawal") {
      const datum = Data.from(entry.utxo.datum!, SDK.WithdrawalOrderDatum);
      eventKey = { WithdrawalEventKey: { withdrawal_id: datum.event.id } };
      due =
        current.header.startTime < datum.inclusion_time &&
        datum.inclusion_time <= current.header.endTime;
      const source = current.sourceEventsByFingerprint.get(
        eventKeyFingerprint(eventKey),
      );
      const validity =
        source?.phase === "Withdrawal"
          ? source.entry.value.validity
          : datum.event.info.validity;
      omittedItem = {
        ...common,
        kind: "withdrawal",
        withdrawalId: datum.event.id,
      };
      outsideItem = { ...omittedItem, validityOverride: validity };
    } else {
      const datum = Data.from(entry.utxo.datum!, SDK.TxOrderDatum);
      const compact = decodeMidgardNativeTxCompact(
        Buffer.from(datum.event.tx.source.compact_cbor, "hex"),
      );
      if (
        computeMidgardNativeTxId(compact).toString("hex") !==
        datum.event.tx.tx_id
      )
        throw new Error(
          "Transition forced L1 source has a false compact transaction id",
        );
      const start = compact.transactionBody.validityIntervalStart,
        end = compact.transactionBody.validityIntervalEnd;
      due =
        current.header.startTime < datum.inclusion_time &&
        datum.inclusion_time <= current.header.endTime &&
        (start === -1n
          ? end === -1n || current.header.startTime <= end
          : end === -1n
            ? start <= current.header.endTime
            : start <= end &&
              start <= current.header.endTime &&
              current.header.startTime <= end);
      eventKey = { ForcedTransactionEventKey: { tx_order_id: datum.event.id } };
      const source = current.sourceEventsByFingerprint.get(
        eventKeyFingerprint(eventKey),
      );
      const verdict =
        source?.phase === "ForcedTransaction"
          ? source.entry.value.verdict
          : "ForcedTxValid";
      omittedItem = {
        ...common,
        kind: "forcedTransaction",
        txOrderId: datum.event.id,
        validityOverride: verdict,
      };
      outsideItem = omittedItem;
    }
    const fingerprint = eventKeyFingerprint(eventKey);
    if (referencesByEvent.has(fingerprint))
      throw new Error("Transition L1 authority has duplicate event identities");
    referencesByEvent.set(fingerprint, entry);
    const source = current.sourceEventsByFingerprint.get(fingerprint);
    if (due && source === undefined) omitted.push(omittedItem);
    if (!due && source !== undefined) outside.push(outsideItem);
  }
  for (const source of current.sourceEventsByFingerprint.values()) {
    if (
      source.phase !== "L2Transaction" &&
      !referencesByEvent.has(source.fingerprint)
    )
      throw new Error(
        "Transition replay lacks authenticated L1 coverage for a committed source",
      );
  }
  const timed = {
    omittedDueL1Events: omitted,
    outOfWindowSourceEvents: outside,
  };
  let completeEvidence: TransitionTraceDetectionEvidence = timed;
  if (
    !(await detectTransitionTraceFaults(current, timed)).some(
      (item) => item.buildable,
    )
  ) {
    completeEvidence = {
      ...(await deriveTransitionTraceReplayEvidence({
        current,
        predecessor: history.reconstructions.at(-2),
        deposits: l1.events
          .filter((entry) => entry.kind === "deposit")
          .map((entry) => ({
            event: entry.utxo,
            eventAssetName: entry.assetName,
            eventRefInputIndex: 0n,
          })),
        network: l1.network,
        depositPolicyId: l1.depositPolicyId,
      })),
      ...timed,
    };
  }
  const detections = await detectTransitionTraceFaults(
    current,
    completeEvidence,
  );
  if (detections.some((item) => !item.buildable))
    throw new Error(
      "Transition replay contains an unbuildable authenticated finding",
    );
  return {
    completeEvidence,
    detections,
    referencesByEvent,
    l1Snapshot: l1.snapshot,
  };
};

export const transitionTraceDetectionId = (index: number, kind: string) =>
  `transition-trace:${index}:${kind}`;

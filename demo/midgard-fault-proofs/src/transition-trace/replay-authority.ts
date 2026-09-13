import { createHash } from "node:crypto";

import {
  computeMidgardNativeTxId,
  decodeMidgardForcedTxCompact,
} from "@al-ft/midgard-core";
import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, paymentCredentialOf } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type HistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import { computeFraudProofRawL1SnapshotEvidenceDigest } from "../workflow/raw-l1-snapshot.js";
import {
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import {
  detectTransitionTraceFaults,
  type TransitionTraceDetection,
  type TransitionTraceDetectionEvidence,
} from "./detect.js";
import {
  readFreshTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "./l1-events.js";
import { eventKeyFingerprint } from "./reconstruct.js";
import { deriveTransitionTraceReplayEvidence } from "./replay.js";
import type {
  OmittedDueL1EventEvidence,
  OutOfWindowSourceEventEvidence,
} from "./witnesses.js";

const readTransitionTraceEventCoverage = ({
  evidence,
  l1Events,
}: {
  evidence: CanonicalBlockEvidence;
  l1Events: TransitionTraceL1Events;
}) => {
  const l1 = readFreshTransitionTraceL1Events(l1Events);
  if (l1Events.headerHash !== evidence.headerHash)
    throw new Error(
      "Transition event evidence targets another canonical block",
    );
  const current = evidence.reconstruction;
  const omitted: OmittedDueL1EventEvidence[] = [];
  const outside: OutOfWindowSourceEventEvidence[] = [];
  const referencesByEvent = new Map<string, (typeof l1.events)[number]>();
  const relevantEvents: (typeof l1.events)[number][] = [];
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
      const compact = decodeMidgardForcedTxCompact(
        Buffer.from(datum.event.tx.submitted_source.compact_cbor, "hex"),
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
    if (due || source !== undefined) relevantEvents.push(entry);
    if (due && source === undefined) omitted.push(omittedItem);
    if (!due && source !== undefined) outside.push(outsideItem);
  }
  // Decision 0007: a committed deposit or withdrawal with no authentic L1
  // origin is the fabricated-family fraud, so it is carried out of coverage as
  // a prerequisite the replay caller raises rather than an abort here. A
  // forced-transaction source has no fabricated family and still aborts.
  const uncovered: SDK.EventKey[] = [];
  for (const source of current.sourceEventsByFingerprint.values()) {
    if (
      source.phase === "L2Transaction" ||
      referencesByEvent.has(source.fingerprint)
    )
      continue;
    if (source.phase === "ForcedTransaction")
      throw new Error(
        "Transition replay lacks authenticated L1 coverage for a committed source",
      );
    uncovered.push(source.eventKey);
  }
  return {
    l1,
    current,
    omitted,
    outside,
    referencesByEvent,
    relevantEvents,
    uncovered,
  };
};

// Each key is an opaque handle owning immutable, already admitted raw evidence.
// The cheap projection key below binds every mutable decision input; keep only
// the latest projection so caller changes cannot grow an unbounded cache.
const eventEvidenceDigests = new WeakMap<
  TransitionTraceL1Events,
  Readonly<{ projection: string; digest: string }>
>();

/** Stable identity for every due or committed event, after full raw admission. */
export const computeTransitionTraceL1EventEvidenceDigest = ({
  evidence,
  l1Events,
}: {
  evidence: CanonicalBlockEvidence;
  l1Events: TransitionTraceL1Events;
}): string => {
  const { l1, current, relevantEvents } = readTransitionTraceEventCoverage({
    evidence,
    l1Events,
  });
  const parameters = Data.from(l1.hub.datum!, SDK.HubOracleDatum);
  const hubScope = l1.snapshot.scopes.find(
    (scope) => scope.role === "hub_oracle",
  )!;
  const hubOutRef = `${l1.hub.txHash}#${l1.hub.outputIndex.toString()}`;
  const hubUnit =
    paymentCredentialOf(hubScope.address).hash + SDK.HUB_ORACLE_ASSET_NAME;
  if (!l1.snapshot.historyUnits.includes(hubUnit))
    throw new Error("Transition event evidence omits governed hub history");
  const relevantUnits = new Set([hubUnit]);
  const relevantOutRefs = new Set([hubOutRef]);
  for (const event of relevantEvents) {
    const policy =
      event.kind === "deposit"
        ? parameters.deposit
        : event.kind === "withdrawal"
          ? parameters.withdrawal
          : parameters.tx_order;
    relevantUnits.add(policy + event.assetName);
    relevantOutRefs.add(
      `${event.utxo.txHash}#${event.utxo.outputIndex.toString()}`,
    );
  }
  const history = l1.snapshot.history.filter((entry) =>
    relevantUnits.has(entry.unit),
  );
  if (history.length !== relevantUnits.size)
    throw new Error("Transition event evidence omits relevant unit history");
  const transactionHashes = new Set(
    history.flatMap((entry) => entry.transactionHashes),
  );
  const headerCbor = SDK.encodeHeaderCbor(current.header).toString("hex");
  const relevantEventOutRefs = [...relevantOutRefs].sort();
  const projection = JSON.stringify([
    evidence.headerHash,
    headerCbor,
    evidence.payloadEnvelopeSha256,
    evidence.payloadSha256,
    relevantEventOutRefs,
    [...relevantUnits].sort(),
  ]);
  const cached = eventEvidenceDigests.get(l1Events);
  if (cached?.projection === projection) return cached.digest;
  // The original complete snapshot remains the admission authority. This is
  // only a transcript projection, never a replacement address/history claim.
  const rawEvidenceDigest = computeFraudProofRawL1SnapshotEvidenceDigest({
    ...l1.snapshot,
    scopes: l1.snapshot.scopes.map((scope) => ({
      ...scope,
      utxos: scope.utxos.filter((entry) => relevantOutRefs.has(entry.outRef)),
    })),
    historyUnits: [...relevantUnits],
    history,
    transactions: l1.snapshot.transactions.filter((entry) =>
      transactionHashes.has(entry.txHash),
    ),
  });
  const digest = createHash("sha256")
    .update(
      canonicalJson(
        {
          schemaVersion: "midgard-transition-trace-event-evidence-v1",
          headerHash: evidence.headerHash,
          headerCbor,
          payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
          payloadSha256: evidence.payloadSha256,
          hubAddress: hubScope.address,
          relevantEventOutRefs,
          rawEvidenceDigest,
        },
        "transition trace event evidence",
      ),
    )
    .digest("hex");
  eventEvidenceDigests.set(l1Events, { projection, digest });
  return digest;
};

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
  const { l1, current, omitted, outside, referencesByEvent, uncovered } =
    readTransitionTraceEventCoverage({ evidence, l1Events });
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
    // Decision 0007: the fabricated family owns a committed deposit or
    // withdrawal whose L1 origin is absent. Its finding at that leaf discharges
    // this prerequisite; a merely consumed origin yields no finding and the
    // block fails closed.
    if (uncovered.length > 0)
      throw new CanonicalReplayPrerequisiteError(
        uncovered.map(
          (eventKey) =>
            replayPrerequisiteFailure(
              evidence.headerHash,
              eventKey,
              "present_source_origin",
            ).failures[0]!,
        ),
      );
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

/** Only proofs that authenticate the exact event whose ledger effect is
 * false: a semantic transition proof, or an out-of-window source event whose
 * membership proof opens the committed leaf. */
export const provenTransitionEventKeyCbor = (
  detection: TransitionTraceDetection,
): string | undefined => {
  if (!detection.buildable) return undefined;
  const fault = detection.fault;
  if ("OutOfWindowSourceEvent" in fault) {
    const witness = fault.OutOfWindowSourceEvent.witness;
    const eventKey: SDK.EventKey =
      "OutOfWindowDeposit" in witness
        ? {
            DepositEventKey: {
              deposit_id: witness.OutOfWindowDeposit.source_membership.key,
            },
          }
        : "OutOfWindowWithdrawal" in witness
          ? {
              WithdrawalEventKey: {
                withdrawal_id:
                  witness.OutOfWindowWithdrawal.source_membership.key,
              },
            }
          : {
              ForcedTransactionEventKey: {
                tx_order_id:
                  witness.OutOfWindowForcedTransaction.source_membership.key,
              },
            };
    return Data.to(eventKey, SDK.EventKey);
  }
  if ("AcceptedTransactionTransitionMismatch" in fault)
    return Data.to(
      fault.AcceptedTransactionTransitionMismatch.witness.claim
        .transition_step_membership.value.event_key,
      SDK.EventKey,
    );
  if (!("InvalidOneStepTransition" in fault)) return undefined;
  const witness = fault.InvalidOneStepTransition.witness;
  const step =
    "L2TransactionTransition" in witness
      ? witness.L2TransactionTransition.trace_proof
      : "ValidDepositTransition" in witness
        ? witness.ValidDepositTransition.trace_proof
        : "ValidWithdrawalTransition" in witness
          ? witness.ValidWithdrawalTransition.trace_proof
          : "InvalidWithdrawalNoOpTransition" in witness
            ? witness.InvalidWithdrawalNoOpTransition.trace_proof
            : witness.InvalidForcedTransactionNoOpTransition.trace_proof;
  return Data.to(step.value.event_key, SDK.EventKey);
};

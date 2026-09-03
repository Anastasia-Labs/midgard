/**
 * W26 event/classification verifier.
 *
 * The operator's classification is deliberately not an input here.  W26
 * rebuilds the source-event view from W15/W16 parser authorities, then compares
 * that view with the digest-bound W25 trace receipt.  This is intentionally a
 * small, pure port of the transition-trace rules: timed L1 events are due in
 * `(start_time, end_time]`; forced events must additionally have a validity
 * interval intersecting the block window.
 */

import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { OutputReference, TxOrderEventV1 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  parseWatcherSettlementIndexerResultV1,
  type WatcherSettlementResultVerificationContextV1,
} from "../indexers/settlement-indexer.js";
import { parseWatcherStateQueueHeaderV1 } from "../indexers/state-queue-indexer.js";
import {
  isWatcherForcedOperatorVerdictV1,
  parseWatcherUserEventIndexerResultV1,
  WATCHER_FORCED_TX_VALID_V1,
  type WatcherForcedOperatorVerdictV1,
  type WatcherIndexedUserEventV1,
  type WatcherTerminalUserEventV1,
} from "../indexers/user-event-indexer.js";
import type { WatcherL1TransportAttestationContextV1 } from "../l1/l1-adapter.js";
import { watcherSha256CanonicalJsonV1 } from "../storage/durable-store.js";
import {
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION,
  type WatcherBlockReplayResultV1,
} from "./block-replay.js";
import {
  WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
  type WatcherPhaseAVerificationResultV1,
} from "./phase-a-verifier.js";

export const WATCHER_EVENT_CLASSIFICATION_VERIFIER_V1_SCHEMA_VERSION =
  "midgard-watcher-event-classification-verifier-v1" as const;

export const WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1 = Object.freeze([
  "malformed_input",
  "unknown_schema",
  "w25_not_accepted",
  "w25_digest_mismatch",
  "w25_prerequisite_mismatch",
  "header_binding_mismatch",
  "user_event_authority_invalid",
  "settlement_authority_invalid",
  "duplicate_source_event",
  "authority_substitution",
  "duplicate_trace_event",
  "fabricated_trace_event",
  "trace_event_phase_mismatch",
  "omitted_due_event",
  "out_of_window_event",
  "withdrawal_validity_mismatch",
  "forced_classification_mismatch",
  "forced_source_mismatch",
] as const);

export type WatcherEventClassificationReasonCodeV1 =
  (typeof WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1)[number];
export type WatcherEventClassificationActionV1 = "accept" | "reject" | "error";
export type WatcherEventClassificationPhaseV1 =
  | "Deposit"
  | "Withdrawal"
  | "ForcedTransaction";

export type WatcherEventClassificationUserAuthorityV1 = Readonly<{
  result: unknown;
  context: Readonly<{
    policy: unknown;
    previousState: unknown;
    observation: unknown;
    publicContext: unknown;
    transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  }>;
}>;

export type WatcherEventClassificationSettlementAuthorityV1 = Readonly<{
  result: unknown;
  context: WatcherSettlementResultVerificationContextV1;
}>;

export type WatcherForcedEventNativeTransactionV1 = Readonly<{
  /** W15's canonical `outRef` (`transaction-hash#output-index`). */
  eventOutRef: string;
  canonicalNativeTxCbor: Uint8Array;
}>;

export type EvaluateWatcherEventClassificationInputV1 = Readonly<{
  header: unknown;
  blockReplay: unknown;
  /** A digest-bound W24 record; W26 rechecks it against W25's phase-A digest. */
  phaseA: unknown;
  userEventAuthorities: readonly WatcherEventClassificationUserAuthorityV1[];
  settlementAuthorities: readonly WatcherEventClassificationSettlementAuthorityV1[];
  forcedNativeTransactions: readonly WatcherForcedEventNativeTransactionV1[];
}>;

export type WatcherEventClassificationFactV1 = Readonly<{
  fingerprint: string;
  phase: WatcherEventClassificationPhaseV1;
  inclusionTime: string;
  withdrawalValidity: "valid" | "invalid" | null;
  forcedInterval: Readonly<{ start: string; end: string }> | null;
  forcedValidity: "valid" | "invalid" | null;
  settlementKind: "initialize_payout" | "refund_withdrawal" | null;
}>;

export type WatcherEventClassificationTraceFactV1 = Readonly<{
  fingerprint: string;
  phase: "Deposit" | "Withdrawal" | "ForcedTransaction" | "L2Transaction";
  stepIndex: number;
  preRoot: string;
  postRoot: string;
  mutationCount: number;
}>;

export type WatcherEventClassificationFindingV1 = Readonly<{
  code: WatcherEventClassificationReasonCodeV1;
  fingerprint: string | null;
  field: string;
}>;

type ForcedValidationFact = Readonly<{
  eventKeyFingerprint: string;
  stepIndex: number;
  authenticatedOperatorValidity: WatcherForcedOperatorVerdictV1;
  canonicalOperatorValidity: WatcherForcedOperatorVerdictV1;
  phaseAStatus: "accepted" | "rejected";
  phaseARejectCode: string | null;
  phaseBStatus: "not_run" | "accepted" | "rejected";
  phaseBRejectCode: string | null;
  canonicalEffectDigest: string;
  canonicalEffectMutationCount: number;
}>;

export type WatcherEventClassificationResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_EVENT_CLASSIFICATION_VERIFIER_V1_SCHEMA_VERSION;
  action: WatcherEventClassificationActionV1;
  reasonCodes: readonly WatcherEventClassificationReasonCodeV1[];
  findings: readonly WatcherEventClassificationFindingV1[];
  classificationPrerequisite: Readonly<{
    requiredVerifier: "W26";
    inputDigest: string;
  }>;
  /** W29 must require this record; W25 acceptance has no verified meaning. */
  w29Eligibility: "w26_accepted_not_w29_verified";
  sourceAuthorityDigest: string | null;
  traceDigest: string | null;
  resultDigest: string;
}>;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NATURAL_OR_NONE = /^(?:-1|0|[1-9][0-9]*)$/u;
const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;
const PHASES = new Set([
  "Deposit",
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
]);
const EVENT_PHASES = new Set(["Deposit", "Withdrawal", "ForcedTransaction"]);
const REASON_ORDER = new Map(
  WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1.map((code, index) => [
    code,
    index,
  ]),
);

const isPlain = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  (Object.getPrototypeOf(value) === Object.prototype ||
    Object.getPrototypeOf(value) === null);

const exact = (
  value: unknown,
  keys: readonly string[],
): Record<string, unknown> | null => {
  if (!isPlain(value) || Reflect.ownKeys(value).length !== keys.length)
    return null;
  const allowed = new Set(keys);
  for (const key of Reflect.ownKeys(value)) {
    const descriptor = Object.getOwnPropertyDescriptor(value, key);
    if (
      typeof key !== "string" ||
      !allowed.has(key) ||
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    )
      return null;
  }
  return value;
};

const fingerprint = (
  phase: WatcherEventClassificationPhaseV1,
  outRef: string,
): string | null => {
  const match = OUT_REF.exec(outRef);
  if (match === null) return null;
  const prefix =
    phase === "Deposit"
      ? "Deposit"
      : phase === "Withdrawal"
        ? "Withdrawal"
        : "ForcedTransaction";
  return `${prefix}:${match[1]}:${match[2]}`;
};

const authenticatedEventFingerprint = (
  phase: WatcherEventClassificationPhaseV1,
  event: WatcherIndexedUserEventV1 | WatcherTerminalUserEventV1,
): string | null => {
  try {
    const eventId = Data.from(event.eventId, OutputReference) as {
      readonly transactionId: string;
      readonly outputIndex: bigint;
    };
    const eventIdFingerprint = `${phase}:${eventId.transactionId}:${eventId.outputIndex.toString()}`;
    return fingerprint(phase, event.nonceOutRef) === eventIdFingerprint
      ? eventIdFingerprint
      : null;
  } catch {
    return null;
  }
};

const traceFingerprintPhase = (
  value: string,
): "Deposit" | "Withdrawal" | "ForcedTransaction" | "L2Transaction" | null => {
  const separator = value.indexOf(":");
  const prefix = separator === -1 ? "" : value.slice(0, separator);
  return PHASES.has(prefix)
    ? (prefix as
        | "Deposit"
        | "Withdrawal"
        | "ForcedTransaction"
        | "L2Transaction")
    : null;
};

export const watcherTimedL1EventIsDueV1 = (
  start: bigint,
  end: bigint,
  inclusion: bigint,
): boolean => start < inclusion && inclusion <= end;

export const watcherForcedIntervalIsDueV1 = (
  start: bigint,
  end: bigint,
  intervalStart: bigint,
  intervalEnd: bigint,
): boolean => {
  if (intervalStart === -1n) return intervalEnd === -1n || start <= intervalEnd;
  if (intervalEnd === -1n) return intervalStart <= end;
  return (
    intervalStart <= intervalEnd && intervalStart <= end && start <= intervalEnd
  );
};

const orderFindings = (
  findings: readonly WatcherEventClassificationFindingV1[],
) =>
  Object.freeze(
    [...findings].sort(
      (left, right) =>
        REASON_ORDER.get(left.code)! - REASON_ORDER.get(right.code)! ||
        (left.fingerprint ?? "").localeCompare(right.fingerprint ?? "") ||
        left.field.localeCompare(right.field),
    ),
  );

/** Pure canonical rule port. Its facts may only be produced by the authority parser below. */
export const evaluateWatcherEventClassificationRulesV1 = (
  input: Readonly<{
    startTime: string;
    endTime: string;
    sources: readonly WatcherEventClassificationFactV1[];
    trace: readonly WatcherEventClassificationTraceFactV1[];
  }>,
): readonly WatcherEventClassificationFindingV1[] => {
  if (
    !NATURAL.test(input.startTime) ||
    !NATURAL.test(input.endTime) ||
    BigInt(input.endTime) <= BigInt(input.startTime)
  ) {
    return Object.freeze([
      { code: "malformed_input", fingerprint: null, field: "$.header.time" },
    ]);
  }
  const start = BigInt(input.startTime);
  const end = BigInt(input.endTime);
  const findings: WatcherEventClassificationFindingV1[] = [];
  const sources = new Map<string, WatcherEventClassificationFactV1>();
  for (const source of input.sources) {
    if (sources.has(source.fingerprint))
      findings.push({
        code: "duplicate_source_event",
        fingerprint: source.fingerprint,
        field: "$.sources",
      });
    else sources.set(source.fingerprint, source);
  }
  const trace = new Map<string, WatcherEventClassificationTraceFactV1>();
  for (const entry of [...input.trace].sort(
    (a, b) => a.stepIndex - b.stepIndex,
  )) {
    if (trace.has(entry.fingerprint))
      findings.push({
        code: "duplicate_trace_event",
        fingerprint: entry.fingerprint,
        field: "$.trace",
      });
    else trace.set(entry.fingerprint, entry);
    if (traceFingerprintPhase(entry.fingerprint) !== entry.phase)
      findings.push({
        code: "trace_event_phase_mismatch",
        fingerprint: entry.fingerprint,
        field: "$.trace.phase",
      });
  }
  for (const entry of trace.values()) {
    if (entry.phase !== "L2Transaction" && !sources.has(entry.fingerprint))
      findings.push({
        code: "fabricated_trace_event",
        fingerprint: entry.fingerprint,
        field: "$.trace.source",
      });
  }
  for (const source of sources.values()) {
    const inclusion = NATURAL.test(source.inclusionTime)
      ? BigInt(source.inclusionTime)
      : null;
    if (inclusion === null) {
      findings.push({
        code: "malformed_input",
        fingerprint: source.fingerprint,
        field: "$.source.inclusionTime",
      });
      continue;
    }
    const forcedIntervalMalformed =
      source.phase === "ForcedTransaction" &&
      source.forcedInterval !== null &&
      (!NATURAL_OR_NONE.test(source.forcedInterval.start) ||
        !NATURAL_OR_NONE.test(source.forcedInterval.end));
    if (forcedIntervalMalformed) {
      findings.push({
        code: "malformed_input",
        fingerprint: source.fingerprint,
        field: "$.forced.interval",
      });
      continue;
    }
    const due =
      watcherTimedL1EventIsDueV1(start, end, inclusion) &&
      (source.phase !== "ForcedTransaction" ||
        (source.forcedInterval !== null &&
          watcherForcedIntervalIsDueV1(
            start,
            end,
            BigInt(source.forcedInterval.start),
            BigInt(source.forcedInterval.end),
          )));
    const entry = trace.get(source.fingerprint);
    if (due && entry === undefined)
      findings.push({
        code: "omitted_due_event",
        fingerprint: source.fingerprint,
        field: "$.trace",
      });
    if (!due && entry !== undefined)
      findings.push({
        code: "out_of_window_event",
        fingerprint: source.fingerprint,
        field: "$.trace",
      });
    if (entry === undefined) continue;
    if (source.phase === "Withdrawal") {
      const expectedSettlement =
        source.withdrawalValidity === "valid"
          ? "initialize_payout"
          : "refund_withdrawal";
      const expectedMutation = source.withdrawalValidity === "valid";
      if (
        source.withdrawalValidity === null ||
        source.settlementKind !== expectedSettlement ||
        (expectedMutation
          ? entry.mutationCount !== 1 || entry.preRoot === entry.postRoot
          : entry.mutationCount !== 0 || entry.preRoot !== entry.postRoot)
      )
        findings.push({
          code: "withdrawal_validity_mismatch",
          fingerprint: source.fingerprint,
          field: "$.withdrawal",
        });
    }
    if (source.phase === "ForcedTransaction" && source.forcedInterval === null)
      findings.push({
        code: "forced_source_mismatch",
        fingerprint: source.fingerprint,
        field: "$.forced",
      });
    if (source.phase === "ForcedTransaction" && entry !== undefined) {
      const expectedMutation = source.forcedValidity === "valid";
      if (
        source.forcedValidity === null ||
        (expectedMutation
          ? entry.mutationCount === 0 || entry.preRoot === entry.postRoot
          : entry.mutationCount !== 0 || entry.preRoot !== entry.postRoot)
      )
        findings.push({
          code: "forced_classification_mismatch",
          fingerprint: source.fingerprint,
          field: "$.forced.classification",
        });
    }
  }
  return orderFindings(findings);
};

const allEvents = (
  result: NonNullable<ReturnType<typeof parseWatcherUserEventIndexerResultV1>>,
): readonly (WatcherIndexedUserEventV1 | WatcherTerminalUserEventV1)[] =>
  result.state === null
    ? []
    : [
        ...result.state.snapshot.activeEvents,
        ...result.state.snapshot.terminalEvents,
      ];

const parseW25 = (
  value: unknown,
): {
  receipt: WatcherBlockReplayResultV1;
  trace: readonly WatcherEventClassificationTraceFactV1[];
  inputDigest: string;
  forcedValidationFacts: readonly ForcedValidationFact[];
} | null => {
  const keys = [
    "schemaVersion",
    "action",
    "reasonCodes",
    "verifiedRequires",
    "downstreamPrerequisite",
    "rejectionSelection",
    "consensusProfileId",
    "headerHash",
    "payloadEnvelopeSha256",
    "payloadSha256",
    "reconstructionDigest",
    "phaseAResultDigest",
    "ruleBundleCommitment",
    "authorityManifestDigest",
    "sourceManifestDigest",
    "effectManifestDigest",
    "priorStateRoot",
    "expectedPriorStateRoot",
    "postStateRoot",
    "expectedPostStateRoot",
    "transactionCount",
    "acceptedCount",
    "acceptedTxIds",
    "intermediateRoots",
    "transactionRoots",
    "eventRoots",
    "stageMismatches",
    "rejections",
    "selectedRejection",
    "forcedValidationFacts",
    "resultDigest",
  ] as const;
  const record = exact(value, keys);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION ||
    record.action !== "accept" ||
    !Array.isArray(record.reasonCodes) ||
    record.reasonCodes.length !== 0 ||
    !isPlain(record.downstreamPrerequisite) ||
    !HEX_32.test(String(record.resultDigest))
  )
    return null;
  const prerequisite = exact(record.downstreamPrerequisite, [
    "schemaVersion",
    "requiredVerifier",
    "inputDigest",
    "w29Eligibility",
  ]);
  if (
    prerequisite === null ||
    prerequisite.schemaVersion !==
      WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION ||
    prerequisite.requiredVerifier !== "W26" ||
    prerequisite.w29Eligibility !== "requires_w26_accept" ||
    !HEX_32.test(String(prerequisite.inputDigest))
  )
    return null;
  const withoutDigest = Object.fromEntries(
    Object.entries(record).filter(([key]) => key !== "resultDigest"),
  );
  if (watcherSha256CanonicalJsonV1(withoutDigest) !== record.resultDigest)
    return null;
  const expectedInputDigest = watcherSha256CanonicalJsonV1({
    headerHash: record.headerHash,
    payloadEnvelopeSha256: record.payloadEnvelopeSha256,
    reconstructionDigest: record.reconstructionDigest,
    phaseAResultDigest: record.phaseAResultDigest,
    ruleBundleCommitment: record.ruleBundleCommitment,
    authorityManifestDigest: record.authorityManifestDigest,
    sourceManifestDigest: record.sourceManifestDigest,
    effectManifestDigest: record.effectManifestDigest,
    forcedValidationFacts: record.forcedValidationFacts,
    priorStateRoot: record.priorStateRoot,
    postStateRoot: record.postStateRoot,
  });
  if (
    expectedInputDigest !== prerequisite.inputDigest ||
    typeof record.headerHash !== "string"
  )
    return null;
  if (!Array.isArray(record.eventRoots)) return null;
  const trace: WatcherEventClassificationTraceFactV1[] = [];
  for (const event of record.eventRoots) {
    const parsed = exact(event, [
      "stepIndex",
      "phase",
      "eventKeyFingerprint",
      "preRoot",
      "postRoot",
      "mutationCount",
    ]);
    const stepIndex = parsed?.stepIndex;
    const mutationCount = parsed?.mutationCount;
    if (
      parsed === null ||
      typeof stepIndex !== "number" ||
      !Number.isSafeInteger(stepIndex) ||
      stepIndex < 0 ||
      typeof parsed.phase !== "string" ||
      !EVENT_PHASES.has(parsed.phase) ||
      typeof parsed.eventKeyFingerprint !== "string" ||
      !HEX_32.test(String(parsed.preRoot)) ||
      !HEX_32.test(String(parsed.postRoot)) ||
      typeof mutationCount !== "number" ||
      !Number.isSafeInteger(mutationCount) ||
      mutationCount < 0
    )
      return null;
    trace.push({
      fingerprint: parsed.eventKeyFingerprint,
      phase: parsed.phase as WatcherEventClassificationTraceFactV1["phase"],
      stepIndex,
      preRoot: parsed.preRoot as string,
      postRoot: parsed.postRoot as string,
      mutationCount,
    });
  }
  if (!Array.isArray(record.transactionRoots)) return null;
  for (const transaction of record.transactionRoots) {
    const parsed = exact(transaction, [
      "txIndex",
      "txId",
      "preRoot",
      "postRoot",
      "mutationCount",
      "committedStepIndex",
      "committedPreRoot",
      "committedPostRoot",
    ]);
    if (
      parsed === null ||
      typeof parsed.txId !== "string" ||
      !HEX_32.test(parsed.txId) ||
      typeof parsed.committedStepIndex !== "number" ||
      !Number.isSafeInteger(parsed.committedStepIndex) ||
      parsed.committedStepIndex < 0 ||
      typeof parsed.mutationCount !== "number" ||
      !Number.isSafeInteger(parsed.mutationCount) ||
      parsed.mutationCount < 0 ||
      !HEX_32.test(String(parsed.preRoot)) ||
      !HEX_32.test(String(parsed.postRoot))
    )
      return null;
    trace.push({
      fingerprint: `L2Transaction:${parsed.txId}`,
      phase: "L2Transaction",
      stepIndex: parsed.committedStepIndex,
      preRoot: parsed.preRoot as string,
      postRoot: parsed.postRoot as string,
      mutationCount: parsed.mutationCount,
    });
  }
  const ordered = [...trace].sort(
    (left, right) => left.stepIndex - right.stepIndex,
  );
  if (ordered.some((entry, index) => entry.stepIndex !== index)) return null;
  if (!Array.isArray(record.forcedValidationFacts)) return null;
  const forcedValidationFacts: ForcedValidationFact[] = [];
  for (const fact of record.forcedValidationFacts) {
    const parsed = exact(fact, [
      "eventKeyFingerprint",
      "stepIndex",
      "authenticatedOperatorValidity",
      "canonicalOperatorValidity",
      "phaseAStatus",
      "phaseARejectCode",
      "phaseBStatus",
      "phaseBRejectCode",
      "canonicalEffectDigest",
      "canonicalEffectMutationCount",
    ]);
    if (
      parsed === null ||
      typeof parsed.eventKeyFingerprint !== "string" ||
      typeof parsed.stepIndex !== "number" ||
      !Number.isSafeInteger(parsed.stepIndex) ||
      parsed.stepIndex < 0 ||
      !isWatcherForcedOperatorVerdictV1(parsed.authenticatedOperatorValidity) ||
      !isWatcherForcedOperatorVerdictV1(parsed.canonicalOperatorValidity) ||
      (parsed.phaseAStatus !== "accepted" &&
        parsed.phaseAStatus !== "rejected") ||
      (parsed.phaseBStatus !== "not_run" &&
        parsed.phaseBStatus !== "accepted" &&
        parsed.phaseBStatus !== "rejected") ||
      (parsed.phaseARejectCode !== null &&
        typeof parsed.phaseARejectCode !== "string") ||
      (parsed.phaseBRejectCode !== null &&
        typeof parsed.phaseBRejectCode !== "string") ||
      !HEX_32.test(String(parsed.canonicalEffectDigest)) ||
      typeof parsed.canonicalEffectMutationCount !== "number" ||
      !Number.isSafeInteger(parsed.canonicalEffectMutationCount) ||
      parsed.canonicalEffectMutationCount < 0
    )
      return null;
    if (
      (parsed.phaseAStatus === "accepted") !==
        (parsed.phaseARejectCode === null) ||
      (parsed.phaseAStatus === "rejected" &&
        (parsed.phaseBStatus !== "not_run" ||
          parsed.phaseBRejectCode !== null)) ||
      (parsed.phaseAStatus === "accepted" &&
        (parsed.phaseBStatus === "not_run" ||
          (parsed.phaseBStatus === "accepted" &&
            parsed.phaseBRejectCode !== null) ||
          (parsed.phaseBStatus === "rejected" &&
            parsed.phaseBRejectCode === null)))
    )
      return null;
    forcedValidationFacts.push(parsed as ForcedValidationFact);
  }
  return {
    receipt: value as WatcherBlockReplayResultV1,
    trace: Object.freeze(ordered),
    inputDigest: prerequisite.inputDigest as string,
    forcedValidationFacts: Object.freeze(forcedValidationFacts),
  };
};

const settlementKindFor = (
  event: WatcherIndexedUserEventV1 | WatcherTerminalUserEventV1,
  authorities: readonly WatcherEventClassificationSettlementAuthorityV1[],
): "initialize_payout" | "refund_withdrawal" | null | "invalid" => {
  const matches: ("initialize_payout" | "refund_withdrawal")[] = [];
  for (const authority of authorities) {
    const parsed = parseWatcherSettlementIndexerResultV1(
      authority.result,
      authority.context,
    );
    if (
      parsed === null ||
      parsed.action !== "accept" ||
      parsed.protocolDecision !== "indexed" ||
      parsed.state === null
    )
      return "invalid";
    for (const { observation } of parsed.state.activeHistory) {
      const transition = observation.transition;
      const consumes =
        transition.consumedOutRefs.length === 1 &&
        transition.consumedOutRefs[0] === event.outRef;
      if (!consumes) continue;
      if (
        transition.kind === "initialize_payout" &&
        transition.relatedSubjectId === event.assetNameHex
      )
        matches.push("initialize_payout");
      if (
        transition.kind === "refund_withdrawal" &&
        transition.subjectId === event.assetNameHex
      )
        matches.push("refund_withdrawal");
    }
  }
  return matches.length === 1
    ? matches[0]!
    : matches.length === 0
      ? null
      : "invalid";
};

const forcedIntervalFor = (
  event: WatcherIndexedUserEventV1 | WatcherTerminalUserEventV1,
  nativeTransactions: readonly WatcherForcedEventNativeTransactionV1[],
): Readonly<{
  start: string;
  end: string;
  txId: string;
}> | null => {
  const matches = nativeTransactions.filter(
    (candidate) => candidate.eventOutRef === event.outRef,
  );
  if (matches.length !== 1) return null;
  try {
    const bytes = Buffer.from(matches[0]!.canonicalNativeTxCbor);
    const decodedEvent = Data.from(
      event.eventCborHex,
      TxOrderEventV1 as never,
    ) as {
      readonly tx: {
        readonly tx_id: string;
        readonly transaction_commitment: string;
      };
    };
    const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(bytes);
    const source = deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(bytes);
    if (
      computeMidgardNativeTxIdV1(full).toString("hex") !==
        decodedEvent.tx.tx_id ||
      computeMidgardNativeTxProofCommitmentV1(source).toString("hex") !==
        decodedEvent.tx.transaction_commitment
    )
      return null;
    return Object.freeze({
      start: full.compact.transactionBody.validityIntervalStart.toString(),
      end: full.compact.transactionBody.validityIntervalEnd.toString(),
      txId: computeMidgardNativeTxIdV1(full).toString("hex"),
    });
  } catch {
    return null;
  }
};

const result = (
  action: WatcherEventClassificationActionV1,
  inputDigest: string,
  findings: readonly WatcherEventClassificationFindingV1[],
  sourceAuthorityDigest: string | null,
  traceDigest: string | null,
): WatcherEventClassificationResultV1 => {
  const reasonCodes = Object.freeze(
    [...new Set(findings.map(({ code }) => code))].sort(
      (left, right) => REASON_ORDER.get(left)! - REASON_ORDER.get(right)!,
    ),
  );
  const core = {
    schemaVersion: WATCHER_EVENT_CLASSIFICATION_VERIFIER_V1_SCHEMA_VERSION,
    action,
    reasonCodes,
    findings,
    classificationPrerequisite: Object.freeze({
      requiredVerifier: "W26" as const,
      inputDigest,
    }),
    w29Eligibility: "w26_accepted_not_w29_verified" as const,
    sourceAuthorityDigest,
    traceDigest,
  };
  return Object.freeze({
    ...core,
    resultDigest: watcherSha256CanonicalJsonV1(core),
  });
};

/** Recomputes W15/W16 authorities before applying the pure canonical rules. */
export const evaluateWatcherEventClassificationV1 = (
  input: EvaluateWatcherEventClassificationInputV1,
): WatcherEventClassificationResultV1 => {
  if (
    !isPlain(input.blockReplay) ||
    input.blockReplay.schemaVersion !== WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION
  )
    return result(
      "error",
      "0".repeat(64),
      [
        {
          code: "unknown_schema",
          fingerprint: null,
          field: "$.blockReplay.schemaVersion",
        },
      ],
      null,
      null,
    );
  if (input.blockReplay.action !== "accept")
    return result(
      "reject",
      "0".repeat(64),
      [
        {
          code: "w25_not_accepted",
          fingerprint: null,
          field: "$.blockReplay.action",
        },
      ],
      null,
      null,
    );
  const rawPrerequisite = input.blockReplay.downstreamPrerequisite;
  if (
    !isPlain(rawPrerequisite) ||
    rawPrerequisite.requiredVerifier !== "W26" ||
    rawPrerequisite.schemaVersion !==
      WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION ||
    rawPrerequisite.w29Eligibility !== "requires_w26_accept"
  )
    return result(
      "reject",
      "0".repeat(64),
      [
        {
          code: "w25_prerequisite_mismatch",
          fingerprint: null,
          field: "$.blockReplay.downstreamPrerequisite",
        },
      ],
      null,
      null,
    );
  if (
    typeof input.blockReplay.resultDigest !== "string" ||
    watcherSha256CanonicalJsonV1(
      Object.fromEntries(
        Object.entries(input.blockReplay).filter(
          ([key]) => key !== "resultDigest",
        ),
      ),
    ) !== input.blockReplay.resultDigest
  )
    return result(
      "error",
      typeof rawPrerequisite.inputDigest === "string"
        ? rawPrerequisite.inputDigest
        : "0".repeat(64),
      [
        {
          code: "w25_digest_mismatch",
          fingerprint: null,
          field: "$.blockReplay.resultDigest",
        },
      ],
      null,
      null,
    );
  const w25 = parseW25(input.blockReplay);
  if (w25 === null)
    return result(
      "error",
      "0".repeat(64),
      [
        {
          code: "w25_digest_mismatch",
          fingerprint: null,
          field: "$.blockReplay",
        },
      ],
      null,
      null,
    );
  const header = parseWatcherStateQueueHeaderV1(input.header);
  if (header === null || header.headerHash !== w25.receipt.headerHash)
    return result(
      "error",
      w25.inputDigest,
      [
        {
          code: "header_binding_mismatch",
          fingerprint: null,
          field: "$.header",
        },
      ],
      null,
      null,
    );
  if (input.userEventAuthorities.length !== 1)
    return result(
      "reject",
      w25.inputDigest,
      [
        {
          code: "authority_substitution",
          fingerprint: null,
          field: "$.userEventAuthorities",
        },
      ],
      null,
      null,
    );
  const phaseA = input.phaseA as WatcherPhaseAVerificationResultV1;
  if (
    !isPlain(phaseA) ||
    phaseA.schemaVersion !== WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION ||
    phaseA.action !== "accept" ||
    !Array.isArray(phaseA.reasonCodes) ||
    phaseA.reasonCodes.length !== 0 ||
    !Array.isArray(phaseA.rejections) ||
    phaseA.rejections.length !== 0 ||
    !Array.isArray(phaseA.acceptedTxIds) ||
    phaseA.resultDigest !== w25.receipt.phaseAResultDigest ||
    watcherSha256CanonicalJsonV1(
      Object.fromEntries(
        Object.entries(phaseA).filter(([key]) => key !== "resultDigest"),
      ),
    ) !== phaseA.resultDigest ||
    phaseA.headerHash !== header.headerHash
  )
    return result(
      "reject",
      w25.inputDigest,
      [
        {
          code: "authority_substitution",
          fingerprint: null,
          field: "$.phaseA",
        },
      ],
      null,
      null,
    );
  const replayedL2Ids = w25.trace
    .filter((entry) => entry.phase === "L2Transaction")
    .map((entry) => entry.fingerprint.slice("L2Transaction:".length));
  if (
    replayedL2Ids.length !== phaseA.acceptedTxIds.length ||
    replayedL2Ids.some((txId, index) => txId !== phaseA.acceptedTxIds[index]) ||
    new Set(replayedL2Ids).size !== replayedL2Ids.length
  )
    return result(
      "reject",
      w25.inputDigest,
      [
        {
          code: "authority_substitution",
          fingerprint: null,
          field: "$.blockReplay.transactionRoots",
        },
      ],
      null,
      null,
    );
  const sources: WatcherEventClassificationFactV1[] = [];
  const sourceEvidence: unknown[] = [];
  for (const authority of input.userEventAuthorities) {
    const parsed = parseWatcherUserEventIndexerResultV1(
      authority.result,
      authority.context,
    );
    if (
      parsed === null ||
      parsed.action !== "accept" ||
      parsed.protocolDecision !== "indexed" ||
      parsed.state === null ||
      parsed.state.snapshot.quarantined
    )
      return result(
        "error",
        w25.inputDigest,
        [
          {
            code: "user_event_authority_invalid",
            fingerprint: null,
            field: "$.userEventAuthorities",
          },
        ],
        null,
        null,
      );
    sourceEvidence.push({
      resultDigest: parsed.resultDigest,
      stateDigest: parsed.state.stateDigest,
    });
    for (const event of allEvents(parsed)) {
      const phase =
        event.kind === "deposit"
          ? "Deposit"
          : event.kind === "withdrawal"
            ? "Withdrawal"
            : "ForcedTransaction";
      const key = authenticatedEventFingerprint(phase, event);
      if (key === null)
        return result(
          "error",
          w25.inputDigest,
          [
            {
              code: "malformed_input",
              fingerprint: null,
              field: "$.userEvent.eventId",
            },
          ],
          null,
          null,
        );
      let withdrawalValidity: WatcherEventClassificationFactV1["withdrawalValidity"] =
        null;
      let settlementKind: WatcherEventClassificationFactV1["settlementKind"] =
        null;
      let forcedInterval: WatcherEventClassificationFactV1["forcedInterval"] =
        null;
      let forcedValidity: WatcherEventClassificationFactV1["forcedValidity"] =
        null;
      if (phase === "Withdrawal") {
        try {
          const classified = settlementKindFor(
            event,
            input.settlementAuthorities,
          );
          if (classified === "invalid" || classified === null)
            return result(
              "reject",
              w25.inputDigest,
              [
                {
                  code: "authority_substitution",
                  fingerprint: key,
                  field: "$.settlementAuthorities",
                },
              ],
              null,
              null,
            );
          settlementKind = classified;
          withdrawalValidity =
            classified === "initialize_payout" ? "valid" : "invalid";
        } catch {
          return result(
            "error",
            w25.inputDigest,
            [
              {
                code: "malformed_input",
                fingerprint: key,
                field: "$.withdrawal",
              },
            ],
            null,
            null,
          );
        }
      }
      if (phase === "ForcedTransaction") {
        const forced = forcedIntervalFor(event, input.forcedNativeTransactions);
        if (forced === null)
          return result(
            "reject",
            w25.inputDigest,
            [
              {
                code: "forced_source_mismatch",
                fingerprint: key,
                field: "$.forcedNativeTransactions",
              },
            ],
            null,
            null,
          );
        if (phaseA.acceptedTxIds.includes(forced.txId))
          return result(
            "reject",
            w25.inputDigest,
            [
              {
                code: "forced_classification_mismatch",
                fingerprint: key,
                field: "$.phaseA.acceptedTxIds",
              },
            ],
            null,
            null,
          );
        forcedInterval = forced;
        const replayed = w25.trace.find((entry) => entry.fingerprint === key);
        if (replayed !== undefined) {
          if (
            !("terminalClassification" in event) ||
            event.terminalClassification === undefined ||
            event.terminalClassification.terminalTransactionHash !==
              event.terminalTransactionHash ||
            event.terminalClassification.terminalPointDigest !==
              event.terminalPointDigest
          )
            return result(
              "reject",
              w25.inputDigest,
              [
                {
                  code: "authority_substitution",
                  fingerprint: key,
                  field: "$.userEvent.terminalClassification",
                },
              ],
              null,
              null,
            );
          const facts = w25.forcedValidationFacts.filter(
            (fact) =>
              fact.eventKeyFingerprint === key &&
              fact.stepIndex === replayed.stepIndex,
          );
          const fact = facts[0];
          const canonicalAccepted =
            fact?.phaseAStatus === "accepted" &&
            fact.phaseBStatus === "accepted";
          if (
            facts.length !== 1 ||
            fact === undefined ||
            fact.authenticatedOperatorValidity !==
              event.terminalClassification.operatorValidity ||
            fact.authenticatedOperatorValidity !==
              fact.canonicalOperatorValidity ||
            fact.canonicalEffectMutationCount !== replayed.mutationCount ||
            (canonicalAccepted
              ? fact.canonicalOperatorValidity !== WATCHER_FORCED_TX_VALID_V1 ||
                replayed.mutationCount === 0 ||
                replayed.preRoot === replayed.postRoot
              : fact.canonicalOperatorValidity === WATCHER_FORCED_TX_VALID_V1 ||
                replayed.mutationCount !== 0 ||
                replayed.preRoot !== replayed.postRoot)
          )
            return result(
              "reject",
              w25.inputDigest,
              [
                {
                  code: "forced_classification_mismatch",
                  fingerprint: key,
                  field: "$.forcedValidationFacts",
                },
              ],
              null,
              null,
            );
          forcedValidity = canonicalAccepted ? "valid" : "invalid";
        }
      }
      sources.push({
        fingerprint: key,
        phase,
        inclusionTime: event.inclusionTime,
        withdrawalValidity,
        forcedInterval,
        forcedValidity,
        settlementKind,
      });
    }
  }
  const findings = evaluateWatcherEventClassificationRulesV1({
    startTime: header.startTime,
    endTime: header.endTime,
    sources,
    trace: w25.trace,
  });
  const sourceAuthorityDigest = watcherSha256CanonicalJsonV1(sourceEvidence);
  const traceDigest = watcherSha256CanonicalJsonV1(w25.trace);
  return result(
    findings.length === 0 ? "accept" : "reject",
    w25.inputDigest,
    findings,
    sourceAuthorityDigest,
    traceDigest,
  );
};

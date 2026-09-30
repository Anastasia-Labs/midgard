import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { computeDaPayloadRoots } from "./commit-block-header/da-payload.js";

export type T2CandidateEventIds = {
  readonly deposits: readonly string[];
  readonly forcedTransactions: readonly string[];
  readonly withdrawals: readonly string[];
};

export type T2ForeignEventResolution =
  | {
      readonly type: "Ready";
      readonly absent: T2CandidateEventIds;
    }
  | {
      readonly type: "AwaitingForeignDa";
      readonly foreignHeaderHash: string;
      readonly reason:
        | "missing"
        | "invalid"
        | "foreign_event_present_requires_finalization";
      readonly detail: string;
      readonly present: T2CandidateEventIds;
    };

export const emptyIds = (): T2CandidateEventIds => ({
  deposits: [],
  forcedTransactions: [],
  withdrawals: [],
});

export const normalizedIds = (ids: readonly string[]): readonly string[] =>
  [...new Set(ids.map((id) => id.toLowerCase()))].sort();

const countsMatchHeader = (
  payload: SDK.DaPayload,
  header: SDK.Header,
): boolean => {
  const body = payload.block_body;
  const counts = body.counts;
  return (
    BigInt(body.deposits.length) === header.depositCount &&
    BigInt(body.forced_transactions.length) === header.forcedTransactionCount &&
    BigInt(body.withdrawals.length) === header.withdrawalCount &&
    BigInt(body.transactions.length) === header.l2TransactionCount &&
    BigInt(body.transition_trace.length) === header.transitionStepCount &&
    counts.depositCount === header.depositCount &&
    counts.forcedTransactionCount === header.forcedTransactionCount &&
    counts.withdrawalCount === header.withdrawalCount &&
    counts.l2TransactionCount === header.l2TransactionCount &&
    counts.totalEventCount === header.totalEventCount &&
    counts.transitionStepCount === header.transitionStepCount &&
    counts.validationTraceCount === header.validationTraceCount &&
    BigInt(body.validation_traces.length) === header.validationTraceCount
  );
};

const verifyForeignPayload = ({
  foreignHeaderHash,
  header,
  payload,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
  readonly payload: SDK.DaPayload;
}): Effect.Effect<boolean> =>
  Effect.gen(function* () {
    const [computedHeaderHash, payloadHeaderHash, roots] = yield* Effect.all(
      [
        SDK.hashBlockHeader(header),
        SDK.hashBlockHeader(payload.block_body.header),
        computeDaPayloadRoots(payload),
      ],
      { concurrency: "unbounded" },
    );
    return (
      computedHeaderHash === foreignHeaderHash &&
      payload.block_body.header_hash === foreignHeaderHash &&
      payloadHeaderHash === foreignHeaderHash &&
      roots.depositsRoot === header.depositsRoot &&
      roots.forcedTransactionsRoot === header.forcedTransactionsRoot &&
      roots.withdrawalsRoot === header.withdrawalsRoot &&
      roots.transactionsRoot === header.transactionsRoot &&
      roots.utxosRoot === header.utxosRoot &&
      roots.transitionTraceRoot === header.transitionTraceRoot &&
      roots.eventToStepRoot === header.eventToStepRoot &&
      roots.validationTracesRoot === header.validationTracesRoot &&
      countsMatchHeader(payload, header)
    );
  }).pipe(Effect.catchAll(() => Effect.succeed(false)));

const classifyCategory = ({
  candidateIds,
  root,
  count,
  payloadEntries,
}: {
  readonly candidateIds: readonly string[];
  readonly root: string;
  readonly count: bigint;
  readonly payloadEntries?: readonly SDK.DaPayloadEntry[];
}):
  | { readonly status: "ready"; readonly absent: readonly string[] }
  | { readonly status: "missing" | "invalid" }
  | {
      readonly status: "present";
      readonly present: readonly string[];
      readonly absent: readonly string[];
    } => {
  const normalized = normalizedIds(candidateIds);
  if (root === SDK.EMPTY_MERKLE_TREE_ROOT) {
    return count === 0n
      ? { status: "ready", absent: normalized }
      : { status: "invalid" };
  }
  if (count === 0n) return { status: "invalid" };
  if (normalized.length === 0) return { status: "ready", absent: [] };
  if (payloadEntries === undefined) return { status: "missing" };
  const foreignIds = new Set(payloadEntries.map(([key]) => key.toLowerCase()));
  const present = normalized.filter((id) => foreignIds.has(id));
  const absent = normalized.filter((id) => !foreignIds.has(id));
  return present.length === 0
    ? { status: "ready", absent }
    : { status: "present", present, absent };
};

export const resolveT2ForeignEventEvidence = ({
  foreignHeaderHash,
  header,
  candidateIds,
  payload,
  payloadError,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
  readonly candidateIds: T2CandidateEventIds;
  readonly payload?: SDK.DaPayload;
  readonly payloadError?: string;
}): Effect.Effect<T2ForeignEventResolution> =>
  Effect.gen(function* () {
    const boundHeaderHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.catchAll(() => Effect.succeed("")),
    );
    if (boundHeaderHash !== foreignHeaderHash) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign header hash binding is invalid",
        present: emptyIds(),
      };
    }
    // Resolve the whole retained foreign evidence window, not only the IDs
    // visible on this pass. A non-empty category needs canonical DA even when
    // no local candidate is visible yet, otherwise a late-indexed event could
    // arrive after the marker was incorrectly declared reusable.
    const needsPayload =
      header.depositsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
      header.forcedTransactionsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT ||
      header.withdrawalsRoot !== SDK.EMPTY_MERKLE_TREE_ROOT;
    if (needsPayload && payloadError !== undefined) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: payloadError,
        present: emptyIds(),
      };
    }
    if (needsPayload && payload === undefined) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "missing",
        detail: "verified foreign DA payload is not locally available",
        present: emptyIds(),
      };
    }
    if (
      payload !== undefined &&
      !(yield* verifyForeignPayload({ foreignHeaderHash, header, payload }))
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign DA payload failed header, root, or count verification",
        present: emptyIds(),
      };
    }
    const deposits = classifyCategory({
      candidateIds: candidateIds.deposits,
      root: header.depositsRoot,
      count: header.depositCount,
      payloadEntries: payload?.block_body.deposits,
    });
    const forcedTransactions = classifyCategory({
      candidateIds: candidateIds.forcedTransactions,
      root: header.forcedTransactionsRoot,
      count: header.forcedTransactionCount,
      payloadEntries: payload?.block_body.forced_transactions,
    });
    const withdrawals = classifyCategory({
      candidateIds: candidateIds.withdrawals,
      root: header.withdrawalsRoot,
      count: header.withdrawalCount,
      payloadEntries: payload?.block_body.withdrawals,
    });
    const classifications = { deposits, forcedTransactions, withdrawals };
    if (
      Object.values(classifications).some(
        (classification) => classification.status === "invalid",
      )
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "invalid",
        detail: "foreign header event root/count evidence is inconsistent",
        present: emptyIds(),
      };
    }
    if (
      Object.values(classifications).some(
        (classification) => classification.status === "missing",
      )
    ) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "missing",
        detail: "foreign event membership requires verified DA",
        present: emptyIds(),
      };
    }
    const present: T2CandidateEventIds = {
      deposits: deposits.status === "present" ? deposits.present : [],
      forcedTransactions:
        forcedTransactions.status === "present"
          ? forcedTransactions.present
          : [],
      withdrawals: withdrawals.status === "present" ? withdrawals.present : [],
    };
    if (Object.values(present).some((ids) => ids.length > 0)) {
      return {
        type: "AwaitingForeignDa",
        foreignHeaderHash,
        reason: "foreign_event_present_requires_finalization",
        detail:
          "foreign DA proves candidate events were included, but foreign-finalization is not yet available",
        present,
      };
    }
    return {
      type: "Ready",
      absent: {
        deposits: deposits.status === "ready" ? deposits.absent : [],
        forcedTransactions:
          forcedTransactions.status === "ready"
            ? forcedTransactions.absent
            : [],
        withdrawals: withdrawals.status === "ready" ? withdrawals.absent : [],
      },
    };
  });

export const decodeStoredPayload = ({
  payloadCbor,
  schemaVersion,
}: {
  readonly payloadCbor: Buffer;
  readonly schemaVersion: number;
}): Promise<SDK.DaPayload> =>
  schemaVersion !== Number(SDK.DA_PAYLOAD_VERSION)
    ? Promise.reject(
        new Error("Stored DA payload schema version must equal canonical V1"),
      )
    : unwrapDaPayload(payloadCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      }).then((unwrapped) => SDK.decodeDaPayload(unwrapped.innerBytes));

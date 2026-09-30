import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import {
  expectedCounts,
  expectedRoots,
  headerCounts,
  headerRoots,
  type PayloadCountSet,
  type PayloadRootSet,
  rootMismatches,
} from "./da-payload.compute-da-payload-roots.js";

const countMismatches = (
  expected: PayloadCountSet,
  actual: PayloadCountSet,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? null
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? null
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? null
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? null : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? null
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? null
      : "transition_step_count",
    expected.validationTraceCount === actual.validationTraceCount
      ? null
      : "validation_trace_count",
  ].filter((field): field is string => field !== null);

const payloadMemberCounts = (payload: SDK.DaPayload): PayloadCountSet => ({
  withdrawalCount: BigInt(payload.block_body.withdrawals.length),
  forcedTransactionCount: BigInt(payload.block_body.forced_transactions.length),
  l2TransactionCount: BigInt(payload.block_body.transactions.length),
  depositCount: BigInt(payload.block_body.deposits.length),
  totalEventCount:
    BigInt(payload.block_body.withdrawals.length) +
    BigInt(payload.block_body.forced_transactions.length) +
    BigInt(payload.block_body.transactions.length) +
    BigInt(payload.block_body.deposits.length),
  transitionStepCount: BigInt(payload.block_body.transition_trace.length),
  validationTraceCount: BigInt(payload.block_body.validation_traces.length),
});

const payloadDeclaredCounts = (payload: SDK.DaPayload): PayloadCountSet =>
  payload.block_body.counts;

export const decodeHeader = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<SDK.Header, DatabaseError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
        SDK.Header,
      ) as SDK.Header,
    catch: (cause) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message: "Failed to decode pending block header CBOR for DA payload",
        cause,
      }),
  });

export const verifyPayloadCommitments = ({
  record,
  header,
  payload,
  roots,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly header: SDK.Header;
  readonly payload: SDK.DaPayload;
  readonly roots: PayloadRootSet;
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const headerHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
    if (
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID] !==
        MIDGARD_CONSENSUS_PROFILE.profileId ||
      payload.version !== SDK.DA_PAYLOAD_VERSION
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Pending journal profile, header generation, and DA payload generation do not match",
          cause: `profile=${
            record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID]
          },payload_version=${payload.version.toString()}`,
        }),
      );
    }
    const computedHeaderHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: DaPayloadsDB.tableName,
            message: "Failed to hash pending block header for DA payload",
            cause,
          }),
      ),
    );
    const expected = expectedRoots(record);
    const counts = expectedCounts(record);
    const mismatches = [
      ...rootMismatches(expected, roots),
      ...rootMismatches(headerRoots(header), roots).map(
        (field) => `header_${field}`,
      ),
      ...countMismatches(counts, payloadDeclaredCounts(payload)),
      ...countMismatches(counts, payloadMemberCounts(payload)).map(
        (field) => `member_${field}`,
      ),
      ...countMismatches(
        headerCounts(header),
        payloadDeclaredCounts(payload),
      ).map((field) => `header_${field}`),
      payload.block_body.event_to_step.length ===
      payload.block_body.transition_trace.length
        ? null
        : "event_to_step_count",
      payload.block_body.validation_traces.length ===
      Number(counts.validationTraceCount)
        ? null
        : "validation_trace_count",
      payload.block_body.header_hash === headerHash
        ? null
        : "payload_header_hash",
      computedHeaderHash === headerHash ? null : "computed_header_hash",
    ].filter((field): field is string => field !== null);
    if (mismatches.length > 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Refusing to persist DA payload because recomputed commitments do not match the pending block header",
          cause: `header_hash=${headerHash},mismatches=${mismatches.join(",")}`,
        }),
      );
    }
  });

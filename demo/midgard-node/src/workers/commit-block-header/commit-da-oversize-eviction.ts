/**
 * A normal transaction whose block cannot fit the DA frame on its own, over a
 * ledger whose empty block does, can never be committed: the commit step-down
 * would rebuild it alone on every tick. Such a transaction is rejected and the
 * block is rebuilt without it.
 */

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { COMMIT_REJECT_CODE_DA_OVERSIZE } from "../../mpf/commit-rejection.js";
import {
  type CommitDaFrameMeasurement,
  DA_PAYLOAD_UPPER_BOUND_HEADER,
  DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
} from "../utils/commit-block-planner.js";

export type CommitEviction = {
  readonly code: string;
  readonly detail: string;
};

const emptyBlockBytes = (header: SDK.Header, headerHash: string): number =>
  SDK.daPayloadEncodedSizeFromUtxoAggregate(
    {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: [],
        withdrawals: [],
        forced_transactions: [],
        transactions: [],
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
        deposits: [],
        transition_trace: [],
        event_to_step: [],
        validation_traces: [],
        validation_trace_witnesses: [],
        counts: {
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          totalEventCount: 0n,
          transitionStepCount: 0n,
          validationTraceCount: 0n,
        },
      },
    },
    { entryCount: 0, encodedTupleBytes: 0 },
  );

/**
 * How much longer the longest-encoding header (and header hash) the DA prefix
 * measurement sizes with can encode than any header the node submits. The
 * measurement's counts are exact, so only the other fields differ; a header
 * with empty hashes and zero integers encodes no longer than any real one.
 */
export const DA_PAYLOAD_HEADER_SLACK_BYTES =
  emptyBlockBytes(
    DA_PAYLOAD_UPPER_BOUND_HEADER,
    DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
  ) -
  emptyBlockBytes(
    {
      ...DA_PAYLOAD_UPPER_BOUND_HEADER,
      prevUtxosRoot: "",
      utxosRoot: "",
      withdrawalsRoot: "",
      forcedTransactionsRoot: "",
      transactionsRoot: "",
      depositsRoot: "",
      transitionTraceRoot: "",
      eventToStepRoot: "",
      validationTracesRoot: "",
      startTime: 0n,
      endTime: 0n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: "",
      operatorVkey: "",
      protocolVersion: 0n,
    },
    "",
  );

/**
 * The transaction a step-down outcome leaves that the exact DA frame refuses
 * whatever header the block gets, when the block without it fits: a block of
 * that one normal transaction and no other event.
 */
export const oversizeSingleNormalTx = (
  outcome: string,
  measurement: CommitDaFrameMeasurement | undefined,
  maxInnerBytes: number,
): { readonly txIdHex: string; readonly detail: string } | undefined => {
  if (
    outcome !== "exact_check_required" ||
    measurement === undefined ||
    measurement.acceptedTxCount !== 1 ||
    measurement.hasMandatoryWork
  )
    return undefined;
  const without = measurement.prefixes[0]?.innerBytesUpperBound;
  const withTx = measurement.prefixes[1]?.innerBytesUpperBound;
  const txId = measurement.acceptedTxIds[0];
  if (
    without === undefined ||
    withTx === undefined ||
    txId === undefined ||
    without > maxInnerBytes ||
    withTx - DA_PAYLOAD_HEADER_SLACK_BYTES <= maxInnerBytes
  )
    return undefined;
  return {
    txIdHex: txId.toString("hex"),
    detail: `The block holding only this transaction exceeds the DA frame: inner_bytes_at_least=${(
      withTx - DA_PAYLOAD_HEADER_SLACK_BYTES
    ).toString()},effective_inner_limit=${maxInnerBytes.toString()},inner_bytes_without_it_at_most=${without.toString()}`,
  };
};

/**
 * Runs the DA frame step-down; while it ends on one normal transaction the
 * frame refuses, adds that transaction to `evictions` (which the step-down's
 * build rejects before validation) and runs it again from a fresh base. Each
 * pass evicts a different transaction, so this ends.
 */
export const stepDownEvictingOversizeNormalTx = <
  B extends { readonly outcome: string },
  E,
  R,
  E2,
  R2,
>({
  stepDown,
  measurement,
  evictions,
  rebase,
  maxInnerBytes,
}: {
  /** One full step-down, re-run on each pass. */
  readonly stepDown: Effect.Effect<B, E, R>;
  /** The measurement of the build the last step-down returned. */
  readonly measurement: () => CommitDaFrameMeasurement | undefined;
  readonly evictions: Map<string, CommitEviction>;
  readonly rebase: Effect.Effect<void, E2, R2>;
  readonly maxInnerBytes: number;
}): Effect.Effect<B, E | E2, R | R2> =>
  Effect.gen(function* () {
    for (;;) {
      const build = yield* stepDown;
      const oversize = oversizeSingleNormalTx(
        build.outcome,
        measurement(),
        maxInnerBytes,
      );
      if (oversize === undefined || evictions.has(oversize.txIdHex))
        return build;
      evictions.set(oversize.txIdHex, {
        code: COMMIT_REJECT_CODE_DA_OVERSIZE,
        detail: oversize.detail,
      });
      yield* Effect.logWarning(
        `🔹 Rejecting tx ${oversize.txIdHex} whose block cannot fit the DA frame; rebuilding without it. ${oversize.detail}`,
      );
      yield* rebase;
    }
  });

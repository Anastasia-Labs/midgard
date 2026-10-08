import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect, Metric, Ref } from "effect";

import { jsonReplacer } from "../../commands/command-utils.js";
import { Entry as LedgerEntry } from "../../database/utils/ledger.js";
import { emitQueueStateMetrics } from "../../fibers/queue-metrics.js";
import { l1NowUnixTimeMs } from "../../l1-heads.js";
import { Globals } from "../../services/index.js";
import {
  requireLandedStateQueue,
  stateQueueContractOf,
} from "../../services/landed-state-queue.js";
import { breakDownTx } from "../../utils.js";
import { BlockTxPayload } from "../utils.js";
import {
  classifyOldestQueuedBlockCandidateReadiness,
  mergeMaturityWindow,
  type MergeReadinessStatus,
  type OldestQueuedBlockCandidateReadiness,
} from "./merge-readiness.js";
import {
  type MergeErrorCode,
  mergeFailureCounter,
  mergeMissingBlockTxsCounter,
} from "./merge-to-confirmed-state.landed-unfinalized-merges.js";

export type CanonicalMergeCandidateReadiness =
  | {
      readonly status: "candidate";
      readonly confirmedUTxO: SDK.StateQueueUTxO;
      readonly firstBlockUTxO: SDK.StateQueueUTxO;
      readonly blockHeader: SDK.Header;
      readonly firstBlockNode: SDK.StateQueueNode;
      readonly readiness: OldestQueuedBlockCandidateReadiness;
    }
  | {
      readonly status: "no_candidate";
      readonly reason: string;
    };

export type MergeTxResult =
  | {
      readonly status: "merged";
      readonly headerHash: string;
      readonly txHash: string;
    }
  | {
      readonly status: Exclude<MergeReadinessStatus, "ready">;
      readonly reason: string;
      readonly headerHash?: string;
      readonly queueLength?: number;
      readonly minQueueLength?: number;
      readonly readyAfterUnixTime?: number;
      readonly nowUnixTime?: number;
    };

const makeJsonSafe = (value: unknown): unknown => {
  try {
    return JSON.parse(JSON.stringify(value, jsonReplacer)) as unknown;
  } catch {
    return formatUnknownError(value);
  }
};

const makeMergeStateQueueError = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message: `${errorCode}: ${message}`,
    cause: {
      error_code: errorCode,
      details: makeJsonSafe(cause),
    },
  });

type MergeFailureOptions = {
  readonly missingBlockTxs?: boolean;
};

export const failMergeWithCode = (
  errorCode: MergeErrorCode,
  message: string,
  cause: unknown,
  options?: MergeFailureOptions,
): Effect.Effect<never, SDK.StateQueueError> =>
  Effect.gen(function* () {
    yield* Metric.increment(mergeFailureCounter);
    if (options?.missingBlockTxs === true) {
      yield* Metric.increment(mergeMissingBlockTxsCounter);
    }
    return yield* Effect.fail(
      makeMergeStateQueueError(errorCode, message, cause),
    );
  });

const firstBlockOutRef = (firstBlockUTxO: SDK.StateQueueUTxO): string =>
  `${firstBlockUTxO.utxo.txHash}#${firstBlockUTxO.utxo.outputIndex.toString()}`;

/**
 * Classifies the oldest queued block for merging. Maturity is judged at the
 * L1 `slotNow` (plan §3.6), never the wall clock: a clock that runs fast must
 * not call a block mature early. An unknown L1 slot fails the attempt with a
 * `StateQueueError`, and the merge fiber retries.
 */
export const fetchCanonicalMergeCandidateReadiness = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  _contracts: SDK.MidgardValidators,
): Effect.Effect<
  CanonicalMergeCandidateReadiness,
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    // The root and its link, from the healthy landed queue (P1).
    const queue = yield* requireLandedStateQueue(
      stateQueueContractOf(fetchConfig),
      "merge",
    );
    const confirmedUTxO = queue.root?.element;
    const firstBlockUTxO = queue.nodes[0]?.element;
    if (confirmedUTxO === undefined || firstBlockUTxO === undefined) {
      return {
        status: "no_candidate",
        reason: "confirmed_state_link_empty",
      } satisfies CanonicalMergeCandidateReadiness;
    }
    if (firstBlockUTxO.datum.key === "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Failed to classify merge candidate",
          cause: "first queued block cannot be a root node",
        }),
      );
    }

    const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
    const firstBlockNode = yield* SDK.getStateQueueNodeFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const recomputedHeaderHash = yield* SDK.hashBlockHeader(blockHeader);
    if (recomputedHeaderHash !== headerNodeKey) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to classify merge candidate: queued block key/hash mismatch",
          cause: `datumKey=${headerNodeKey},computed=${recomputedHeaderHash}`,
        }),
      );
    }

    const mergeMaturity = mergeMaturityWindow(
      lucid,
      Number(blockHeader.endTime),
    );
    const nowUnixTime = yield* l1NowUnixTimeMs(lucid).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Merge paused until the L1 slot is known",
            cause,
          }),
      ),
    );
    return {
      status: "candidate",
      confirmedUTxO,
      firstBlockUTxO,
      blockHeader,
      firstBlockNode,
      readiness: classifyOldestQueuedBlockCandidateReadiness({
        firstBlockOutRef: firstBlockOutRef(firstBlockUTxO),
        headerHash: recomputedHeaderHash,
        currentDaAvailability: firstBlockNode.da_attestation,
        provenFraud: firstBlockNode.proven_fraud,
        validFromUnixTime: mergeMaturity.validFromUnixTime,
        readyAfterUnixTime: mergeMaturity.readyAfterUnixTime,
        nowUnixTime,
      }),
    } satisfies CanonicalMergeCandidateReadiness;
  });

export const mergeSemanticSkipResult = (
  readiness: Exclude<
    OldestQueuedBlockCandidateReadiness,
    { readonly status: "ready" }
  >,
): MergeTxResult => ({
  status: readiness.status,
  headerHash: readiness.headerHash,
  reason: readiness.reason,
  readyAfterUnixTime: readiness.readyAfterUnixTime,
  nowUnixTime: readiness.nowUnixTime,
});

type MergeDecodedBlockTx = {
  readonly txId: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly LedgerEntry[];
};

type MergeBlockTxPreflightError = {
  readonly index: number;
  readonly txIdHex: string;
  readonly reason: "DECODE_FAILED" | "TX_ID_MISMATCH";
  readonly details: string;
  readonly decodedTxIdHex?: string;
};

export const preflightDecodeBlockTxs = (
  blockTxs: readonly BlockTxPayload[],
): Effect.Effect<readonly MergeDecodedBlockTx[], MergeBlockTxPreflightError> =>
  Effect.forEach(
    blockTxs,
    (blockTx, index) =>
      Effect.gen(function* () {
        const txIdHex = blockTx.txId.toString("hex");
        const decoded = yield* breakDownTx(blockTx.txCbor).pipe(
          Effect.mapError(
            (cause): MergeBlockTxPreflightError => ({
              index,
              txIdHex,
              reason: "DECODE_FAILED",
              details: formatUnknownError(cause),
            }),
          ),
        );
        if (!decoded.txId.equals(blockTx.txId)) {
          return yield* Effect.fail<MergeBlockTxPreflightError>({
            index,
            txIdHex,
            reason: "TX_ID_MISMATCH",
            decodedTxIdHex: decoded.txId.toString("hex"),
            details:
              "Computed tx_id from payload does not match BlocksDB tx_id",
          });
        }
        return {
          txId: blockTx.txId,
          spent: decoded.spent,
          produced: decoded.produced,
        } satisfies MergeDecodedBlockTx;
      }),
    { concurrency: "unbounded" },
  );

/** Blocks in the healthy landed queue (P1), published to the queue gauges. */
export const getStateQueueLength = (
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<number, SDK.StateQueueError, Globals | SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const queue = yield* requireLandedStateQueue(
      stateQueueContractOf(fetchConfig),
      "merge",
    );
    yield* Ref.set(globals.BLOCKS_IN_QUEUE, queue.nodes.length);
    yield* emitQueueStateMetrics;
    return queue.nodes.length;
  });

export const slotFromUnixTime = (
  lucid: LucidEvolution,
  unixTimeMs: number,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      const slot = Number(lucid.unixTimeToSlot(unixTimeMs));
      if (!Number.isSafeInteger(slot) || slot < 0) {
        throw new Error(`invalid slot=${slot.toString()}`);
      }
      return slot;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to convert merge valid-from time to a slot",
        cause,
      }),
  });

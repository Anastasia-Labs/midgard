import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { l1NowUnixTimeMs } from "../../l1-heads.js";
import {
  landedStateQueueTail,
  landedStateQueueUTxOs,
  requireLandedStateQueue,
  stateQueueContractOf,
} from "../../services/landed-state-queue.js";

// The sibling SDK is built in its own TypeScript program, so its exported
// `Effect` values can carry a distinct branded generator identity during DTS
// emit. Normalize SDK helpers once at the worker boundary.
export const localizeSdkEffect = <A, E, R = never>(
  effect: unknown,
): Effect.Effect<A, E, R> => effect as Effect.Effect<A, E, R>;

export const getConfirmedStateFromStateQueueDatumLocal = (
  nodeDatum: SDK.LinkedListNodeView,
): Effect.Effect<
  { readonly data: SDK.ConfirmedState; readonly link: unknown },
  SDK.DataCoercionError
> => localizeSdkEffect(SDK.getConfirmedStateFromStateQueueDatum(nodeDatum));

export const getHeaderFromStateQueueDatumLocal = (
  nodeDatum: SDK.LinkedListNodeView,
): Effect.Effect<SDK.Header, SDK.DataCoercionError> =>
  localizeSdkEffect(SDK.getHeaderFromStateQueueDatum(nodeDatum));

export const hashBlockHeaderLocal = (
  header: SDK.Header,
): Effect.Effect<string, SDK.HashingError> =>
  localizeSdkEffect(SDK.hashBlockHeader(header));

export const updateLatestBlocksDatumAndGetTheNewHeaderLocal = (
  lucid: Parameters<
    typeof SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram
  >[0],
  latestBlocksDatum: SDK.LinkedListNodeView,
  newUTxOsRoot: string,
  transactionsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  transitionCommitments: SDK.HeaderTransitionCommitments,
  endTime: bigint,
  validationContext: Pick<
    SDK.Header,
    "blockSlot" | "expectedNetworkId" | "minFeeA" | "minFeeB"
  >,
): Effect.Effect<
  {
    readonly nodeDatum: SDK.LinkedListNodeView;
    readonly header: SDK.Header;
  },
  | SDK.DataCoercionError
  | SDK.HeaderTransitionCommitmentsError
  | SDK.LucidError
  | SDK.HashingError
> =>
  localizeSdkEffect(
    SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      latestBlocksDatum,
      newUTxOsRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
      transitionCommitments,
      endTime,
      validationContext,
    ),
  );

export const getLatestBlockDatumEndTime = (
  latestBlocksDatum: SDK.LinkedListNodeView,
): Effect.Effect<Date, SDK.DataCoercionError> =>
  latestBlocksDatum.key === "Empty"
    ? getConfirmedStateFromStateQueueDatumLocal(latestBlocksDatum).pipe(
        Effect.map(
          ({ data: confirmedState }) =>
            new Date(Number(confirmedState.endTime)),
        ),
      )
    : getHeaderFromStateQueueDatumLocal(latestBlocksDatum).pipe(
        Effect.map((latestHeader) => new Date(Number(latestHeader.endTime))),
      );

export const stateQueueOutRef = (block: SDK.StateQueueUTxO): string =>
  `${block.utxo.txHash}#${block.utxo.outputIndex.toString()}`;

export const stateQueueBaseHeaderHash = (
  block: SDK.StateQueueUTxO,
): Effect.Effect<string, SDK.DataCoercionError | SDK.HashingError, never> =>
  Effect.gen(function* () {
    if (block.datum.key === "Empty") {
      const { data } = yield* getConfirmedStateFromStateQueueDatumLocal(
        block.datum,
      );
      return data.headerHash;
    }
    const header = yield* getHeaderFromStateQueueDatumLocal(block.datum);
    return yield* hashBlockHeaderLocal(header);
  });

/** The landed queue's tail: the latest committed block (P1). */
export const fetchLatestCommittedBlockLocal = (
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  landedStateQueueTail(
    stateQueueContractOf(fetchConfig),
    "the latest committed block",
  );

export type CommitAppendFenceReferences = {
  readonly confirmedStateRefInput?: SDK.StateQueueUTxO["utxo"];
  readonly headStateQueueNodeRefInput?: SDK.StateQueueUTxO["utxo"];
};

/**
 * The header ends of every unattested node after the root, failing instead
 * when any of them is past its DA attestation deadline. Appending to an
 * expired unattested suffix only creates more work for permissionless
 * correction, so the commit pauses until correction removes it. Every pending
 * node counts, including tails hidden behind an attested queue head. Both the
 * append-fence cap and the build's fence references go through this one check,
 * so a commit refuses an expired suffix with the same error wherever it first
 * meets it. "Now" is the L1 `slotNow` (plan §3.6), never the wall clock: a
 * clock that runs fast must not pause commits early. While no L1 tip has been
 * read the commit pauses the same way, and the next tick retries.
 */
const unexpiredUnattestedSuffixEndTimes = (
  lucid: LucidEvolution,
  ordered: readonly SDK.StateQueueUTxO[],
): Effect.Effect<readonly number[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const nowMs = BigInt(
      yield* l1NowUnixTimeMs(lucid).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: "Commit paused until the L1 slot is known",
              cause,
            }),
        ),
      ),
    );
    const unattestedEndTimesMs: number[] = [];
    for (const entry of ordered.slice(1)) {
      const node = yield* localizeSdkEffect<
        SDK.StateQueueNode,
        SDK.DataCoercionError
      >(SDK.getStateQueueNodeFromStateQueueDatum(entry.datum)).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: "Failed to inspect pending DA attestation before commit",
              cause,
            }),
        ),
      );
      if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
        continue;
      }
      if (nowMs >= node.header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Commit paused until expired unattested suffix is corrected",
            cause: `expired_out_ref=${stateQueueOutRef(entry)}`,
          }),
        );
      }
      unattestedEndTimesMs.push(Number(node.header.endTime));
    }
    return unattestedEndTimesMs;
  });

/**
 * Q61's append fence as a cap on the new header's end. The header end is the
 * append's inclusive upper bound, so it must fall strictly before the DA
 * attestation deadline of every unattested node still in the queue, not only
 * the head's: an append landing after a pending tail's deadline takes the
 * tail that timeout correction is about to remove. Only the head's
 * fence is enforced on chain; the rest is this node's own build policy. An
 * on-chain node already past its deadline leaves no end to cap: the fence
 * fails with the build's expired-suffix refusal instead.
 */
export const resolveCommitAppendFenceEndTimeCapLocal = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<
  number | undefined,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const ordered = yield* landedStateQueueUTxOs(
      stateQueueContractOf(fetchConfig),
      "the commit append fence",
    );
    const unattestedEndTimesMs = yield* unexpiredUnattestedSuffixEndTimes(
      lucid,
      ordered,
    );
    return unattestedEndTimesMs.length === 0
      ? undefined
      : Math.min(...unattestedEndTimesMs) +
          Number(SDK.DA_ATTESTATION_TIMEOUT_MS) -
          1;
  });

/**
 * Resolves the exact singleton root/current-head reference inputs required by
 * Q61's append fence. The landed queue is read again immediately before the
 * transaction is built; if the expected tail changed, this attempt aborts and
 * the caller rebuilds from canonical state instead of journaling a stale
 * append.
 */
export const resolveCommitAppendFenceReferencesLocal = (
  lucid: LucidEvolution,
  fetchConfig: SDK.StateQueueFetchConfig,
  expectedTail: SDK.StateQueueUTxO,
): Effect.Effect<
  CommitAppendFenceReferences,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const ordered = yield* landedStateQueueUTxOs(
      stateQueueContractOf(fetchConfig),
      "the commit append fence",
    );
    yield* unexpiredUnattestedSuffixEndTimes(lucid, ordered);
    const canonicalTail = ordered.at(-1);
    if (canonicalTail === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Canonical state queue is empty",
          cause: "missing confirmed-state root",
        }),
      );
    }
    if (stateQueueOutRef(canonicalTail) !== stateQueueOutRef(expectedTail)) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `expected_tail=${stateQueueOutRef(expectedTail)},canonical_tail=${stateQueueOutRef(canonicalTail)}`,
        }),
      );
    }
    if (ordered.length === 1) {
      if (canonicalTail.datum.key !== "Empty") {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Canonical state queue is missing its root",
            cause: `tail=${stateQueueOutRef(canonicalTail)}`,
          }),
        );
      }
      return {};
    }

    const root = ordered[0];
    const head = ordered[1];
    if (
      root === undefined ||
      head === undefined ||
      root.datum.key !== "Empty" ||
      canonicalTail.datum.key === "Empty"
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Canonical state-queue topology is malformed",
          cause: `nodes=${ordered.length.toString()}`,
        }),
      );
    }
    return {
      confirmedStateRefInput: root.utxo,
      ...(stateQueueOutRef(head) === stateQueueOutRef(canonicalTail)
        ? {}
        : { headStateQueueNodeRefInput: head.utxo }),
    };
  });

/**
 * Revalidates a known state-queue tail against the landed queue. Appending
 * or merging can recreate the same logical node at a new out-ref, so a
 * replacement under the same NFT is still accepted while it is the tail.
 */
export const fetchExpectedStateQueueTailLocal = (
  fetchConfig: SDK.StateQueueFetchConfig,
  expectedTail: SDK.StateQueueUTxO,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const queue = yield* requireLandedStateQueue(
      stateQueueContractOf(fetchConfig),
      "the commit base revalidation",
    );
    const element = [
      ...(queue.root === null ? [] : [queue.root]),
      ...queue.nodes,
    ].find(({ element }) => element.assetName === expectedTail.assetName);
    if (element === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `expected_asset_name=${expectedTail.assetName},matches=0`,
        }),
      );
    }
    const candidate = element.element;
    if (
      candidate.utxo.txHash === expectedTail.utxo.txHash &&
      candidate.utxo.outputIndex === expectedTail.utxo.outputIndex
    ) {
      return expectedTail;
    }
    if (candidate.datum.next !== "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `asset_name=${expectedTail.assetName},outref=${stateQueueOutRef(candidate)}`,
        }),
      );
    }
    return candidate;
  });

import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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

export const getHeaderV1FromStateQueueDatumLocal = (
  nodeDatum: SDK.LinkedListNodeView,
): Effect.Effect<SDK.HeaderV1, SDK.DataCoercionError> =>
  localizeSdkEffect(SDK.getHeaderV1FromStateQueueDatum(nodeDatum));

export const hashBlockHeaderV1Local = (
  header: SDK.HeaderV1,
): Effect.Effect<string, SDK.HashingError> =>
  localizeSdkEffect(SDK.hashBlockHeaderV1(header));

export const updateLatestBlocksDatumAndGetTheNewHeaderV1Local = (
  lucid: Parameters<
    typeof SDK.updateLatestBlocksDatumAndGetTheNewHeaderV1Program
  >[0],
  latestBlocksDatum: SDK.LinkedListNodeView,
  newUTxOsRoot: string,
  transactionsRoot: string,
  depositsRoot: string,
  withdrawalsRoot: string,
  transitionCommitments: SDK.HeaderTransitionCommitmentsV1,
  endTime: bigint,
  validationContext: Pick<
    SDK.HeaderV1,
    "blockSlot" | "expectedNetworkId" | "minFeeA" | "minFeeB"
  >,
): Effect.Effect<
  {
    readonly nodeDatum: SDK.LinkedListNodeView;
    readonly header: SDK.HeaderV1;
  },
  | SDK.DataCoercionError
  | SDK.HeaderTransitionCommitmentsError
  | SDK.LucidError
  | SDK.HashingError
> =>
  localizeSdkEffect(
    SDK.updateLatestBlocksDatumAndGetTheNewHeaderV1Program(
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
    : getHeaderV1FromStateQueueDatumLocal(latestBlocksDatum).pipe(
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
    const header = yield* getHeaderV1FromStateQueueDatumLocal(block.datum);
    return yield* hashBlockHeaderV1Local(header);
  });

export const fetchLatestCommittedBlockLocal = (
  lucid: Parameters<typeof SDK.fetchLatestCommittedBlockProgram>[0],
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<SDK.StateQueueUTxO, SDK.StateQueueError | SDK.LucidError> =>
  localizeSdkEffect(SDK.fetchLatestCommittedBlockProgram(lucid, fetchConfig));

export type CommitAppendFenceReferences = {
  readonly confirmedStateRefInput?: SDK.StateQueueUTxO["utxo"];
  readonly headStateQueueNodeRefInput?: SDK.StateQueueUTxO["utxo"];
};

/**
 * Resolves the exact singleton root/current-head reference inputs required by
 * Q61's append fence. The full topology is refetched immediately before the
 * transaction is built; if the expected tail changed, this attempt aborts and
 * the caller rebuilds from canonical state instead of journaling a stale
 * append.
 */
export const resolveCommitAppendFenceReferencesLocal = (
  lucid: Parameters<typeof SDK.fetchSortedStateQueueUTxOsProgram>[0],
  fetchConfig: SDK.StateQueueFetchConfig,
  expectedTail: SDK.StateQueueUTxO,
): Effect.Effect<
  CommitAppendFenceReferences,
  SDK.StateQueueError | SDK.LucidError | SDK.LinkedListError
> =>
  Effect.gen(function* () {
    const ordered = yield* localizeSdkEffect<
      SDK.StateQueueUTxO[],
      SDK.LucidError | SDK.LinkedListError
    >(SDK.fetchSortedStateQueueUTxOsProgram(lucid, fetchConfig));
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
 * Revalidates a known state-queue tail through its unique NFT instead of
 * scanning every UTxO at the state-queue address. Appending or merging can
 * recreate the same logical node at a new out-ref, so a replacement is still
 * decoded and checked as a tail rather than being rejected solely by out-ref.
 */
export const fetchExpectedStateQueueTailLocal = (
  lucid: Parameters<typeof SDK.fetchLatestCommittedBlockProgram>[0],
  fetchConfig: SDK.StateQueueFetchConfig,
  expectedTail: SDK.StateQueueUTxO,
): Effect.Effect<SDK.StateQueueUTxO, SDK.StateQueueError | SDK.LucidError> =>
  Effect.gen(function* () {
    const expectedUnit = toUnit(
      fetchConfig.stateQueuePolicyId,
      expectedTail.assetName,
    );
    const candidates = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(fetchConfig.stateQueueAddress, expectedUnit),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to fetch expected state-queue tail unit at: ${fetchConfig.stateQueueAddress}`,
          cause,
        }),
    });
    if (candidates.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `expected_unit=${expectedUnit},matches=0`,
        }),
      );
    }
    if (candidates.length !== 1) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Expected state-queue tail unit is not unique",
          cause: `unit=${expectedUnit},matches=${candidates.length.toString()}`,
        }),
      );
    }

    const candidate = candidates[0];
    if (
      candidate.txHash === expectedTail.utxo.txHash &&
      candidate.outputIndex === expectedTail.utxo.outputIndex
    ) {
      return expectedTail;
    }

    const replacement = yield* localizeSdkEffect<SDK.StateQueueUTxO, unknown>(
      SDK.utxoToStateQueueUTxO(candidate, fetchConfig.stateQueuePolicyId),
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: "Failed to authenticate replacement state-queue tail",
            cause,
          }),
      ),
    );
    if (replacement.datum.next !== "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Commit base is stale; aborting block build before creating a pending journal",
          cause: `unit=${expectedUnit},outref=${stateQueueOutRef(replacement)}`,
        }),
      );
    }
    return replacement;
  });

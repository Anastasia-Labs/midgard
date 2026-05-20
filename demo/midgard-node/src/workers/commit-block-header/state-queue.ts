import * as SDK from "@al-ft/midgard-sdk";
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
  endTime: bigint,
): Effect.Effect<
  { readonly nodeDatum: SDK.LinkedListNodeView; readonly header: SDK.Header },
  SDK.DataCoercionError | SDK.LucidError | SDK.HashingError
> =>
  localizeSdkEffect(
    SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      latestBlocksDatum,
      newUTxOsRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
      endTime,
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

export const fetchLatestCommittedBlockLocal = (
  lucid: Parameters<typeof SDK.fetchLatestCommittedBlockProgram>[0],
  fetchConfig: SDK.StateQueueFetchConfig,
): Effect.Effect<SDK.StateQueueUTxO, SDK.StateQueueError | SDK.LucidError> =>
  localizeSdkEffect(SDK.fetchLatestCommittedBlockProgram(lucid, fetchConfig));

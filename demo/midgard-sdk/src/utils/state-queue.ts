import { ConfirmedState, Header } from "../tx-builder/ledger-state.js";
import {
  Data,
  LucidEvolution,
  UTxO,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { NodeKey } from "../tx-builder/linked-list.js";
import { Effect, Option } from "effect";
import { StateQueue } from "../tx-builder/index.js";
import { getNodeDatumFromUTxO } from "./linked-list.js";
import { MerkleRoot, POSIXTime } from "../tx-builder/common.js";
import { Datum } from "@/tx-builder/state-queue/types.js";
import { getSingleAssetApartFromAda } from "./common.js";
import { getHeaderFromStateQueueDatum, hashHeader } from "./ledger-state.js";

type StateQueueUTxO = StateQueue.StateQueueUTxO;

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToStateQueueUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<StateQueueUTxO, Error> =>
  Effect.gen(function* () {
    const datum = yield* getNodeDatumFromUTxO(utxo);
    const [sym, assetName, _qty] = yield* getSingleAssetApartFromAda(
      utxo.assets,
    );
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new Error("UTxO's NFT policy ID is not the same as the state queue's"),
      );
    }
    return { utxo, datum, assetName };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToStateQueueUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<StateQueueUTxO[], Error> => {
  const effects = utxos.map((u) => utxoToStateQueueUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

/**
 * Given a StateQueue datum, this function confirmes the node is root
 * (i.e. no keys in its datum), and attempts to coerce its underlying data into
 * a `ConfirmedState`.
 */
export const getConfirmedStateFromStateQueueDatum = (
  nodeDatum: Datum,
): Effect.Effect<{ data: ConfirmedState; link: NodeKey }, Error> => {
  try {
    if (nodeDatum.key === "Empty") {
      const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
      return Effect.succeed({
        data: confirmedState,
        link: nodeDatum.next,
      });
    } else {
      return Effect.fail(new Error("Given UTxO is not root"));
    }
  } catch {
    return Effect.fail(new Error("Could not coerce to a node datum"));
  }
};

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlocksDatum - Datum of the UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeader = (
  lucid: LucidEvolution,
  latestBlocksDatum: Datum,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRootOption: Option.Option<MerkleRoot>,
  withdrawalsRootOption: Option.Option<MerkleRoot>,
  endTime: POSIXTime,
): Effect.Effect<{ nodeDatum: Datum; header: Header }, Error> =>
  Effect.gen(function* () {
    const walletAddress : string = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) => new Error(`Failed to find the wallet: ${e}`),
    });

    const pubKeyHash = paymentCredentialOf(walletAddress).hash;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      return {
        nodeDatum: {
          ...latestBlocksDatum,
          next: { Key: { key: confirmedState.headerHash } },
        },
        header: {
          prevUtxosRoot: confirmedState.utxoRoot,
          utxosRoot: newUTxOsRoot,
          transactionsRoot,
          depositsRoot: Option.getOrElse(depositsRootOption, () => "00".repeat(32)),
          withdrawalsRoot: Option.getOrElse(withdrawalsRootOption, () => "00".repeat(32)),
          startTime: confirmedState.endTime,
          endTime,
          prevHeaderHash: confirmedState.headerHash,
          operatorVkey: pubKeyHash,
          protocolVersion: confirmedState.protocolVersion,
        },
      };
    } else {
      const latestHeader =
        yield* getHeaderFromStateQueueDatum(latestBlocksDatum);
      const prevHeaderHash = yield* hashHeader(latestHeader);
      return {
        nodeDatum: {
          ...latestBlocksDatum,
          next: { Key: { key: prevHeaderHash } },
        },
        header: {
          ...latestHeader,
          prevUtxosRoot: latestHeader.utxosRoot,
          utxosRoot: newUTxOsRoot,
          transactionsRoot,
          startTime: latestHeader.endTime,
          endTime,
          prevHeaderHash,
          operatorVkey: pubKeyHash,
        },
      };
    }
  });

export const findLinkStateQueueUTxO = (
  link: NodeKey,
  utxos: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO, Error> => {
  if (link === "Empty") {
    return Effect.fail(new Error('Given link is "Empty"'));
  } else {
    const foundLink = utxos.find(
      (u: StateQueueUTxO) =>
        u.datum.key !== "Empty" && u.datum.key.Key.key === link.Key.key,
    );
    if (foundLink) {
      return Effect.succeed(foundLink);
    } else {
      return Effect.fail(new Error("Link not found"));
    }
  }
};

/**
 * Returns a sorted array of `StateQueueUTxO`s where the confirmed state's UTxO
 * is the head element, and the following elements are linked from their
 * previous elements.
 *
 * TODO: Make it more efficient. Currently that same list of all state queue
 *       UTxOs is traversed to find the next link UTxO multiple times. It might
 *       be better to drop link UTxOs when found so that subsequent lookups
 *       become cheaper.
 */
export const sortStateQueueUTxOs = (
  stateQueueUTxOs: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO[], Error> =>
  Effect.gen(function* () {
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      stateQueueUTxOs.map((u) =>
        Effect.gen(function* () {
          const dataAndLink = yield* getConfirmedStateFromStateQueueDatum(
            u.datum,
          );
          return { ...dataAndLink, utxo: u };
        }),
      ),
    );
    if (filteredForConfirmedState.length === 1) {
      const { utxo: confirmedStateUTxO, link: linkToOldestBlock } =
        filteredForConfirmedState[0];
      const sorted: StateQueueUTxO[] = [confirmedStateUTxO];
      let link = linkToOldestBlock;
      while (link !== "Empty") {
        const linkUTxO = yield* findLinkStateQueueUTxO(link, stateQueueUTxOs);
        sorted.push(linkUTxO);
        link = linkUTxO.datum.next;
      }
      return sorted;
    } else {
      yield* Effect.fail(
        new Error("Confirmed state not found among state queue UTxOs."),
      );
      return [];
    }
  });

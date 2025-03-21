import { ConfirmedState, Header } from "../tx-builder/ledger-state.js";
import {
  Data,
  LucidEvolution,
  UTxO,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { NodeKey } from "../tx-builder/linked-list.js";
import { Effect } from "effect";
import { StateQueue } from "../tx-builder/index.js";
import { getNodeDatumFromUTxO } from "./linked-list.js";
import { MerkleRoot, POSIXTime } from "../tx-builder/common.js";
import { Datum } from "@/tx-builder/state-queue/types.js";
import { getHeaderFromBlockUTxO, hashHeader } from "./ledger-state.js";

export const getLinkFromBlockUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<NodeKey, Error> => {
  const nodeDatum = getNodeDatumFromUTxO(blockUTxO);
  return Effect.map(nodeDatum, (nd) => nd.next);
};

/**
 * Given a block UTxO, this function confirmes the node is root (i.e. no keys
 * in its datum), and attempts to coerce its underlying data into a
 * `ConfirmedState`.
 */
export const getConfirmedStateFromUTxO = (
  blockUTxO: UTxO,
): Effect.Effect<{ data: ConfirmedState; link: NodeKey }, Error> => {
  const datumCBOR = blockUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, StateQueue.Datum);
      if (nodeDatum.key === "Empty") {
        const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
        return Effect.succeed({ data: confirmedState, link: nodeDatum.next });
      } else {
        return Effect.fail(new Error("Given UTxO is not root"));
      }
    } catch {
      return Effect.fail(new Error("Could not coerce to a node datum"));
    }
  } else {
    return Effect.fail(new Error("No datum found"));
  }
};

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlock - UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeader = (
  lucid: LucidEvolution,
  latestBlock: UTxO,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  endTime: POSIXTime,
): Effect.Effect<{ nodeDatum: Datum; header: Header }, Error> =>
  Effect.gen(function* () {
    // const latestBlock = yield* fetchLatestCommittedBlockProgram(lucid, config);
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) => new Error(`Failed to find the wallet: ${e}`),
    });

    const pubKeyHash = paymentCredentialOf(walletAddress).hash;

    const latestNodeDatum = yield* getNodeDatumFromUTxO(latestBlock);

    if (latestNodeDatum.key === "Empty") {
      const confirmedState = yield* getConfirmedStateFromUTxO(latestBlock);
      return {
        nodeDatum: {
          ...latestNodeDatum,
          next: { Key: { key: confirmedState.data.headerHash } },
        },
        header: {
          prevUtxosRoot: confirmedState.data.utxoRoot,
          utxosRoot: newUTxOsRoot,
          transactionsRoot,
          depositsRoot: "00".repeat(32),
          withdrawalsRoot: "00".repeat(32),
          startTime: confirmedState.data.endTime,
          endTime,
          prevHeaderHash: confirmedState.data.headerHash,
          operatorVkey: pubKeyHash,
          protocolVersion: confirmedState.data.protocolVersion,
        },
      };
    } else {
      const latestHeader = yield* getHeaderFromBlockUTxO(latestBlock);
      const prevHeaderHash = yield* hashHeader(latestHeader);
      return {
        nodeDatum: {
          ...latestNodeDatum,
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

export const getLatestBlocksUtxosRoot = (
  latestBlock: UTxO,
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const latestNodeDatum = yield* getNodeDatumFromUTxO(latestBlock);
    if (latestNodeDatum.key === "Empty") {
      const confirmedState = yield* getConfirmedStateFromUTxO(latestBlock);
      return confirmedState.data.utxoRoot;
    } else {
      const latestHeader = yield* getHeaderFromBlockUTxO(latestBlock);
      return latestHeader.utxosRoot;
    }
  });

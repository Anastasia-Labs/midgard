import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  fromText,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { CommitBlockParams, FetchConfig } from "./types.js";
import { fetchLatestCommitedBlockProgram } from "../../endpoints/state-queue/fetch-latest-block.js";
import { Header } from "../ledger-state.js";
import {
  getHeaderFromBlockUTxO,
  hashHeader,
} from "../../utils/ledger-state.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { getConfirmedStateFromUTxO } from "@/utils/state-queue.js";
import { NodeDatum } from "../linked-list.js";

/**
 * Builds portions of a tx required for submitting a new block, using the
 * provided `LucidEvolution` instance, fetch config, and required parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param commitParams - Parameters required for committing to state queue.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const commitTxBuilder = (
  lucid: LucidEvolution,
  config: FetchConfig,
  {
    newUTxOsRoot,
    transactionsRoot,
    endTime,
    stateQueueSpendingScript,
    policyId,
    stateQueueMintingScript,
  }: CommitBlockParams,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) => new Error(`Failed to find the wallet: ${e}`),
    });
    const pubkeyHash = paymentCredentialOf(walletAddress).hash;
    const latestBlock = yield* fetchLatestCommitedBlockProgram(lucid, config);
    const latestNodeDatum = yield* getNodeDatumFromUTxO(latestBlock);
    const makeNewHeaderProgram: () => Effect.Effect<
      { nodeDatum: NodeDatum; header: Header },
      Error
    > = () => {
      if (latestNodeDatum.key === "Empty") {
        const confirmedStateProgram = getConfirmedStateFromUTxO(latestBlock);
        return Effect.map(confirmedStateProgram, (confirmedState) => {
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
              operatorVkey: pubkeyHash,
              protocolVersion: confirmedState.data.protocolVersion,
            },
          };
        });
      } else {
        const latestHeaderProgram = getHeaderFromBlockUTxO(latestBlock);
        return Effect.andThen(latestHeaderProgram, (latestHeader) => {
          const prevHeaderHashProgram = hashHeader(latestHeader);
          return Effect.map(prevHeaderHashProgram, (prevHeaderHash) => {
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
              },
            };
          });
        });
      }
    };
    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      yield* makeNewHeaderProgram();
    // console.log("newHeader :>> ", newHeader);
    const assets: Assets = {
      [toUnit(policyId, fromText("Node") + newHeader.prevHeaderHash)]: 1n,
    };
    const newNodeDatum: NodeDatum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: Data.castTo(newHeader, Header),
    };
    const tx = lucid
      .newTx()
      // .validFrom(Number(newHeader.startTime))
      // .validTo(Number(endTime))
      .collectFrom([latestBlock], "d87980") // TODO: Placeholder redeemer.
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newNodeDatum, NodeDatum) },
        assets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(updatedNodeDatum, NodeDatum) },
        latestBlock.assets,
      )
      .mintAssets(assets, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

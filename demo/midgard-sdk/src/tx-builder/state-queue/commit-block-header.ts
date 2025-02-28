import {
  Data,
  LucidEvolution,
  TxBuilder,
  paymentCredentialOf,
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
    const makeNewHeaderProgram: () => Effect.Effect<Header, Error> = () => {
      if (latestNodeDatum.key === "Empty") {
        const confirmedStateProgram = getConfirmedStateFromUTxO(latestBlock);
        return Effect.map(confirmedStateProgram, (confirmedState) => ({
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
        }));
      } else {
        const latestHeaderProgram = getHeaderFromBlockUTxO(latestBlock);
        return Effect.andThen(latestHeaderProgram, (latestHeader) => {
          const prevHeaderHashProgram = hashHeader(latestHeader);
          return Effect.map(prevHeaderHashProgram, (prevHeaderHash) => ({
            ...latestHeader,
            prevUtxosRoot: latestHeader.utxosRoot,
            utxosRoot: newUTxOsRoot,
            transactionsRoot,
            startTime: latestHeader.endTime,
            endTime,
            prevHeaderHash,
          }));
        });
      }
    };
    const newHeader: Header = yield* makeNewHeaderProgram();
    const tx = lucid
      .newTx()
      .validFrom(Number(newHeader.startTime))
      .validTo(Number(endTime))
      .collectFrom([latestBlock], "d87980") // TODO: Placeholder redeemer.
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newHeader, Header) },
        latestBlock.assets,
      )
      .attach.Script(stateQueueSpendingScript);
    return tx;
  });

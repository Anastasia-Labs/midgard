import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { CommitBlockParams, FetchConfig } from "@/types/state-queue.js";
import { fetchLatestCommitedBlockProgram } from "@/endpoints/state-queue/fetch-latest-block.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { hashHeader } from "@/utils/ledger-state.js";

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
  { newUTxOsRoot, transactionsRoot, endTime }: CommitBlockParams,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const latestBlock = yield* fetchLatestCommitedBlockProgram(lucid, config);
    const latestNodeDatum = yield* getNodeDatumFromUTxO(latestBlock);
    const latestHeaderProgram: Effect.Effect<Header, Error> = Effect.try({
      try: () => Data.castFrom(latestNodeDatum.data, Header),
      catch: (e) => new Error(`Failed coercing latest block's datum: ${e}`),
    });
    const latestHeader: Header = yield* latestHeaderProgram;
    const prevHeaderHash = yield* hashHeader(latestHeader);
    const newHeader = {
      ...latestHeader,
      prevUtxosRoot: latestHeader.utxosRoot,
      utxosRoot: newUTxOsRoot,
      transactionsRoot,
      startTime: latestHeader.endTime,
      endTime,
      prevHeaderHash,
    };
    const tx = lucid
      .newTx()
      .validFrom(Number(latestHeader.endTime))
      .validTo(Number(endTime))
      .collectFrom([latestBlock], "d87980") // TODO: Placeholder redeemer.
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newHeader, Header) },
        latestBlock.assets,
      );
    return tx;
  });

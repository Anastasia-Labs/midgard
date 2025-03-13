import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  fromText,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { CommitBlockParams, Datum, FetchConfig } from "./types.js";
import { Header } from "../ledger-state.js";
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
  {
    anchorUTxO: latestBlock,
    updatedAnchorDatum: updatedNodeDatum,
    newHeader,
    stateQueueSpendingScript,
    policyId,
    stateQueueMintingScript,
  }: CommitBlockParams,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const newHeaderHash = yield* hashHeader(newHeader);

    const assets: Assets = {
      [toUnit(policyId, fromText("Node") + newHeaderHash)]: 1n,
    };

    const newNodeDatum: Datum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: Data.castTo(newHeader, Header),
    };

    const tx = lucid
      .newTx()
      // .validFrom(Number(newHeader.startTime))
      // .validTo(Number(endTime))
      .collectFrom([latestBlock], Data.void())
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newNodeDatum, Datum) },
        assets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(updatedNodeDatum, Datum) },
        latestBlock.assets,
      )
      .mintAssets(assets, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

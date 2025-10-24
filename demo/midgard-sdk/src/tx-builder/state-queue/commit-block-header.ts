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
import { HashingError } from "@/utils/common.js";

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
): Effect.Effect<TxBuilder, HashingError> =>
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

    // Add 1 minute
    const endTime = Date.now();
    const endTimePlusOneMinute = endTime + 60000;
    const tx = lucid
      .newTx()
      .validTo(endTimePlusOneMinute)
      .collectFrom([latestBlock.utxo], Data.void())
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newNodeDatum, Datum) },
        assets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(updatedNodeDatum, Datum) },
        latestBlock.utxo.assets,
      )
      .mintAssets(assets, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

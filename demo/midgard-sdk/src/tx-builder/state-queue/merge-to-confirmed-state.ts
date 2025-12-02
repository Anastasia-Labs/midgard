import { Effect } from "effect";
import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  toUnit,
} from "@lucid-evolution/lucid";
import { ConfirmedState, Header } from "../ledger-state.js";
import {
  getHeaderFromStateQueueDatum,
  hashHeader,
} from "../../utils/ledger-state.js";
import { NodeDatum } from "../linked-list.js";
import { Redeemer, FetchConfig, MergeParams } from "./types.js";
import { getConfirmedStateFromStateQueueDatum } from "@/utils/state-queue.js";
import { DataCoercionError, HashingError } from "@/utils/common.js";

/**
 * Merge
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param mergeParams - Parameters needed for building the merge transaction.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const mergeTxBuilder = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
  {
    confirmedUTxO,
    firstBlockUTxO,
    stateQueueSpendingScript,
    stateQueueMintingScript,
  }: MergeParams,
): Effect.Effect<TxBuilder, HashingError | DataCoercionError> =>
  Effect.gen(function* () {
    const { data: currentConfirmedState } =
      yield* getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum);
    const blockHeader: Header = yield* getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash = yield* hashHeader(blockHeader);
    const newConfirmedState = {
      ...currentConfirmedState,
      headerHash,
      prevHeaderHash: currentConfirmedState.headerHash,
      utxoRoot: blockHeader.utxosRoot,
      startTime: currentConfirmedState.endTime,
      endTime: blockHeader.endTime,
    };
    const newConfirmedNodeDatum: NodeDatum = {
      ...confirmedUTxO.datum,
      data: Data.castTo(newConfirmedState, ConfirmedState),
      next: firstBlockUTxO.datum.next,
    };
    const assetsToBurn: Assets = {
      [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
    };
    const tx = lucid
      .newTx()
      .collectFrom(
        [confirmedUTxO.utxo, firstBlockUTxO.utxo],
        Data.to("MergeToConfirmedState", Redeemer),
      )
      .pay.ToContract(
        fetchConfig.stateQueueAddress,
        {
          kind: "inline",
          value: Data.to(newConfirmedNodeDatum, NodeDatum),
        },
        confirmedUTxO.utxo.assets,
      )
      .mintAssets(assetsToBurn, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

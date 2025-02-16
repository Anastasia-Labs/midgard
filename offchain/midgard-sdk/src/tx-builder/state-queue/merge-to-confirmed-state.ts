import { Effect } from "effect";
import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  toUnit,
} from "@lucid-evolution/lucid";
import { fetchConfirmedStateAndItsLinkProgram } from "@/endpoints/state-queue/fetch-confirmed-state-and-its-link.js";
import { FetchConfig } from "@/types/state-queue.js";
import { getSingleAssetApartFromAda } from "@/utils/common.js";
import { ConfirmedState, Header } from "@/types/contracts/ledger-state.js";
import { getNodeDatumFromUTxO } from "@/utils/linked-list.js";
import { hashHeader } from "@/utils/ledger-state.js";
import { NodeDatum } from "@/types/contracts/linked-list/index.js";
import { Redeemer } from "@/types/contracts/state-queue.js";

/**
 * Merge
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const mergeTxBuilder = (
  lucid: LucidEvolution,
  fetchConfig: FetchConfig,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig);
    if (!firstBlockUTxO) {
      return yield* Effect.fail(new Error("No blocks in queue"));
    } else {
      const confirmedNode = yield* getNodeDatumFromUTxO(confirmedUTxO);
      const blockNodeDatum = yield* getNodeDatumFromUTxO(firstBlockUTxO);
      const currentConfirmedNodeDatum = confirmedNode;
      const currentConfirmedState: ConfirmedState = yield* Effect.try({
        try: () =>
          Data.castFrom(currentConfirmedNodeDatum.data, ConfirmedState),
        catch: (e) => new Error(`${e}`),
      });
      const blockHeader: Header = yield* Effect.try({
        try: () => Data.castFrom(blockNodeDatum.data, Header),
        catch: (e) => new Error(`${e}`),
      });
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
        ...currentConfirmedNodeDatum,
        data: Data.castTo(newConfirmedState, ConfirmedState),
      };
      const [nftSym, nftName, _nftQty] = yield* getSingleAssetApartFromAda(
        firstBlockUTxO.assets,
      );
      const assetsToBurn: Assets = {
        [toUnit(nftSym, nftName)]: -1n,
      };
      const tx = lucid
        .newTx()
        .collectFrom(
          [confirmedUTxO, firstBlockUTxO],
          Data.to("MergeToConfirmedState", Redeemer),
        )
        .pay.ToContract(
          fetchConfig.stateQueueAddress,
          {
            kind: "inline",
            value: Data.to(newConfirmedNodeDatum, NodeDatum),
          },
          confirmedUTxO.assets,
        )
        .mintAssets(assetsToBurn);
      return tx;
    }
  });

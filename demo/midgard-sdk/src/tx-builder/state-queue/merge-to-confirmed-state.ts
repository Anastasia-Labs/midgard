import { Effect } from "effect";
import {
  Assets,
  Data,
  LucidEvolution,
  TxBuilder,
  toUnit,
} from "@lucid-evolution/lucid";
import { fetchConfirmedStateAndItsLinkProgram } from "../../endpoints/state-queue/fetch-confirmed-state-and-its-link.js";
import { getSingleAssetApartFromAda } from "../../utils/common.js";
import { ConfirmedState, Header } from "../ledger-state.js";
import { getNodeDatumFromUTxO } from "../../utils/linked-list.js";
import {
  getHeaderFromBlockUTxO,
  hashHeader,
} from "../../utils/ledger-state.js";
import { NodeDatum } from "../linked-list.js";
import { Redeemer, FetchConfig, MergeParams } from "./types.js";

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
  { stateQueueSpendingScript, stateQueueMintingScript }: MergeParams,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const { confirmed: confirmedUTxO, link: firstBlockUTxO } =
      yield* fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig);
    // console.log("firstBlockUTxO :>> ", firstBlockUTxO);
    if (!firstBlockUTxO) {
      return yield* Effect.fail(new Error("No blocks in queue"));
    } else {
      const confirmedNode = yield* getNodeDatumFromUTxO(confirmedUTxO);
      const currentConfirmedNodeDatum = confirmedNode;
      const currentConfirmedState: ConfirmedState = yield* Effect.try({
        try: () =>
          Data.castFrom(currentConfirmedNodeDatum.data, ConfirmedState),
        catch: (e) => new Error(`${e}`),
      });
      const blockNode: NodeDatum = yield* getNodeDatumFromUTxO(firstBlockUTxO);
      // console.log("blockNode :>> ", blockNode);
      const blockHeader: Header = yield* getHeaderFromBlockUTxO(firstBlockUTxO);
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
        next: blockNode.next,
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
        .mintAssets(assetsToBurn, Data.void())
        .attach.Script(stateQueueSpendingScript)
        .attach.Script(stateQueueMintingScript);
      return tx;
    }
  });

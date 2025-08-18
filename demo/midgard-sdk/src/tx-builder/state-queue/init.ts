import { ConfirmedState } from "../ledger-state.js";
import { NodeDatum } from "../linked-list.js";
import { InitParams } from "./types.js";
import {
  LucidEvolution,
  TxBuilder,
  Assets,
  toUnit,
  Data,
  OutputDatum,
  fromText,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const initTxBuilder = (
  lucid: LucidEvolution,
  { policyId, address, stateQueueMintingScript }: InitParams,
): Effect.Effect<TxBuilder, Error> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(policyId, fromText("Node"))]: 1n,
    };

    const confirmedState: ConfirmedState = {
      headerHash: "00".repeat(28),
      prevHeaderHash: "00".repeat(28),
      utxoRoot: "00".repeat(32),
      startTime: BigInt(Date.now()),
      endTime: BigInt(Date.now()),
      protocolVersion: 0n,
    };
    const datum: NodeDatum = {
      key: "Empty",
      next: "Empty",
      data: Data.castTo(confirmedState, ConfirmedState),
    };

    const outputDatum: OutputDatum = {
      kind: "inline",
      value: Data.to(datum, NodeDatum),
    };

    const tx = lucid
      .newTx()
      .mintAssets(assets, Data.void())
      .pay.ToAddressWithData(address, outputDatum, assets)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

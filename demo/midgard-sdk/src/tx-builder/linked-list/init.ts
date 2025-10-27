import { Effect } from "effect";
import {
  LucidEvolution,
  TxBuilder,
  Assets,
  toUnit,
  Data,
  fromText,
  Script,
} from "@lucid-evolution/lucid";
import { NodeDatum } from "./linked-list.js";

export type LinkedListInitParams = {
  policyId: string;
  address: string;
  mintScript: Script;
  data: any;
};

export const initLinkedListTxBuilder = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.policyId, fromText("Node"))]: 1n,
    };

    const nodeDatum: NodeDatum = {
      key: "Empty",
      next: "Empty",
      data: params.data,
    };

    const encodedDatum = Data.to<NodeDatum>(nodeDatum, NodeDatum);

    const tx = lucid
      .newTx()
      .mintAssets(assets, Data.to("Init"))
      .pay.ToAddressWithData(
        params.address,
        { kind: "inline", value: encodedDatum },
        assets,
      )
      .attach.Script(params.mintScript);

    return tx;
  });

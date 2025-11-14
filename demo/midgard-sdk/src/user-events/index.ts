export * from "./deposit.js";
export * from "./tx-order.js";
export * from "./withdrawal.js";

import { 
    Assets, 
    Data, 
    LucidEvolution, 
    MintingPolicy, 
    TxBuilder, 
    UTxO 
} from "@lucid-evolution/lucid";

export const UserEventMintRedeemerSchema = Data.Enum([
  Data.Object({
    AuthenticateEvent: Data.Object({
      nonceInputIndex: Data.Integer(),
      eventOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      witnessRegistrationRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnEventNFT: Data.Object({
      nonceAssetName: Data.Bytes(),
      witnessUnregistrationRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type UserEventMintRedeemer = Data.Static<typeof UserEventMintRedeemerSchema>;
export const UserEventMintRedeemer = UserEventMintRedeemerSchema as unknown as UserEventMintRedeemer;

export type MintTransactionParams ={
  lucid: LucidEvolution;
  inputUtxo: UTxO;
  nft: string;
  mintRedeemer: string;
  scriptAddress: string;
  datum: string;
  assets: Assets;
  validTo: number;
  mintingPolicy: MintingPolicy;
}

export function buildMintTransaction(params: MintTransactionParams): TxBuilder {
  const {
    lucid,
    inputUtxo,
    nft,
    mintRedeemer,
    scriptAddress,
    datum,
    assets,
    validTo,
    mintingPolicy
  } = params;

  return lucid
    .newTx()
    .collectFrom([inputUtxo])
    .mintAssets(
        {
            [nft]: 1n 
        },
        mintRedeemer
    )
    .pay.ToAddressWithData(
      scriptAddress,
      { 
        kind: "inline", 
        value: datum 
     },
     assets
    )
    .validTo(validTo)
    .attach.MintingPolicy(mintingPolicy);
}
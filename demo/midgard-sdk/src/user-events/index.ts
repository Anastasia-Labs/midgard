import { 
    Assets, 
    Data, 
    LucidEvolution, 
    MintingPolicy, 
    TxBuilder, 
    UTxO 
} from "@lucid-evolution/lucid";
import { Int } from "effect/Schema";

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

export type UserEventMintTransactionParams ={
  lucid: LucidEvolution;
  inputUtxo: UTxO;
  nft: string;
  mintRedeemer: string;
  scriptAddress: string;
  datum: string;
  extraAssets?: Assets;
  validTo: number | bigint;
  mintingPolicy: MintingPolicy;
}

export const buildUserEventMintTransaction = (params: UserEventMintTransactionParams): TxBuilder => {
  const {
    lucid,
    inputUtxo,
    nft,
    mintRedeemer,
    scriptAddress,
    datum,
    extraAssets,
    validTo,
    mintingPolicy
  } = params;

  const validToNumber = typeof validTo === 'bigint' ? Number(validTo) :
                       validTo;

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
     { 
        [nft]:1n,
        ...extraAssets
     },
    )
    .validTo(validToNumber)
    .attach.MintingPolicy(mintingPolicy);
}

export * from "./deposit.js";
export * from "./tx-order.js";
export * from "./withdrawal.js";
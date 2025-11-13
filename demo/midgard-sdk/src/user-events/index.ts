import { Assets, Data, LucidEvolution, MintingPolicy, TxBuilder, UTxO } from "@lucid-evolution/lucid";

export * from "./deposit.js";
export * from "./tx-order.js";
export * from "./withdrawal.js";

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

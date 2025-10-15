import { Address, PolicyId, Script, Data, UTxO } from "@lucid-evolution/lucid";
import { OutputReferenceSchema, POSIXTimeSchema } from "@/tx-builder/common.js";
import { NodeDatumSchema } from "@/tx-builder/linked-list.js";
import { Header } from "@/tx-builder/ledger-state.js";

export const ConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type Config = Data.Static<typeof ConfigSchema>;
export const Config = ConfigSchema as unknown as Config;

export const RedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    CommitBlockHeader: Data.Object({ operator: Data.Bytes() }),
  }),
  Data.Literal("MergeToConfirmedState"),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulentOperator: Data.Bytes(),
    }),
  }),
]);
export type Redeemer = Data.Static<typeof RedeemerSchema>;
export const Redeemer = RedeemerSchema as unknown as Redeemer;

export type Datum = Data.Static<typeof NodeDatumSchema>;
export const Datum = NodeDatumSchema as unknown as Datum;

export type StateQueueUTxO = {
  utxo: UTxO;
  datum: Datum;
  assetName: string;
};

export type FetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

export type CommitBlockParams = {
  anchorUTxO: StateQueueUTxO;
  updatedAnchorDatum: Datum;
  newHeader: Header;
  stateQueueSpendingScript: Script;
  policyId: PolicyId;
  stateQueueMintingScript: Script;
};

export type MergeParams = {
  confirmedUTxO: StateQueueUTxO;
  firstBlockUTxO: StateQueueUTxO;
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
};

export type InitParams = {
  policyId: PolicyId;
  address: Address;
  stateQueueMintingScript: Script;
};

import { Address, PolicyId, Script, Data } from "@lucid-evolution/lucid";
import {
  MerkleRoot,
  OutputReferenceSchema,
  POSIXTime,
  POSIXTimeSchema,
} from "../common.js";
import { NodeDatumSchema } from "../linked-list.js";

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
      fraudulent_operator: Data.Bytes(),
    }),
  }),
]);
export type Redeemer = Data.Static<typeof RedeemerSchema>;
export const Redeemer = RedeemerSchema as unknown as Redeemer;

export type Datum = Data.Static<typeof NodeDatumSchema>;
export const Datum = NodeDatumSchema as unknown as Datum;

export type FetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

export type CommitBlockParams = {
  newUTxOsRoot: MerkleRoot;
  transactionsRoot: MerkleRoot;
  endTime: POSIXTime;
  stateQueueSpendingScript: Script;
  policyId: PolicyId;
  stateQueueMintingScript: Script;
};

export type MergeParams = {
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
};

export type InitParams = {
  policyId: PolicyId;
  address: Address;
  stateQueueMintingScript: Script;
};

import { Data } from "@lucid-evolution/lucid";
import { POSIXTimeSchema } from "../common.js";

export const DatumSchema = Data.Object({
  registrationTime: POSIXTimeSchema,
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const WitnessStatusSchema = Data.Enum([
  Data.Literal("Registered"),
  Data.Literal("Active"),
  Data.Literal("Retired"),
]);
export type WitnessStatus = Data.Static<typeof WitnessStatusSchema>;
export const WitnessStatus = WitnessStatusSchema as unknown as WitnessStatus;

export const MintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    Register: Data.Object({
      keyToPrepend: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorRefInputIndex: Data.Integer(),
      activeOperatorAssetName: Data.Bytes(),
      retiredOperatorRefInputIndex: Data.Integer(),
      retiredOperatorAssetName: Data.Bytes(),
      prependedNodeOutputIndex: Data.Integer(),
      anchorNodeOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Activate: Data.Object({
      nodeToActivateKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorRefInputIndex: Data.Integer(),
      retiredOperatorAsset_name: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
      activeOperatorsInsertedNodeOutputIndex: Data.Integer(),
      activeOperatorsAnchorNodeOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Deregister: Data.Object({
      nodeToDeregisterKey: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveDuplicateSlashBond: Data.Object({
      duplicateNodeKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      duplicateNodeRefInputIndex: Data.Integer(),
      duplicateNodeRefInputAssetName: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
      witnessStatus: WitnessStatusSchema,
    }),
  }),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

import { Data } from "@lucid-evolution/lucid";
import { POSIXTimeSchema } from "./common.js";

export const DatumSchema = Data.Object({
  commitmentTime: Data.Nullable(POSIXTimeSchema),
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const SpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateCommitmentTime: Data.Object({
      activeNodeInputIndex: Data.Integer(),
      prevActiveNodeRefInputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type SpendRedeemer = Data.Static<typeof SpendRedeemerSchema>;
export const SpendRedeemer = SpendRedeemerSchema as unknown as SpendRedeemer;

export const MintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    ActivateOperator: Data.Object({
      newActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorAppendedNodeOutputIndex: Data.Integer(),
      activeOperatorAnchorNodeOutputIndex: Data.Integer(),
      registeredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorSlashBond: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      activeOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorRemovedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      retiredOperatorInsertedNodeOutputIndex: Data.Integer(),
      retiredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

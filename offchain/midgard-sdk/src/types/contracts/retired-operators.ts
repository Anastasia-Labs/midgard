import { Data } from "@lucid-evolution/lucid";
import { POSIXTimeSchema } from "./common.js";

export const DatumSchema = Data.Object({
  commitmentTime: Data.Nullable(POSIXTimeSchema),
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export const MintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    RetireOperator: Data.Object({
      newRetireOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorAppendedNodeOutputIndex: Data.Integer(),
      retiredOperatorAnchorNodeOutputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RecoverOperatorBond: Data.Object({
      retiredOperatorKey: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorSlashBond: Data.Object({
      slashedRetiredOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorSlashedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorNodeInputIndex: Data.Integer(),
      state_queueRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

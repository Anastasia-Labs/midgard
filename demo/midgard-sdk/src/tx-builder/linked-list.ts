import { Data } from "@lucid-evolution/lucid";
import { ValueSchema } from "./common.js";

export const NodeKeySchema = Data.Enum([
  Data.Object({ Key: Data.Object({ key: Data.Bytes() }) }),
  Data.Literal("Empty"),
]);
export type NodeKey = Data.Static<typeof NodeKeySchema>;
export const NodeKey = NodeKeySchema as unknown as NodeKey;

export const NodeDatumSchema = Data.Object({
  key: NodeKeySchema,
  next: NodeKeySchema,
  data: Data.Any(),
});
export type NodeDatum = Data.Static<typeof NodeDatumSchema>;
export const NodeDatum = NodeDatumSchema as unknown as NodeDatum;

export const CommonSchema = Data.Object({
  ownCS: Data.Bytes(),
  mint: ValueSchema,
  nodeInputs: Data.Array(NodeKeySchema),
  nodeOutputs: Data.Array(NodeKeySchema),
});
export type Common = Data.Static<typeof CommonSchema>;
export const Common = CommonSchema as unknown as Common;

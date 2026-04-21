import { POSIXTimeSchema, VerificationKeyHashSchema } from "@/common.js";
import { Data } from "@lucid-evolution/lucid";
import { Element } from "@/linked-list.js";

export const ActiveOperatorNodeDataSchema = Data.Object({
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorNodeData = Data.Static<
  typeof ActiveOperatorNodeDataSchema
>;
export const ActiveOperatorNodeData =
  ActiveOperatorNodeDataSchema as unknown as ActiveOperatorNodeData;

export const RetiredOperatorNodeSchema = Data.Object({
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RetiredOperatorNodeData = Data.Static<
  typeof RetiredOperatorNodeSchema
>;
export const RetiredOperatorNodeData =
  RetiredOperatorNodeSchema as unknown as RetiredOperatorNodeData;

export const RegisteredNodeDataSchema = Data.Object({
  operator: VerificationKeyHashSchema,
});
export type RegisteredNodeData = Data.Static<typeof RegisteredNodeDataSchema>;
export const RegisteredNodeData =
  RegisteredNodeDataSchema as unknown as RegisteredNodeData;

export function getActiveOperatorNodeData(element: Element) {
  if ("Node" in element.data) {
    const node = Data.castFrom(element.data.Node, ActiveOperatorNodeDataSchema);
    return node.bond_unlock_time;
  }
  return null;
}

export function getRetiredOperatorNodeData(element: Element) {
  if ("Node" in element.data) {
    const node = Data.castFrom(element.data.Node, RetiredOperatorNodeSchema);
    return node.bond_unlock_time;
  }
  return null;
}

export function getRegisteredOperatorNodeData(element: Element) {
  if ("Node" in element.data) {
    const node = Data.castFrom(element.data.Node, RegisteredNodeDataSchema);
    return node.activation_time;
  }
  return null;
}

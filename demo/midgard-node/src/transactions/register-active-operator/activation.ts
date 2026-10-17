import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";

/**
 * Existing registered-operator liveness exception: only the earliest registration
 * can restore an empty active set. The activation transaction consumes this root,
 * so another activation invalidates the witness rather than weakening the check.
 * The caller authenticates the registered node against the registered policy.
 */
export const canActivateRegisteredOperatorImmediately = (
  activeRoot: Pick<SDK.NodeWithDatum, "utxo" | "datum">,
  registeredNode: SDK.LinkedListNodeView,
  activeOperators: Pick<
    SDK.MidgardValidators["activeOperators"],
    "policyId" | "spendingScriptAddress"
  >,
): boolean =>
  activeRoot.utxo.address === activeOperators.spendingScriptAddress &&
  activeRoot.utxo.assets[
    toUnit(activeOperators.policyId, SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME)
  ] === 1n &&
  activeRoot.datum.key === "Empty" &&
  activeRoot.datum.next === "Empty" &&
  registeredNode.key !== "Empty" &&
  registeredNode.next === "Empty";

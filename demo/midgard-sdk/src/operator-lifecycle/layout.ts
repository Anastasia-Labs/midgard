/**
 * Register/activate layout types and list-position helpers.
 */
import { compareHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@/operator-lifecycle/primitives.js";
import type { OutputReference } from "@/common.js";
import { type UTxO } from "@lucid-evolution/lucid";

export type ReferenceScriptPublication = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type NodeWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.LinkedListNodeView;
  readonly assetName: string;
};

export type RegisterRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly activeOperatorRefInputIndex: bigint;
  readonly retiredOperatorRefInputIndex: bigint;
  readonly prependedNodeOutputIndex: bigint;
  readonly anchorNodeOutputIndex: bigint;
};

export type ActivateRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly retiredOperatorRefInputIndex: bigint;
  readonly registeredOperatorsRedeemerIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly registeredOperatorsAnchorNodeInputOutRef: OutputReference;
  readonly registeredOperatorsAnchorNodeOutputIndex: bigint;
  readonly activeOperatorsInsertedNodeOutputIndex: bigint;
  readonly activeOperatorsAnchorNodeOutputIndex: bigint;
};

/**
 * Lexicographically compares two hex strings by byte value.
 */
const compareHash28 = (left: string, right: string): number =>
  compareHex(left, right, { byteLength: 28 });

/**
 * Returns whether an asset unit belongs to the given policy and has positive
 * quantity.
 */
const isPolicyAsset = (unit: string, quantity: bigint, policyId: string) =>
  unit !== "lovelace" && quantity > 0n && unit.startsWith(policyId);

/**
 * Formats a register-layout derivation for logs.
 */
export const registerLayoutToLogString = (
  layout: RegisterRedeemerLayout,
): string =>
  `hub_ref=${layout.hubOracleRefInputIndex.toString()},active_ref=${layout.activeOperatorRefInputIndex.toString()},retired_ref=${layout.retiredOperatorRefInputIndex.toString()},prepended_out=${layout.prependedNodeOutputIndex.toString()},anchor_out=${layout.anchorNodeOutputIndex.toString()}`;

/**
 * Formats an activate-layout derivation for logs.
 */
export const activateLayoutToLogString = (
  layout: ActivateRedeemerLayout,
): string =>
  [
    `hub_ref=${layout.hubOracleRefInputIndex.toString()}`,
    `retired_ref=${layout.retiredOperatorRefInputIndex.toString()}`,
    `registered_redeemer=${layout.registeredOperatorsRedeemerIndex.toString()}`,
    `active_redeemer=${layout.activeOperatorsRedeemerIndex.toString()}`,
    `registered_anchor_outref=${layout.registeredOperatorsAnchorNodeInputOutRef.transactionId}#${layout.registeredOperatorsAnchorNodeInputOutRef.outputIndex.toString()}`,
    `registered_anchor_out=${layout.registeredOperatorsAnchorNodeOutputIndex.toString()}`,
    `active_inserted_out=${layout.activeOperatorsInsertedNodeOutputIndex.toString()}`,
    `active_anchor_out=${layout.activeOperatorsAnchorNodeOutputIndex.toString()}`,
  ].join(",");

/**
 * Returns whether a node carries the provided key hash as its own key.
 */
export const nodeKeyEquals = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => node.key !== "Empty" && node.key.Key.key === keyHash;

/**
 * Returns whether a node points to the provided key hash via its `next` link.
 */
export const linkPointsTo = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => node.next !== "Empty" && node.next.Key.key === keyHash;

/**
 * Returns whether a node is the correct ordered "not member" witness for the
 * provided key hash.
 */
export const orderedNotMemberWitness = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => {
  const lowerBoundSatisfied =
    node.key === "Empty" || compareHash28(node.key.Key.key, keyHash) < 0;
  const upperBoundSatisfied =
    node.next === "Empty" || compareHash28(keyHash, node.next.Key.key) < 0;
  return lowerBoundSatisfied && upperBoundSatisfied;
};

/**
 * Returns whether a node is the correct append-anchor witness for the active
 * operators list.
 */
export const activeAppendAnchorWitness = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean =>
  node.next === "Empty" &&
  (node.key === "Empty" || compareHash28(node.key.Key.key, keyHash) < 0);

/**
 * Extracts the unique asset name minted under a given policy from an asset map.
 */
export const getAssetNameByPolicy = (
  assets: Readonly<Record<string, bigint>>,
  policyId: string,
): string | null => {
  const entries = Object.entries(assets).filter(([unit, quantity]) =>
    isPolicyAsset(unit, quantity, policyId),
  );
  if (entries.length !== 1) {
    return null;
  }
  return entries[0][0].slice(56);
};

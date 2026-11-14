/**
 * What the node's event projection (N1, plan §5.4, §5.5 P2/P4) reads from
 * the deployment: per event list, its policy, its list and retention
 * addresses, and its retirement observer. Plain JSON, so a soak plugin can
 * take it from `soak.json`.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";

import type { TrackedSet } from "../types.js";

export const EVENT_KINDS = ["deposit", "withdrawal"] as const;
export type EventKind = (typeof EVENT_KINDS)[number];

export type EventListConfig = Readonly<{
  kind: EventKind;
  /** The list's minting policy (56 hex): its tokens name the event keys. */
  policyId: string;
  /** The list's spending address, as raw address bytes (hex). */
  listAddress: string;
  /** The retention address holding external payloads (hex address bytes). */
  retentionAddress: string;
  /** The retirement observer's script hash (56 hex). */
  retirementScriptHash: string;
}>;

/** The slot clock that turns a slot into POSIX milliseconds. */
export type SlotTime = Readonly<{
  zeroTime: number;
  zeroSlot: number;
  slotLength: number;
}>;

export type EventProjectionConfig = Readonly<{
  /** The ledger network id the reward accounts carry (0 testnets, 1 mainnet). */
  networkId: number;
  lists: readonly EventListConfig[];
}>;

const addressHex = (bech32: string): string =>
  getAddressDetails(bech32).address.hex.toLowerCase();

/** The config of a deployment's two event lists. */
export const eventProjectionConfigFromContracts = (
  pair: SDK.EventHistoryContractPair,
  networkId: 0 | 1,
): EventProjectionConfig => ({
  networkId,
  lists: EVENT_KINDS.map((kind) => {
    const contracts = pair[kind];
    return {
      kind,
      policyId: contracts.list.policyId,
      listAddress: addressHex(contracts.list.spendingScriptAddress),
      retentionAddress: addressHex(contracts.retention.spendingScriptAddress),
      retirementScriptHash: contracts.retirement.withdrawalScriptHash,
    };
  }),
});

/** The follower tracked set the projection needs: list and retention outputs, list mints. */
export const eventTrackedSet = (config: EventProjectionConfig): TrackedSet => ({
  addresses: new Set(
    config.lists.flatMap((list) => [list.listAddress, list.retentionAddress]),
  ),
  paymentCredentials: new Set(),
  policies: new Set(config.lists.map((list) => list.policyId)),
});

/** POSIX milliseconds at the start of `slot`. */
export const slotToPosixMs = (slotTime: SlotTime, slot: number): number =>
  slotTime.zeroTime + (slot - slotTime.zeroSlot) * slotTime.slotLength;

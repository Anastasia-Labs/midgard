/**
 * What the node's event projection (N1, plan §5.4, §5.5 P2/P4) reads from
 * the deployment: per event list, its policy, its list and retention
 * addresses, and its retirement observer. Plain JSON, so a soak plugin can
 * take it from `soak.json`.
 */
import type { TrackedSet } from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";

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

export class EventProjectionConfigError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "EventProjectionConfigError";
  }
}

const HEX = /^(?:[0-9a-f]{2})+$/u;

const hexOf = (value: unknown, what: string, bytes?: number): string => {
  if (
    typeof value !== "string" ||
    !HEX.test(value) ||
    (bytes !== undefined && value.length !== bytes * 2)
  )
    throw new EventProjectionConfigError(
      `${what} must be ${bytes === undefined ? "" : `${bytes} bytes of `}lowercase hex`,
    );
  return value;
};

const field = (value: unknown, name: string): unknown =>
  typeof value === "object" && value !== null
    ? (value as Record<string, unknown>)[name]
    : undefined;

/** Reads and checks a config given as JSON (the soak plugin's options). */
export const parseEventProjectionConfig = (
  value: unknown,
): EventProjectionConfig => {
  const networkId = field(value, "networkId");
  if (networkId !== 0 && networkId !== 1)
    throw new EventProjectionConfigError("networkId must be 0 or 1");
  const lists = field(value, "lists");
  if (!Array.isArray(lists) || lists.length === 0)
    throw new EventProjectionConfigError("lists must be a non-empty array");
  const parsed = lists.map((list: unknown, index): EventListConfig => {
    const kind = field(list, "kind");
    if (kind !== "deposit" && kind !== "withdrawal")
      throw new EventProjectionConfigError(
        `lists[${index}].kind must be deposit or withdrawal`,
      );
    return {
      kind,
      policyId: hexOf(field(list, "policyId"), `lists[${index}].policyId`, 28),
      listAddress: hexOf(
        field(list, "listAddress"),
        `lists[${index}].listAddress`,
      ),
      retentionAddress: hexOf(
        field(list, "retentionAddress"),
        `lists[${index}].retentionAddress`,
      ),
      retirementScriptHash: hexOf(
        field(list, "retirementScriptHash"),
        `lists[${index}].retirementScriptHash`,
        28,
      ),
    };
  });
  if (new Set(parsed.map((list) => list.kind)).size !== parsed.length)
    throw new EventProjectionConfigError("each kind may appear once");
  if (new Set(parsed.map((list) => list.policyId)).size !== parsed.length)
    throw new EventProjectionConfigError("each list needs its own policy");
  return { networkId, lists: parsed };
};

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

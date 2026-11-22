import { Value } from "@al-ft/midgard-sdk/core";
import { valueToAssets } from "@al-ft/midgard-sdk/users";
import { Data } from "@lucid-evolution/lucid";

/**
 * A user event as the watcher's validation replay binds it: the deposit,
 * withdrawal or forced order an operator committed, read from the L1
 * follower's facts at a state-queue header's cutoff (W3). The record is
 * descriptive; the capability below is the authority.
 */

export type WatcherUserEventKind = "deposit" | "withdrawal" | "forced_order";

export type WatcherUserEventNetwork =
  | "Mainnet"
  | "Preprod"
  | "Preview"
  | "Custom";

/** Where the event's originating output was admitted on L1. */
export type WatcherUserEventAdmission = Readonly<{
  blockHash: string;
  slot: string;
  blockNo: string;
  transactionHash: string;
  transactionIndex: string;
  outputIndex: string;
}>;

export type WatcherUserEvent = Readonly<{
  kind: WatcherUserEventKind;
  /** The event id: `OutputReference` CBOR (hex). */
  eventId: string;
  /** The event id as `txHash#index`. */
  nonceOutRef: string;
  /** The minting policy of the event's token: the list policy, or the tx-order policy. */
  policyId: string;
  /** The event token's asset name: the list key, or the tx-order nonce name. */
  assetNameHex: string;
  inclusionTime: string;
  /** The event (`DepositEvent`, `WithdrawalEvent` or `TxOrderEvent`), aiken-serialised CBOR. */
  eventCborHex: string;
  /** A deposit's original L1 assets (`Value` CBOR); null for the other kinds. */
  originalAssetsCborHex: string | null;
  admission: WatcherUserEventAdmission;
}>;

/** The state-queue header an event read is scoped to, and its tx's index in its block. */
export type WatcherUserEventHeaderCutoff = Readonly<{
  headerHash: string;
  headerCborHex: string;
  queueOutRef: string;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
  transactionIndex: string;
}>;

export type WatcherUserEventAuthorityRead = Readonly<{
  hubOraclePolicyId: string;
  blueprintHash: string;
  network: WatcherUserEventNetwork;
  event: WatcherUserEvent;
  throughHeader: WatcherUserEventHeaderCutoff;
}>;

const userEventAuthorityBrand: unique symbol = Symbol(
  "watcher-user-event-authority",
);

/** An admitted user-event read; only its source can mint one. */
export type WatcherUserEventAuthority = Readonly<{
  [userEventAuthorityBrand]: true;
}>;

type AuthoritySource = Readonly<{
  /** Re-reads the event from the facts; throws when they no longer hold it. */
  read(): Promise<WatcherUserEventAuthorityRead>;
  /** False once a rewind below the cutoff (or a store reset) retired the read. */
  current(): boolean;
}>;

const authoritySources = new WeakMap<object, AuthoritySource>();

/** Mints the capability for a source that read the event from follower facts. */
export const admitWatcherUserEventAuthority = (
  source: AuthoritySource,
): WatcherUserEventAuthority => {
  const authority = Object.freeze({}) as WatcherUserEventAuthority;
  authoritySources.set(authority, source);
  return authority;
};

const sourceOf = (authority: WatcherUserEventAuthority): AuthoritySource => {
  const source = authoritySources.get(authority);
  if (source === undefined)
    throw new Error("user-event authority is not admitted");
  return source;
};

/** The current read of an admitted capability: the facts must still hold it. */
export const readWatcherUserEventAuthority = async (
  authority: WatcherUserEventAuthority,
): Promise<WatcherUserEventAuthorityRead> => {
  const source = sourceOf(authority);
  if (!source.current())
    throw new Error("user-event authority was retired by an L1 rewind");
  return await source.read();
};

/** Synchronous fence: call after the last await, before using a read. */
export const assertWatcherUserEventAuthorityCurrent = (
  authority: WatcherUserEventAuthority,
): void => {
  if (!sourceOf(authority).current())
    throw new Error("user-event authority was retired by an L1 rewind");
};

/** A deposit's original L1 assets, as the follower's event projection recorded them. */
export const watcherOriginalDepositAssets = (
  event: Pick<WatcherUserEvent, "kind" | "originalAssetsCborHex">,
) => {
  if (event.kind !== "deposit" || event.originalAssetsCborHex === null)
    throw new Error("Original deposit funds require a deposit event");
  return valueToAssets(Data.from(event.originalAssetsCborHex, Value));
};

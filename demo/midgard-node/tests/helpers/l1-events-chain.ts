/**
 * Event-list traffic for the node event projection's tests, simulator
 * corpus and B2 bench: list Orders (inline or external payload) admitted by
 * a nonce-consuming tx, and retirements that burn the token under the
 * retirement observer's zero withdrawal. The bytes are the SDK's, so the
 * projection opens them exactly as it opens the chain's.
 */
import type { OutRef } from "@al-ft/midgard-l1-follower";
import type { SimOutput, SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type {
  EventKind,
  EventListConfig,
  EventProjectionConfig,
} from "../../src/l1-events/index.js";

const repeat = (byte: string, bytes: number): string => byte.repeat(bytes);

const list = (kind: EventKind, seed: string): EventListConfig => ({
  kind,
  policyId: repeat(`${seed}1`, 28),
  listAddress: `70${repeat(`${seed}2`, 28)}`,
  retentionAddress: `70${repeat(`${seed}3`, 28)}`,
  retirementScriptHash: repeat(`${seed}4`, 28),
});

export const EVENTS_CONFIG: EventProjectionConfig = {
  networkId: 0,
  lists: [list("deposit", "a"), list("withdrawal", "b")],
};

export const listOf = (kind: EventKind): EventListConfig =>
  EVENTS_CONFIG.lists.find((entry) => entry.kind === kind)!;

/** An address no projection tracks (payouts and change go here). */
export const ELSEWHERE = Buffer.from(`70${repeat("ee", 28)}`, "hex");

const owner = repeat("cd", 28);
const auth = { PublicKeyCredential: [owner] as [string] };
const address = { paymentCredential: auth, stakeCredential: null };
const BOUNDS = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15_000n,
  maxPayloadNodes: 1_024n,
};

export const eventIdOf = (nonce: OutRef): SDK.OutputReference => ({
  transactionId: nonce.txHash.toString("hex"),
  outputIndex: BigInt(nonce.index),
});

/** The event key of the event a nonce admits (hex). */
export const eventKeyOf = (nonce: OutRef): string =>
  Effect.runSync(SDK.eventHistoryKey(eventIdOf(nonce)));

const payloadOf = (
  kind: EventKind,
  id: SDK.OutputReference,
  external: boolean,
): SDK.EventHistoryPayload => {
  const data = external ? "ab".repeat(600) : "ab";
  return kind === "deposit"
    ? {
        DepositPayload: {
          event: {
            id,
            info: { l2_address: address, l2_network_id: 0n, l2_datum: data },
          },
        },
      }
    : {
        WithdrawalPayload: {
          event: {
            id,
            info: {
              body: {
                l2_outref: id,
                l2_owner: owner,
                l2_value: new Map([["", new Map([["", 9_000_000n]])]]),
                l1_address: address,
                l1_datum: "NoDatum",
              },
              signature: [repeat("44", 32), repeat("55", 64)],
              validity: "WithdrawalIsValid",
            },
          },
          refund_address: address,
          refund_datum: { InlineDatum: { data } },
        },
      };
};

export type EventOrder = Readonly<{
  kind: EventKind;
  nonce: OutRef;
  key: string;
  /** The list Order output. */
  order: SimOutput;
  /** The retention output an external payload needs, created first. */
  retained: SimOutput | null;
}>;

/** The Order (and retention output, for an external payload) of one event. */
export const eventOrder = (
  kind: EventKind,
  nonce: OutRef,
  options: Readonly<{
    external?: boolean;
    inclusionTime?: bigint;
    next?: string | null;
  }> = {},
): EventOrder => {
  const config = listOf(kind);
  const id = eventIdOf(nonce);
  const key = eventKeyOf(nonce);
  const plan = SDK.prepareEventHistoryPayload(
    payloadOf(kind, id, options.external === true),
    auth,
    BOUNDS,
  );
  const datum = Data.to(
    {
      position: { Key: [key] },
      next: options.next ?? null,
      protected_until: 0n,
      payload: {
        Order: {
          facts: {
            event_id: id,
            inclusion_time: options.inclusionTime ?? 1_000n,
            location: plan.location,
            structural_lovelace: 2_000_000n,
            structural_refund_key: owner,
          },
        },
      },
    },
    SDK.EventHistoryNode,
  );
  return {
    kind,
    nonce,
    key,
    order: {
      address: Buffer.from(config.listAddress, "hex"),
      lovelace: 7_000_000n,
      assets: new Map([[config.policyId, new Map([[key, 1n]])]]),
      datum: Buffer.from(datum, "hex"),
    },
    retained:
      plan.kind === "External"
        ? {
            address: Buffer.from(config.retentionAddress, "hex"),
            lovelace: 3_000_000n,
            datum: Buffer.from(plan.datumCbor, "hex"),
          }
        : null,
  };
};

/** The list root (token name ""), pointing at `next`. */
export const rootOutput = (kind: EventKind, next: string | null): SimOutput => {
  const config = listOf(kind);
  return {
    address: Buffer.from(config.listAddress, "hex"),
    lovelace: 2_000_000n,
    assets: new Map([[config.policyId, new Map([["", 1n]])]]),
    datum: Buffer.from(
      Data.to(
        { position: "Root", next, protected_until: 0n, payload: "RootContent" },
        SDK.EventHistoryNode,
      ),
      "hex",
    ),
  };
};

/** A list Filler at `key` (not an event: the projection ignores it). */
export const fillerOutput = (kind: EventKind, key: string): SimOutput => {
  const config = listOf(kind);
  return {
    address: Buffer.from(config.listAddress, "hex"),
    lovelace: 2_000_000n,
    assets: new Map([[config.policyId, new Map([[key, 1n]])]]),
    datum: Buffer.from(
      Data.to(
        {
          position: { Key: [key] },
          next: null,
          protected_until: 0n,
          payload: { Filler: { refund_key: owner } },
        },
        SDK.EventHistoryNode,
      ),
      "hex",
    ),
  };
};

/**
 * The tx that admits `order`: it consumes the nonce, mints the key and
 * creates the Order (output 0), referencing `retainedRef` for an external
 * payload.
 */
export const admissionTx = (
  order: EventOrder,
  nonce: number,
  retainedRef?: OutRef,
): SimTx => ({
  inputs: [order.nonce],
  outputs: [order.order],
  mint: new Map([[listOf(order.kind).policyId, new Map([[order.key, 1n]])]]),
  ...(retainedRef === undefined ? {} : { referenceInputs: [retainedRef] }),
  invalidAfter: 90_000_000,
  nonce,
});

const PURPOSES = {
  absorbed: "AbsorbDeposit",
  payout_initialized: "InitializeWithdrawalPayout",
} as const;

/** The retirement observer redeemer's witness for a retirement with `reason`. */
export const retirementWitness = (
  reason: "absorbed" | "payout_initialized" | "refunded",
): SDK.EventHistoryRetirementWitness => ({
  predecessor_input_index: 0n,
  order_input_index: 1n,
  predecessor_output_index: 0n,
  funds_output_index: 1n,
  structural_refund_output_index: null,
  confirmed_reference_index: 0n,
  settlement_reference_index: 1n,
  external_reference_index: null,
  membership: { phas_root: repeat("99", 32), count: 1n, proof: [] },
  purpose:
    reason === "refunded"
      ? { RefundInvalidWithdrawal: { validity: "WithdrawalIsValid" } }
      : PURPOSES[reason],
});

/**
 * The tx that retires the event at `orderOutRef`: it spends the Order,
 * burns the key and carries the retirement observer's zero withdrawal next
 * to a key-credential withdrawal that sorts after it, so the observer's
 * redeemer pointer is its ledger-sorted index.
 */
export const retirementTx = (
  order: Pick<EventOrder, "kind" | "key">,
  orderOutRef: OutRef,
  reason: "absorbed" | "payout_initialized" | "refunded",
  nonce: number,
): SimTx => {
  const config = listOf(order.kind);
  const network = EVENTS_CONFIG.networkId;
  const observer = Buffer.concat([
    Buffer.of(0xf0 | network),
    Buffer.from(config.retirementScriptHash, "hex"),
  ]);
  const keyAccount = Buffer.concat([
    Buffer.of(0xe0 | network),
    Buffer.alloc(28, 0x01),
  ]);
  const args: SDK.EventHistoryRetirementArgs = {
    hub_reference_index: 0n,
    witness: retirementWitness(reason),
  };
  return {
    inputs: [orderOutRef],
    outputs: [{ address: ELSEWHERE, lovelace: 5_000_000n }],
    mint: new Map([[config.policyId, new Map([[order.key, -1n]])]]),
    // Body order puts the key account first; the ledger sorts the script first.
    withdrawals: [
      { rewardAccount: keyAccount, amount: 0n },
      { rewardAccount: observer, amount: 0n },
    ],
    redeemers: [
      { purpose: "spend", index: 0, data: Buffer.from("00", "hex") },
      {
        purpose: "reward",
        index: 0,
        data: Buffer.from(Data.to(args, SDK.EventHistoryRetirementArgs), "hex"),
      },
    ],
    invalidAfter: 90_000_000,
    nonce,
  };
};

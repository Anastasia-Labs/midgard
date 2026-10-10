/**
 * Why a header left the state queue, and the order a release proof walks
 * departures in: each `departureKind` arm on hand-built txs, and the
 * stop-at-`other` rule of `resolveMergedHeaders` on a real follower store,
 * including two departures in one block.
 */
import {
  type FactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import { simStoreOptions } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { createWatcherQueueHeaderSource } from "../../src/l1-follower/observation.js";
import {
  departureKind,
  type WatcherDepartureKind,
} from "../../src/l1-follower/projection.departed-headers.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  watcherProjection,
} from "../../src/l1-follower/projection.js";
import {
  h28,
  redeemer,
  SIM_WATCHER_DEPLOYMENT,
} from "../support/l1-follower-state-queue-traffic.js";

const D = SIM_WATCHER_DEPLOYMENT;
const POLICY = D.stateQueueMint;
const HEADER = h28("a1");
const OTHER = h28("b2");
const ROOT_TX = "77".repeat(32);
const ROOT = `${ROOT_TX}#0`;
const ZERO_ROOT = "00".repeat(32);
const outRef = { transactionId: ROOT_TX, outputIndex: 0n };

type Entry = Parameters<typeof departureKind>[0];

const burn = (...headers: string[]) =>
  new Map([
    [
      POLICY,
      new Map(
        headers.map((header) => [
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`,
          -1n,
        ]),
      ),
    ],
  ]);

const entry = (
  value: SDK.StateQueueRedeemer | null,
  mint: Map<string, Map<string, bigint>>,
  rootOut = 1n,
): Entry =>
  ({
    tx: {
      mint,
      redeemers: value === null ? [] : redeemer(value),
      outputs: [
        {
          assets: new Map([
            [POLICY, new Map([[SDK.STATE_QUEUE_ROOT_ASSET_NAME, rootOut]])],
          ]),
        },
      ],
    },
  }) as unknown as Entry;

const merge = (header: string): SDK.StateQueueRedeemer => ({
  MergeToConfirmedStateV1: {
    yield_to_ref_input_index: 0n,
    header_node_key: header,
    confirmed_state_input_outref: outRef,
    confirmed_state_output_index: 0n,
    m_settlement_redeemer_index: null,
    merged_block_withdrawals_root: ZERO_ROOT,
    merged_block_forced_transactions_root: ZERO_ROOT,
    merged_block_transactions_root: ZERO_ROOT,
    merged_block_deposits_root: ZERO_ROOT,
    merged_block_transition_trace_root: ZERO_ROOT,
    merged_block_event_to_step_root: ZERO_ROOT,
    merged_block_validation_traces_root: ZERO_ROOT,
    merged_block_withdrawal_count: 0n,
    merged_block_forced_transaction_count: 0n,
    merged_block_l2_transaction_count: 0n,
    merged_block_deposit_count: 0n,
    merged_block_total_event_count: 0n,
    merged_block_transition_step_count: 0n,
    merged_block_validation_trace_count: 0n,
  },
});

const removals: Readonly<
  Record<
    Exclude<WatcherDepartureKind, "merged" | "other">,
    SDK.StateQueueRedeemer
  >
> = {
  RemoveFraudulentBlockHeader: {
    RemoveFraudulentBlockHeader: {
      yield_to_ref_input_index: 0n,
      fraudulent_operator: h28("aa"),
      fraudulent_blocks_header_hash: HEADER,
      slashing_approach: {
        SlashActiveOperator: {
          active_operators_redeemer_index: 0n,
          m_fraud_prover_reward_output_index: null,
        },
      },
      fraud_proof_ref_input_index: 0n,
      block_removal_approach: {
        RemoveLastFraudulentBlock: {
          anchor_element_input_outref: outRef,
          anchor_element_output_index: 0n,
        },
      },
    },
  },
  RemoveUnattestedBlockAfterTimeout: {
    RemoveUnattestedBlockAfterTimeout: {
      yield_to_ref_input_index: 0n,
      timed_out_header_hash: HEADER,
      removal_approach: {
        RemoveLastUnattestedBlock: {
          predecessor_input_outref: outRef,
          predecessor_output_index: 0n,
        },
      },
    },
  },
  RemoveUnavailableBlockAfterTimeout: {
    RemoveUnavailableBlockAfterTimeout: {
      yield_to_ref_input_index: 0n,
      unavailable_header_hash: HEADER,
      challenge_asset_name: "cc".repeat(32),
      removal_approach: {
        RemoveTimedOutHead: {
          confirmed_state_input_outref: outRef,
          confirmed_state_output_index: 0n,
        },
      },
    },
  },
};

const kindOf = (value: Entry, header = HEADER) =>
  departureKind(value, header, new Set([ROOT]), POLICY);

describe("departureKind", () => {
  it("names a merge of exactly this header over the spent root", () => {
    expect(kindOf(entry(merge(HEADER), burn(HEADER)))).toBe("merged");
  });

  it("calls a merge of another header, an unspent root, a lost root or a second burn other", () => {
    expect(kindOf(entry(merge(OTHER), burn(HEADER)))).toBe("other");
    expect(
      departureKind(
        entry(merge(HEADER), burn(HEADER)),
        HEADER,
        new Set(),
        POLICY,
      ),
    ).toBe("other");
    expect(kindOf(entry(merge(HEADER), burn(HEADER), 0n))).toBe("other");
    expect(kindOf(entry(merge(HEADER), burn(HEADER, OTHER)))).toBe("other");
  });

  it.each(Object.entries(removals))(
    "names a %s that burns exactly this header's node",
    (kind, value) => {
      expect(kindOf(entry(value, burn(HEADER)))).toBe(kind);
    },
  );

  it.each(Object.entries(removals))(
    "calls a %s that burns another node or two nodes other",
    (_kind, value) => {
      expect(kindOf(entry(value, burn(OTHER)))).toBe("other");
      expect(kindOf(entry(value, burn(HEADER, OTHER)))).toBe("other");
    },
  );

  it("calls a tx without one decodable state-queue mint redeemer other", () => {
    expect(kindOf(entry(null, burn(HEADER)))).toBe("other");
    const twice = entry(merge(HEADER), burn(HEADER));
    const tx = (twice as unknown as { tx: { redeemers: unknown[] } }).tx;
    tx.redeemers = [...tx.redeemers, ...tx.redeemers];
    expect(kindOf(twice)).toBe("other");
    const garbled = entry(merge(HEADER), burn(HEADER));
    (
      garbled as unknown as { tx: { redeemers: { data: Buffer }[] } }
    ).tx.redeemers[0]!.data = Buffer.from("00", "hex");
    expect(kindOf(garbled)).toBe("other");
  });
});

/** Blocks at heights 1..10, slot = 10 * height; release depth 3 makes slot 80 final. */
const TIP = 10;
const R = 3;
const stores: FactStore[] = [];
afterEach(async () => {
  for (const store of stores.splice(0)) await store.close();
});

type Departure = Readonly<{
  header: string;
  kind: WatcherDepartureKind;
  slot: number;
  txIndex: number;
}>;

const releaseOf = async (departures: readonly Departure[]) => {
  const store = openSqliteFactStore({
    ...simStoreOptions([watcherProjection(D)], 6, "sqlite"),
    path: ":memory:",
  });
  stores.push(store);
  expect((await store.start()).kind).toBe("ready");
  const hash = (height: number) => Buffer.alloc(32, height);
  await store.transaction("write", async (tx) => {
    for (let height = 1; height <= TIP; height += 1)
      await tx.query(
        "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, ?, 0)",
        [
          height * 10,
          hash(height),
          height,
          height === 1 ? null : hash(height - 1),
        ],
      );
    await tx.query(
      "INSERT INTO l1_follower_cursor (slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot) VALUES (?, ?, ?, 0, 10, ?, 0)",
      [TIP * 10, hash(TIP), TIP, hash(1)],
    );
    for (const departure of departures)
      await tx.query(
        `INSERT INTO ${WATCHER_DEPARTED_HEADERS_TABLE} (header_hash, kind, departure_tx_hash, departure_tx_index, departure_block_hash, departure_height, from_slot) VALUES (?, ?, ?, ?, ?, ?, ?)`,
        [
          Buffer.from(departure.header, "hex"),
          departure.kind,
          Buffer.alloc(32, departure.txIndex + departure.slot),
          departure.txIndex,
          hash(departure.slot / 10),
          departure.slot / 10,
          departure.slot,
        ],
      );
  });
  const observation = {
    nativePoint: { slot: "0" },
    finalizedHeaders: departures.map(({ header }) => ({ headerHash: header })),
  } as unknown as WatcherAuthenticatedStateQueueObservation;
  const released = await createWatcherQueueHeaderSource(store, {
    releaseDepth: R,
  }).resolveMergedHeaders({ observation });
  return [...released.keys()];
};

describe("resolveMergedHeaders", () => {
  it("releases merges and removals in chain order and stops at the first other", async () => {
    expect(
      await releaseOf([
        { header: h28("01"), kind: "merged", slot: 30, txIndex: 0 },
        {
          header: h28("02"),
          kind: "RemoveFraudulentBlockHeader",
          slot: 40,
          txIndex: 0,
        },
        { header: h28("03"), kind: "other", slot: 50, txIndex: 0 },
        { header: h28("04"), kind: "merged", slot: 60, txIndex: 0 },
      ]),
    ).toEqual([h28("01"), h28("02")]);
  });

  it("releases nothing past the release-final block", async () => {
    expect(
      await releaseOf([
        { header: h28("01"), kind: "merged", slot: 80, txIndex: 0 },
        { header: h28("02"), kind: "merged", slot: 90, txIndex: 0 },
      ]),
    ).toEqual([h28("01")]);
  });

  it("orders two departures in one block by their tx index", async () => {
    // The other departure is read first but sits later in the block.
    expect(
      await releaseOf([
        { header: h28("05"), kind: "other", slot: 30, txIndex: 1 },
        { header: h28("06"), kind: "merged", slot: 30, txIndex: 0 },
      ]),
    ).toEqual([h28("06")]);
    expect(
      await releaseOf([
        { header: h28("07"), kind: "merged", slot: 30, txIndex: 1 },
        { header: h28("08"), kind: "other", slot: 30, txIndex: 0 },
      ]),
    ).toEqual([]);
  });
});

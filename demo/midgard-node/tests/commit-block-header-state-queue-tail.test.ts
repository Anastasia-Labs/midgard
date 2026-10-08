import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { resolveLiveTailCommitBase } from "../src/workers/commit-block-header/pending-journal.js";
import { fetchExpectedStateQueueTailLocal } from "../src/workers/commit-block-header/state-queue.js";
import { resolveCommitValidityInterval } from "../src/workers/utils/commit-end-time.js";
import { seedLandedStateQueue } from "./helpers/landed-state-queue.js";
import { provideDatabaseLayers } from "./utils.js";

const policyId = "aa".repeat(28);
const stateQueueAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const headerFixture = (overrides: Partial<SDK.Header> = {}): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "11".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1_000n,
  endTime: 2_000n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "22".repeat(28),
  operatorVkey: "33".repeat(28),
  protocolVersion: 1n,
  ...overrides,
});

const makeTail = async ({
  txHash = "44".repeat(32),
  outputIndex = 0,
  header = headerFixture(),
  next = "Empty",
}: {
  readonly txHash?: string;
  readonly outputIndex?: number;
  readonly header?: SDK.Header;
  readonly next?: SDK.LinkedListNodeView["next"];
} = {}): Promise<SDK.StateQueueUTxO> => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next,
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  };
  const utxo: UTxO = {
    txHash,
    outputIndex,
    address: stateQueueAddress,
    assets: {
      lovelace: 3_000_000n,
      [toUnit(policyId, assetName)]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView(datum),
  };
  return { utxo, datum, assetName };
};

const config: SDK.StateQueueFetchConfig = {
  stateQueueAddress,
  stateQueuePolicyId: policyId,
};

const contracts = {
  stateQueue: {
    spendingScriptAddress: stateQueueAddress,
    policyId,
  },
} as unknown as SDK.MidgardValidators;

/** The confirmed-state root, linking to the node keyed `next`. */
const rootLinkingTo = (next: string): UTxO => ({
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: {
    lovelace: 3_000_000n,
    [toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
  },
  datum: SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: next } },
    data: SDK.castConfirmedStateToData({
      headerHash: "22".repeat(28),
      prevHeaderHash: "00".repeat(28),
      utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      startTime: 0n,
      endTime: 0n,
      protocolVersion: 1n,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

const keyOf = (node: SDK.StateQueueUTxO): string =>
  node.datum.key === "Empty" ? "" : node.datum.key.Key.key;

/** `effect` over the landed queue: the root, then `nodes` in list order. */
const overLandedQueue = <A, E>(
  nodes: readonly SDK.StateQueueUTxO[],
  effect: Effect.Effect<A, E, SqlClient.SqlClient>,
  extra: readonly UTxO[] = [],
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.zipRight(
        seedLandedStateQueue({ spendingScriptAddress: stateQueueAddress }, [
          rootLinkingTo(keyOf(nodes[0]!)),
          ...nodes.map((node) => node.utxo),
          ...extra,
        ]),
        effect,
      ),
    ),
  );

describe("commit-block expected state-queue tail lookup (landed queue)", () => {
  it("returns the expected tail unchanged while it is the landed tail", async () => {
    const expected = await makeTail();
    const actual = await overLandedQueue(
      [expected],
      fetchExpectedStateQueueTailLocal(config, expected),
    );
    expect(actual).toBe(expected);
  });

  it("accepts an out-ref replacement that preserves the logical tail header", async () => {
    const expected = await makeTail();
    const replacement = await makeTail({
      txHash: "55".repeat(32),
      outputIndex: 1,
    });

    const actual = await overLandedQueue(
      [replacement],
      resolveLiveTailCommitBase(contracts, expected, MIDGARD_CONSENSUS_PROFILE),
    );

    expect(actual.utxo.txHash).toBe(replacement.utxo.txHash);
    expect(actual.utxo.outputIndex).toBe(replacement.utxo.outputIndex);
  });

  it("classifies the expected NFT becoming a non-tail as a stale commit base", async () => {
    const expected = await makeTail();
    const newTail = await makeTail({
      txHash: "77".repeat(32),
      header: headerFixture({ startTime: 2_000n, endTime: 3_000n }),
    });
    const advanced = await makeTail({
      txHash: "66".repeat(32),
      next: { Key: { key: keyOf(newTail) } },
    });

    const outcome = await overLandedQueue(
      [advanced, newTail],
      Effect.either(
        resolveLiveTailCommitBase(
          contracts,
          expected,
          MIDGARD_CONSENSUS_PROFILE,
        ),
      ),
    );
    expect(outcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message:
          "Commit base is stale; aborting block build before creating a pending journal",
      },
    });
  });

  it("fails closed when the expected unit is gone, and when it is duplicated", async () => {
    const expected = await makeTail();
    const other = await makeTail({
      txHash: "99".repeat(32),
      header: headerFixture({ startTime: 2_000n, endTime: 3_000n }),
    });
    const missingOutcome = await overLandedQueue(
      [other],
      Effect.either(
        resolveLiveTailCommitBase(
          contracts,
          expected,
          MIDGARD_CONSENSUS_PROFILE,
        ),
      ),
    );
    expect(missingOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message:
          "Commit base is stale; aborting block build before creating a pending journal",
      },
    });

    // A second live output under the same key makes the landed queue
    // unhealthy: the commit stops on the named reason.
    const duplicate = await makeTail({ txHash: "88".repeat(32) });
    const duplicateOutcome = await overLandedQueue(
      [expected],
      Effect.either(fetchExpectedStateQueueTailLocal(config, expected)),
      [duplicate.utxo],
    );
    expect(duplicateOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message: expect.stringContaining(
          "The landed state queue is unhealthy (duplicate_key)",
        ),
      },
    });
  });
});

// ---------------------------------------------------------------------------
// Q60 / D-S12 — the commit `end_time` bound, off-chain half.
//
// The on-chain rule (`commit_bound_header_time_is_valid`,
// onchain/aiken/lib/midgard/state-queue.ak) accepts exactly one `end_time` per
// commit transaction: the inclusive upper bound of that transaction's validity
// interval. Cardano surfaces `invalid_hereafter` as an *exclusive* upper bound,
// so the off-chain form of the same identity is `header.endTime === validTo - 1`.
//
// These cases reuse the absolute vector of the Aiken family in
// onchain/aiken/lib/midgard/state-queue.test.ak so both languages are pinned to
// the same numbers:
//
//   accepted header end_time / inclusive upper bound   1_700_000_480_000
//   transaction validTo (invalid_hereafter, exclusive) 1_700_000_480_001
//   far-future end_time (ten years on)                 2_015_360_480_000
//   strictly-inside end_time (1_000 ms below the bound) 1_700_000_479_000
//
// That last value is load-bearing. The guard is an equality, but a case sitting
// AT the anchor, one millisecond above it, or ten years above it is satisfied
// by a `<=` or interval-membership guard just as well — so a suite built only
// from those cannot tell the real guard from a weaker one. The strictly-inside
// case is the one that can.
// ---------------------------------------------------------------------------

const ACCEPTED_HEADER_END_TIME_MS = 1_700_000_480_000;
const COMMIT_VALID_TO_MS = 1_700_000_480_001;
const FAR_FUTURE_HEADER_END_TIME_MS = 2_015_360_480_000;
// Strictly between the widest production `validFrom` and the anchor. The Aiken
// family pins the same absolute value in
// `state_queue_commit_end_time_rejects_an_end_strictly_inside_the_commit_window`.
const STRICTLY_INSIDE_HEADER_END_TIME_MS = 1_700_000_479_000;
// The widest interval the production guard admits for this validTo.
const WIDEST_COMMIT_VALID_FROM_MS = 1_700_000_000_001;
// `env.max_validity_range_length` in onchain/aiken/env/default.ak, over which
// the validator normalizes the *inclusive* span.
const ONCHAIN_MAX_INCLUSIVE_RANGE_MS = 480_000;

const SLOT_LENGTH_MS = 1_000;

const fakeSlotLucid = () =>
  ({
    unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / SLOT_LENGTH_MS),
    slotToUnixTime: (slot: number) => slot * SLOT_LENGTH_MS,
  }) as unknown as LucidEvolution;

const submitSlotSnapshot = {
  source: "test",
  currentSlot: 1_700_000_300,
  observedAtMs: 1_700_000_300_000,
  slotLengthMs: SLOT_LENGTH_MS,
} as const;

describe("commit-block header end_time is anchored to the commit validity bound", () => {
  it("derives the header end_time as the inclusive upper bound of the interval it builds", () => {
    const interval = resolveCommitValidityInterval({
      lucid: fakeSlotLucid(),
      submitSlotSnapshot,
      validToMs: COMMIT_VALID_TO_MS,
    });

    expect(interval.validToMs).toBe(COMMIT_VALID_TO_MS);
    expect(interval.validFromMs).toBe(1_700_000_240_000);
    expect(interval.inclusiveUpperBoundMs).toBe(ACCEPTED_HEADER_END_TIME_MS);

    // This inclusive upper bound is the value the builder hands to the header,
    // so it satisfies the guard gating every production commit.
    expect(
      SDK.commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(interval.inclusiveUpperBoundMs),
        validTo: interval.validToMs,
      }),
    ).toBe(true);
  });

  it("accepts the maximum end_time the production validity guard allows", () => {
    const validFrom = COMMIT_VALID_TO_MS - SDK.COMMIT_MAX_VALIDITY_RANGE_MS;
    expect(validFrom).toBe(1_700_000_000_001);
    expect(
      SDK.isCommitValidityInterval({
        validFrom,
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(true);
    expect(
      SDK.commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(ACCEPTED_HEADER_END_TIME_MS),
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(true);

    // The node's guard is stated over the exclusive span and the validator's
    // over the inclusive one, so the widest interval the node will ever build
    // is one millisecond *inside* what the validator accepts. Conservative in
    // the safe direction: the node cannot construct a commit the chain rejects
    // on this rule.
    expect(ACCEPTED_HEADER_END_TIME_MS - validFrom).toBe(479_999);
    expect(ACCEPTED_HEADER_END_TIME_MS - validFrom).toBeLessThanOrEqual(
      ONCHAIN_MAX_INCLUSIVE_RANGE_MS,
    );
  });

  it("keeps the same anchor when only the validity lower bound moves", () => {
    // Lower-bound control: the anchor is the upper bound, so widening or
    // narrowing the interval from below changes nothing about the accepted
    // header end_time.
    for (const validFrom of [1_700_000_000_001, 1_700_000_479_001]) {
      expect(
        SDK.isCommitValidityInterval({
          validFrom,
          validTo: COMMIT_VALID_TO_MS,
        }),
      ).toBe(true);
      expect(
        SDK.commitHeaderMatchesValidityUpperBound({
          headerEndTime: BigInt(ACCEPTED_HEADER_END_TIME_MS),
          validTo: COMMIT_VALID_TO_MS,
        }),
      ).toBe(true);
    }
  });

  it("rejects a header end_time one millisecond above the bound", () => {
    expect(
      SDK.commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(ACCEPTED_HEADER_END_TIME_MS + 1),
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(false);
  });

  it("rejects a far-future header end_time", () => {
    expect(
      SDK.commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(FAR_FUTURE_HEADER_END_TIME_MS),
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(false);
  });

  it("rejects a header end_time strictly below the bound", () => {
    // The case that separates `=== validTo - 1` from `<= validTo - 1`. It is
    // inside the interval the commit is actually built with, so a membership
    // guard would accept it.
    const validFrom = COMMIT_VALID_TO_MS - SDK.COMMIT_MAX_VALIDITY_RANGE_MS;
    expect(validFrom).toBe(WIDEST_COMMIT_VALID_FROM_MS);

    // The interval itself is admissible, so the rejection below is attributable
    // to the header guard rather than to the interval guard.
    expect(
      SDK.isCommitValidityInterval({
        validFrom,
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(true);
    expect(STRICTLY_INSIDE_HEADER_END_TIME_MS).toBeGreaterThan(validFrom);
    expect(STRICTLY_INSIDE_HEADER_END_TIME_MS).toBeLessThan(
      ACCEPTED_HEADER_END_TIME_MS,
    );

    expect(
      SDK.commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(STRICTLY_INSIDE_HEADER_END_TIME_MS),
        validTo: COMMIT_VALID_TO_MS,
      }),
    ).toBe(false);
  });

  it("rejects every header end_time strictly inside the commit validity interval", () => {
    // Depth sweep from both ends of the interior: the interval's own lower
    // bound, one millisecond in, the midpoint, and one millisecond below the
    // anchor. None is the anchor, so none may be accepted.
    for (const headerEndTimeMs of [
      WIDEST_COMMIT_VALID_FROM_MS,
      WIDEST_COMMIT_VALID_FROM_MS + 1,
      1_700_000_240_000,
      ACCEPTED_HEADER_END_TIME_MS - 1,
    ]) {
      expect(headerEndTimeMs).toBeGreaterThanOrEqual(
        WIDEST_COMMIT_VALID_FROM_MS,
      );
      expect(headerEndTimeMs).toBeLessThan(ACCEPTED_HEADER_END_TIME_MS);
      expect(
        SDK.commitHeaderMatchesValidityUpperBound({
          headerEndTime: BigInt(headerEndTimeMs),
          validTo: COMMIT_VALID_TO_MS,
        }),
      ).toBe(false);
    }
  });

  it("re-anchors the lower bound rather than emitting an over-long interval", () => {
    // A stale submit slot would otherwise open the interval 480_001 ms before
    // validTo — one millisecond too wide for the validator's inclusive span.
    const interval = resolveCommitValidityInterval({
      lucid: fakeSlotLucid(),
      submitSlotSnapshot: { ...submitSlotSnapshot, currentSlot: 1_600_000_000 },
      validToMs: COMMIT_VALID_TO_MS,
    });

    expect(interval.validFromMs).toBe(1_700_000_001_000);
    expect(interval.inclusiveUpperBoundMs).toBe(ACCEPTED_HEADER_END_TIME_MS);
    expect(
      interval.inclusiveUpperBoundMs - interval.validFromMs,
    ).toBeLessThanOrEqual(ONCHAIN_MAX_INCLUSIVE_RANGE_MS);
  });
});

import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either, Ref } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

import { observeAndRecordAttestationTimeoutQueue } from "../src/fibers/attestation-timeout-correction.js";
import {
  observeAttestationTimeoutQueue,
  timeoutCorrectionJournalNeedsRecovery,
} from "../src/services/attestation-timeout-observation.js";
import type { AttestationTimeoutCorrectionHealth } from "../src/services/globals.js";
import {
  resolveCommitAppendFenceEndTimeCapLocal,
  resolveCommitAppendFenceReferencesLocal,
} from "../src/workers/commit-block-header/state-queue.js";
import { registerTestL1Tip, TEN_MINUTES_MS } from "./helpers/l1-tip.js";

const policyId = "aa".repeat(28);
const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const header = (endTime: bigint): SDK.Header => ({
  ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "77".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  startTime: 0n,
  endTime,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "88".repeat(28),
  operatorVkey: "99".repeat(28),
  protocolVersion: 1n,
});
const fixture = async (tailApplied = false, headApplied = true) => {
  const firstHash = await Effect.runPromise(SDK.hashBlockHeader(header(1000n)));
  const tailHash = await Effect.runPromise(SDK.hashBlockHeader(header(2000n)));
  const node = (
    assetName: string,
    datum: SDK.LinkedListNodeView,
    byte: string,
  ): UTxO => ({
    txHash: byte.repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n, [toUnit(policyId, assetName)]: 1n },
    datum: SDK.encodeLinkedListNodeView(datum),
  });
  const root = node(
    SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    {
      key: "Empty",
      next: { Key: { key: firstHash } },
      data: SDK.castConfirmedStateToData({
        headerHash: "88".repeat(28),
        prevHeaderHash: "00".repeat(28),
        utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        startTime: 0n,
        endTime: 0n,
        protocolVersion: 1n,
      }) as SDK.LinkedListNodeView["data"],
    },
    "00",
  );
  const first = node(
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + firstHash,
    {
      key: { Key: { key: firstHash } },
      next: { Key: { key: tailHash } },
      data: SDK.castStateQueueNodeToData({
        proven_fraud: null,
        header: header(1000n),
        da_attestation: headApplied
          ? { Attested: { commitment_hash: "11".repeat(32) } }
          : SDK.NO_DA_ATTESTATION,
      }) as SDK.LinkedListNodeView["data"],
    },
    "11",
  );
  const tail = node(
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + tailHash,
    {
      key: { Key: { key: tailHash } },
      next: "Empty",
      data: SDK.castStateQueueNodeToData({
        proven_fraud: null,
        header: header(2000n),
        da_attestation: tailApplied
          ? { Attested: { commitment_hash: "22".repeat(32) } }
          : SDK.NO_DA_ATTESTATION,
      }) as SDK.LinkedListNodeView["data"],
    },
    "22",
  );
  const outputs = [root, first, tail];
  const api = {
    utxosAt: vi.fn(async () => outputs),
    utxosAtWithUnit: vi.fn(async (_address: string, unit: string) =>
      outputs.filter((x) => x.assets[unit] === 1n),
    ),
  } as unknown as LucidEvolution;
  const queue = await Promise.all(
    outputs.map((utxo) =>
      Effect.runPromise(SDK.utxoToStateQueueUTxO(utxo, policyId)),
    ),
  );
  return { api, queue, tailHash };
};

describe("pending queue attestation expiry", () => {
  it("finds near-expiry and expired tails behind attested predecessors", async () => {
    const { queue, tailHash } = await fixture();
    expect(
      await Effect.runPromise(
        observeAttestationTimeoutQueue(
          queue,
          SDK.DA_ATTESTATION_TIMEOUT_MS + 2_000n - 50_000n,
          120_000n,
        ),
      ),
    ).toMatchObject({ status: "near-timeout", headerHash: tailHash });
    expect(
      await Effect.runPromise(
        observeAttestationTimeoutQueue(
          queue,
          SDK.DA_ATTESTATION_TIMEOUT_MS + 2_000n,
          120_000n,
        ),
      ),
    ).toMatchObject({ status: "timed-out", headerHash: tailHash });
    expect(
      await Effect.runPromise(
        observeAttestationTimeoutQueue(
          (await fixture(true)).queue,
          9_000_000n,
          120_000n,
        ),
      ),
    ).toEqual({ status: "queue-attested" });
  });
  it("fences the commit end below every unattested node's deadline, not only the head's", async () => {
    const fetchConfig = {
      stateQueueAddress: address,
      stateQueuePolicyId: policyId,
    };
    const timeoutMs = Number(SDK.DA_ATTESTATION_TIMEOUT_MS);
    // L1 now at slot 0, before every fixture node's deadline: an expired node
    // refuses instead. The stub client has no slot mapping, so slot s starts
    // at s * 1000 ms.
    const tipAtZero = (api: LucidEvolution) => registerTestL1Tip(api, 0);
    // The head (end 1000) is attested and the tail (end 2000) is not. The
    // on-chain fence reads the head only; the build must still land before
    // the tail's deadline, or timeout correction loses the tail to it.
    const unattestedTail = await fixture();
    tipAtZero(unattestedTail.api);
    expect(
      await Effect.runPromise(
        resolveCommitAppendFenceEndTimeCapLocal(
          unattestedTail.api,
          fetchConfig,
        ),
      ),
    ).toBe(2_000 + timeoutMs - 1);
    const attested = await fixture(true);
    tipAtZero(attested.api);
    expect(
      await Effect.runPromise(
        resolveCommitAppendFenceEndTimeCapLocal(attested.api, fetchConfig),
      ),
    ).toBeUndefined();
    // An unattested head fences first, on chain as well, whatever follows it:
    // the earliest deadline wins, not the youngest node's or the last one's.
    for (const tailApplied of [false, true]) {
      const unattestedHead = await fixture(tailApplied, false);
      tipAtZero(unattestedHead.api);
      expect(
        await Effect.runPromise(
          resolveCommitAppendFenceEndTimeCapLocal(
            unattestedHead.api,
            fetchConfig,
          ),
        ),
      ).toBe(1_000 + timeoutMs - 1);
    }
  });
  it("refuses to fence an expired unattested suffix with the build's own expired-suffix error", async () => {
    const fetchConfig = {
      stateQueueAddress: address,
      stateQueuePolicyId: policyId,
    };
    const timeoutMs = Number(SDK.DA_ATTESTATION_TIMEOUT_MS);
    const cases = [
      // An unattested tail behind an attested head, its deadline 2000 + T.
      { queue: await fixture(), deadlineMs: 2_000 + timeoutMs },
      // An unattested head, its deadline 1000 + T, before an attested tail.
      { queue: await fixture(true, false), deadlineMs: 1_000 + timeoutMs },
    ];
    for (const { queue, deadlineMs } of cases) {
      const fence = () =>
        Effect.runPromise(
          resolveCommitAppendFenceEndTimeCapLocal(queue.api, fetchConfig),
        );
      // One slot before the deadline the node still caps the end.
      const tip = registerTestL1Tip(queue.api, deadlineMs / 1_000 - 1);
      expect(await fence()).toBe(deadlineMs - 1);
      // At the deadline no end remains below it: the fence refuses exactly
      // as the build's fence references do, rather than leaving an end cap
      // in the past for a later validity check to trip over.
      tip.setTipSlot(deadlineMs / 1_000);
      await expect(fence()).rejects.toThrow(
        "Commit paused until expired unattested suffix is corrected",
      );
      await expect(
        Effect.runPromise(
          resolveCommitAppendFenceReferencesLocal(
            queue.api,
            fetchConfig,
            queue.queue[2]!,
          ),
        ),
      ).rejects.toThrow(
        "Commit paused until expired unattested suffix is corrected",
      );
    }
  });
  it("reads L1 now, so a wall clock 10 minutes fast pauses no commit", async () => {
    const fetchConfig = {
      stateQueueAddress: address,
      stateQueuePolicyId: policyId,
    };
    const timeoutMs = Number(SDK.DA_ATTESTATION_TIMEOUT_MS);
    // The tail's deadline is 2000 + T. L1 now is one slot before it; the wall
    // clock is 10 minutes past it.
    const queue = await fixture();
    const tip = registerTestL1Tip(queue.api, (2_000 + timeoutMs) / 1_000 - 1);
    vi.useFakeTimers({ toFake: ["Date"] });
    onTestFinished(() => {
      vi.useRealTimers();
    });
    vi.setSystemTime(2_000 + timeoutMs + TEN_MINUTES_MS);
    expect(
      await Effect.runPromise(
        resolveCommitAppendFenceEndTimeCapLocal(queue.api, fetchConfig),
      ),
    ).toBe(2_000 + timeoutMs - 1);
    expect(
      await Effect.runPromise(
        resolveCommitAppendFenceReferencesLocal(
          queue.api,
          fetchConfig,
          queue.queue[2]!,
        ),
      ),
    ).toEqual({
      confirmedStateRefInput: queue.queue[0]!.utxo,
      headStateQueueNodeRefInput: queue.queue[1]!.utxo,
    });
    expect(tip.reads()).toBeGreaterThan(0);
  });
  it("pauses the commit while no L1 tip has been read, and resumes once one is", async () => {
    const fetchConfig = {
      stateQueueAddress: address,
      stateQueuePolicyId: policyId,
    };
    const queue = await fixture();
    const { registerL1TipSource } = await import("../src/l1-heads.js");
    let available = false;
    registerL1TipSource(
      [queue.api],
      () =>
        available
          ? Effect.succeed(0)
          : Effect.fail(new Error("Ogmios unreachable")),
      { slotLengthMs: 1_000, monotonicNowMs: () => (available ? 1_000 : 0) },
    );
    await expect(
      Effect.runPromise(
        resolveCommitAppendFenceEndTimeCapLocal(queue.api, fetchConfig),
      ),
    ).rejects.toThrow("Commit paused until the L1 slot is known");
    available = true;
    expect(
      await Effect.runPromise(
        resolveCommitAppendFenceEndTimeCapLocal(queue.api, fetchConfig),
      ),
    ).toBe(2_000 + Number(SDK.DA_ATTESTATION_TIMEOUT_MS) - 1);
  });
  it("refuses a commit preflight extending an expired suffix and permits an applied tail", async () => {
    const expiredAtSlot =
      Number(SDK.DA_ATTESTATION_TIMEOUT_MS + 2_000n) / 1_000;
    {
      const expired = await fixture();
      registerTestL1Tip(expired.api, expiredAtSlot);
      await expect(
        Effect.runPromise(
          resolveCommitAppendFenceReferencesLocal(
            expired.api,
            { stateQueueAddress: address, stateQueuePolicyId: policyId },
            expired.queue[2]!,
          ),
        ),
      ).rejects.toThrow("expired unattested suffix");
      const applied = await fixture(true);
      registerTestL1Tip(applied.api, expiredAtSlot);
      expect(
        await Effect.runPromise(
          resolveCommitAppendFenceReferencesLocal(
            applied.api,
            { stateQueueAddress: address, stateQueuePolicyId: policyId },
            applied.queue[2]!,
          ),
        ),
      ).toEqual({
        confirmedStateRefInput: applied.queue[0]!.utxo,
        headStateQueueNodeRefInput: applied.queue[1]!.utxo,
      });
    }
  });
});

it("records the tick's one queue classification for readiness, and returns a classification failure instead of raising it", async () => {
  const health = Ref.unsafeMake<AttestationTimeoutCorrectionHealth>({
    lastProgressAtMs: 0,
    lastQueueReadAtMs: 0,
    correctionProgress: null,
    lastFailureAtMs: 0,
    lastError: null,
    consecutiveFailures: 0,
    oldestUnattestedHeader: null,
  });
  const nowMs = Number(SDK.DA_ATTESTATION_TIMEOUT_MS) + 2_000;
  const { queue, tailHash } = await fixture();
  const timedOut = await Effect.runPromise(
    observeAndRecordAttestationTimeoutQueue(health, queue, {
      l1NowMs: nowMs,
      readAtMs: nowMs,
    }),
  );
  expect(Either.getOrThrow(timedOut)).toMatchObject({
    status: "timed-out",
    headerHash: tailHash,
  });
  const recorded = Effect.runSync(Ref.get(health));
  expect(recorded).toMatchObject({
    lastQueueReadAtMs: nowMs,
    oldestUnattestedHeader: {
      headerHash: tailHash,
      deadlineMs: 2_000 + Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    },
  });

  // An undecodable queue is unknown: nothing recorded, the failure returned.
  const undecodable = queue.map((entry, index) =>
    index === 2
      ? {
          ...entry,
          datum: {
            ...entry.datum,
            data: 7n as unknown as SDK.LinkedListNodeView["data"],
          },
        }
      : entry,
  );
  const failed = await Effect.runPromise(
    observeAndRecordAttestationTimeoutQueue(health, undecodable, {
      l1NowMs: nowMs + 1,
      readAtMs: nowMs + 1,
    }),
  );
  expect(Either.isLeft(failed)).toBe(true);
  expect(Effect.runSync(Ref.get(health))).toEqual(recorded);

  const attested = await Effect.runPromise(
    observeAndRecordAttestationTimeoutQueue(
      health,
      (await fixture(true)).queue,
      { l1NowMs: nowMs + 2, readAtMs: nowMs + 2 },
    ),
  );
  expect(Either.getOrThrow(attested)).toEqual({ status: "queue-attested" });
  expect(Effect.runSync(Ref.get(health))).toMatchObject({
    lastQueueReadAtMs: nowMs + 2,
    oldestUnattestedHeader: null,
  });
});

it("judges the DA-attestation deadline at L1 now: a local clock 10 minutes fast times nothing out", async () => {
  const health = Ref.unsafeMake<AttestationTimeoutCorrectionHealth>({
    lastProgressAtMs: 0,
    lastQueueReadAtMs: 0,
    correctionProgress: null,
    lastFailureAtMs: 0,
    lastError: null,
    consecutiveFailures: 0,
    oldestUnattestedHeader: null,
  });
  const deadlineMs = Number(SDK.DA_ATTESTATION_TIMEOUT_MS) + 2_000;
  // L1 now is well before the tail's deadline (more than the alert lead);
  // the local clock reads 10 minutes past it.
  const l1NowMs = deadlineMs - 1_000_000;
  const readAtMs = deadlineMs + TEN_MINUTES_MS;
  const { queue, tailHash } = await fixture();
  const observed = await Effect.runPromise(
    observeAndRecordAttestationTimeoutQueue(health, queue, {
      l1NowMs,
      readAtMs,
    }),
  );
  expect(Either.getOrThrow(observed)).toMatchObject({
    status: "waiting",
    headerHash: tailHash,
  });
  expect(Effect.runSync(Ref.get(health))).toMatchObject({
    lastQueueReadAtMs: readAtMs,
  });
});

it("dispatches retained attempt recovery after another actor removes the target, and reopens completed targets on rollback", async () => {
  const { queue, tailHash } = await fixture(true);
  const afterRemoval = queue.slice(0, -1);
  expect(
    await Effect.runPromise(
      observeAttestationTimeoutQueue(afterRemoval, 9_000_000n, 120_000n),
    ),
  ).toEqual({ status: "queue-attested" });
  expect(
    timeoutCorrectionJournalNeedsRecovery(
      { completed: false, targetHeaderHash: tailHash },
      afterRemoval,
    ),
  ).toBe(true);
  expect(
    timeoutCorrectionJournalNeedsRecovery(
      { completed: true, targetHeaderHash: tailHash },
      afterRemoval,
    ),
  ).toBe(false);
  expect(
    timeoutCorrectionJournalNeedsRecovery(
      { completed: true, targetHeaderHash: tailHash },
      queue,
    ),
  ).toBe(true);
  expect(timeoutCorrectionJournalNeedsRecovery(undefined, queue)).toBe(false);
});

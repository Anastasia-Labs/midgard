import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import { observeAndRecordAttestationTimeoutQueue } from "../src/fibers/attestation-timeout-correction.js";
import {
  observeAttestationTimeoutQueue,
  timeoutCorrectionJournalNeedsRecovery,
} from "../src/services/attestation-timeout-observation.js";
import type { AttestationTimeoutCorrectionHealth } from "../src/services/globals.js";
import { resolveCommitAppendFenceReferencesLocal } from "../src/workers/commit-block-header/state-queue.js";

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
const fixture = async (tailApplied = false) => {
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
    assets: { lovelace: 3_000_000n, [toUnit(policyId, assetName)]: 1n },
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
        da_attestation: { Attested: { da_bond_asset_name: "11".repeat(32) } },
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
          ? { Attested: { da_bond_asset_name: "22".repeat(32) } }
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
  it("refuses a commit preflight extending an expired suffix and permits an applied tail", async () => {
    const now = vi
      .spyOn(Date, "now")
      .mockReturnValue(Number(SDK.DA_ATTESTATION_TIMEOUT_MS + 2_000n));
    try {
      const expired = await fixture();
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
    } finally {
      now.mockRestore();
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
    observeAndRecordAttestationTimeoutQueue(health, queue, nowMs),
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
    observeAndRecordAttestationTimeoutQueue(health, undecodable, nowMs + 1),
  );
  expect(Either.isLeft(failed)).toBe(true);
  expect(Effect.runSync(Ref.get(health))).toEqual(recorded);

  const attested = await Effect.runPromise(
    observeAndRecordAttestationTimeoutQueue(
      health,
      (await fixture(true)).queue,
      nowMs + 2,
    ),
  );
  expect(Either.getOrThrow(attested)).toEqual({ status: "queue-attested" });
  expect(Effect.runSync(Ref.get(health))).toMatchObject({
    lastQueueReadAtMs: nowMs + 2,
    oldestUnattestedHeader: null,
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

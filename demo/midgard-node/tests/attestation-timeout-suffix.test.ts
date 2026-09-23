import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  observeAttestationTimeoutQueue,
  timeoutCorrectionJournalNeedsRecovery,
} from "../src/services/attestation-timeout-observation.js";
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
        observeAttestationTimeoutQueue(queue, 3_550_000n, 120_000n),
      ),
    ).toMatchObject({ status: "near-timeout", headerHash: tailHash });
    expect(
      await Effect.runPromise(
        observeAttestationTimeoutQueue(queue, 3_602_000n, 120_000n),
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
    const now = vi.spyOn(Date, "now").mockReturnValue(3_602_000);
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

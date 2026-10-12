import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { currentPromiseScheduling } from "../src/availability/promise-current-scheduling.js";
import { promiseSchedulingSource } from "../src/availability/promise-scheduling-source.js";
import type { StateQueueHeaderRecord } from "../src/domain.js";
import { makePayloadFixture } from "./helpers.js";
import { followerBoundary } from "./helpers/follower-boundary.js";

const fixture = async () => {
  const payload = await makePayloadFixture(1);
  const policy = "ab".repeat(28),
    identity = "cd".repeat(28);
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: identity,
    headerHash: payload.headerHash,
    payload: Uint8Array.of(1, 2, 3, 4),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: 14020,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });
  const digest = computeDaSha256Hash(
    Buffer.from(SDK.encodeDaAvailabilityCommitment(commitment), "hex"),
  ).toString("hex");
  const deployment = {
    hubOraclePolicyId: identity,
    contracts: {
      stateQueue: { policyId: policy, spendingScriptAddress: "queue" },
      correctionLock: { spendingScriptAddress: "lock" },
      availabilityChallenge: {
        policyId: "ef".repeat(28),
        spendingScriptAddress: "availability",
      },
    },
  } as SDK.DaAvailabilityDeployment;
  const point = { slot: 50, blockNo: 5, blockHash: "12".repeat(32) };
  const cutoff = Number(payload.header.endTime) + 720000;
  const boundary = {
    slot: Math.ceil(cutoff / 1000),
    blockNo: 50,
    blockHash: "34".repeat(32),
  };
  const status = {
    Attested: { commitment_hash: SDK.daAvailabilityCommitmentHash(commitment) },
  };
  const output = (
    index: number,
    address: string,
    unit: string,
    datum: string,
  ): UTxO => ({
    txHash: "56".repeat(32),
    outputIndex: index,
    address,
    assets: { lovelace: 3000000n, [unit]: 1n },
    datum,
  });
  const root = output(
    0,
    "queue",
    policy + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    SDK.encodeLinkedListNodeView({
      key: "Empty",
      next: { Key: { key: payload.headerHash } },
      data: Data.castTo(SDK.makeGenesisConfirmedState(0n), SDK.ConfirmedState),
    }),
  );
  const node = output(
    1,
    "queue",
    policy + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + payload.headerHash,
    SDK.encodeLinkedListNodeView({
      key: { Key: { key: payload.headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header: payload.header, da_attestation: status, proven_fraud: null },
        SDK.StateQueueNode,
      ),
    }),
  );
  const lock = output(
    2,
    "lock",
    SDK.correctionLockUnit(identity),
    Data.to("Idle", SDK.CorrectionLockDatum),
  );
  const header: StateQueueHeaderRecord = {
    deploymentFingerprint: "78".repeat(32),
    headerHash: payload.headerHash,
    stateQueueOutRef: `${node.txHash}#1`,
    blockAssetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + payload.headerHash,
    header: payload.header,
    computedHeaderHash: payload.headerHash,
    daAttestation: status,
    observedChainPoint: {
      slot: point.slot,
      blockHeight: point.blockNo,
      blockHash: point.blockHash,
    },
    finalized: true,
    status: "attested",
    validationErrors: [],
    updatedAt: "2026-10-02T00:00:00Z",
  };
  const args = {
    deployment,
    boundary,
    canonicalTimeMs: cutoff,
    openWindowMs: 720000,
    rawSnapshot: {
      stateQueueUtxos: [root, node],
      availabilityUtxos: [] as UTxO[],
      correctionLockUtxos: [lock],
    },
    complete: true,
    liabilities: [{ header, commitment, commitmentDigest: digest }],
    readCanonicalPoint: vi.fn(async () => ({ point, tip: boundary })),
    assertCurrent: vi.fn(async () => {}),
  };
  return { args, digest, node, root, lock, cutoff };
};

describe("current timing scheduling versus protected restoration", () => {
  it("omits only current scheduling after exact cutoff and complete authenticated absence", async () => {
    const f = await fixture();
    expect(
      await currentPromiseScheduling({
        ...f.args,
        canonicalTimeMs: f.cutoff - 1,
      }),
    ).toEqual(new Set([f.digest]));
    expect(await currentPromiseScheduling(f.args)).toEqual(new Set());
    expect(f.args.readCanonicalPoint).toHaveBeenCalledWith({
      slot: 50,
      blockNo: 5,
      blockHash: "12".repeat(32),
    });
    expect(f.args.liabilities).toHaveLength(1);
    expect(f.args.liabilities[0]!.commitmentDigest).toBe(f.digest);
  });
  it("does not infer absence from missing root, filtered discovery or incomplete raw coverage", async () => {
    const f = await fixture();
    await expect(
      currentPromiseScheduling({ ...f.args, complete: false }),
    ).rejects.toThrow("Complete");
    await expect(
      currentPromiseScheduling({
        ...f.args,
        rawSnapshot: { ...f.args.rawSnapshot, stateQueueUtxos: [f.node] },
      }),
    ).rejects.toThrow();
    const challenged = {
      ...f.node,
      datum: SDK.encodeLinkedListNodeView({
        key: { Key: { key: f.args.liabilities[0]!.header.headerHash } },
        next: "Empty",
        data: Data.castTo(
          {
            header: f.args.liabilities[0]!.header.header,
            da_attestation: {
              Challenged: {
                commitment_hash: SDK.daAvailabilityCommitmentHash(
                  f.args.liabilities[0]!.commitment,
                ),
                challenge_asset_name: "90".repeat(32),
              },
            },
            proven_fraud: null,
          },
          SDK.StateQueueNode,
        ),
      }),
    };
    await expect(
      currentPromiseScheduling({
        ...f.args,
        rawSnapshot: {
          ...f.args.rawSnapshot,
          stateQueueUtxos: [f.root, challenged],
        },
      }),
    ).rejects.toThrow("Authenticated challenge record is missing");
  });
  it("requires original checkpoint ancestry and the exact current selected point/height", async () => {
    const f = await fixture();
    await expect(
      currentPromiseScheduling({
        ...f.args,
        readCanonicalPoint: async () => null,
      }),
    ).rejects.toThrow("not canonical");
    await expect(
      currentPromiseScheduling({
        ...f.args,
        readCanonicalPoint: async () => ({
          point: { slot: 50, blockNo: 5, blockHash: "12".repeat(32) },
          tip: { ...f.args.boundary, blockNo: 51 },
        }),
      }),
    ).rejects.toThrow("exact boundary");
    await expect(
      currentPromiseScheduling({
        ...f.args,
        assertCurrent: async () => {
          throw new Error("rollback generation changed");
        },
      }),
    ).rejects.toThrow("rollback generation");
  });
  it("authenticates raw root and idle correction lock even with no old liabilities", async () => {
    const f = await fixture();
    expect(
      await currentPromiseScheduling({ ...f.args, liabilities: [] }),
    ).toEqual(new Set());
    await expect(
      currentPromiseScheduling({
        ...f.args,
        liabilities: [],
        rawSnapshot: { ...f.args.rawSnapshot, correctionLockUtxos: [] },
      }),
    ).rejects.toThrow();
  });
  it("revalidates complete raw identities at the final fence despite unchanged point and counts", async () => {
    const f = await fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    let queue = f.args.rawSnapshot.stateQueueUtxos;
    const source = promiseSchedulingSource({
      lucid: {
        slotToUnixTime: () => f.cutoff,
        utxosAt: async (address: string) =>
          address === "queue" ? queue : address === "lock" ? [f.lock] : [],
      } as unknown as LucidEvolution,
      deployment: f.args.deployment,
      openWindowMs: 720000,
      reads: { canonicalPoint: async () => null },
      readBoundary: async (readScope) => {
        expect(readScope).toBe(scope);
        return followerBoundary(f.args.boundary);
      },
      assertActuationCurrent: async () => {},
    });
    try {
      const receipt = await source.capture({
        liabilities: [],
        rawSnapshot: f.args.rawSnapshot,
        complete: true,
        scope,
        point: f.args.boundary,
        generation: 0,
      });
      await expect(
        source.assertCurrent(receipt.digest, scope),
      ).resolves.toBeUndefined();
      queue = [
        {
          ...f.root,
          assets: { ...f.root.assets, lovelace: f.root.assets.lovelace + 1n },
        },
        f.node,
      ];
      await expect(source.assertCurrent(receipt.digest, scope)).rejects.toThrow(
        "raw evidence changed",
      );
    } finally {
      scope.close();
    }
  });
  it("does not let an older overlapping capture replace the newer certificate", async () => {
    const f = await fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    let releaseFirst!: () => void;
    let enteredFirst!: () => void;
    const entered = new Promise<void>((resolve) => {
      enteredFirst = resolve;
    });
    const firstBarrier = new Promise<void>((resolve) => {
      releaseFirst = resolve;
    });
    let first = true;
    let queue = [f.root, f.node];
    const source = promiseSchedulingSource({
      lucid: {
        slotToUnixTime: () => f.cutoff,
        utxosAt: async (address: string) =>
          address === "queue" ? queue : address === "lock" ? [f.lock] : [],
      } as unknown as LucidEvolution,
      deployment: f.args.deployment,
      openWindowMs: 720000,
      reads: { canonicalPoint: async () => null },
      readBoundary: async () => followerBoundary(f.args.boundary),
      assertActuationCurrent: async () => {
        if (first) {
          first = false;
          enteredFirst();
          await firstBarrier;
        }
      },
    });
    const input = {
      liabilities: [],
      rawSnapshot: f.args.rawSnapshot,
      complete: true,
      scope,
      point: f.args.boundary,
      generation: 0,
    };
    const older = source.capture(input);
    const olderResult = older.then(
      () => "accepted",
      (error: unknown) => String(error),
    );
    try {
      await entered;
      queue = [
        {
          ...f.root,
          assets: { ...f.root.assets, lovelace: f.root.assets.lovelace + 1n },
        },
        f.node,
      ];
      const newer = await source.capture({
        ...input,
        rawSnapshot: { ...f.args.rawSnapshot, stateQueueUtxos: queue },
      });
      releaseFirst();
      expect(await olderResult).toContain("superseded");
      await expect(
        source.assertCurrent(newer.digest, scope),
      ).resolves.toBeUndefined();
    } finally {
      releaseFirst();
      await olderResult;
      scope.close();
    }
  });
});

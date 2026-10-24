import type { AvailabilityOperationRecord } from "@al-ft/midgard-core/availability-operation-journal";
import {
  computeFraudProofRawL1PointId,
  FraudProofL1CheckpointChangedError,
} from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import { CML, utxoToCore } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { deriveWatcherDaBondPoolObservation } from "../../src/availability/pool-observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "../../src/availability/published-payload.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  DA_BOND_POOL_ADDRESS,
  DA_BOND_POOL_POLICY_ID,
  daBondPoolUtxo,
  parametersFixture,
} from "../support/availability-challenge-fixture.js";
import {
  availabilityFollowerIo,
  availabilityFollowerL1,
} from "../support/availability-follower-l1.js";
import {
  availabilityOperationIntent as intent,
  deferredAvailabilityRead as deferred,
} from "../support/availability-operation-intent.js";

const sdk = vi.hoisted(() => ({ release: vi.fn() }));
const io = availabilityFollowerIo();
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityChallengeSnapshotFromUtxos: async (
    _deployment: unknown,
    headerHash: string,
  ) => ({ headerHash }),
  // SDK walking is tested separately; drive the actual watcher reader wiring.
  resolveDaAvailabilityWorkflowRelease: sdk.release,
}));
// Real capture/sibling drain; admission and decoding have separate fixtures.
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation: () => undefined,
}));
vi.mock("../../src/runtime/deployment-identity.js", () => ({
  assertVerifiedWatcherDeploymentIdentity: () => undefined,
}));

const nextTurn = () => new Promise<void>((resolve) => setImmediate(resolve));
const identity = {
  manifestId: "11".repeat(32),
  blueprintHash: "22".repeat(32),
} as VerifiedWatcherDeploymentIdentity;
const deployment = {
  hubOraclePolicyId: "33".repeat(28),
  contracts: {
    availabilityChallenge: {
      spendingScriptAddress: "availability",
      policyId: "44".repeat(28),
    },
    stateQueue: {
      spendingScriptAddress: "queue",
      policyId: "55".repeat(28),
    },
    correctionLock: { spendingScriptAddress: "lock" },
    daBondPool: {
      spendingScriptAddress: DA_BOND_POOL_ADDRESS,
      policyId: DA_BOND_POOL_POLICY_ID,
    },
  },
} as SDK.DaAvailabilityDeployment;
const observation = (slot: number) =>
  ({
    deploymentIdentityDigest: identity.manifestId,
    observationDigest: slot.toString(),
    nativePoint: {
      slot: slot.toString(),
      blockNo: slot.toString(),
      blockHash: "66".repeat(32),
      finalityDepth: "30",
    },
    finalizedHeaders: [],
  }) as unknown as WatcherAuthenticatedStateQueueObservation;
const fixture = () => {
  const l1 = availabilityFollowerL1(io);
  return {
    l1,
    intake: createWatcherAvailabilityObservation({
      identity,
      deployment,
      l1,
      confirmationDepth: 30,
    }),
  };
};
beforeEach(() => {
  for (const mock of [...Object.values(io), sdk.release]) mock.mockReset();
  io.address.mockResolvedValue([]);
  io.inclusion.mockResolvedValue(null);
  io.outrefs.mockResolvedValue({ outputs: [], spends: [] });
});

describe("availability captures on the follower's reads", () => {
  it.each([
    [1, "reaches the transaction reader"],
    [30, "refuses the tip view"],
  ] as const)(
    "reads published proof payload at minimum depth %i and %s at finality depth 1",
    async (minimumConfirmationDepth, _outcome) => {
      const { l1 } = fixture();
      const current = {
        ...observation(10),
        nativePoint: { ...observation(10).nativePoint, finalityDepth: "1" },
        finalizedHeaders: [
          {
            headerHash: "77".repeat(28),
            daAvailability: {
              Published: { terminal_commitment: "88".repeat(32) },
            },
          },
        ],
      } as unknown as WatcherAuthenticatedStateQueueObservation;
      const payloadSource = createWatcherL1AvailabilityPayloadSource({
        identity,
        deployment,
        l1,
        minimumConfirmationDepth,
        lucid: { slotToUnixTime: (slot) => slot * 1000 },
        currentObservation: () => current,
      });
      io.history.mockResolvedValue({
        transactions: [
          { txHash: "first", inclusionPoint: observation(1).nativePoint },
        ],
      });
      io.transaction.mockRejectedValue(
        new Error("authenticated payload reader reached"),
      );
      await expect(
        payloadSource.fetchPayloadByHeaderHash("77".repeat(28)),
      ).rejects.toThrow(
        minimumConfirmationDepth === 1
          ? "authenticated payload reader reached"
          : "authenticated deployment observation",
      );
      if (minimumConfirmationDepth === 1)
        expect(io.transaction).toHaveBeenCalledWith(
          expect.objectContaining({ txHash: "first" }),
        );
      else expect(io.transaction).not.toHaveBeenCalled();
    },
  );

  it("refuses a transaction shallower than the reader's minimum depth", async () => {
    const { intake } = fixture();
    const signed = intent();
    io.inclusion.mockResolvedValue({ blockNo: "7", pointId: "inclusion" });
    io.transaction.mockResolvedValue({ confirmationDepth: 29 });
    await expect(intake.operation(observation(11), signed)).rejects.toThrow(
      "shallower than the required confirmation depth",
    );
  });

  it("checks the capture's point is canonical only after every read, and refuses a point a rollback removed", async () => {
    const { intake } = fixture();
    const started = deferred();
    const release = deferred();
    io.address.mockImplementation(async () => {
      started.resolve();
      await release.promise;
      return [];
    });
    io.status.mockResolvedValue({
      kind: "point_not_canonical",
      detail: "rolled back",
    });
    const first = intake.snapshot(observation(10), "77".repeat(28));
    const refused = first.catch((error: unknown) => error);
    await started.promise;
    await nextTurn();
    expect(io.address).toHaveBeenCalledTimes(4);
    expect(io.status).not.toHaveBeenCalled();
    release.resolve();
    const error = await refused;
    expect(error).toBeInstanceOf(FraudProofL1CheckpointChangedError);
    expect((error as Error).message).toContain("left the canonical chain");
    expect(io.status.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "10",
    ]);
    for (const order of io.address.mock.invocationCallOrder)
      expect(order).toBeLessThan(io.status.mock.invocationCallOrder[0]!);
  });

  it("runs captures at different points concurrently, each checked at its own point", async () => {
    const { intake } = fixture();
    const started = deferred();
    const release = deferred();
    io.address.mockImplementation(async () => {
      started.resolve();
      await release.promise;
      return [];
    });
    const first = intake.snapshot(observation(10), "77".repeat(28));
    await started.promise;
    const second = intake.operation(observation(11), intent());
    await expect(second).resolves.toEqual({
      status: "unspent",
      currentSlot: 11,
    });
    expect(io.outrefs.mock.calls[0]![0].point.slot).toBe("11");
    release.resolve();
    await expect(first).resolves.toMatchObject({ headerHash: "77".repeat(28) });
    expect(io.status.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "11",
      "10",
    ]);
  });

  it("preserves the authenticated current block for included-intent history pruning", async () => {
    const { intake } = fixture();
    const signed = intent();
    const body = CML.Transaction.from_cbor_hex(signed.signedCbor).body();
    io.inclusion.mockResolvedValue({
      blockNo: "7",
      pointId: "canonical-inclusion",
    });
    io.transaction.mockResolvedValue({
      bodyCbor: body.to_cbor_hex(),
      confirmationDepth: 35,
    });
    const current = {
      ...observation(11),
      nativePoint: { ...observation(11).nativePoint, blockNo: "19" },
    };
    await expect(intake.operation(current, signed)).resolves.toEqual({
      status: "included",
      txHash: signed.txHash,
      inclusionPoint: "canonical-inclusion",
      confirmationDepth: 34,
      currentSlot: 11,
      currentBlockNo: 19,
    });
  });

  it("reports a verified foreign spend with its depth below the finalized tip", async () => {
    const { intake } = fixture();
    const spent = `${"aa".repeat(32)}#0`;
    const collateral = `${"bb".repeat(32)}#1`;
    io.outrefs.mockResolvedValue({
      outputs: [],
      spends: [
        {
          outRef: spent,
          spendingTxHash: "cc".repeat(32),
          spendPoint: {
            slot: "5",
            blockNo: "5",
            blockHash: "dd".repeat(32),
            pointId: "spend-point",
          },
        },
      ],
    });
    await expect(
      intake.operation(observation(11), {
        ...intent(),
        spentOutRefs: [spent],
        collateralOutRefs: [collateral],
      }),
    ).resolves.toEqual({
      status: "inputs_missing",
      currentSlot: 11,
      missingOutRefs: [spent, collateral],
      // Six blocks to the observed point plus its twenty-nine successors.
      foreignSpends: [
        {
          outRef: spent,
          spendingTxHash: "cc".repeat(32),
          spendPoint: "spend-point",
          confirmationDepth: 35,
        },
      ],
    });
  });

  it("settles every sibling read of a failed snapshot before it fails", async () => {
    const { intake } = fixture();
    const release = deferred();
    let settled = false;
    io.address.mockImplementation(async ({ address }: { address: string }) => {
      if (address === "availability") throw new Error("address read failed");
      if (address === "queue") {
        await release.promise;
        settled = true;
      }
      return [];
    });
    const first = intake.snapshot(observation(10), "77".repeat(28));
    let failure: unknown;
    void first.catch((error: unknown) => {
      failure = error;
    });
    await nextTurn();
    expect(failure).toBeUndefined();
    release.resolve();
    await expect(first).rejects.toThrow("address read failed");
    expect(settled).toBe(true);
    expect(io.status).not.toHaveBeenCalled();
  });

  it("refuses an operation whose inputs may have been pruned instead of reading them as missing", async () => {
    const { intake } = fixture();
    const spent = `${"aa".repeat(32)}#0`;
    io.outrefs.mockResolvedValue({
      outputs: [],
      spends: [],
      beyondRetention: [spent],
    });
    await expect(
      intake.operation(observation(11), {
        ...intent(),
        spentOutRefs: [spent],
        collateralOutRefs: [],
      }),
    ).rejects.toThrow("may have been pruned");
  });

  describe("workflow release readers (P20)", () => {
    const released = {
      reason: "challenge-closed",
      txHash: "ce".repeat(32),
      spendPoint: "9:ab",
      confirmationDepth: 32,
    };
    const spendPoint = (slot: number) => ({
      slot: slot.toString(),
      blockNo: slot.toString(),
      blockHash: "77".repeat(32),
      pointId: computeFraudProofRawL1PointId({
        slot: slot.toString(),
        blockNo: slot.toString(),
        blockHash: "77".repeat(32),
      }),
    });
    const captured = async (slot = 11) => {
      const { intake } = fixture();
      let readers!: SDK.DaAvailabilityForeignSpendReaders;
      sdk.release.mockImplementation(async (given) => {
        readers = given;
        return released;
      });
      const open = {
        intent: { id: "open" },
        state: "confirmed",
      } as unknown as AvailabilityOperationRecord;
      await expect(
        intake.workflowRelease(observation(slot), open, "88".repeat(28)),
      ).resolves.toBe(released);
      expect(sdk.release).toHaveBeenCalledWith(
        expect.anything(),
        open,
        "88".repeat(28),
        30,
      );
      expect(io.status.mock.calls.map(([input]) => input.point.slot)).toEqual([
        slot.toString(),
      ]);
      return readers;
    };

    it("projects the actual tip from inclusive native finalityDepth", async () => {
      const readers = await captured(11);
      await expect(readers.readBoundary()).resolves.toStrictEqual({
        pointId: computeFraudProofRawL1PointId({
          slot: "11",
          blockNo: "11",
          blockHash: "66".repeat(32),
        }),
        blockNo: 40,
      });
    });

    it("reads a spend only as the verified exact-outref read at the finalized point reports it", async () => {
      const readers = await captured(11);
      const outRef = `${"aa".repeat(32)}#1`;
      io.outrefs.mockResolvedValueOnce({
        outputs: [],
        spends: [
          {
            outRef,
            spendingTxHash: "bb".repeat(32),
            spendPoint: spendPoint(9),
          },
        ],
      });
      await expect(
        readers.fetchSpend({ txHash: "aa".repeat(32), outputIndex: 1 }),
      ).resolves.toStrictEqual({
        transactionId: "bb".repeat(32),
        point: { slot: 9, blockHash: "77".repeat(32) },
      });
      expect(io.outrefs).toHaveBeenLastCalledWith(
        expect.objectContaining({
          outRefs: [outRef],
          point: expect.objectContaining({ slot: "11", blockNo: "11" }),
        }),
      );
      io.predecessor.mockResolvedValueOnce(spendPoint(8));
      await expect(readers.fetchAncestor(9)).resolves.toStrictEqual({
        slot: 8,
        blockHash: "77".repeat(32),
      });
      // Unspent, or a spend the raw bytes did not bear out: no spend.
      io.outrefs.mockResolvedValueOnce({
        outputs: [{ outRef }],
        spends: [],
      });
      await expect(
        readers.fetchSpend({ txHash: "aa".repeat(32), outputIndex: 1 }),
      ).resolves.toBeUndefined();
      io.outrefs.mockResolvedValueOnce({ outputs: [], spends: [] });
      await expect(
        readers.fetchSpend({ txHash: "aa".repeat(32), outputIndex: 1 }),
      ).resolves.toBeUndefined();
    });

    it("reads the spender's raw transaction at its exact block, never above the finalized point", async () => {
      const readers = await captured(11);
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        CML.TransactionOutputList.new(),
        7n,
      );
      const txHash = CML.hash_transaction(body).to_hex();
      const cbor = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      ).to_cbor_hex();
      const ancestor = { slot: 8, blockHash: "77".repeat(32) };
      const at = { slot: 9, blockHash: "77".repeat(32) };
      io.inclusion.mockResolvedValueOnce(spendPoint(9));
      io.transaction.mockResolvedValueOnce({
        txHash,
        bodyCbor: body.to_cbor_hex(),
        witnessSetCbor: CML.TransactionWitnessSet.new().to_cbor_hex(),
        isValid: true,
        confirmationDepth: 30,
      });
      await expect(
        readers.readTransaction({ ancestor, point: at, txHash }),
      ).resolves.toStrictEqual({
        txHash,
        point: { ...at, blockNo: 9 },
        cbor,
      });
      expect(io.transaction).toHaveBeenLastCalledWith(
        expect.objectContaining({
          txHash,
          expectedInclusionPoint: spendPoint(9),
        }),
      );
      // Included elsewhere, or not at all: not this spend.
      io.inclusion.mockResolvedValueOnce(spendPoint(10));
      await expect(
        readers.readTransaction({ ancestor, point: at, txHash }),
      ).resolves.toBeUndefined();
      io.inclusion.mockResolvedValueOnce(null);
      await expect(
        readers.readTransaction({ ancestor, point: at, txHash }),
      ).resolves.toBeUndefined();
      io.inclusion.mockResolvedValueOnce(spendPoint(12));
      await expect(
        readers.readTransaction({
          ancestor,
          point: { slot: 12, blockHash: "77".repeat(32) },
          txHash,
        }),
      ).rejects.toThrow(
        "Availability input spend lies above the canonical boundary",
      );
    });
  });
});

describe("DA bond pool read (spec #685 E5, #691)", () => {
  const raw = (utxo: ReturnType<typeof daBondPoolUtxo>) => ({
    outRef: `${utxo.txHash}#${utxo.outputIndex}`,
    outputCbor: utxoToCore(utxo).output().to_cbor_hex(),
  });

  it("reads the pool address at the pinned finalized point, with or without pending headers", async () => {
    const { intake } = fixture();
    const pool = daBondPoolUtxo(5_000_000_000n);
    io.address.mockResolvedValue([raw(pool)]);
    await expect(intake.pool(observation(12))).resolves.toMatchObject({
      txHash: pool.txHash,
      outputIndex: pool.outputIndex,
      assets: pool.assets,
      datum: pool.datum,
    });
    expect(io.status.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "12",
    ]);
    expect(io.address).toHaveBeenCalledWith(
      expect.objectContaining({
        address: DA_BOND_POOL_ADDRESS,
        point: expect.objectContaining({ slot: "12" }),
      }),
    );
  });

  it("reads a Withdrawing pool the local source re-encodes canonically", async () => {
    const { intake } = fixture();
    const pool = daBondPoolUtxo(5_000_000_000n, {
      Withdrawing: { unlock_at: 1_900_000_000_000n },
    });
    const output = utxoToCore(pool).output();
    // A raw read may serve `to_canonical_cbor_hex` of an output, which turns
    // lucid's indefinite-length datum list definite-length.
    const outputCbor = output.to_canonical_cbor_hex();
    expect(outputCbor).not.toBe(output.to_cbor_hex());
    io.address.mockResolvedValue([
      { outRef: `${pool.txHash}#${pool.outputIndex}`, outputCbor },
    ]);
    const read = await intake.pool(observation(12));
    expect(read?.datum).not.toBe(pool.datum);
    expect(
      deriveWatcherDaBondPoolObservation({
        pool: read,
        policyId: DA_BOND_POOL_POLICY_ID,
        parameters: parametersFixture(),
        nowMs: 0n,
      }),
    ).toMatchObject({
      state: "withdrawing",
      unlockAt: "1900000000000",
      alerts: { withdrawing: true },
    });
  });

  it("reads no pool NFT as missing and fails closed on a duplicated pool", async () => {
    const { intake } = fixture();
    io.address.mockResolvedValue([]);
    await expect(intake.pool(observation(12))).resolves.toBeUndefined();
    const pool = daBondPoolUtxo(5_000_000_000n);
    io.address.mockResolvedValue([raw(pool), raw({ ...pool, outputIndex: 1 })]);
    await expect(intake.pool(observation(13))).rejects.toThrow(
      "more than one output",
    );
  });

  it("refuses a pool read below the deployment's finality depth", async () => {
    const { intake } = fixture();
    await expect(
      intake.pool({
        ...observation(12),
        nativePoint: { ...observation(12).nativePoint, finalityDepth: "1" },
      } as unknown as WatcherAuthenticatedStateQueueObservation),
    ).rejects.toThrow("exact finalized deployment observation");
    expect(io.address).not.toHaveBeenCalled();
  });
});

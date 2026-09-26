import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "../../src/availability/published-payload.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";

const sourcePolicy = vi.hoisted(() => ({
  observationDepth: "release_finality" as "release_finality" | "inclusion",
}));
const io = vi.hoisted(() => ({
  pin: vi.fn(),
  address: vi.fn(),
  inclusion: vi.fn(),
  outrefs: vi.fn(),
  history: vi.fn(),
  transaction: vi.fn(),
}));
vi.mock("@al-ft/midgard-fault-proofs", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-fault-proofs")>()),
  localKupmiosHttpOgmiosRawSourceDetails: () => ({
    deploymentIdentityDigest: "11".repeat(32),
    blueprintHash: "22".repeat(32),
    observationDepth: sourcePolicy.observationDepth,
    // Mocked source release depth; the observations below sit at this depth.
    confirmationDepth: 30,
  }),
  pinAdmittedLocalKupmiosBoundaryAtPoint: io.pin,
  readAdmittedLocalKupmiosAddressUtxosAtPoint: io.address,
  readAdmittedLocalKupmiosTransactionInclusion: io.inclusion,
  readAdmittedLocalKupmiosUtxosByOutRefAtPoint: io.outrefs,
  readAdmittedLocalKupmiosUnitHistoryAtPoint: io.history,
  readAdmittedLocalKupmiosRawTransaction: io.transaction,
}));
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityChallengeSnapshotFromUtxos: async (
    _deployment: unknown,
    headerHash: string,
  ) => ({ headerHash }),
}));
// Admission and decoding have separate fixture suites. Keep the real shared
// capture lock and sibling-drain helper here, controlling only the I/O edges.
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation: () => undefined,
}));
vi.mock("../../src/runtime/deployment-identity.js", () => ({
  assertVerifiedWatcherDeploymentIdentity: () => undefined,
}));

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
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
  const source = {} as LocalKupmiosFraudProofRawSource;
  return {
    source,
    intake: createWatcherAvailabilityObservation({
      identity,
      deployment,
      source,
    }),
  };
};
const intent = () => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    0n,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedCbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
    spentOutRefs: [],
    collateralOutRefs: [],
  } as unknown as AvailabilityOperationIntent;
};
beforeEach(() => {
  sourcePolicy.observationDepth = "release_finality";
  for (const mock of Object.values(io)) mock.mockReset();
  io.pin.mockResolvedValue(undefined);
  io.address.mockResolvedValue([]);
  io.inclusion.mockResolvedValue(null);
  io.outrefs.mockResolvedValue([]);
});

describe("availability captures sharing a local source", () => {
  it.each(["inclusion", "release_finality"] as const)(
    "reads published proof payload at %s authority without changing availability actuation",
    async (observationDepth) => {
      sourcePolicy.observationDepth = observationDepth;
      const { source } = fixture();
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
        rawSource: source,
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
        observationDepth === "inclusion"
          ? "authenticated payload reader reached"
          : "authenticated deployment observation",
      );
      if (observationDepth === "inclusion")
        expect(io.transaction).toHaveBeenCalledWith(
          expect.objectContaining({ minimumConfirmationDepth: 1 }),
        );
      else expect(io.transaction).not.toHaveBeenCalled();
    },
  );

  it("pins an operation only after a concurrent snapshot has finished all address reads", async () => {
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
    await nextTurn();
    expect(io.address).toHaveBeenCalledTimes(3);
    expect(io.pin.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "10",
    ]);
    expect(io.inclusion).not.toHaveBeenCalled();
    release.resolve();
    await expect(first).resolves.toMatchObject({ headerHash: "77".repeat(28) });
    await expect(second).resolves.toEqual({
      status: "unspent",
      currentSlot: 11,
    });
    expect(io.pin.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "10",
      "11",
    ]);
    expect(io.outrefs.mock.calls[0]![0].point.slot).toBe("11");
  });

  it("drains a failed snapshot's siblings before releasing a queued capture", async () => {
    const { intake } = fixture();
    const started = deferred();
    const release = deferred();
    io.address.mockImplementation(async ({ address }: { address: string }) => {
      if (address === "availability") throw new Error("address read failed");
      if (address === "queue") {
        started.resolve();
        await release.promise;
      }
      return [];
    });
    const first = intake.snapshot(observation(10), "77".repeat(28));
    const failed = expect(first).rejects.toThrow("address read failed");
    await started.promise;
    const second = intake.operation(observation(11), intent());
    await nextTurn();
    expect(io.pin).toHaveBeenCalledTimes(1);
    expect(io.inclusion).not.toHaveBeenCalled();
    release.resolve();
    await failed;
    await expect(second).resolves.toEqual({
      status: "unspent",
      currentSlot: 11,
    });
    expect(io.pin).toHaveBeenCalledTimes(2);
  });

  it("holds a public history capture until failed transaction reads have drained", async () => {
    const { intake, source } = fixture();
    const started = deferred();
    const release = deferred();
    const published = {
      ...observation(10),
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
      rawSource: source,
      lucid: { slotToUnixTime: (slot) => slot * 1_000 },
      currentObservation: () => published,
    });
    io.history.mockResolvedValue({
      transactions: [
        { txHash: "first", inclusionPoint: observation(1).nativePoint },
        { txHash: "second", inclusionPoint: observation(2).nativePoint },
      ],
    });
    io.transaction.mockImplementation(
      async ({ txHash }: { txHash: string }) => {
        if (txHash === "first") throw new Error("history read failed");
        started.resolve();
        await release.promise;
        return {};
      },
    );
    const first = payloadSource.fetchPayloadByHeaderHash("77".repeat(28));
    const failed = expect(first).rejects.toThrow("history read failed");
    await started.promise;
    const second = intake.snapshot(observation(11), "99".repeat(28));
    await nextTurn();
    expect(io.transaction).toHaveBeenCalledTimes(2);
    expect(io.pin).toHaveBeenCalledTimes(1);
    expect(io.address).not.toHaveBeenCalled();
    release.resolve();
    await failed;
    await expect(second).resolves.toMatchObject({
      headerHash: "99".repeat(28),
    });
    expect(io.pin.mock.calls.map(([input]) => input.point.slot)).toEqual([
      "10",
      "11",
    ]);
  });
});

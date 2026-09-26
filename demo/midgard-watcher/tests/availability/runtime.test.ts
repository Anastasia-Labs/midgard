import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherAvailabilityRuntime,
  type WatcherAvailabilityStatusTransition,
} from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";

// Keep the real serialized reconciliation and transition reporting; isolate
// wallet, source admission and chain I/O, which have their own fixture suites.
const io = vi.hoisted(() => ({
  reconcile: vi.fn(),
  snapshot: vi.fn(),
  payload: vi.fn(),
}));
vi.mock("@al-ft/midgard-core/availability-operation-journal", () => ({
  openAvailabilityOperationJournal: () => ({
    assertRunning() {},
    close() {},
    halt() {},
  }),
}));
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityOperationLimits: () => ({}),
  reconcileDaAvailabilityOperations: io.reconcile,
}));
vi.mock("@lucid-evolution/lucid", async (original) => ({
  ...(await original<typeof import("@lucid-evolution/lucid")>()),
  Lucid: async () => ({
    selectWallet: { fromSeed() {} },
    wallet: () => ({ address: async () => "availability" }),
  }),
  paymentCredentialOf: (address: string) => ({ hash: address }),
}));
vi.mock("@lucid-evolution/scalus-uplc", () => ({
  createScalusEvaluator: () => ({}),
}));
vi.mock("../../src/l1/native-reward-account.js", () => ({
  WatcherLocalKupmios: class {},
}));
vi.mock("../../src/runtime/process-config.js", () => ({
  loadWatcherSecretText: async () => "seed",
}));
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation() {},
}));
vi.mock("../../src/storage/retained-da-runtime.js", () => ({
  createWatcherRetainedDaRuntime: async () => ({
    sources: [{ sourceId: "public", fetchPayloadByHeaderHash: io.payload }],
    async close() {},
  }),
  bindWatcherL1AvailabilityPayloadSource: () => ({ close() {} }),
}));
vi.mock("../../src/availability/deployment.js", () => ({
  createWatcherAvailabilityDeployment: async () => ({
    contracts: { stateQueue: { policyId: "policy" } },
    parameters: {},
  }),
}));
// The intake carries the verified source's release depth; a sentinel distinct
// from every profile depth proves the runtime forwards it and substitutes none.
vi.mock("../../src/availability/observation.js", () => ({
  createWatcherAvailabilityObservation: () => ({
    snapshot: io.snapshot,
    confirmationDepth: 17,
  }),
}));
vi.mock("../../src/availability/published-payload.js", () => ({
  createWatcherL1AvailabilityPayloadSource: () => ({}),
}));

const observation = (slot: number) =>
  ({
    observationDigest: String(slot),
    deploymentIdentityDigest: "deployment",
    nativePoint: {
      slot: String(slot),
      blockNo: String(slot),
      blockHash: "11".repeat(32),
      finalityDepth: "30",
    },
    finalizedHeaders: [],
    finalizedCorrectionLock: { datum: "Idle" },
  }) as unknown as WatcherAuthenticatedStateQueueObservation;
const fixture = (
  onStatusTransition?: (event: WatcherAvailabilityStatusTransition) => void,
  currentObservation?: () => WatcherAuthenticatedStateQueueObservation,
) =>
  createWatcherAvailabilityRuntime({
    config: {
      watcherConfig: {
        l1: {
          source: {
            sourceMode: "local_node",
            queryServices: [
              { kind: "kupo", endpoint: "http://kupo" },
              { kind: "ogmios", endpoint: "http://ogmios" },
            ],
          },
        },
      },
      availability: { keySource: {}, journalPath: "unused" },
    } as unknown as WatcherProcessConfig,
    identity: {
      network: "Custom",
      manifestId: "deployment",
    } as VerifiedWatcherDeploymentIdentity,
    rawSource: {} as LocalKupmiosFraudProofRawSource,
    ...(currentObservation === undefined
      ? {}
      : {
          faultProofObservation: {
            rawSource: {} as LocalKupmiosFraudProofRawSource,
            currentObservation,
          },
        }),
    proverWalletAddress: "prover",
    ...(onStatusTransition === undefined ? {} : { onStatusTransition }),
  });

beforeEach(() => {
  io.reconcile.mockReset().mockResolvedValue([]);
  io.payload.mockReset().mockResolvedValue({ ok: true });
});
afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("availability reconciliation status transitions", () => {
  it("deduplicates blocked causes across native points and emits recovery only after reconciliation", async () => {
    const events: WatcherAvailabilityStatusTransition[] = [];
    const runtime = await fixture((event) => events.push(event));
    await runtime.reconcile(observation(1), false);
    expect(events).toHaveLength(0);
    io.reconcile.mockRejectedValue(new Error("snapshot boundary changed"));
    await runtime.reconcile(observation(2), false);
    await runtime.reconcile(observation(3), false);
    expect(events).toHaveLength(1);
    expect(events[0]).toMatchObject({
      status: { phase: "blocked", detail: "snapshot boundary changed" },
      observationDigest: "2",
      nativePoint: { slot: "2" },
    });
    io.reconcile.mockRejectedValue(new Error("peer unavailable"));
    await runtime.reconcile(observation(4), false);
    expect(events).toHaveLength(2);
    let complete!: (value: never[]) => void;
    io.reconcile.mockImplementationOnce(
      () =>
        new Promise<never[]>((resolve) => {
          complete = resolve;
        }),
    );
    const recovery = runtime.reconcile(observation(5), false);
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(events).toHaveLength(2);
    complete([]);
    await recovery;
    expect(events[2]).toMatchObject({
      status: { phase: "ready" },
      observationDigest: "5",
    });
    await runtime.reconcile(observation(6), false);
    expect(events[3]?.status.phase).toBe("blocked");
    await runtime.close();
  });

  it("reports a changed blocked transaction and preserves waiting recovery semantics", async () => {
    const events: WatcherAvailabilityStatusTransition[] = [];
    const runtime = await fixture((event) => events.push(event));
    io.reconcile.mockResolvedValue([
      { status: "conflict", txHash: "aa".repeat(32) },
    ]);
    await runtime.reconcile(observation(1), false);
    await runtime.reconcile(observation(2), false);
    io.reconcile.mockResolvedValue([
      { status: "conflict", txHash: "bb".repeat(32) },
    ]);
    await runtime.reconcile(observation(3), false);
    io.reconcile.mockResolvedValue([
      { status: "waiting", txHash: "bb".repeat(32) },
    ]);
    await runtime.reconcile(observation(4), false);
    await runtime.reconcile(observation(5), false);
    expect(events.map(({ status }) => [status.phase, status.txHash])).toEqual([
      ["blocked", "aa".repeat(32)],
      ["blocked", "bb".repeat(32)],
      ["waiting", "bb".repeat(32)],
    ]);
    await runtime.close();
  });

  it.each([-60_000, 60_000])(
    "uses monotonic duration while wall time moves by %i ms",
    async (jump) => {
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(1_800_000_000_000);
      let tick = 100;
      vi.spyOn(performance, "now").mockImplementation(() => tick);
      const events: WatcherAvailabilityStatusTransition[] = [];
      const runtime = await fixture((event) => events.push(event));
      io.reconcile.mockImplementation(async () => {
        tick += 250;
        vi.setSystemTime(Date.now() + jump);
        throw new Error("unavailable");
      });
      await runtime.reconcile(observation(1), false);
      expect(events[0]?.elapsedMs).toBe(250);
      expect(events[0]?.observedAt).toBe(
        new Date(1_800_000_000_000 + jump).toISOString(),
      );
      await runtime.close();
    },
  );

  it("bounds diagnostic detail without changing the runtime failure or exposing embedded CBOR", async () => {
    const events: WatcherAvailabilityStatusTransition[] = [];
    const runtime = await fixture((event) => events.push(event));
    const detail = `provider failed ${"ab".repeat(4096)} ${"detail ".repeat(400)}`;
    io.reconcile.mockRejectedValue(new Error(detail));
    await runtime.reconcile(observation(1), false);
    expect(runtime.status().detail).toBe(detail);
    expect(events[0]?.status.detail).toHaveLength(2048);
    expect(events[0]?.status.detail).toContain("[hex omitted]");
    expect(events[0]?.status.detail).not.toContain("ab".repeat(64));
    expect(Object.isFrozen(events[0]?.status.pendingHeaders)).toBe(true);
    await runtime.close();
  });

  it("does not emit a recovery from work invalidated during reconciliation", async () => {
    const events: WatcherAvailabilityStatusTransition[] = [];
    const runtime = await fixture((event) => events.push(event));
    io.reconcile.mockRejectedValueOnce(new Error("blocked"));
    await runtime.reconcile(observation(1), false);
    io.reconcile.mockImplementationOnce(async () => {
      runtime.invalidateForRollback();
      return [];
    });
    await runtime.reconcile(observation(2), false);
    expect(events).toHaveLength(1);
    await runtime.reconcile(observation(3), false);
    expect(events[1]?.status.phase).toBe("ready");
    await runtime.close();
  });

  it("keeps the callback optional and does not convert a sink error into availability failure", async () => {
    const withoutSink = await fixture();
    io.reconcile.mockRejectedValueOnce(new Error("blocked"));
    await expect(
      withoutSink.reconcile(observation(1), false),
    ).resolves.toBeUndefined();
    expect(withoutSink.status().phase).toBe("blocked");
    await withoutSink.close();
    const sinkError = new Error("diagnostic sink failed");
    const runtime = await fixture(() => {
      throw sinkError;
    });
    io.reconcile.mockRejectedValueOnce(
      new Error("actual availability failure"),
    );
    await expect(runtime.reconcile(observation(2), false)).rejects.toBe(
      sinkError,
    );
    expect(runtime.status().detail).toBe("actual availability failure");
    await runtime.close();
  });
});

it("projects pending availability onto current inclusion without changing finalized actuation", async () => {
  const finalized = {
    ...observation(1),
    finalizedHeaders: [
      {
        headerHash: "known",
        daAvailability: { Attested: { da_bond_asset_name: "bond" } },
      },
      {
        headerHash: "published",
        daAvailability: {
          Challenged: {
            da_bond_asset_name: "bond",
            challenge_asset_name: "challenge",
          },
        },
      },
      {
        headerHash: "removed",
        daAvailability: { Attested: { da_bond_asset_name: "bond" } },
      },
    ],
  } as unknown as WatcherAuthenticatedStateQueueObservation;
  let included = {
    ...observation(2),
    nativePoint: { ...observation(2).nativePoint, finalityDepth: "1" },
    finalizedHeaders: [
      {
        headerHash: "known",
        daAvailability: { Attested: { da_bond_asset_name: "bond" } },
      },
      {
        headerHash: "published",
        daAvailability: { Published: { terminal_commitment: "published" } },
      },
      {
        headerHash: "new-challenge",
        daAvailability: {
          Challenged: {
            da_bond_asset_name: "bond",
            challenge_asset_name: "challenge",
          },
        },
      },
      {
        headerHash: "new-attested",
        daAvailability: { Attested: { da_bond_asset_name: "bond" } },
      },
      { headerHash: "unattested", daAvailability: "Unattested" },
    ],
  } as unknown as WatcherAuthenticatedStateQueueObservation;
  const runtime = await fixture(undefined, () => included);
  // Pending state remains conservative when finalized intake cannot yet resolve.
  io.reconcile.mockRejectedValueOnce(new Error("finalized intake unavailable"));
  await runtime.reconcile(finalized, false);
  const before = runtime.status();
  expect(await runtime.pendingAvailabilityHeaders(included)).toEqual(
    new Set(["known", "new-challenge"]),
  );
  expect(runtime.status()).toEqual(before);
  expect(io.reconcile).toHaveBeenCalledTimes(1);
  expect(io.reconcile.mock.calls[0]![0].minimumConfirmationDepth).toBe(17);
  expect(await runtime.pendingAvailabilityHeaders(finalized)).toEqual(
    new Set(["known", "published", "removed"]),
  );
  await expect(
    runtime.pendingAvailabilityHeaders(observation(3)),
  ).rejects.toThrow("current authenticated observation");
  included = { ...included, deploymentIdentityDigest: "another-deployment" };
  await expect(runtime.pendingAvailabilityHeaders(included)).rejects.toThrow(
    "current authenticated observation",
  );
  included = { ...included, deploymentIdentityDigest: "deployment" };
  runtime.invalidateForRollback();
  await expect(runtime.pendingAvailabilityHeaders(included)).rejects.toThrow(
    "current authenticated observation",
  );
  await runtime.close();
});

const includedAttestation = () =>
  ({
    ...observation(2),
    nativePoint: { ...observation(2).nativePoint, finalityDepth: "1" },
    finalizedHeaders: [
      {
        headerHash: "new",
        daAvailability: { Attested: { da_bond_asset_name: "bond" } },
      },
    ],
  }) as unknown as WatcherAuthenticatedStateQueueObservation;

it.each(["not_found", "transport_error", "timeout"])(
  "defers newly included attestation on public DA %s without waiting for finality",
  async (status) => {
    const included = includedAttestation();
    const runtime = await fixture(undefined, () => included);
    await runtime.reconcile(observation(1), false);
    io.payload.mockResolvedValueOnce({ ok: false, attempts: [{ status }] });
    expect(await runtime.pendingAvailabilityHeaders(included)).toEqual(
      new Set(["new"]),
    );
    expect(await runtime.pendingAvailabilityHeaders(included)).toEqual(
      new Set(),
    );
    expect(io.reconcile).toHaveBeenCalledTimes(1);
    await runtime.close();
  },
);

it.each(["invalid_content", "rejected", "conflict"])(
  "leaves public DA %s to mandatory classifier verification",
  async (status) => {
    const included = includedAttestation();
    const runtime = await fixture(undefined, () => included);
    await runtime.reconcile(observation(1), false);
    io.payload.mockResolvedValueOnce({ ok: false, attempts: [{ status }] });
    expect(await runtime.pendingAvailabilityHeaders(included)).toEqual(
      new Set(),
    );
    await runtime.close();
  },
);

it("does not suppress unexpected public DA failures", async () => {
  const included = includedAttestation();
  const runtime = await fixture(undefined, () => included);
  await runtime.reconcile(observation(1), false);
  const failure = new Error("malformed payload");
  io.payload.mockRejectedValueOnce(failure);
  await expect(runtime.pendingAvailabilityHeaders(included)).rejects.toBe(
    failure,
  );
  await runtime.close();
});

it("rejects inclusion classification revoked during public DA lookup", async () => {
  const included = includedAttestation();
  const runtime = await fixture(undefined, () => included);
  await runtime.reconcile(observation(1), false);
  io.payload.mockImplementationOnce(async () => {
    runtime.invalidateForRollback();
    return { ok: true };
  });
  await expect(runtime.pendingAvailabilityHeaders(included)).rejects.toThrow(
    "current authenticated observation",
  );
  await runtime.close();
});

import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherAvailabilityRuntime } from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";

// The isolation of runtime.test.ts, but over a real journal on disk: a
// rollback must leave the journal as usable as it found it.
const io = vi.hoisted(() => ({
  reconcile: vi.fn(),
  snapshot: vi.fn(),
  payload: vi.fn(),
  journalPath: "",
}));
vi.mock(
  "@al-ft/midgard-core/availability-operation-journal",
  async (original) => {
    const real =
      await original<
        typeof import("@al-ft/midgard-core/availability-operation-journal")
      >();
    return {
      ...real,
      openAvailabilityOperationJournal: () =>
        real.openAvailabilityOperationJournal(io.journalPath),
    };
  },
);
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
vi.mock("../../src/availability/deployment.js", async () => {
  const support = await import("../support/availability-challenge-fixture.js");
  return {
    createWatcherAvailabilityDeployment: async () => ({
      contracts: {
        stateQueue: { policyId: "policy" },
        daBondPool: { policyId: support.DA_BOND_POOL_POLICY_ID },
      },
      parameters: support.parametersFixture(),
    }),
  };
});
vi.mock("../../src/availability/observation.js", () => ({
  createWatcherAvailabilityObservation: () => ({
    pool: async () => undefined,
    snapshot: io.snapshot,
    confirmationDepth: 17,
  }),
}));
vi.mock("../../src/availability/published-payload.js", () => ({
  createWatcherL1AvailabilityPayloadSource: () => ({}),
}));

const observation = (slot: number, blockHash = "11".repeat(32)) =>
  ({
    observationDigest: `${slot.toString()}:${blockHash}`,
    deploymentIdentityDigest: "deployment",
    nativePoint: {
      slot: String(slot),
      blockNo: String(slot),
      blockHash,
      finalityDepth: "30",
    },
    finalizedHeaders: [],
    finalizedCorrectionLock: { datum: "Idle" },
  }) as unknown as WatcherAuthenticatedStateQueueObservation;
const fixture = () =>
  createWatcherAvailabilityRuntime({
    config: {
      watcherConfig: {
        l1: {
          requestTimeoutMs: 10_000,
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
    l1: {
      reads: {},
      store: {},
      provider: {},
    } as unknown as Parameters<
      typeof createWatcherAvailabilityRuntime
    >[0]["l1"],
    confirmationDepth: 17,
    proverWalletAddress: "prover",
    onDaBondPool: () => undefined,
    onDaBondPoolReadFailure: () => undefined,
  });

let dir: string;
beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "watcher-availability-rollback-"));
  io.journalPath = join(dir, "journal.sqlite");
  io.reconcile.mockReset().mockResolvedValue([]);
  io.payload.mockReset().mockResolvedValue({ ok: true });
});
afterEach(() => {
  rmSync(dir, { recursive: true, force: true });
});

describe("availability runtime across a rollback of its finalized observation", () => {
  it.each([
    [
      "an earlier slot",
      { kind: "point", slot: "3", blockHash: "22".repeat(32) },
    ],
    [
      "the same slot on another block",
      { kind: "point", slot: "5", blockHash: "33".repeat(32) },
    ],
    ["origin", { kind: "origin" }],
  ] as const)(
    "rewinds to %s and re-derives without latching the journal",
    async (_label, point) => {
      const runtime = await fixture();
      try {
        await runtime.reconcile(observation(5), false);
        expect(runtime.status().phase).toBe("ready");
        runtime.invalidateForRollback(point);
        expect(runtime.status()).toMatchObject({
          phase: "waiting",
          pendingHeaders: [],
        });
        await runtime.reconcile(observation(4, "44".repeat(32)), false);
        expect(io.reconcile).toHaveBeenCalledTimes(2);
        expect(runtime.status().phase).toBe("ready");
        // Another process sharing the wallet's journal is not refused either.
        const shared = openAvailabilityOperationJournal(io.journalPath);
        try {
          expect(() =>
            shared.acquire("actor", "other-process", Date.now(), 1_000),
          ).not.toThrow();
        } finally {
          shared.close();
        }
      } finally {
        await runtime.close();
      }
    },
  );

  it("fails readiness with a held intent's reason until fresh evidence clears it", async () => {
    const runtime = await fixture();
    try {
      io.reconcile.mockResolvedValue([
        {
          status: "held",
          txHash: "aa".repeat(32),
          expectedOutRefs: [],
          detail: "Inclusion evidence does not authenticate it",
        },
      ]);
      await runtime.reconcile(observation(5), false);
      expect(runtime.status()).toMatchObject({
        phase: "blocked",
        txHash: "aa".repeat(32),
        detail: "Inclusion evidence does not authenticate it",
      });
      io.reconcile.mockResolvedValue([]);
      await runtime.reconcile(observation(6), false);
      expect(runtime.status().phase).toBe("ready");
    } finally {
      await runtime.close();
    }
  });
});

vi.mock("../../src/storage/retained-da-runtime.read-scope.js", () => ({
  withWatcherRetainedDaReadScope: async (
    input: { scope: SDKScope },
    read: () => Promise<unknown>,
  ) => input.scope.read(read),
}));
type SDKScope = import("@al-ft/midgard-sdk").DaAvailabilityReadScope;
vi.mock("../../src/availability/runtime.read-attempt.js", async (original) => ({
  ...(await original<
    typeof import("../../src/availability/runtime.read-attempt.js")
  >()),
  watcherAvailabilityAuthenticatedOpenDeadline: () => Date.now() + 2_400_000,
}));

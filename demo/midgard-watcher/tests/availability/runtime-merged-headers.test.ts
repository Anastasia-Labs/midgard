import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherAvailabilityRuntime } from "../../src/availability/runtime.js";
import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherMergedHeaderProof,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";

// The same isolation as runtime.test.ts: real serialized reconciliation, no
// wallet, source admission or chain I/O.
const io = vi.hoisted(() => ({
  reconcile: vi.fn(),
  snapshot: vi.fn(),
  payload: vi.fn(),
}));
vi.mock("@al-ft/midgard-core/availability-operation-journal", () => ({
  openAvailabilityOperationJournal: () => ({
    assertRunning() {},
    workflows: () => [],
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

const ATTESTED = { Attested: { commitment_hash: "bond" } };

const observation = (
  slot: number,
  headers: readonly string[],
  finalityDepth = "30",
) =>
  ({
    observationDigest: String(slot),
    deploymentIdentityDigest: "deployment",
    nativePoint: {
      slot: String(slot),
      blockNo: String(slot),
      blockHash: "11".repeat(32),
      finalityDepth,
    },
    finalizedHeaders: headers.map((headerHash) => ({
      headerHash,
      daAvailability: ATTESTED,
    })),
    finalizedCorrectionLock: { datum: "Idle" },
  }) as unknown as WatcherAuthenticatedStateQueueObservation;

const proof = (headerHash: string): WatcherMergedHeaderProof => ({
  headerHash,
  mergeTransactionHash: "4d".repeat(32),
  mergeBlockHash: "4e".repeat(32),
  mergeSlot: "1200",
  mergeBlockNo: "120",
  confirmationDepth: "3",
});

const fixture = (
  mergedHeaders: readonly string[] | undefined,
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
    ...(mergedHeaders === undefined
      ? {}
      : {
          mergedHeaders: async () =>
            new Map(mergedHeaders.map((hash) => [hash, proof(hash)])),
        }),
    proverWalletAddress: "prover",
    onDaBondPool: () => undefined,
    onDaBondPoolReadFailure: () => undefined,
  });

beforeEach(() => {
  io.reconcile.mockReset().mockResolvedValue([]);
  io.payload.mockReset().mockResolvedValue({ ok: true });
  // Stop reconciliation at the snapshot read: which headers reach it is the
  // subject, and a failed read reports the pending set as blocked.
  io.snapshot.mockReset().mockRejectedValue(new Error("snapshot stopped"));
});

describe("availability runtime behind an L1 merge", () => {
  it.each([
    ["merged", ["merged"], ["live"]],
    ["unmerged", [], ["merged", "live"]],
  ] as const)(
    "snapshots only unmerged pending headers (%s)",
    async (_label, merged, snapshotted) => {
      const runtime = await fixture(merged);
      await runtime.reconcile(observation(1, ["merged", "live"]), false);
      expect(
        io.snapshot.mock.calls.map(([, headerHash]) => headerHash),
      ).toEqual(snapshotted);
      expect(runtime.status()).toMatchObject({
        phase: "blocked",
        pendingHeaders: snapshotted,
      });
      await runtime.close();
    },
  );

  it("keeps every attested header pending when no merge reader is wired", async () => {
    const runtime = await fixture(undefined);
    await runtime.reconcile(observation(1, ["merged", "live"]), false);
    expect(io.snapshot).toHaveBeenCalledTimes(2);
    await runtime.close();
  });

  it("does not defer classification or fetch public DA for a merged inclusion header", async () => {
    const included = observation(2, ["merged"], "1");
    const runtime = await fixture([], () => included);
    await runtime.reconcile(observation(1, []), false);
    io.payload.mockResolvedValue({
      ok: false,
      attempts: [{ status: "not_found" }],
    });
    expect(
      await runtime.pendingAvailabilityHeaders(included, new Set(["merged"])),
    ).toEqual(new Set());
    expect(io.payload).not.toHaveBeenCalled();
    // Unmerged, the same pruned payload still defers classification.
    expect(await runtime.pendingAvailabilityHeaders(included)).toEqual(
      new Set(["merged"]),
    );
    expect(io.payload).toHaveBeenCalledOnce();
    await runtime.close();
  });
});

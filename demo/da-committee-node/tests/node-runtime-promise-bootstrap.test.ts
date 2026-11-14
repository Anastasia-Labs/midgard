import { identityFromSeedHex } from "@al-ft/midgard-core/da-libp2p-identity";
import { describe, expect, it, vi } from "vitest";

import type { availabilityResponderFromConfig } from "../src/availability/factory.js";
import { AvailabilityResponder } from "../src/availability/responder.js";
import type { CommitteeL1Readiness } from "../src/l1/follower/l1-follower.js";
import { openCommitteeNodeRuntime } from "../src/node-runtime.js";
import { loadDaSigner } from "../src/signer.js";
import type { CommitteeStore } from "../src/store.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

const seam = vi.hoisted(() => ({
  openStore: vi.fn(),
  follower: vi.fn(),
  followerStopped: vi.fn(),
  factory: vi.fn(),
  started: vi.fn(),
  stopped: vi.fn(),
  published: vi.fn(),
  reconciled: vi.fn(),
  payloads: vi.fn(),
}));
vi.mock("../src/store/factory.js", () => ({
  openCommitteeStore: seam.openStore,
}));
vi.mock("../src/l1/follower/l1-follower.js", async (original) => ({
  ...(await original<typeof import("../src/l1/follower/l1-follower.js")>()),
  startCommitteeL1Follower: seam.follower,
}));
vi.mock("../src/availability/factory.js", () => ({
  availabilityResponderFromConfig: seam.factory,
}));
vi.mock("../src/coordinator/factory.js", () => ({
  onChainCoordinatorFromConfig: async () => ({}),
}));
vi.mock("../src/peer/coordinator.js", () => ({
  PeerSignatureCoordinator: class {
    publishSignature = seam.published;
  },
}));
vi.mock("../src/coordinator/submitter-reconciler.js", () => ({
  SubmitterReconciler: class {
    reconcileHeader = seam.reconciled;
  },
}));
vi.mock("../src/da/libp2p/index.js", async (original) => {
  const real = await original<typeof import("../src/da/libp2p/index.js")>();
  return {
    ...real,
    DaLibp2pPayloadSource: class {
      fetchPayloadCandidates = seam.payloads;
    },
    DaLibp2pNode: class {
      setGossipHandler() {}
      start = seam.started;
      stop = seam.stopped;
    },
  };
});

const fixture = async () => {
  vi.clearAllMocks();
  seam.started.mockResolvedValue(undefined);
  seam.stopped.mockResolvedValue(undefined);
  seam.published.mockResolvedValue("posted");
  seam.reconciled.mockResolvedValue({ status: "pending" });
  const f = await promiseAdmissionFixture();
  seam.payloads.mockImplementation(f.payloadSource.fetchPayloadCandidates);
  const identity = await identityFromSeedHex("01".repeat(32));
  seam.openStore.mockResolvedValue(f.store);
  seam.followerStopped.mockResolvedValue(undefined);
  // The follower's facts hold no DA params output, so the runtime gets no
  // DA chain reader: a null follower store.
  const following: { reasons: CommitteeL1Readiness[] } = { reasons: [] };
  seam.follower.mockResolvedValue({
    source: { ...f.provider, readiness: () => following.reasons },
    store: null,
    provider: null,
    lucid: async () => {
      throw new Error("no Lucid in this fixture");
    },
    stop: seam.followerStopped,
  });
  const adoption = {
    policyArtifactPath: "/selected",
    trustedPolicyDigest: "00".repeat(32),
    resourceProfilePath: "/selected",
    trustedResourceProfileDigest: "00".repeat(32),
    calibrationEvidencePath: "/selected",
    trustedCalibrationEvidenceDigest: "00".repeat(32),
    faultModelPath: "/selected",
    trustedFaultModelDigest: "00".repeat(32),
  };
  const config = {
    ...f.config,
    l1SubmissionEnabled: true,
    daTransport: {
      ...f.config.daTransport,
      peers: [
        {
          peerId: identity.peerId,
          signerIndex: 0,
          daVkey: f.signerValidation.signerPublicKeyHex,
          multiaddrs: [],
          roles: ["committee" as const],
        },
      ],
    },
    availabilityPromiseAdoption: adoption,
    cardanoL1Source: { networkMagic: 2 },
  };
  const { DaPeerRegistry } = await import("../src/da/libp2p/DaPeerRegistry.js");
  return {
    ...f,
    following,
    local: {
      config,
      signer: await loadDaSigner("hex:" + "00".repeat(31) + "01"),
      committeeValidation: f.signerValidation,
      signerValidation: f.signerValidation,
      daIdentity: identity,
      daPeerRegistry: DaPeerRegistry.fromConfig(config.daTransport),
      libp2pPrivateKeySource: "seed:" + "01".repeat(32),
    },
  };
};

describe("actual node unsigned promise enrollment", () => {
  it("scans once the follower is ready, without signing or financial actions, then enables the same service only after owned compaction", async () => {
    const f = await fixture();
    let readPins: (() => readonly string[]) | undefined;
    const compact = vi.fn(async () => {
      expect(readPins).toBeDefined();
      expect(await f.store.listDaSignatures()).toEqual([]);
      expect(seam.published).not.toHaveBeenCalled();
      expect(seam.reconciled).not.toHaveBeenCalled();
      expect(seam.started).not.toHaveBeenCalled();
      return [];
    });
    seam.factory.mockImplementation(
      async (_config: unknown, store: CommitteeStore) => {
        expect(store).toBe(f.store);
        expect(await store.listDaSignatures()).toEqual([]);
        return {
          responder: new AvailabilityResponder({
            deploymentFingerprint: f.config.deploymentFingerprint,
            deploymentIdentity: "fixture",
            store,
            reconcile: async () => "ready",
            discover: async () => [],
            execute: async () => "pending",
          }),
          promiseAdmissionSource: f.source,
          close: () => {},
          bindRetirementOperationalPins: (read: () => readonly string[]) => {
            readPins = read;
          },
          compactRetainedPromises: compact,
        } satisfies Awaited<ReturnType<typeof availabilityResponderFromConfig>>;
      },
    );
    const runtime = await openCommitteeNodeRuntime(f.local, {});
    try {
      expect(compact).toHaveBeenCalledOnce();
      expect(runtime.service.latestL1View()).toBeDefined();
      expect(readPins?.()).toEqual(
        runtime.service.readRetirementOperationalPins(),
      );
      expect(await runtime.service.tick()).toMatchObject({
        signedHeaders: 1,
        errors: [],
      });
      expect(await f.store.listDaSignatures()).toHaveLength(1);
      expect(seam.published).toHaveBeenCalledOnce();
    } finally {
      await runtime.close();
    }
    expect(seam.followerStopped).toHaveBeenCalledOnce();
  });

  it("does not enable signing or start external loops when factory enrollment fails after the genuine scan", async () => {
    const f = await fixture();
    seam.factory.mockRejectedValue(
      new Error("controlled factory evidence unavailable"),
    );
    await expect(openCommitteeNodeRuntime(f.local, {})).rejects.toThrow(
      "controlled factory evidence unavailable",
    );
    expect(seam.followerStopped).toHaveBeenCalledOnce();
    expect(seam.started).not.toHaveBeenCalled();
    expect(seam.published).not.toHaveBeenCalled();
    expect(seam.reconciled).not.toHaveBeenCalled();
  });

  it("holds the bootstrap scan on a follower reason no wait clears, reporting it with the process up, then proceeds once it clears", async () => {
    const f = await fixture();
    f.following.reasons = [
      { reason: "rollback_beyond_k", detail: "rolled back 7 blocks" },
    ];
    seam.factory.mockImplementation(
      async (_config: unknown, store: CommitteeStore) => {
        expect(f.following.reasons).toEqual([]);
        return {
          responder: new AvailabilityResponder({
            deploymentFingerprint: f.config.deploymentFingerprint,
            deploymentIdentity: "fixture",
            store,
            reconcile: async () => "ready",
            discover: async () => [],
            execute: async () => "pending",
          }),
          promiseAdmissionSource: f.source,
          close: () => {},
        } satisfies Awaited<ReturnType<typeof availabilityResponderFromConfig>>;
      },
    );
    const held: string[] = [];
    const runtime = await openCommitteeNodeRuntime(f.local, {}, (reasons) => {
      held.push(reasons.map(({ reason }) => reason).join(","));
      expect(seam.factory).not.toHaveBeenCalled();
      expect(seam.published).not.toHaveBeenCalled();
      // An operator repaired the follower.
      f.following.reasons = [];
    });
    try {
      expect(held).toEqual(["rollback_beyond_k"]);
      expect(seam.factory).toHaveBeenCalledOnce();
    } finally {
      await runtime.close();
    }
  });

  it("stops only when the caller's onL1Held throws (a one-shot run), stopping the follower and signing nothing", async () => {
    const f = await fixture();
    f.following.reasons = [
      { reason: "rollback_beyond_k", detail: "rolled back 7 blocks" },
    ];
    await expect(
      openCommitteeNodeRuntime(f.local, {}, (reasons) => {
        throw new Error(
          reasons.map(({ reason, detail }) => `${reason}: ${detail}`).join(),
        );
      }),
    ).rejects.toThrow("rollback_beyond_k: rolled back 7 blocks");
    expect(seam.followerStopped).toHaveBeenCalledOnce();
    expect(seam.factory).not.toHaveBeenCalled();
    expect(seam.started).not.toHaveBeenCalled();
    expect(seam.published).not.toHaveBeenCalled();
  });
});

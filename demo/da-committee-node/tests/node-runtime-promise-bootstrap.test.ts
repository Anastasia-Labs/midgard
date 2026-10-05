import { join } from "node:path";

import { identityFromSeedHex } from "@al-ft/midgard-core/da-libp2p-identity";
import { describe, expect, it, vi } from "vitest";

import type { availabilityResponderFromConfig } from "../src/availability/factory.js";
import { AvailabilityResponder } from "../src/availability/responder.js";
import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
import { FileChainSyncConsumerCursorStore } from "../src/l1/provider.file-chain-sync-consumer-cursor-store.js";
import { FileChainSyncCursorStore } from "../src/l1/provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "../src/l1/provider.local-node-chain-authority.js";
import { LocalNodeStateQueueProvider } from "../src/l1/provider.local-node-state-queue-provider.js";
import { openCommitteeNodeRuntime } from "../src/node-runtime.js";
import { loadDaSigner } from "../src/signer.js";
import type { CommitteeStore } from "../src/store.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

const seam = vi.hoisted(() => ({
  openStore: vi.fn(),
  provider: vi.fn(),
  chainReader: vi.fn(),
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
vi.mock("../src/l1/provider.js", async (original) => ({
  ...(await original<typeof import("../src/l1/provider.js")>()),
  providerFromConfig: seam.provider,
}));
vi.mock("../src/l1/da-attestation-reader.js", () => ({
  daAttestationReaderFromConfig: seam.chainReader,
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
  const point = {
    network: f.config.network,
    slot: 0,
    blockHash: "00".repeat(32),
    providerSource: "chain-sync:fixture",
    observedAt: new Date(0).toISOString(),
  };
  const cursorStore = new FileChainSyncCursorStore(
    join(f.dir, "chain-sync.json"),
    "11".repeat(32),
  );
  const consumed = new FileChainSyncConsumerCursorStore(
    join(f.dir, "consumer.json"),
    "11".repeat(32),
  );
  const authority = new LocalNodeChainAuthority(
    "fixture",
    f.config.network,
    {
      next: async () => ({
        event: { direction: "roll_forward", point },
        tip: point,
      }),
    },
    cursorStore,
  );
  const provider = new LocalNodeStateQueueProvider(
    authority,
    [
      {
        ...f.provider,
        currentChainPoint: async () => point,
        fetchStateQueueNodes: f.provider.fetchStateQueueNodes,
        fetchStateQueueSnapshot: f.provider.fetchStateQueueSnapshot,
        fetchStateQueueReplayCheckpoints: async (anchor, current) => {
          expect(anchor).toEqual(current);
          return [];
        },
      },
    ],
    ["fixture"],
    consumed,
  );
  seam.openStore.mockResolvedValue(f.store);
  seam.provider.mockResolvedValue(provider);
  const fetchDaParams: DaAttestationChainReader["fetchDaParams"] =
    async () => ({
      outRef: "00".repeat(32) + "#0",
      ownerCount: 1,
      updateThreshold: 1,
      rawDatum: {
        committee: f.config.daParams.committeeHex,
        committee_signers_hash: f.config.daParams.committeeSignersHash,
        da_threshold: 1n,
        owners: [],
        update_threshold: 1n,
      },
      committeeHex: f.config.daParams.committeeHex,
      committeeSignersHash: f.config.daParams.committeeSignersHash,
      threshold: f.config.daParams.threshold,
    });
  seam.chainReader.mockResolvedValue({ fetchDaParams });
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
    cardanoL1Source: {
      sourceMode: "local_node" as const,
      authorityNodeId: "fixture",
      authorityDigest: "11".repeat(32),
      networkMagic: 2,
    },
  };
  const { DaPeerRegistry } = await import("../src/da/libp2p/DaPeerRegistry.js");
  return {
    ...f,
    provider,
    consumed,
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
  it("consumes the real cursor without signing or financial actions, then enables the same service only after owned compaction", async () => {
    const f = await fixture();
    expect(await f.consumed.load()).toBeUndefined();
    let readPins: (() => readonly string[]) | undefined;
    const compact = vi.fn(async () => {
      expect(readPins).toBeDefined();
      expect(await f.consumed.load()).toBeDefined();
      expect(await f.store.listDaSignatures()).toEqual([]);
      expect(seam.published).not.toHaveBeenCalled();
      expect(seam.reconciled).not.toHaveBeenCalled();
      expect(seam.started).not.toHaveBeenCalled();
      return [];
    });
    seam.factory.mockImplementation(
      async (_config: unknown, store: CommitteeStore) => {
        expect(store).toBe(f.store);
        expect(await f.consumed.load()).toEqual(
          await f.provider.currentChainSyncCursor(),
        );
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
  });

  it("does not enable signing or start external loops when factory enrollment fails after the genuine scan", async () => {
    const f = await fixture();
    seam.factory.mockRejectedValue(
      new Error("controlled factory evidence unavailable"),
    );
    await expect(openCommitteeNodeRuntime(f.local, {})).rejects.toThrow(
      "controlled factory evidence unavailable",
    );
    expect(await f.consumed.load()).toBeDefined();
    expect(seam.started).not.toHaveBeenCalled();
    expect(seam.published).not.toHaveBeenCalled();
    expect(seam.reconciled).not.toHaveBeenCalled();
  });
  it("preserves quarantine when existing decisions lack the durable consumed cursor", async () => {
    const f = await fixture();
    const normal = f.service(f.store, false);
    await normal.initialize();
    expect(await normal.tick()).toMatchObject({ signedHeaders: 2, errors: [] });
    expect(await f.consumed.load()).toBeUndefined();
    await expect(openCommitteeNodeRuntime(f.local, {})).rejects.toThrow();
    expect(seam.factory).not.toHaveBeenCalled();
    expect(seam.started).not.toHaveBeenCalled();
    expect(seam.reconciled).not.toHaveBeenCalled();
    expect(await f.consumed.load()).toBeUndefined();
    const reopened = await f.open();
    expect(await reopened.getL1SourceState()).toMatchObject({
      status: "quarantined",
    });
    expect(await reopened.listDaSignatures()).toHaveLength(2);
  });
});

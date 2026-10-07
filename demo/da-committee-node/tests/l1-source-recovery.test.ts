import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  l1SourceAuthorityDigest,
  type LoadedCommitteeConfig,
} from "../src/config.js";
import { FileChainSyncConsumerCursorStore } from "../src/l1/provider.file-chain-sync-consumer-cursor-store.js";
import { FileChainSyncCursorStore } from "../src/l1/provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "../src/l1/provider.local-node-chain-authority.js";
import { LocalNodeStateQueueProvider } from "../src/l1/provider.local-node-state-queue-provider.js";
import { parseL1RecoveryCommand } from "../src/l1/recovery-command.js";
import {
  type L1RecoveryCertificate,
  RECOVERABLE_L1_REASON,
  verifyL1Recovery,
} from "../src/l1/recovery-incident.js";
import { scanStateQueue } from "../src/l1/state-queue-scanner.js";
import { type CommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { makePayloadFixture, minimalConfig, tempDir } from "./helpers.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";
import {
  localSignature,
  verified,
} from "./helpers/quarantined-committee-store.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";

const databases = postgresTestDatabases("codex_rel_cq");
const opened = new Set<CommitteeStore>();
afterEach(async () => {
  for (const store of opened) await store.close?.();
  opened.clear();
});
afterAll(() => databases.dropAll());
const fixture = async (withSignature = false) => {
  const dir = await tempDir();
  const store = await PostgresCommitteeStore.open(
    (await databases.create()).url,
  );
  opened.add(store);
  const base = minimalConfig({
    manifestPath: "/public-manifest",
    deploymentInfoPath: "/public-deployment",
    signerSeed: "01".repeat(32),
    signerPublicKey: "02".repeat(32),
  });
  const l1Source = {
    sourceMode: "local_node",
    authorityNodeId: "fixture",
    chainSyncProviderUrl: "fixture:events",
    queryProviderUrls: ["fixture:query"],
  } as const;
  const config: LoadedCommitteeConfig = {
    ...base,
    l1Source,
    availabilityJournalPath: undefined,
    availabilitySubmitterKeySource: undefined,
    cardanoL1Source: {
      sourceMode: "local_node",
      authorityNodeId: "fixture",
      authorityDigest: l1SourceAuthorityDigest(base.network, l1Source),
      networkMagic: 42,
    },
  };
  await store.initDeployment({
    marker: makeDeploymentMarker(config.deploymentFingerprint),
    manifestSha256: config.deploymentManifestSha256,
    contractDeploymentInfoSha256: config.contractDeploymentInfoSha256,
    manifestRaw: config.deploymentManifestRaw,
  });
  const payload = await makePayloadFixture(1);
  const chain = createStateQueueChain({
    deploymentIdentityDigest: config.deploymentFingerprint,
    stateQueuePolicyId: config.stateQueuePolicyId,
    headers: [],
    tip: 1,
  });
  const initialQueue = chain.queue();
  chain.mine({ append: payload });
  for (let i = 0; i < 20; i++) chain.mine();
  let point = {
    network: config.network,
    slot: 100,
    blockHash: "10".repeat(32),
    providerSource: "chain-sync:fixture",
    observedAt: new Date(0).toISOString(),
  };
  const authority = new LocalNodeChainAuthority(
    "fixture",
    config.network,
    {
      next: async (cursor) => ({
        ...(cursor && cursor.point.slot === point.slot
          ? {}
          : { event: { direction: "roll_forward" as const, point } }),
        tip: point,
      }),
    },
    new FileChainSyncCursorStore(join(dir, "cursor.json"), "11".repeat(32)),
  );
  const query = {
    currentChainPoint: async () => point,
    fetchStateQueueNodes: async () => chain.snapshot().nodes,
    fetchStateQueueSnapshot: async () => chain.snapshot(),
    fetchStateQueueReplayCheckpoints: vi.fn(
      chain.fetchStateQueueReplayCheckpoints,
    ),
  };
  const provider = new LocalNodeStateQueueProvider(
    authority,
    [query],
    ["fixture"],
    new FileChainSyncConsumerCursorStore(
      join(dir, "consumed.json"),
      "11".repeat(32),
    ),
  );
  const first = await provider.fetchStateQueueSnapshot();
  await provider.acknowledgeChainSyncCursor(first.chainSyncCursor!);
  const ack = vi.spyOn(provider, "acknowledgeChainSyncCursor");
  await store.saveL1SourceState({
    schemaVersion: 1,
    sourceMode: "local_node",
    network: config.network,
    authoritySha256: l1SourceAuthorityDigest(config.network, config.l1Source),
    status: "healthy",
    observations: [],
    observedAt: new Date(0).toISOString(),
    stateQueueReplayAnchor: {
      deploymentIdentityDigest: config.deploymentFingerprint,
      stateQueuePolicyId: config.stateQueuePolicyId,
      blockNo: "0",
      transactionIndex: "0",
      queue: initialQueue,
    },
  });
  if (withSignature) await store.saveDaSignature(localSignature());
  await store.quarantineL1Decisions({
    ...(await store.getL1SourceState())!,
    status: "quarantined",
    quarantineReason: RECOVERABLE_L1_REASON,
    quarantinedAt: new Date(1).toISOString(),
  });
  const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 30_000 });
  const snapshot = await store.readL1RecoverySnapshot();
  const verify = () => verifyL1Recovery({ config, provider, snapshot, scope });
  return {
    store,
    config,
    provider,
    query,
    snapshot,
    scope,
    verify,
    ack,
    advanceNative: () => {
      point = { ...point, slot: point.slot + 1, blockHash: "20".repeat(32) };
    },
  };
};

describe("explicit unsigned L1-source recovery", () => {
  it("clears only the held source after actual complete native replay, without consuming cursor or effects", async () => {
    const f = await fixture();
    const consumed = await f.provider.loadConsumedChainSyncCursor();
    const certificate = await f.verify();
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    await f.store.applyL1RecoveryCertificate(certificate);
    const after = (await f.store.readL1RecoverySnapshot()).data;
    expect(after.chainCursor?.status).toBe("healthy");
    expect({ ...after, chainCursor: f.snapshot.data.chainCursor }).toEqual(
      f.snapshot.data,
    );
    expect(await f.provider.loadConsumedChainSyncCursor()).toEqual(consumed);
    expect(f.ack).not.toHaveBeenCalled();
    await expect(
      f.store.applyL1RecoveryCertificate(certificate),
    ).rejects.toThrow("unverified_or_consumed");
    f.scope.close();
  });
  it("rejects a same-scope changed whole incident at the backend CAS", async () => {
    const f = await fixture();
    const certificate = await f.verify();
    const records = await scanStateQueue(f.provider, {
      deploymentFingerprint: f.config.deploymentFingerprint,
      deploymentIdentityDigest: f.config.deploymentFingerprint,
      stateQueuePolicyId: f.config.stateQueuePolicyId,
      daAttestationPolicyId: f.config.daAttestationPolicyId,
      finalityDepth: f.config.finalityDepth,
      consensusProfile: f.config.consensusProfile,
      terminalReplayAnchor: f.snapshot.data.chainCursor!.stateQueueReplayAnchor,
    });
    await f.store.upsertStateQueueHeader(records[0]!);
    await expect(
      f.store.applyL1RecoveryCertificate(certificate),
    ).rejects.toThrow("recovery_incident_changed");
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    expect(f.ack).not.toHaveBeenCalled();
    f.scope.close();
  });
  it("does not accept an opaque token supplied without native verification", async () => {
    const f = await fixture();
    await expect(
      f.store.applyL1RecoveryCertificate({} as L1RecoveryCertificate),
    ).rejects.toThrow("unverified_or_consumed");
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    f.scope.close();
  });
  it("refuses configured availability journals without an exclusive all-actor barrier", async () => {
    const f = await fixture();
    await expect(
      verifyL1Recovery({
        ...f,
        config: {
          ...f.config,
          availabilityJournalPath: "/configured-journal",
        },
      }),
    ).rejects.toThrow("journal_exclusive_inventory_unavailable");
    expect(f.query.fetchStateQueueReplayCheckpoints).not.toHaveBeenCalled();
    f.scope.close();
  });
  it("refuses every retained signature without changing its bytes, posting status or source", async () => {
    const f = await fixture(true);
    const snapshot = await f.store.readL1RecoverySnapshot();
    await expect(verifyL1Recovery({ ...f, snapshot })).rejects.toThrow(
      "prior_da_signatures_requires_reconciliation",
    );
    expect(await f.store.readL1RecoverySnapshot()).toEqual(snapshot);
    expect(f.query.fetchStateQueueReplayCheckpoints).not.toHaveBeenCalled();
    expect(f.ack).not.toHaveBeenCalled();
    f.scope.close();
  });
  it("preserves non-conflicted retained payload bytes during a source-only clear", async () => {
    const f = await fixture();
    const retained = await f.store.saveDaPayload(verified);
    const snapshot = await f.store.readL1RecoverySnapshot();
    const certificate = await verifyL1Recovery({ ...f, snapshot });
    await f.store.applyL1RecoveryCertificate(certificate);
    expect(await f.store.getDaPayload(verified.headerHash)).toEqual(retained);
    expect((await f.store.readL1RecoverySnapshot()).data.daPayloads).toEqual(
      snapshot.data.daPayloads,
    );
    expect(f.ack).not.toHaveBeenCalled();
    f.scope.close();
  });
  it("refuses a changed native cursor at the final backend fence", async () => {
    const f = await fixture();
    const certificate = await f.verify();
    f.advanceNative();
    await expect(
      f.store.applyL1RecoveryCertificate(certificate),
    ).rejects.toThrow("native_source_changed_during_recovery");
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    expect(f.ack).not.toHaveBeenCalled();
    f.scope.close();
  });
  it("refuses an expired proof before the backend can clear the held source", async () => {
    const f = await fixture();
    const certificate = await f.verify();
    f.scope.close();
    await expect(
      f.store.applyL1RecoveryCertificate(certificate),
    ).rejects.toThrow();
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    expect(f.ack).not.toHaveBeenCalled();
  });
  it("does not substitute an empty replay when the actual query surface lacks its native capability", async () => {
    const f = await fixture();
    Object.defineProperty(f.query, "fetchStateQueueReplayCheckpoints", {
      value: undefined,
    });
    await expect(f.verify()).rejects.toThrow(
      "has no authenticated ordered history source",
    );
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
    expect(f.ack).not.toHaveBeenCalled();
    f.scope.close();
  });
});

it("rejects force/reset and malformed incident options instead of changing state", () => {
  expect(parseL1RecoveryCommand(["inspect"])).toEqual({ kind: "inspect" });
  expect(
    parseL1RecoveryCommand([
      "--incident",
      "00".repeat(32),
      "--timeout-ms",
      "1000",
    ]),
  ).toEqual({ kind: "recover", incident: "00".repeat(32), timeoutMs: 1000 });
  for (const argv of [
    ["--force"],
    ["--reset"],
    ["--incident", "old"],
    ["--incident", "00".repeat(32), "--timeout-ms", "0"],
  ])
    expect(() => parseL1RecoveryCommand(argv)).toThrow(
      "invalid_recovery_arguments",
    );
});

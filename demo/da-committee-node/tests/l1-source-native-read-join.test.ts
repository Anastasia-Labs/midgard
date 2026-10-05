import { createServer } from "node:http";
import { get } from "node:http";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { afterAll, afterEach, expect, it, vi } from "vitest";

import {
  l1SourceAuthorityDigest,
  type LoadedCommitteeConfig,
} from "../src/config.js";
import { FileChainSyncConsumerCursorStore } from "../src/l1/provider.file-chain-sync-consumer-cursor-store.js";
import { FileChainSyncCursorStore } from "../src/l1/provider.file-chain-sync-cursor-store.js";
import { LocalNodeChainAuthority } from "../src/l1/provider.local-node-chain-authority.js";
import { LocalNodeStateQueueProvider } from "../src/l1/provider.local-node-state-queue-provider.js";
import {
  RECOVERABLE_L1_REASON,
  verifyL1Recovery,
} from "../src/l1/recovery-incident.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { makePayloadFixture, minimalConfig, tempDir } from "./helpers.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";
import { localSignature } from "./helpers/quarantined-committee-store.js";
import { createStateQueueChain } from "./helpers/state-queue-chain.js";

const databases = postgresTestDatabases("codex_rel_cq");
const opened = new Set<CommitteeStore>();
afterEach(async () => {
  for (const store of opened) await store.close?.();
  opened.clear();
});
afterAll(() => databases.dropAll());
const fixture = async (backend: "JSON" | "Postgres", withSignature = false) => {
  const dir = await tempDir();
  const store =
    backend === "JSON"
      ? await JsonFileCommitteeStore.open(dir)
      : await PostgresCommitteeStore.open((await databases.create()).url);
  opened.add(store);
  const base = minimalConfig({
    dir,
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
    dir,
    chain,
    authority,
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

it.each([
  { expire: false, fail: true },
  { expire: true, fail: true },
  { expire: true, fail: false },
])(
  "joins a physical sibling before CQ refusal (%j)",
  async ({ expire, fail }) => {
    const f = await fixture("JSON");
    let arrived!: () => void;
    const started = new Promise<void>((resolve) => {
      arrived = resolve;
    });
    let finishResponse: (() => void) | undefined;
    let readEnded = false;
    const server = createServer((_request, response) => {
      response.setHeader("connection", "close");
      finishResponse = () => response.end("complete snapshot read");
      arrived();
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("test listener missing");
    const pending = {
      ...f.query,
      fetchStateQueueSnapshot: async () => {
        await new Promise<void>((resolve, reject) => {
          get(`http://127.0.0.1:${address.port}`, (response) => {
            response.resume();
            response.once("end", () => {
              readEnded = true;
              resolve();
            });
            response.once("error", reject);
          }).once("error", reject);
        });
        return f.chain.snapshot();
      },
    };
    const broken = {
      ...f.query,
      fetchStateQueueSnapshot: async () => {
        await started;
        if (fail) throw new Error("actual query surface unavailable");
        return f.chain.snapshot();
      },
    };
    const provider = new LocalNodeStateQueueProvider(
      f.authority,
      [broken, pending],
      ["broken", "physical-sibling"],
      new FileChainSyncConsumerCursorStore(
        join(f.dir, "consumed.json"),
        "11".repeat(32),
      ),
    );
    const ack = vi.spyOn(provider, "acknowledgeChainSyncCursor");
    let returned = false;
    const verification = verifyL1Recovery({ ...f, provider }).catch((error) => {
      returned = true;
      return error;
    });
    try {
      await started;
      for (let i = 0; i < 5; i++)
        await new Promise<void>((resolve) => setImmediate(resolve));
      expect(readEnded).toBe(false);
      expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
      expect(ack).not.toHaveBeenCalled();
      expect(returned).toBe(false);
      if (expire) f.scope.close();
      finishResponse!();
      const error = await verification;
      expect(readEnded).toBe(true);
      expect(error).toBeInstanceOf(Error);
      expect((error as Error).message).toBe(
        fail
          ? "actual query surface unavailable"
          : "Availability read scope closed",
      );
      expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
      expect(ack).not.toHaveBeenCalled();
    } finally {
      finishResponse?.();
      await verification;
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
      f.scope.close();
    }
  },
);

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "midgard-watcher";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture";
import { beforeEach, expect, it, vi } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { verifyJourneyPublicDa } from "./public-da-preflight.js";

const mocked = vi.hoisted(() => ({
  create: vi.fn(),
  fetch: vi.fn(),
  close: vi.fn(),
}));
vi.mock("midgard-watcher", async (importOriginal) => ({
  ...(await importOriginal<typeof import("midgard-watcher")>()),
  createWatcherRetainedDaRuntime: mocked.create,
}));

const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
const peerId = "12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
const watcherConfig = parseWatcherConfig({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: "11".repeat(32),
          endpoint: "https://a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: "22".repeat(32),
          endpoint: "https://b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 30,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 30,
      },
    },
  },
  da: {
    peers: [
      {
        identity: "committee",
        multiaddr: `/dns4/da.example/tcp/443/p2p/${peerId}`,
      },
    ],
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_TEST_ROLLBACK_KEY",
    },
  },
  proverWallet: {
    keySource: { kind: "environment", variable: "MIDGARD_TEST_PROVER_KEY" },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});
const predecessor = {
  headerHash: "aa".repeat(28),
  payloadEnvelopeCbor: Buffer.from("820102", "hex"),
};
const current = {
  headerHash: "bb".repeat(28),
  payloadEnvelopeCbor: Buffer.from("820103", "hex"),
};

beforeEach(() => {
  vi.resetAllMocks();
  mocked.create.mockResolvedValue({
    deploymentFingerprint: deploymentIdentity.manifestId,
    sources: [{ fetchPayloadByHeaderHash: mocked.fetch }],
    close: mocked.close,
  });
  mocked.fetch.mockImplementation(async (hash: string) => ({
    ok: true,
    sourcePeerId: peerId,
    payloadEnvelopeCbor:
      hash === predecessor.headerHash
        ? predecessor.payloadEnvelopeCbor
        : current.payloadEnvelopeCbor,
  }));
});

it("uses only the actual config and identity, fetches both headers, and records exact byte receipts", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-da-preflight-"));
  try {
    const receipt = await verifyJourneyPublicDa({
      directory,
      watcherConfig,
      deploymentIdentity,
      predecessor,
      current,
    });
    expect(mocked.create).toHaveBeenCalledExactlyOnceWith({
      watcherConfig,
      deploymentIdentity,
    });
    expect(mocked.fetch.mock.calls).toEqual([
      [predecessor.headerHash],
      [current.headerHash],
    ]);
    expect(receipt).toMatchObject({
      outcome: "passed",
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    expect(receipt.receipts).toHaveLength(2);
    expect(
      receipt.receipts.every(
        (item) =>
          item.sourcePeerId === peerId &&
          item.bytes === 3 &&
          item.durationMs >= 0,
      ),
    ).toBe(true);
    expect(
      await readJourneyArtifact(join(directory, "public-da-preflight.json")),
    ).toEqual(receipt);
    expect(mocked.close).toHaveBeenCalledOnce();
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

it.each(["different bytes", "different peer", "transport failure"])(
  "stops and preserves failed preflight evidence on %s",
  async (kind) => {
    const directory = await mkdtemp(join(tmpdir(), "journey-da-preflight-"));
    try {
      mocked.fetch.mockResolvedValueOnce({
        ok: true,
        sourcePeerId: peerId,
        payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
      });
      mocked.fetch.mockResolvedValueOnce(
        kind === "transport failure"
          ? { ok: false, attempts: [{ status: "transport_error" }] }
          : {
              ok: true,
              sourcePeerId: kind === "different peer" ? "l1-fallback" : peerId,
              payloadEnvelopeCbor: Buffer.from("changed"),
            },
      );
      await expect(
        verifyJourneyPublicDa({
          directory,
          watcherConfig,
          deploymentIdentity,
          predecessor,
          current,
        }),
      ).rejects.toThrow();
      const receipt = await readJourneyArtifact<{
        outcome: string;
        receipts: unknown[];
        error: string;
      }>(join(directory, "public-da-preflight.json"));
      expect(receipt.outcome).toBe("failed");
      expect(receipt.receipts).toHaveLength(1);
      expect(receipt.error).toBeTruthy();
      expect(mocked.close).toHaveBeenCalledOnce();
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  },
);

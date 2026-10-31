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
import {
  fetchJourneyPublicDaPayload,
  type OpenJourneyPublicDaRuntime,
  verifyJourneyPublicDa,
  verifyStackJourneyPublicDa,
} from "./public-da-preflight.js";

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
      sourceMode: "local_node",
      authorityNodeId: "watcher-node",
      chainSync: {
        kind: "cardano_node_socket",
        socketPath: "/run/cardano/node.socket",
        nodeConfigPath: "/etc/cardano/config.json",
        genesisConfigPath: "/etc/cardano/shelley-genesis.json",
        genesisIdentitySha256: "11".repeat(32),
      },
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: { depth: 30 },
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

/**
 * Public DA as an attached stack serves it: each peer answers from a script
 * of answers, one per ask with the last repeating; the clock is the pause.
 */
const servingAfter = (
  answers: readonly ("missing" | "served")[],
  servingPeerIds: readonly string[] = [peerId],
) => {
  const asked: string[] = [];
  let closed = 0;
  let round = 0;
  const openRuntime = (async () => ({
    deploymentFingerprint: deploymentIdentity.manifestId,
    sources: servingPeerIds.map((sourcePeerId) => ({
      fetchPayloadByHeaderHash: async (headerHash: string) => {
        asked.push(sourcePeerId);
        return answers[Math.min(round, answers.length - 1)] === "served" &&
          sourcePeerId === servingPeerIds.at(-1)
          ? {
              ok: true,
              sourcePeerId,
              payloadEnvelopeCbor:
                headerHash === current.headerHash
                  ? current.payloadEnvelopeCbor
                  : predecessor.payloadEnvelopeCbor,
            }
          : { ok: false, attempts: [{ status: "not_found" }] };
      },
    })),
    close: async () => {
      closed += 1;
    },
  })) as unknown as OpenJourneyPublicDaRuntime;
  let clock = 0;
  const availability = {
    waitMs: 10_000,
    now: () => clock,
    pause: async () => {
      clock += 2_000;
      round += 1;
    },
  };
  return { openRuntime, availability, asked, closed: () => closed };
};

it("waits while public DA trails the stack's attestation, then returns the bytes a peer serves", async () => {
  const peers = ["12D3KooWFirstPeerWithoutTheCopy", peerId];
  const da = servingAfter(["missing", "missing", "served"], peers);
  await expect(
    fetchJourneyPublicDaPayload({
      watcherConfig,
      deploymentIdentity,
      headerHash: current.headerHash,
      availability: da.availability,
      openRuntime: da.openRuntime,
    }),
  ).resolves.toEqual({
    payloadEnvelopeCbor: current.payloadEnvelopeCbor,
    sourcePeerId: peerId,
  });
  expect(da.asked).toEqual([...peers, ...peers, ...peers]);
  expect(da.closed()).toBe(1);
});

it.each([
  ["once the availability wait ends", true, 6],
  ["at the first answer without one", false, 1],
])(
  "fails naming the header when no peer serves it, %s",
  async (_, waits, asks) => {
    const da = servingAfter(["missing"]);
    await expect(
      fetchJourneyPublicDaPayload({
        watcherConfig,
        deploymentIdentity,
        headerHash: current.headerHash,
        ...(waits ? { availability: da.availability } : {}),
        openRuntime: da.openRuntime,
      }),
    ).rejects.toThrow(`Public DA serves no payload for ${current.headerHash}`);
    expect(da.asked).toHaveLength(asks);
    expect(da.closed()).toBe(1);
  },
);

it("proves an attached stack's public DA serves both staged payloads exactly, waiting while it trails", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-da-preflight-"));
  try {
    const peers = ["12D3KooWFirstPeerWithoutTheCopy", peerId];
    const da = servingAfter(["missing", "served"], peers);
    const evidence = await verifyStackJourneyPublicDa({
      directory,
      watcherConfig,
      deploymentIdentity,
      predecessor,
      current,
      availability: da.availability,
      openRuntime: da.openRuntime,
    });
    expect(evidence).toMatchObject({
      outcome: "passed",
      deploymentFingerprint: deploymentIdentity.manifestId,
      receipts: [
        { headerHash: predecessor.headerHash, sourcePeerId: peerId, bytes: 3 },
        { headerHash: current.headerHash, sourcePeerId: peerId, bytes: 3 },
      ],
    });
    expect(
      await readJourneyArtifact(join(directory, "public-da-preflight.json")),
    ).toEqual(evidence);

    const changed = servingAfter(["served"]);
    await expect(
      verifyStackJourneyPublicDa({
        directory,
        watcherConfig,
        deploymentIdentity,
        predecessor,
        current: {
          ...current,
          payloadEnvelopeCbor: Buffer.from("8201", "hex"),
        },
        availability: changed.availability,
        openRuntime: changed.openRuntime,
      }),
    ).rejects.toThrow(`Public DA changed staged payload ${current.headerHash}`);
    expect(
      await readJourneyArtifact(join(directory, "public-da-preflight.json")),
    ).toMatchObject({
      outcome: "failed",
      receipts: [{ headerHash: predecessor.headerHash }],
    });
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

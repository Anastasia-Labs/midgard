/**
 * Every committee Lucid construction site on a `Custom` network: the slot
 * mapping comes from the site's own Ogmios (network magic, Shelley genesis,
 * submit-slot snapshot) or the client is refused by name before Lucid is
 * built. Named networks make no Ogmios slot query. The Ogmios here is a local
 * HTTP JSON-RPC fake.
 */
import { createServer, type IncomingMessage, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { join } from "node:path";

import {
  Blockfrost,
  Kupmios,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  SLOT_CONFIG_NETWORK,
  type SlotConfig,
} from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderFromConfig } from "../src/availability/factory.js";
import type {
  CommitteeL1ClientConfig,
  LoadedCommitteeConfig,
} from "../src/config.js";
import {
  l1SubmitterWalletPreflightFromConfig,
  onChainCoordinatorFromConfig,
} from "../src/coordinator/factory.js";
import { daAttestationReaderFromConfig } from "../src/l1/da-attestation-reader.js";
import { lucidFromProviderUrl } from "../src/l1/lucid.js";
import {
  committeeLucidSlotOptions,
  DaCommitteeCustomSlotMappingError,
} from "../src/l1/lucid-network.js";
import { providerFromUrl } from "../src/l1/provider.js";
import { minimalConfig, tempDir } from "./helpers.js";

const CUSTOM_MAGIC = 424_242;
const PREPROD_MAGIC = 1;
const SLOT_LENGTH_MS = 1_000;
const MAGIC_PREFLIGHT_ID = "midgard-network-magic-preflight";

type FakeOgmiosOptions = {
  readonly networkMagic: number;
  readonly genesisSlotLengthMs?: number;
  readonly failSlotGenesisQuery?: boolean;
};

type FakeOgmios = {
  readonly url: string;
  readonly startTimeMs: number;
  /** One entry per request: "health", or the JSON-RPC method and id. */
  readonly requests: string[];
  readonly close: () => Promise<void>;
};

const readBody = (request: IncomingMessage): Promise<string> =>
  new Promise((resolve, reject) => {
    let body = "";
    request.on("data", (chunk: Buffer) => (body += chunk.toString("utf8")));
    request.on("end", () => resolve(body));
    request.on("error", reject);
  });

const servers: Server[] = [];

/**
 * A synchronized Ogmios whose chain started an hour ago on whole seconds, so
 * the slot it reports is the one its Shelley genesis puts at the wall clock.
 */
const startFakeOgmios = async ({
  networkMagic,
  genesisSlotLengthMs = SLOT_LENGTH_MS,
  failSlotGenesisQuery = false,
}: FakeOgmiosOptions): Promise<FakeOgmios> => {
  const startTimeMs = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;
  const currentSlot = () =>
    Math.floor((Date.now() - startTimeMs) / SLOT_LENGTH_MS);
  const requests: string[] = [];
  const server = createServer((request, response) => {
    void (async () => {
      const reply = (status: number, body: unknown) => {
        response.writeHead(status, { "content-type": "application/json" });
        response.end(JSON.stringify(body));
      };
      if (request.method === "GET" && request.url === "/health") {
        requests.push("health");
        const slot = currentSlot();
        return reply(200, {
          connectionStatus: "connected",
          networkSynchronization: 1,
          lastKnownTip: { slot },
          lastTipUpdate: new Date(
            startTimeMs + slot * SLOT_LENGTH_MS,
          ).toISOString(),
        });
      }
      const rpc = JSON.parse(await readBody(request)) as {
        readonly method: string;
        readonly id: string;
      };
      requests.push(`${rpc.method}#${rpc.id}`);
      if (rpc.method === "queryNetwork/tip") {
        return reply(200, { jsonrpc: "2.0", result: { slot: currentSlot() } });
      }
      if (rpc.method === "queryNetwork/genesisConfiguration") {
        if (failSlotGenesisQuery && rpc.id !== MAGIC_PREFLIGHT_ID) {
          return reply(500, { error: "genesis unavailable" });
        }
        return reply(200, {
          jsonrpc: "2.0",
          result: {
            networkMagic,
            startTime: new Date(startTimeMs).toISOString(),
            slotLength: { milliseconds: genesisSlotLengthMs },
          },
        });
      }
      return reply(404, { error: `unexpected ${rpc.method}` });
    })();
  });
  servers.push(server);
  await new Promise<void>((resolve) =>
    server.listen(0, "127.0.0.1", () => resolve()),
  );
  const { port } = server.address() as AddressInfo;
  return {
    url: `ws://127.0.0.1:${port.toString()}`,
    startTimeMs,
    requests,
    close: () =>
      new Promise((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      ),
  };
};

const genesisMapping = (ogmios: FakeOgmios): SlotConfig => ({
  zeroTime: ogmios.startTimeMs,
  zeroSlot: 0,
  slotLength: SLOT_LENGTH_MS,
});

const kupmiosUrl = (ogmios: FakeOgmios) =>
  `kupmios:http://127.0.0.1:1442|${ogmios.url}`;

let lucidBuilds: () => number;

beforeEach(() => {
  // Lucid reads protocol parameters when it is built, and at no other point
  // on these paths: no read means no Lucid.
  const kupmios = vi
    .spyOn(Kupmios.prototype, "getProtocolParameters")
    .mockResolvedValue(PROTOCOL_PARAMETERS_DEFAULT);
  const blockfrost = vi
    .spyOn(Blockfrost.prototype, "getProtocolParameters")
    .mockResolvedValue(PROTOCOL_PARAMETERS_DEFAULT);
  lucidBuilds = () => kupmios.mock.calls.length + blockfrost.mock.calls.length;
});

afterEach(async () => {
  await Promise.all(
    servers.splice(0).map(
      (server) =>
        new Promise<void>((resolve) => {
          server.closeAllConnections();
          server.close(() => resolve());
        }),
    ),
  );
});

/** Private field read: the L1 clients keep their Lucid to themselves. */
const lucidOf = (holder: unknown): LucidEvolution =>
  (holder as { readonly lucid: LucidEvolution }).lucid;

type KupmiosSite = {
  readonly name: string;
  readonly build: (input: {
    readonly network: string;
    readonly ogmios: FakeOgmios;
    readonly networkMagic: number;
  }) => Promise<LucidEvolution>;
};

const KUPMIOS_SITES: readonly KupmiosSite[] = [
  {
    name: "the state-queue provider (providerFromUrl)",
    build: async ({ network, ogmios, networkMagic }) =>
      lucidOf(
        await providerFromUrl(kupmiosUrl(ogmios), {
          network,
          cardanoL1Source: {
            sourceMode: "local_node",
            authorityNodeId: "local-cardano-node",
            authorityDigest: "ab".repeat(32),
            networkMagic,
          },
          stateQueueAddress: "addr_test1statequeue",
          stateQueuePolicyId: "cc".repeat(28),
          deploymentFingerprint: "f".repeat(64),
          finalityDepth: 1,
          hubOraclePolicyId: "99".repeat(28),
          correctionLockAddress: "addr_test1correctionlock",
          fraudProofPolicyId: "98".repeat(28),
          fraudProofAddress: "addr_test1fraudproof",
        }),
      ),
  },
  {
    name: "the coordinator and availability client (lucidFromProviderUrl)",
    build: async ({ network, ogmios, networkMagic }) =>
      (
        await lucidFromProviderUrl(
          kupmiosUrl(ogmios),
          network,
          undefined,
          networkMagic,
        )
      ).lucid,
  },
  {
    name: "the DA attestation reader",
    build: async ({ network, ogmios, networkMagic }) => {
      const dir = await tempDir();
      const reader = await daAttestationReaderFromConfig({
        network,
        cardanoL1Source: {
          sourceMode: "local_node",
          authorityNodeId: "local-cardano-node",
          authorityDigest: "ab".repeat(32),
          networkMagic,
        },
        l1Source: {
          sourceMode: "local_node",
          authorityNodeId: "local-cardano-node",
          chainSyncProviderUrl: `chain-sync:ogmios:${ogmios.url}`,
          chainSyncCursorPath: join(dir, "chain-sync-cursor.json"),
          queryProviderUrls: [kupmiosUrl(ogmios)],
        },
        localState: { kind: "file", path: join(dir, "state.json") },
      } as unknown as LoadedCommitteeConfig);
      return lucidOf(reader);
    },
  },
];

describe("committeeLucidSlotOptions", () => {
  it("adds nothing on a named network and queries no Ogmios", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: PREPROD_MAGIC });
    for (const network of ["Mainnet", "Preprod", "Preview"] as const) {
      await expect(
        committeeLucidSlotOptions({
          network,
          route: { provider: "kupmios", ogmiosUrl: ogmios.url },
          networkMagic: PREPROD_MAGIC,
        }),
      ).resolves.toEqual({});
    }
    expect(ogmios.requests).toEqual([]);
  });

  it("derives the Custom mapping from the route's Ogmios after its magic matches", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    await expect(
      committeeLucidSlotOptions({
        network: "Custom",
        route: { provider: "kupmios", ogmiosUrl: ogmios.url },
        networkMagic: CUSTOM_MAGIC,
      }),
    ).resolves.toEqual({ slotConfig: genesisMapping(ogmios) });
    expect(ogmios.requests[0]).toBe(
      `queryNetwork/genesisConfiguration#${MAGIC_PREFLIGHT_ID}`,
    );
  });
});

describe.each(KUPMIOS_SITES)("$name on Custom", ({ build }) => {
  it("builds Lucid on the slot mapping of its Ogmios's Shelley genesis", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    const lucid = await build({
      network: "Custom",
      ogmios,
      networkMagic: CUSTOM_MAGIC,
    });
    expect(lucid.config().slotConfig).toEqual(genesisMapping(ogmios));
  });

  it.each([
    {
      failure: "a failing Shelley genesis query",
      ogmios: { networkMagic: CUSTOM_MAGIC, failSlotGenesisQuery: true },
      reason:
        /the Shelley genesis query from the Ogmios at .* failed: HTTP 500/u,
    },
    {
      failure: "an Ogmios on another network",
      ogmios: { networkMagic: 42 },
      reason:
        /the network-magic check from the Ogmios at .* failed: Ogmios network magic does not match/u,
    },
    {
      failure: "a genesis slot length the snapshot disagrees with",
      ogmios: { networkMagic: CUSTOM_MAGIC, genesisSlotLengthMs: 2_000 },
      reason:
        /the slot mapping check from the Ogmios at .* failed: Custom slot length disagreement/u,
    },
  ])("refuses $failure by name before Lucid is built", async (scenario) => {
    const ogmios = await startFakeOgmios(scenario.ogmios);
    const built = build({
      network: "Custom",
      ogmios,
      networkMagic: CUSTOM_MAGIC,
    });
    await expect(built).rejects.toThrow(DaCommitteeCustomSlotMappingError);
    await expect(built).rejects.toThrow(scenario.reason);
    await expect(built).rejects.toThrow(ogmios.url);
    expect(lucidBuilds()).toBe(0);
  });
});

describe.each(KUPMIOS_SITES)("$name on Preprod", ({ name, build }) => {
  it("keeps Lucid's built-in mapping and makes no Ogmios slot query", async () => {
    const ogmios = await startFakeOgmios({ networkMagic: PREPROD_MAGIC });
    const lucid = await build({
      network: "Preprod",
      ogmios,
      networkMagic: PREPROD_MAGIC,
    });
    expect(lucid.config().slotConfig).toEqual(SLOT_CONFIG_NETWORK.Preprod);
    // The state-queue provider's standing network-magic check predates the
    // slot mapping and runs on every network; nothing else may query.
    expect(ogmios.requests).toEqual(
      name.includes("providerFromUrl")
        ? [`queryNetwork/genesisConfiguration#${MAGIC_PREFLIGHT_ID}`]
        : [],
    );
  });
});

describe("Blockfrost on Custom", () => {
  const BLOCKFROST_URL = "blockfrost:https://blockfrost.example/api/v0#project";
  const BLOCKFROST_REFUSAL = new DaCommitteeCustomSlotMappingError(
    "Refusing the Custom Lucid client on Blockfrost at https://blockfrost.example/api/v0: Blockfrost serves no Custom network, so no slot mapping can be read from it; use kupmios:<kupo-url>|<ogmios-url>",
  );

  it("is refused by name at the coordinator and availability client", async () => {
    await expect(
      lucidFromProviderUrl(BLOCKFROST_URL, "Custom", undefined, CUSTOM_MAGIC),
    ).rejects.toThrow(BLOCKFROST_REFUSAL);
    expect(lucidBuilds()).toBe(0);
  });

  it("is refused by name at the DA attestation reader", async () => {
    await expect(
      daAttestationReaderFromConfig({
        network: "Custom",
        cardanoL1Source: {
          sourceMode: "external_providers",
          providerAuthorityIds: ["a", "b"],
          authorityDigest: "ab".repeat(32),
          networkMagic: CUSTOM_MAGIC,
        },
        l1Source: {
          sourceMode: "external_providers",
          providers: [
            {
              identity: "a",
              url: BLOCKFROST_URL,
              operationalIdentity: {
                operatorId: "a",
                transport: "https",
                backendKey: "a",
              },
            },
          ],
        },
      } as unknown as LoadedCommitteeConfig),
    ).rejects.toThrow(BLOCKFROST_REFUSAL);
    expect(lucidBuilds()).toBe(0);
  });
});

describe("the L1 factories main() calls build Custom Lucid on the genesis mapping", () => {
  class Built extends Error {}

  const customConfig = async (
    ogmios: FakeOgmios,
  ): Promise<CommitteeL1ClientConfig> => {
    const dir = await tempDir();
    const url = kupmiosUrl(ogmios);
    return {
      ...minimalConfig({
        dir,
        manifestPath: join(dir, "manifest.json"),
        deploymentInfoPath: join(dir, "deployment.json"),
        signerSeed: "00".repeat(32),
        signerPublicKey: "11".repeat(32),
      }),
      network: "Custom",
      cardanoL1Source: { networkMagic: CUSTOM_MAGIC },
      cardanoProviderUrls: [url],
      l1Source: {
        sourceMode: "local_node",
        authorityNodeId: "local-cardano-node",
        chainSyncProviderUrl: `chain-sync:ogmios:${ogmios.url}`,
        queryProviderUrls: [url],
      },
      l1SubmissionEnabled: true,
      l1SubmitterKeySource: "private-key:attestation",
      availabilityJournalPath: join(dir, "availability.sqlite"),
      availabilitySubmitterKeySource: "private-key:responder",
    };
  };

  /** The real site, stopped once it has built its Lucid. */
  const capturingSite = () => {
    const built: LucidEvolution[] = [];
    return {
      built,
      lucidFromProviderUrl: async (
        ...args: Parameters<typeof lucidFromProviderUrl>
      ): ReturnType<typeof lucidFromProviderUrl> => {
        built.push((await lucidFromProviderUrl(...args)).lucid);
        throw new Built();
      },
    };
  };

  const unreachable = async (): Promise<never> => {
    throw new Error("unreachable after the Lucid client is built");
  };

  it.each([
    {
      factory: "onChainCoordinatorFromConfig",
      run: (
        config: CommitteeL1ClientConfig,
        site: ReturnType<typeof capturingSite>,
      ) =>
        onChainCoordinatorFromConfig(config, {} as never, undefined, {
          lucidFromProviderUrl: site.lucidFromProviderUrl,
          selectL1SubmitterWallet: unreachable,
          assertL1SubmitterWalletPreflight: unreachable,
          preflightL1SubmitterWallet: unreachable,
          fetchDaAttestationReferenceScripts: unreachable,
        }),
    },
    {
      factory: "l1SubmitterWalletPreflightFromConfig",
      run: (
        config: CommitteeL1ClientConfig,
        site: ReturnType<typeof capturingSite>,
      ) =>
        l1SubmitterWalletPreflightFromConfig(config, {
          lucidFromProviderUrl: site.lucidFromProviderUrl,
          selectL1SubmitterWallet: unreachable,
          preflightL1SubmitterWallet: unreachable,
        }),
    },
    {
      factory: "availabilityResponderFromConfig",
      run: (
        config: CommitteeL1ClientConfig,
        site: ReturnType<typeof capturingSite>,
      ) =>
        availabilityResponderFromConfig(
          config,
          {} as never,
          {
            fetchStateQueueNodes: async () => [],
            currentChainSyncCursor: unreachable,
          },
          { lucidFromProviderUrl: site.lucidFromProviderUrl },
        ),
    },
  ])("$factory", async ({ run }) => {
    const ogmios = await startFakeOgmios({ networkMagic: CUSTOM_MAGIC });
    const site = capturingSite();
    await expect(run(await customConfig(ogmios), site)).rejects.toThrow(Built);
    expect(site.built).toHaveLength(1);
    expect(site.built[0]!.config().slotConfig).toEqual(genesisMapping(ogmios));
  });
});

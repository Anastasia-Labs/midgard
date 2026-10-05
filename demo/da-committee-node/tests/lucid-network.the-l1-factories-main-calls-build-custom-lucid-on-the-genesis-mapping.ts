import { join } from "node:path";

import {
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

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
import { DaCommitteeCustomSlotMappingError } from "../src/l1/lucid-network.js";
import { minimalConfig, tempDir } from "./helpers.js";
import {
  CUSTOM_MAGIC,
  type FakeOgmios,
  genesisMapping,
  KUPMIOS_SITES,
  kupmiosUrl,
  lucidBuilds,
  MAGIC_PREFLIGHT_ID,
  PREPROD_MAGIC,
  startFakeOgmios,
} from "./lucid-network.start-fake-ogmios.js";

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
          { getRetirementFloor: async () => undefined } as never,
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

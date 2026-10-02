import { Kupmios, Lucid } from "@lucid-evolution/lucid";

import type { LoadedCommitteeConfig } from "../config.js";
import {
  assertOgmiosNetworkMagic,
  committeeLucidSlotOptions,
} from "./lucid-network.js";
import { chainPointBatchDeadlineMs } from "./provider.chain-point-batch.js";
import { FileChainSyncConsumerCursorStore } from "./provider.file-chain-sync-consumer-cursor-store.js";
import {
  l1AuthorityProviderSource,
  localAuthorityFingerprint,
  localNodeChainAuthorityFromConfig,
  parseKupmiosUrl,
} from "./provider.local-node-chain-authority-from-config.js";
import {
  localNodeChainCursorPath,
  LocalNodeStateQueueProvider,
  requireStateQueueReplaySource,
} from "./provider.local-node-state-queue-provider.js";
import {
  type ChainPointAwareStateQueueProvider,
  LucidStateQueueProvider,
} from "./provider.lucid-state-queue-provider.js";
import { MultiStateQueueProvider } from "./provider.multi-state-queue-provider.js";
import {
  assertNetworkMagic,
  BLOCKFROST_REQUEST_TIMEOUT_MS,
} from "./provider.ogmios-rpc-session.js";
import { FixtureStateQueueProvider } from "./provider.parse-fixture-chain-sync-events.js";
import {
  type CanonicalChainPoint,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import {
  kupmiosChainPointResolver,
  kupmiosCurrentChainPointResolver,
  normalizeNetwork,
} from "./provider.request-ogmios-descendant-depth.js";
import {
  createLocalKupmiosStateQueueReplayProvider,
  fetchOgmiosTipBlockNo,
  kupoHoldsChainPoint,
} from "./state-queue-replay-provider.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

export const providerFromConfig = async (
  config: LoadedCommitteeConfig,
): Promise<StateQueueProvider> => {
  const urls = config.cardanoProviderUrls;
  if (config.l1Source.sourceMode === "local_node") {
    const localSource = config.l1Source;
    const authority = localNodeChainAuthorityFromConfig(config);
    const cursorPath = localNodeChainCursorPath(localSource, config.localState);
    const authorityFingerprint = localAuthorityFingerprint(
      config.network,
      localSource.authorityNodeId,
      localSource.chainSyncProviderUrl,
    );
    const queryProviders = await Promise.all(
      urls.map(async (url) =>
        requireStateQueueReplaySource(url, await providerFromUrl(url, config)),
      ),
    );
    const pointAware = queryProviders.map((provider, index) => {
      if (!("currentChainPoint" in provider)) {
        throw new Error(
          `local_node query surface ${index.toString()} cannot prove its current chain point`,
        );
      }
      return provider as ChainPointAwareStateQueueProvider;
    });
    return new LocalNodeStateQueueProvider(
      authority,
      pointAware,
      urls.map(
        (_, index) =>
          `query:${localSource.authorityNodeId}:${index.toString()}`,
      ),
      new FileChainSyncConsumerCursorStore(
        `${cursorPath}.watcher-consumer-v1`,
        authorityFingerprint,
      ),
    );
  }
  if (
    config.cardanoL1Source.sourceMode === "external_providers" &&
    (urls.length < 2 ||
      config.cardanoL1Source.providerAuthorityIds.length !== urls.length)
  ) {
    throw new Error(
      "external_providers mode requires at least two matched provider authority identities",
    );
  }
  const providers = await Promise.all(
    urls.map(async (url, index) =>
      requireStateQueueReplaySource(
        url,
        await providerFromUrl(url, config, index),
      ),
    ),
  );
  return new MultiStateQueueProvider(providers, {
    sourceMode: "external_providers",
    identities: config.l1Source.providers.map(({ identity }) => identity),
  });
};

export const providerFromUrl = async (
  url: string,
  config: Pick<
    LoadedCommitteeConfig,
    "network" | "cardanoL1Source" | "stateQueueAddress" | "stateQueuePolicyId"
  > & {
    readonly deploymentFingerprint?: string;
    readonly finalityDepth?: number;
    /** Bounds one snapshot's chain-point resolution; see the batch module. */
    readonly l1ViewFatalMs?: number;
    readonly hubOraclePolicyId?: string;
    readonly correctionLockAddress?: string;
    readonly fraudProofPolicyId?: string;
    readonly fraudProofAddress?: string;
  },
  providerIndex = 0,
): Promise<StateQueueProvider> => {
  if (url.startsWith("fixture:")) {
    return new FixtureStateQueueProvider(
      url.slice("fixture:".length),
      config.network,
    );
  }
  if (url.startsWith("file:")) {
    return new FixtureStateQueueProvider(new URL(url).pathname, config.network);
  }
  if (url.startsWith("blockfrost:")) {
    // Blockfrost serves no authenticated ordered state-queue history, so a
    // committee reading the queue through it could never follow a change.
    throw new Error(
      "blockfrost: cannot serve the state queue: it has no authenticated ordered history source; use kupmios:<kupo-url>|<ogmios-url>",
    );
  }
  if (url.startsWith("kupmios:")) {
    if (config.deploymentFingerprint === undefined) {
      throw new Error(
        "Kupmios state-queue replay requires a deployment fingerprint",
      );
    }
    if (config.finalityDepth === undefined) {
      throw new Error("Kupmios state-queue replay requires the finality depth");
    }
    if (
      config.hubOraclePolicyId === undefined ||
      config.correctionLockAddress === undefined ||
      config.fraudProofPolicyId === undefined ||
      config.fraudProofAddress === undefined
    ) {
      throw new Error(
        "Kupmios state-queue replay requires deployment-bound CorrectionLock and fraud-proof identities",
      );
    }
    const { kupoUrl, ogmiosUrl, headers } = parseKupmiosUrl(url);
    const network = normalizeNetwork(config.network);
    const slotOptions = await committeeLucidSlotOptions({
      network,
      route: { provider: "kupmios", ogmiosUrl },
      networkMagic: config.cardanoL1Source.networkMagic,
    });
    await assertOgmiosNetworkMagic(
      ogmiosUrl,
      config.cardanoL1Source.networkMagic,
    );
    const lucid = await Lucid(
      new Kupmios(kupoUrl, ogmiosUrl, headers),
      network,
      slotOptions,
    );
    return new LucidStateQueueProvider({
      lucid,
      stateQueueAddress: config.stateQueueAddress,
      stateQueuePolicyId: config.stateQueuePolicyId,
      providerSource: l1AuthorityProviderSource(
        config,
        providerIndex,
        `kupmios:${kupoUrl}|${ogmiosUrl}`,
      ),
      chainPointResolver: kupmiosChainPointResolver(
        lucid,
        kupoUrl,
        fetch,
        ogmiosUrl,
        config.network,
        Math.max(1, config.finalityDepth),
        config.cardanoL1Source.networkMagic,
        config.l1ViewFatalMs === undefined
          ? {}
          : {
              batchDeadlineMs: chainPointBatchDeadlineMs(config.l1ViewFatalMs),
            },
      ),
      currentChainPointResolver: kupmiosCurrentChainPointResolver(
        config.network,
        kupoUrl,
        ogmiosUrl,
        config.cardanoL1Source.networkMagic,
      ),
      tipBlockNoResolver: () => fetchOgmiosTipBlockNo(ogmiosUrl, fetch),
      chainPointHeldResolver: (point) =>
        kupoHoldsChainPoint(kupoUrl, point, fetch),
      replayCheckpoints: createLocalKupmiosStateQueueReplayProvider({
        deploymentIdentityDigest: config.deploymentFingerprint,
        stateQueuePolicyId: config.stateQueuePolicyId,
        stateQueueAddress: config.stateQueueAddress,
        hubOraclePolicyId: config.hubOraclePolicyId,
        correctionLockAddress: config.correctionLockAddress,
        fraudProofPolicyId: config.fraudProofPolicyId,
        fraudProofAddress: config.fraudProofAddress,
        kupoUrl,
        ogmiosUrl,
      }),
    });
  }
  throw new Error(
    `unsupported CARDANO_PROVIDER_URLS entry ${url}; supported forms are fixture:<path>, file:<path>, and kupmios:<kupo-url>|<ogmios-url>`,
  );
};

export const parseBlockfrostUrl = (
  value: string,
): { readonly apiUrl: string; readonly projectId: string } => {
  const raw = value.slice("blockfrost:".length);
  const hashIndex = raw.lastIndexOf("#");
  if (hashIndex <= 0 || hashIndex === raw.length - 1) {
    throw new Error(
      "blockfrost provider URL must be blockfrost:<api-url>#<project-id>",
    );
  }
  return {
    apiUrl: raw.slice(0, hashIndex),
    projectId: raw.slice(hashIndex + 1),
  };
};

export const blockfrostCurrentChainPointResolver =
  (
    network: string,
    apiUrl: string,
    projectId: string,
    timeoutMs = BLOCKFROST_REQUEST_TIMEOUT_MS,
    networkMagic?: number,
  ) =>
  async (): Promise<CanonicalChainPoint> => {
    const [latest, liveNetwork] = await Promise.all([
      blockfrostJson(
        apiUrl,
        projectId,
        "/blocks/latest",
        parseBlockfrostLatestBlock,
        timeoutMs,
      ),
      blockfrostJson(
        apiUrl,
        projectId,
        "/genesis",
        parseBlockfrostNetwork,
        timeoutMs,
      ),
    ]);
    assertNetworkMagic(
      network,
      liveNetwork.networkMagic,
      "Blockfrost",
      networkMagic,
    );
    return {
      network,
      slot: latest.slot,
      blockHash: latest.hash,
      blockHeight: latest.height,
      providerSource: `blockfrost:${apiUrl}`,
      observedAt: new Date().toISOString(),
    };
  };

const blockfrostJson = async <T>(
  apiUrl: string,
  projectId: string,
  path: string,
  parse: (value: unknown) => T,
  timeoutMs: number,
): Promise<T> => {
  const response = await fetch(`${apiUrl.replace(/\/$/, "")}${path}`, {
    headers: { project_id: projectId },
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!response.ok) {
    throw new Error(
      `Blockfrost ${path} returned ${response.status.toString()} ${response.statusText}`,
    );
  }
  return parse(await response.json());
};

type BlockfrostLatestBlock = {
  readonly slot: number;
  readonly hash: string;
  readonly height: number;
};

const parseBlockfrostLatestBlock = (value: unknown): BlockfrostLatestBlock => {
  const block = getRecord(value, "Blockfrost latest block");
  return {
    slot: safeSlot(block.slot, "Blockfrost latest block slot"),
    hash: safeBlockHash(block.hash, "Blockfrost latest block hash"),
    height: safeSlot(block.height, "Blockfrost latest block height"),
  };
};

const parseBlockfrostNetwork = (
  value: unknown,
): { readonly networkMagic: number } => {
  const result = getRecord(value, "Blockfrost genesis");
  return {
    networkMagic: safeSlot(
      result.network_magic,
      "Blockfrost genesis network magic",
    ),
  };
};

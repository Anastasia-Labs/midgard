/**
 * The watcher's runtime configuration as the tooling generates it. `devnet-stack
 * up`, the e2e full stack and the watcher-journeys harness all build it here,
 * so a field the watcher needs cannot reach one of them and miss another.
 */
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import type {
  WatcherConfig,
  WatcherConfigMode,
  WatcherL1SourceConfig,
  WatcherWalletKeySource,
} from "midgard-watcher";

/**
 * The watcher module the configuration is generated for: its schema version
 * and its own parser. `devnet-stack` passes the module it loads from the
 * watcher's dist, the others the package import.
 */
export type WatcherConfigModule = Readonly<{
  WATCHER_CONFIG_SCHEMA_VERSION: WatcherConfig["schemaVersion"];
  parseWatcherConfig: (value: unknown) => WatcherConfig;
}>;

/** Request timeouts, concurrency and deadlines. */
export type WatcherLimits = Readonly<{
  l1: Pick<WatcherConfig["l1"], "requestTimeoutMs" | "maxConcurrency">;
  da: Pick<WatcherConfig["da"], "requestTimeoutMs" | "maxConcurrency">;
  deadlines: WatcherConfig["deadlines"];
}>;

/** The limits the devnet watchers run with. */
export const DEVNET_WATCHER_LIMITS: WatcherLimits = {
  l1: { requestTimeoutMs: 30_000, maxConcurrency: 8 },
  da: { requestTimeoutMs: 30_000, maxConcurrency: 8 },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
};

/** What the caller knows about its run; every field is required. */
export type WatcherConfigFacts = Readonly<{
  mode: WatcherConfigMode;
  network: Pick<WatcherConfig, "targetNetwork" | "customNetwork">;
  source: WatcherL1SourceConfig;
  /**
   * The deployment's L1 origin, where the watcher's follower starts. The
   * watcher itself accepts a configuration without one and holds unready
   * (`l1_origin_not_configured`); a generated configuration always carries
   * the run's real origin, so `undefined` is refused.
   */
  origin: L1Origin | undefined;
  finalityDepth: number;
  daPeers: readonly Readonly<{ identity: string; multiaddr: string }>[];
  storagePath: string;
  rollbackAuthorityKeySource: WatcherWalletKeySource;
  proverKeySource: WatcherWalletKeySource;
  limits: WatcherLimits;
}>;

/**
 * The watcher configuration for `facts`, as JSON to write (`input`) and as
 * the watcher's own parser admits it (`config`): nothing is returned that the
 * watcher would refuse. Refuses without the deployment's L1 origin, never
 * falling back to a template's.
 */
export const buildWatcherConfig = (
  watcher: WatcherConfigModule,
  facts: WatcherConfigFacts,
) => {
  const { origin, limits, network } = facts;
  if (origin === undefined)
    throw new Error(
      "Cannot generate the watcher configuration: the deployment's L1 origin (l1.origin) is unknown. " +
        "A generated watcher configuration always carries the run's origin, derived from its hub-oracle nonce",
    );
  const input = {
    schemaVersion: watcher.WATCHER_CONFIG_SCHEMA_VERSION,
    mode: facts.mode,
    targetNetwork: network.targetNetwork,
    ...(network.customNetwork === undefined
      ? {}
      : { customNetwork: network.customNetwork }),
    l1: {
      source: facts.source,
      origin: { slot: origin.slot, blockHash: origin.blockHash },
      requestTimeoutMs: limits.l1.requestTimeoutMs,
      maxConcurrency: limits.l1.maxConcurrency,
      finality: { depth: facts.finalityDepth },
    },
    da: {
      peers: facts.daPeers.map(({ identity, multiaddr }) => ({
        identity,
        multiaddr,
      })),
      requestTimeoutMs: limits.da.requestTimeoutMs,
      maxConcurrency: limits.da.maxConcurrency,
    },
    storage: {
      driver: "sqlite",
      path: facts.storagePath,
      rollbackAuthorityKeySource: facts.rollbackAuthorityKeySource,
    },
    proverWallet: { keySource: facts.proverKeySource },
    deadlines: {
      daFetchMs: limits.deadlines.daFetchMs,
      daPublishMs: limits.deadlines.daPublishMs,
      proofConstructMs: limits.deadlines.proofConstructMs,
      proofSubmitMs: limits.deadlines.proofSubmitMs,
    },
  };
  return { input, config: watcher.parseWatcherConfig(input) };
};

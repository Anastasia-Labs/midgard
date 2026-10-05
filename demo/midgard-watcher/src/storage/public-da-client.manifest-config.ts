import {
  assertDeploymentMarkerMatches,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DeploymentManifest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { parseWatcherConfig } from "../runtime/config.js";
import { parseDaPeers } from "../runtime/config.parse-providers.js";
import {
  boundedInteger,
  WATCHER_CONFIG_BOUNDS,
  type WatcherConfig,
} from "../runtime/config.watcher-config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import type {
  WatcherPublicDaClock,
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequest,
} from "./public-da-client.strict-inner-payload.js";

/** Read-only DA clients need no prover wallet, L1 reader or watcher database. */
export type ManifestPublicDaClientOptions = Readonly<{
  deploymentManifest: DeploymentManifest;
  peers: readonly Readonly<{ identity: string; multiaddr: string }>[];
  requestTimeoutMs: number;
  fetchTimeoutMs: number;
  maxConcurrency: number;
}>;

export type PublicDaFetchConfig = Readonly<{
  da: WatcherConfig["da"];
  deadlines: Readonly<{ daFetchMs: number }>;
}>;

export const parseManifestPublicDaClientOptions = (
  options: ManifestPublicDaClientOptions,
): Readonly<{ manifest: DeploymentManifest; config: PublicDaFetchConfig }> => {
  const manifest = verifyFinalizedDeploymentManifest(
    structuredClone(options.deploymentManifest),
  );
  const requestTimeoutMs = boundedInteger(
    options.requestTimeoutMs,
    "$.da.requestTimeoutMs",
    WATCHER_CONFIG_BOUNDS.requestTimeoutMs,
  );
  const daFetchMs = boundedInteger(
    options.fetchTimeoutMs,
    "$.deadlines.daFetchMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  if (daFetchMs < requestTimeoutMs)
    throw new Error("DA fetch deadline must cover one request timeout");
  return {
    manifest,
    config: Object.freeze({
      da: Object.freeze({
        peers: parseDaPeers(options.peers, manifest.network),
        requestTimeoutMs,
        maxConcurrency: boundedInteger(
          options.maxConcurrency,
          "$.da.maxConcurrency",
          WATCHER_CONFIG_BOUNDS.concurrency,
        ),
      }),
      deadlines: Object.freeze({ daFetchMs }),
    }),
  };
};

export type PublicDaClientAuthorityOptions =
  | ManifestPublicDaClientOptions
  | {
      readonly config: WatcherConfig;
      readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    };
export const parsePublicDaClientAuthority = (
  options: PublicDaClientAuthorityOptions,
) => {
  let configValue: PublicDaFetchConfig;
  let fingerprint: string;
  let customNetwork: WatcherPublicDaRequest["customNetwork"];
  let manifestNetwork: WatcherPublicDaRequest["manifestNetwork"];
  if ("deploymentManifest" in options) {
    const { manifest, config } = parseManifestPublicDaClientOptions(options);
    configValue = config;
    fingerprint = manifest.manifestId;
    if (manifest.network === "Custom") {
      manifestNetwork = Object.freeze({
        deploymentManifest: manifest,
        peers: config.da.peers.map(({ identity, multiaddr }) => ({
          identity,
          multiaddr,
        })),
      });
    }
  } else {
    const config = parseWatcherConfig(options.config);
    configValue = config;
    fingerprint = options.deploymentIdentity.manifestId;
    assertDeploymentMarkerMatches(
      makeDeploymentMarker(fingerprint),
      options.deploymentIdentity.durableMarker,
      "watcher public DA client",
    );
    if (config.targetNetwork !== options.deploymentIdentity.network)
      throw new Error("target network mismatch");
    if (config.targetNetwork === "Custom") {
      assertVerifiedWatcherDeploymentIdentity(options.deploymentIdentity);
      customNetwork = Object.freeze({
        watcherConfig: config,
        deploymentIdentity: options.deploymentIdentity,
      });
    }
  }

  return { config: configValue, fingerprint, customNetwork, manifestNetwork };
};

export const assertPublicDaClientTransport = (options: {
  readonly transport: WatcherPublicDaLibp2pTransportV1;
  readonly clock?: WatcherPublicDaClock;
}): void => {
  if (
    typeof options.transport !== "object" ||
    options.transport === null ||
    typeof options.transport.request !== "function"
  ) {
    throw new Error("invalid libp2p transport");
  }
  if (
    options.clock !== undefined &&
    (typeof options.clock.now !== "function" ||
      typeof options.clock.setTimeout !== "function" ||
      typeof options.clock.clearTimeout !== "function")
  ) {
    throw new Error("invalid clock");
  }
};

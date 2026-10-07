import { createHash } from "node:crypto";
import { readFile, realpath, stat } from "node:fs/promises";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import {
  type DeploymentManifest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { WatcherConfig } from "midgard-watcher";

import {
  codeStamp,
  requireFreshDists,
  runtimeDistTargets,
} from "./dist-freshness.js";
import type { Layout } from "./layout.js";
import { releasePaths } from "./watcher-release.js";

export type AcceptanceNativeRunBinding = Readonly<{
  runDir: string;
  deploymentManifestId: string;
  releaseAuthoritySha256: string;
  codeStamp: string;
  nativeBinarySha256: string;
  publicConfigurationSha256: Readonly<Record<string, string>>;
}>;

export type AcceptanceNativeReadConfig = Readonly<{
  native: typeof import("midgard-watcher/native-chain-sync");
  watcher: typeof import("midgard-watcher");
  watcherConfig: WatcherConfig;
  binaryPath: string;
  binding: AcceptanceNativeRunBinding;
  assertUnchanged(readScope?: DaAvailabilityReadScope): Promise<void>;
}>;

/** Reads existing public run files only; never resolves any wallet/key source. */
export const loadAcceptanceNativeReadConfig = async (
  layout: Layout,
  scope: DaAvailabilityReadScope,
): Promise<AcceptanceNativeReadConfig> => {
  scope.assertCurrent();
  const [watcher, native] = await Promise.all([
    import("midgard-watcher"),
    import("midgard-watcher/native-chain-sync"),
  ]);
  scope.assertCurrent();
  for (const [specifier, file] of [
    ["midgard-watcher", "index.js"],
    ["midgard-watcher/native-chain-sync", "native-chain-sync.js"],
  ]) {
    if (
      (await realpath(fileURLToPath(import.meta.resolve(specifier!)))) !==
      (await realpath(join(layout.watcherRoot, "dist", file!)))
    )
      throw new Error(
        "acceptance native reader resolved a different watcher distribution",
      );
  }
  if (
    native.startWatcherNativeChainSync !== watcher.startWatcherNativeChainSync
  )
    throw new Error(
      "acceptance native reader has duplicate watcher receipt state",
    );
  requireFreshDists(layout, "read-only payout acceptance");
  const stamp = codeStamp(runtimeDistTargets(layout));
  const paths = releasePaths(layout);
  const binaryPath = join(layout.bin, "midgard-l1-node-transport");
  const files = [
    layout.watcherProcessConfig,
    layout.watcherRuntimeConfig,
    layout.contractManifest,
    paths.authority,
    paths.rules,
    paths.manifest,
    layout.hostCardanoConfig,
    layout.shelleyGenesis,
    binaryPath,
  ];
  const read = async (path: string, readScope = scope): Promise<Buffer> => {
    scope.assertCurrent();
    readScope.assertCurrent();
    const metadata = await stat(path);
    if (
      (await realpath(path)) !== path ||
      !metadata.isFile() ||
      metadata.size > 64 * 1024 * 1024
    )
      throw new Error(
        "acceptance native public run file path or size is invalid",
      );
    const bytes = await readFile(path, { signal: readScope.signal });
    scope.assertCurrent();
    readScope.assertCurrent();
    if (bytes.length === 0 || bytes.length > 64 * 1024 * 1024)
      throw new Error(
        "acceptance native public run file is empty or oversized",
      );
    return bytes;
  };
  const bytes = new Map<string, Buffer>();
  for (const path of files) bytes.set(path, await read(path));
  const digest = (value: Uint8Array): string =>
    createHash("sha256").update(value).digest("hex");
  const hashes = Object.freeze(
    Object.fromEntries(
      [...bytes].map(([path, value]) => [path, digest(value)]),
    ),
  );
  const json = (path: string): unknown =>
    watcher.parseWatcherStrictJsonValue(bytes.get(path)!.toString("utf8"));
  const processConfig = watcher.parseWatcherProcessConfig(
    json(layout.watcherProcessConfig),
  );
  const watcherConfig = watcher.parseWatcherConfig(
    json(layout.watcherRuntimeConfig),
  );
  const source = watcherConfig.l1.source;
  if (
    source.sourceMode !== "local_node" ||
    watcherConfig.targetNetwork !== "Custom" ||
    watcherConfig.mode !== "acceptance" ||
    processConfig.watcherRuntimeConfigPath !== layout.watcherRuntimeConfig ||
    processConfig.nativeChainSyncBinaryPath !== binaryPath ||
    processConfig.deploymentAuthorityPath !== paths.authority ||
    processConfig.ruleBundlePath !== paths.rules ||
    processConfig.faultProofInfrastructure.manifestPath !== paths.manifest ||
    source.chainSync.socketPath !== layout.cardanoSocket ||
    source.chainSync.nodeConfigPath !== layout.hostCardanoConfig ||
    source.chainSync.genesisConfigPath !== layout.shelleyGenesis ||
    canonicalJson(
      processConfig.watcherConfig,
      "acceptance watcher process config",
    ) !== canonicalJson(watcherConfig, "acceptance watcher runtime config")
  )
    throw new Error(
      "acceptance native reader configuration belongs to another run",
    );
  const manifest = json(layout.contractManifest) as DeploymentManifest;
  const releasedManifest = json(paths.manifest) as DeploymentManifest;
  verifyFinalizedDeploymentManifest(manifest);
  verifyFinalizedDeploymentManifest(releasedManifest);
  if (
    manifest.manifestId !== releasedManifest.manifestId ||
    manifest.network !== watcherConfig.targetNetwork
  )
    throw new Error(
      "acceptance native reader release belongs to another deployment",
    );
  // The public loader authenticates the signed identity/rules; it reads no key.
  const authority = await watcher.loadWatcherVerifiedDeploymentAuthority({
    path: paths.authority,
    ruleBundlePath: paths.rules,
  });
  scope.assertCurrent();
  if (authority.deploymentIdentity.manifestId !== manifest.manifestId)
    throw new Error(
      "acceptance native reader signed release belongs to another deployment",
    );
  await native.deriveWatcherNativeGenesisIdentity({ watcherConfig });
  scope.assertCurrent();
  const assertUnchanged = async (readScope = scope): Promise<void> => {
    scope.assertCurrent();
    readScope.assertCurrent();
    for (const path of files)
      if (digest(await read(path, readScope)) !== hashes[path])
        throw new Error(
          "acceptance native public run configuration changed during read",
        );
    if (codeStamp(runtimeDistTargets(layout)) !== stamp)
      throw new Error("acceptance native reader code changed during read");
    scope.assertCurrent();
    readScope.assertCurrent();
  };
  await assertUnchanged();
  return Object.freeze({
    native,
    watcher,
    watcherConfig,
    binaryPath,
    assertUnchanged,
    binding: Object.freeze({
      runDir: resolve(layout.runDir),
      deploymentManifestId: manifest.manifestId,
      releaseAuthoritySha256: hashes[paths.authority]!,
      codeStamp: stamp,
      nativeBinarySha256: hashes[binaryPath]!,
      publicConfigurationSha256: hashes,
    }),
  });
};

import { createHash } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parse } from "dotenv";
import {
  makeWatcherFinalityPolicy,
  parseWatcherProcessConfig,
  parseWatcherTrustedHeadAuthorityProcessConfig,
} from "midgard-watcher";

import { localUrl } from "./config.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { nativeLedgerPaths } from "./native-ledger.js";
import type { StackProcesses } from "./process.js";
import { prepareStackRelease } from "./release.js";

/** Release signatures and measured funding profiles are inputs, never fabricated. */
export async function generateWatcherServices(
  processes: StackProcesses,
  publicDaMultiaddr: string,
) {
  const { watcher } = processes.config;
  if (
    !resolve(watcher.configDirectory).startsWith(
      `${resolve(processes.config.runDirectory)}/`,
    )
  )
    throw new Error(
      "Generated watcher configuration must live inside the run directory",
    );
  const template = parseWatcherProcessConfig(
    await readJsonIfPresent(watcher.processTemplate),
  );
  if (template.watcherConfig.l1.source.sourceMode !== "local_node")
    throw new Error("Require local Kupmios watcher chain sync");
  const authorityTemplate = (await readJsonIfPresent(
    watcher.authorityTemplate,
  )) as Record<string, unknown>;
  const release = await prepareStackRelease(processes);
  const manifest = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  if (release.deploymentIdentity.manifestId !== manifest.manifestId)
    throw new Error(
      "Watcher release artifacts must be signed for this exact deployment",
    );
  const env = parse(await readFile(watcher.composeEnvFile));
  for (const key of [
    "WATCHER_RECORD_KEY_FILE",
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
    "WATCHER_BEARER_FILE",
  ])
    if (!env[key])
      throw new Error(`Missing ${key} in watcher Compose environment`);
  if (resolve(env.WATCHER_BEARER_FILE!) !== resolve(watcher.bearerFile))
    throw new Error("Watcher readiness and service credentials differ");
  await mkdir(watcher.configDirectory, { recursive: true, mode: 0o700 });
  const l1Directory = nativeLedgerPaths(processes).directory;
  const genesis = await readFile(join(l1Directory, "shelley-genesis.json"));
  const genesisIdentitySha256 = createHash("sha256")
    .update(genesis)
    .digest("hex");
  const watcherConfig = {
    ...template.watcherConfig,
    l1: {
      ...template.watcherConfig.l1,
      source: {
        ...template.watcherConfig.l1.source,
        chainSync: {
          ...template.watcherConfig.l1.source.chainSync,
          genesisIdentitySha256,
        },
        queryServices: [
          {
            kind: "kupo",
            identity: "local-kupo",
            endpoint: processes.env.L1_KUPO_KEY!,
          },
          {
            kind: "ogmios",
            identity: "local-ogmios",
            endpoint: processes.env.L1_OGMIOS_KEY!.replace(/^http:/, "ws:"),
          },
        ],
      },
    },
    da: {
      ...template.watcherConfig.da,
      peers: [{ identity: "public-retained-da", multiaddr: publicDaMultiaddr }],
    },
  };
  const processConfig = parseWatcherProcessConfig({
    ...template,
    watcherConfig,
  });
  const policy = makeWatcherFinalityPolicy(
    processConfig.watcherConfig,
    release.deploymentIdentity,
  );
  if (policy === null)
    throw new Error("Watcher finality policy could not be bound");
  const authorityConfig = parseWatcherTrustedHeadAuthorityProcessConfig({
    ...authorityTemplate,
    policy,
    endpoint: processConfig.trustedHeadAuthorityEndpoint,
  });
  localUrl(processConfig.operationsEndpoint);
  localUrl(authorityConfig.endpoint);
  await writeDurableJson(
    join(watcher.configDirectory, "watcher-process.json"),
    processConfig,
  );
  await writeDurableJson(
    join(watcher.configDirectory, "watcher-runtime.json"),
    processConfig.watcherConfig,
  );
  await writeDurableJson(
    join(watcher.configDirectory, "authority.json"),
    authorityConfig,
  );
  const source = (await processes.command(
    "watcher-compose-configuration",
    "docker",
    [
      "compose",
      "--env-file",
      watcher.composeEnvFile,
      "-f",
      "compose.yaml",
      "config",
      "--format",
      "json",
    ],
    {},
    resolve(processes.config.nodeRoot, "../midgard-watcher"),
  )) as {
    services: Record<
      string,
      { volumes: { source: string; target: string; type: string }[] }
    >;
    volumes: Record<string, unknown>;
  };
  const replace = (service: string, target: string, path: string) => {
    const volume = source.services[service]!.volumes.find(
      (value) => value.target === target,
    );
    if (!volume) throw new Error(`Watcher Compose is missing ${target}`);
    volume.source = path;
  };
  replace(
    "watcher-authority",
    "/etc/midgard/authority.json",
    join(watcher.configDirectory, "authority.json"),
  );
  replace(
    "watcher",
    "/etc/midgard/watcher-process.json",
    join(watcher.configDirectory, "watcher-process.json"),
  );
  replace(
    "watcher",
    "/etc/midgard/watcher-runtime.json",
    join(watcher.configDirectory, "watcher-runtime.json"),
  );
  replace("watcher", "/etc/midgard/bundles", watcher.releaseDirectory);
  replace("watcher", "/cardano-config", l1Directory);
  replace("watcher", "/ipc", join(processes.config.nodeRoot, "cardano/ipc"));
  for (const volume of Object.values(source.volumes) as Record<
    string,
    unknown
  >[])
    delete volume.name;
  return {
    ...source,
    operationsEndpoint: processConfig.operationsEndpoint,
    authorityEndpoint: authorityConfig.endpoint,
  };
}

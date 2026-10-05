import { randomBytes } from "node:crypto";
import { existsSync } from "node:fs";
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import type { WatcherTrustedHeadAuthorityProcessConfig } from "midgard-watcher";

import {
  finishWatcherAuthorityProvisioning,
  FRESH_AUTHORITY_PROFILE,
  prepareWatcherAuthorityProvisioning,
} from "../../src/devnet-stack/watcher-authority-provisioning.js";
import { writeJourneyArtifact } from "./artifacts.js";

export const journeyAuthorityKeySources = (runDirectory: string) => {
  const source = (name: string) => ({
    kind: "file" as const,
    path: join(runDirectory, "secrets", name),
  });
  return {
    rollback: source("watcher-rollback.key"),
    prover: source("watcher-prover.seed"),
    availability: source("watcher-availability.seed"),
    bearer: source("watcher-trusted-bearer.key"),
    record: source("watcher-trusted-record.key"),
  };
};

/** Explicit fresh ownership precedes shared runtime and key creation. Completed
 * sessions only verify the selected store; ephemeral ports do not change identity. */
export const provisionJourneyAuthority = async (input: {
  runDirectory: string;
  runtimeDirectory: string;
  configPath: string;
  endpoint: string;
  policy: WatcherTrustedHeadAuthorityProcessConfig["policy"];
  publisherSeed: string;
  availabilitySeed: string;
  initialize: boolean;
  cliPath?: string;
}) => {
  const keys = journeyAuthorityKeySources(input.runDirectory);
  const config: WatcherTrustedHeadAuthorityProcessConfig = {
    schemaVersion: "midgard-watcher-trusted-head-authority-process-config-v1",
    directory: join(input.runtimeDirectory, "trusted-head"),
    liveRecordLimit: FRESH_AUTHORITY_PROFILE.liveRecordLimit,
    endpoint: input.endpoint,
    policy: input.policy,
    recordAuthenticationKeySource: keys.record,
    httpBearerSecretSource: keys.bearer,
  };
  const descriptorPath = join(
    input.runDirectory,
    "work/journeys/authority-provisioning.json",
  );
  const prepared = prepareWatcherAuthorityProvisioning({
    config,
    descriptorPath,
    initialize: input.initialize,
    secretPaths: Object.values(keys).map((source) => source.path),
    protectedPaths: [input.runtimeDirectory],
  });
  const secret = async (path: string, initial: () => string) => {
    if (existsSync(path)) return;
    if (!prepared.allowMissingSecrets)
      throw Error("Established journey watcher secret is missing");
    await writeFile(path, initial(), { mode: 0o600, flag: "wx" });
  };
  await secret(keys.rollback.path, () => randomBytes(32).toString("hex"));
  await secret(keys.prover.path, () => input.publisherSeed);
  await secret(keys.availability.path, () => input.availabilitySeed);
  await secret(keys.bearer.path, () => randomBytes(32).toString("hex"));
  await secret(keys.record.path, () => randomBytes(32).toString("hex"));
  await mkdir(input.runtimeDirectory, { recursive: true, mode: 0o700 });
  await writeJourneyArtifact(input.configPath, config);
  await finishWatcherAuthorityProvisioning({
    prepared,
    config,
    configPath: input.configPath,
    descriptorPath,
    cliPath:
      input.cliPath ??
      fileURLToPath(
        new URL("../../../midgard-watcher/dist/cli.js", import.meta.url),
      ),
  });
  return config;
};

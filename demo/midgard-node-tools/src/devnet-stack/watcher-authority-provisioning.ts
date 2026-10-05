import { execFileSync } from "node:child_process";
import { lstatSync, readFileSync, realpathSync } from "node:fs";
import { dirname, join } from "node:path";

import {
  decodeWatcherAuthenticationKey32,
  loadWatcherSecretText,
  openWatcherTrustedHeadAuthorityStore,
  watcherSecretSourceIdentity,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "midgard-watcher";

import {
  assertAuthorityProvisioningDescriptor,
  completeAuthorityProvisioning,
  FRESH_AUTHORITY_PROFILE,
  prepareAuthorityProvisioning,
  syncProvisioningPath,
} from "./authority-provisioning-descriptor.js";

export { FRESH_AUTHORITY_PROFILE } from "./authority-provisioning-descriptor.js";
const bindingFor = (
  config: WatcherTrustedHeadAuthorityProcessConfig,
  secretPaths: readonly string[],
) => ({
  directory: config.directory,
  policyDigest: config.policy.policyDigest,
  deploymentMarker: config.policy.deploymentMarker,
  recordKeySourceIdentity: watcherSecretSourceIdentity(
    config.recordAuthenticationKeySource,
  ),
  bearerSourceIdentity: watcherSecretSourceIdentity(
    config.httpBearerSecretSource,
  ),
  secretPaths: [...secretPaths],
});
export const prepareWatcherAuthorityProvisioning = (input: {
  config: WatcherTrustedHeadAuthorityProcessConfig;
  descriptorPath: string;
  secretPaths: readonly string[];
  protectedPaths: readonly string[];
  initialize: boolean;
}) => {
  if (input.config.liveRecordLimit !== FRESH_AUTHORITY_PROFILE.liveRecordLimit)
    throw Error("fresh authority profile geometry differs");
  return prepareAuthorityProvisioning({
    ...input,
    binding: bindingFor(input.config, input.secretPaths),
  });
};
export const finishWatcherAuthorityProvisioning = async (input: {
  prepared: ReturnType<typeof prepareWatcherAuthorityProvisioning>;
  config: WatcherTrustedHeadAuthorityProcessConfig;
  configPath: string;
  descriptorPath: string;
  cliPath: string;
}) => {
  const { descriptor, completed } = input.prepared;
  assertAuthorityProvisioningDescriptor(input.descriptorPath, descriptor);
  if (
    input.config.liveRecordLimit !== FRESH_AUTHORITY_PROFILE.liveRecordLimit ||
    JSON.stringify(bindingFor(input.config, descriptor.binding.secretPaths)) !==
      JSON.stringify(descriptor.binding)
  )
    throw Error("authority provisioning configuration changed");
  for (const path of descriptor.binding.secretPaths) {
    // Namespace durability only: normal runtime loaders read the secret values.
    syncProvisioningPath(path);
    syncProvisioningPath(dirname(path));
    syncProvisioningPath(dirname(dirname(path)));
  }
  if (!completed) {
    const output = execFileSync(
      process.execPath,
      [
        input.cliPath,
        "authority-init",
        "--config",
        input.configPath,
        "--generation",
        descriptor.generation,
      ],
      {
        encoding: "utf8",
        timeout: 30000,
        killSignal: "SIGKILL",
        maxBuffer: 65536,
        env: {
          ...process.env,
          MIDGARD_CONFIG_MODE: "disabled",
          MIDGARD_DOTENV_MODE: "disabled",
        },
      },
    );
    const result = JSON.parse(output);
    if (
      result.command !== "authority-init" ||
      result.state !== "initialized" ||
      result.productionReady !== false ||
      result.generation !== descriptor.generation ||
      result.liveRecordLimit !== FRESH_AUTHORITY_PROFILE.liveRecordLimit ||
      result.policyDigest !== input.config.policy.policyDigest
    )
      throw Error("authority initialization acknowledgement differs");
  }
  const selectorPath = join(input.config.directory, "authority-backend.json");
  const selected = lstatSync(selectorPath);
  if (
    !selected.isFile() ||
    selected.size < 1 ||
    selected.size > 32768 ||
    realpathSync(selectorPath) !== selectorPath
  )
    throw Error("selected authority identity is invalid");
  const before = readFileSync(selectorPath, "utf8");
  if (JSON.parse(before).generation !== descriptor.generation)
    throw Error(
      "selected authority generation differs from provisioning intent",
    );
  const record = await loadWatcherSecretText(
    input.config.recordAuthenticationKeySource,
  );
  const store = await openWatcherTrustedHeadAuthorityStore({
    directory: input.config.directory,
    policy: input.config.policy,
    liveRecordLimit: input.config.liveRecordLimit,
    recordAuthenticationKey: decodeWatcherAuthenticationKey32(record),
  });
  try {
    await store.readCurrent();
    if (readFileSync(selectorPath, "utf8") !== before)
      throw Error(
        "selected authority changed during provisioning verification",
      );
  } finally {
    store.close();
  }
  if (!completed)
    completeAuthorityProvisioning(input.descriptorPath, descriptor);
};

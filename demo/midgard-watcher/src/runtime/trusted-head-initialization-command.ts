import { loadWatcherTrustedHeadAuthorityProcessConfigFile } from "./process-config.js";
import { assertAuthorityGeometry } from "./trusted-head-authority.envelope-codec.js";
import { initializeSelectedAuthorityStore } from "./trusted-head-authority.selected-store.js";
import { loadWatcherTrustedHeadAuthoritySecrets } from "./trusted-head-runtime.js";

/** Explicit offline provisioning only. Caller owns the independent new volume
 * and retains generation unchanged across retries. This command never starts
 * HTTP, imports/repairs existing state, creates keys or changes generation. */
export const initializeWatcherTrustedHeadAuthorityCommand = async (
  configPath: string,
  generation: string,
) => {
  const config =
    await loadWatcherTrustedHeadAuthorityProcessConfigFile(configPath);
  assertAuthorityGeometry(generation, config.liveRecordLimit);
  const { recordAuthenticationKey } =
    await loadWatcherTrustedHeadAuthoritySecrets(config);
  await initializeSelectedAuthorityStore({
    directory: config.directory,
    policy: config.policy,
    liveRecordLimit: config.liveRecordLimit,
    recordAuthenticationKey,
    generation,
  });
  return Object.freeze({
    generation,
    liveRecordLimit: config.liveRecordLimit,
    policyDigest: config.policy.policyDigest,
  });
};

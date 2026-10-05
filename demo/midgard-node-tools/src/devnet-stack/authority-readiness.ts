import { createHash } from "node:crypto";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { Layout, RunEnv } from "./layout.js";
import { servicePorts } from "./layout.js";
import type { ValidatedReadinessProbe } from "./service-readiness.js";
import { loadWatcherModule, readFinalizedManifest } from "./watcher-release.js";

/** Read-only authenticated authority admission; never opens or repairs its store. */
export const authorityReadinessProbe = (
  layout: Layout,
  run: RunEnv,
): ValidatedReadinessProbe => {
  const endpoint = `http://127.0.0.1:${servicePorts(run).watcherAuthority}`;
  return {
    binding: JSON.stringify({
      kind: "recorded_trusted_head_authority",
      endpoint,
      processConfig: layout.watcherProcessConfig,
      authorityConfig: layout.watcherAuthorityConfig,
      contractManifest: layout.contractManifest,
    }),
    check: async (timeoutMs) => {
      const watcher = await loadWatcherModule(layout);
      const [config, authorityConfig] = await Promise.all([
        watcher.loadWatcherProcessConfigFile(layout.watcherProcessConfig),
        watcher.loadWatcherTrustedHeadAuthorityProcessConfigFile(
          layout.watcherAuthorityConfig,
        ),
      ]);
      const verified = await watcher.loadWatcherVerifiedDeploymentAuthority({
        path: config.deploymentAuthorityPath,
        ruleBundlePath: config.ruleBundlePath,
      });
      const manifest = readFinalizedManifest(layout);
      const policy = watcher.makeWatcherFinalityPolicy(
        config.watcherConfig,
        verified.deploymentIdentity,
      );
      const release = await watcher
        .watcherDeploymentReleaseFinalityAuthority(verified.deploymentIdentity)
        .verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
      if (
        policy === null ||
        policy.network !== "Custom" ||
        policy.sourceMode !== "local_node" ||
        policy.confirmationDepth !== String(release.policy.confirmationDepth) ||
        policy.maximumPreFinalityRollbackDepth !==
          String(release.policy.confirmationDepth) ||
        policy.maximumPostFinalityRecoveryDepth !== "2160" ||
        JSON.stringify(policy.deploymentMarker) !==
          JSON.stringify(makeDeploymentMarker(manifest.manifestId)) ||
        authorityConfig.policy.policyDigest !== policy.policyDigest ||
        authorityConfig.endpoint !== endpoint ||
        config.trustedHeadAuthorityEndpoint !== endpoint
      )
        return false;
      const [recordText, rollbackText, bearerText] = await Promise.all([
        watcher.loadWatcherSecretText(
          authorityConfig.recordAuthenticationKeySource,
        ),
        watcher.loadWatcherSecretText(
          config.watcherConfig.storage.rollbackAuthorityKeySource,
        ),
        watcher.loadWatcherSecretText(config.httpBearerSecretSource),
      ]);
      // This is the authority protocol's recordKeyId derivation, not a file fingerprint.
      const expectedId = createHash("sha256")
        .update(watcher.decodeWatcherAuthenticationKey32(recordText))
        .digest("hex");
      const client = watcher.createWatcherTrustedHeadAuthorityClient({
        endpoint,
        policy,
        authenticationKey:
          watcher.decodeWatcherAuthenticationKey32(rollbackText),
        httpSecret: watcher.decodeWatcherHttpBearerSecret(bearerText),
        requestTimeoutMs: timeoutMs,
      });
      if ((await client.readRecordAuthenticationKeyId()) !== expectedId)
        return false;
      await client.readCurrent();
      return true;
    },
  };
};

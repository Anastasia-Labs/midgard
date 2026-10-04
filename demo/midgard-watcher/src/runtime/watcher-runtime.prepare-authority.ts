import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";
import { createSqliteHistoricalNativeScriptCheckpointStore } from "@al-ft/midgard-fault-proofs";
import { Kupmios } from "@lucid-evolution/lucid";

import { loadWatcherWorkflowFundingProfileOverlay } from "../funding/workflow-funding-profile-overlay.js";
import { makeWatcherFinalityPolicy } from "../l1/finality-engine.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import { loadWatcherVerifiedDeploymentAuthority } from "./deployment-authority.js";
import { watcherDeploymentReleaseFinalityPolicy } from "./deployment-identity.js";
import {
  refusePermanently,
  WatcherPermanentRefusalError,
} from "./permanent-refusal.js";
import { type WatcherProcessConfig } from "./process-config.js";
import { createWatcherStartupProgress } from "./startup-progress.js";
import { createWatcherTrustedHeadClientRuntime } from "./trusted-head-runtime.js";
import {
  prepareJournalDirectory,
  requireWatcherRuntimeConfig,
} from "./watcher-runtime.create-watcher-native-event-handler.js";

export const prepareWatcherRuntimeAuthority = async (
  input: Readonly<{ config: WatcherProcessConfig }>,
  startup: ReturnType<typeof createWatcherStartupProgress>,
) => {
  await startup("runtime_configuration", () =>
    refusePermanently("runtime_configuration", () =>
      requireWatcherRuntimeConfig(input.config),
    ),
  );
  await prepareJournalDirectory(input.config.workflowJournalDirectory);
  const deploymentAuthority = await startup("deployment_authority", () =>
    refusePermanently("deployment_authority", () =>
      loadWatcherVerifiedDeploymentAuthority({
        path: input.config.deploymentAuthorityPath,
        ruleBundlePath: input.config.ruleBundlePath,
      }),
    ),
  );
  const { deploymentIdentity } = deploymentAuthority;
  const policy = makeWatcherFinalityPolicy(
    input.config.watcherConfig,
    deploymentIdentity,
  );
  const releaseDepth = String(
    watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy
      .confirmationDepth,
  );
  if (
    policy === null ||
    (policy.network !== "Preprod" && policy.network !== "Custom") ||
    policy.sourceMode !== "local_node" ||
    policy.confirmationDepth !== releaseDepth ||
    policy.maximumPreFinalityRollbackDepth !== releaseDepth ||
    policy.maximumPostFinalityRecoveryDepth !== "2160"
  ) {
    throw new WatcherPermanentRefusalError(
      "finality_policy",
      new Error(
        "watcher production finality differs from the verified release",
      ),
    );
  }
  const localL1Source = input.config.watcherConfig.l1.source;
  if (localL1Source.sourceMode !== "local_node") {
    throw new Error("watcher production runtime requires local-node authority");
  }
  const trusted = await createWatcherTrustedHeadClientRuntime({
    config: input.config,
    policy,
    additionalSecretSources: [input.config.availability.keySource],
  });
  const historicalNativeScriptCheckpointStore =
    createSqliteHistoricalNativeScriptCheckpointStore({
      path: input.config.watcherConfig.storage.path,
      rollbackAuthenticationKey: trusted.rollbackAuthenticationKey,
    });
  const fundingProfileOverlay = await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: input.config.fundingProfileBundlePath,
    deploymentIdentity,
  });
  const sqlite = await openWatcherSqliteDurableBackend({
    path: input.config.watcherConfig.storage.path,
  });

  return {
    deploymentAuthority,
    deploymentIdentity,
    policy,
    localL1Source,
    trusted,
    historicalNativeScriptCheckpointStore,
    fundingProfileOverlay,
    sqlite,
  };
};

export const createWatcherRuntimeProverWallet = (
  input: Readonly<{ config: WatcherProcessConfig }>,
  proverSecret: string,
  kupoService: Readonly<{ endpoint: string }>,
  ogmiosService: Readonly<{ endpoint: string }>,
) => {
  const proverWalletAddress = resolveProverSigner(
    proverSecret.startsWith("ed25519_sk")
      ? {
          network: input.config.watcherConfig.targetNetwork,
          walletPrivateKey: proverSecret,
        }
      : {
          network: input.config.watcherConfig.targetNetwork,
          walletSeedPhrase: proverSecret,
        },
    Object.freeze({}),
  ).address;
  const proverUtxoProvider = new Kupmios(
    kupoService.endpoint,
    ogmiosService.endpoint,
  );
  return { proverWalletAddress, proverUtxoProvider };
};

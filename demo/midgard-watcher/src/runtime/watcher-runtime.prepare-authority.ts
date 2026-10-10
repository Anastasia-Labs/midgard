import { createHash } from "node:crypto";

import { resolveProverSigner } from "@al-ft/midgard-fault-proofs";

import { loadWatcherWorkflowFundingProfileOverlay } from "../funding/workflow-funding-profile-overlay.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import { loadWatcherVerifiedDeploymentAuthority } from "./deployment-authority.js";
import { watcherDeploymentReleaseFinalityPolicy } from "./deployment-identity.js";
import { warnWatcherLegacyJournalDirectories } from "./legacy-journal-directories.js";
import {
  refusePermanently,
  WatcherPermanentRefusalError,
} from "./permanent-refusal.js";
import {
  decodeWatcherAuthenticationKey32,
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "./process-config.js";
import { createWatcherStartupProgress } from "./startup-progress.js";
import {
  prepareJournalDirectory,
  requireWatcherRuntimeConfig,
} from "./watcher-runtime.launch-checks.js";

const sha256 = (value: Uint8Array | string): string =>
  createHash("sha256").update(value).digest("hex");

/** A secret's identities: its text, and its bytes when it is 32-byte hex. */
const secretCandidateIds = (value: string): ReadonlySet<string> => {
  const ids = new Set([sha256(value)]);
  if (/^[0-9a-f]{64}$/u.test(value))
    ids.add(sha256(Uint8Array.from(Buffer.from(value, "hex"))));
  return ids;
};

/** Refuses any two secrets that share a value, as text or as 32-byte hex. */
const assertDistinctSecretCandidates = (texts: readonly string[]): void => {
  const seen = new Set<string>();
  for (const text of texts) {
    const ids = secretCandidateIds(text);
    if ([...ids].some((id) => seen.has(id)))
      throw new Error(
        "the rollback authentication key and the prover and availability wallet secrets must be pairwise distinct",
      );
    for (const id of ids) seen.add(id);
  }
};

/**
 * Loads the rollback authentication key (the HMAC key of the watcher's
 * journals, queues and stores) from `storage.rollbackAuthorityKeySource`.
 * The key and both wallets' secrets must be pairwise distinct; a shared
 * value is a configuration refusal no restart clears, made before the
 * operations server binds.
 */
export const loadWatcherRollbackAuthenticationKey = async (
  config: WatcherProcessConfig,
): Promise<Uint8Array> => {
  const texts = await Promise.all(
    [
      config.watcherConfig.storage.rollbackAuthorityKeySource,
      config.watcherConfig.proverWallet.keySource,
      config.availability.keySource,
    ].map(async (source) => await loadWatcherSecretText(source)),
  );
  return await refusePermanently("secret_distinctness", () => {
    const key = decodeWatcherAuthenticationKey32(texts[0]!);
    assertDistinctSecretCandidates(texts);
    return Uint8Array.from(key);
  });
};

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
  await warnWatcherLegacyJournalDirectories(
    input.config.workflowJournalDirectory,
  );
  const deploymentAuthority = await startup("deployment_authority", () =>
    refusePermanently("deployment_authority", () =>
      loadWatcherVerifiedDeploymentAuthority({
        path: input.config.deploymentAuthorityPath,
        ruleBundlePath: input.config.ruleBundlePath,
      }),
    ),
  );
  const { deploymentIdentity } = deploymentAuthority;
  const watcherConfig = input.config.watcherConfig;
  const releaseDepth =
    watcherDeploymentReleaseFinalityPolicy(deploymentIdentity).policy
      .confirmationDepth;
  if (
    deploymentIdentity.network !== watcherConfig.targetNetwork ||
    (watcherConfig.targetNetwork !== "Preprod" &&
      watcherConfig.targetNetwork !== "Custom") ||
    watcherConfig.l1.finality.depth !== releaseDepth
  ) {
    throw new WatcherPermanentRefusalError(
      "finality_policy",
      new Error(
        "watcher production finality differs from the verified release",
      ),
    );
  }
  const localL1Source = watcherConfig.l1.source;
  const rollbackAuthenticationKey = await loadWatcherRollbackAuthenticationKey(
    input.config,
  );
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
    localL1Source,
    rollbackAuthenticationKey,
    fundingProfileOverlay,
    sqlite,
  };
};

/**
 * A watcher wallet's enterprise address, from its secret (a bech32 private
 * key or a seed phrase), derived exactly as the prover signer and the
 * availability actor select it. Address derivation only: the runtime never
 * holds a live signer from it.
 */
export const resolveWatcherRuntimeWalletAddress = (
  input: Readonly<{ config: WatcherProcessConfig }>,
  secret: string,
): string =>
  resolveProverSigner(
    secret.startsWith("ed25519_sk")
      ? {
          network: input.config.watcherConfig.targetNetwork,
          walletPrivateKey: secret,
        }
      : {
          network: input.config.watcherConfig.targetNetwork,
          walletSeedPhrase: secret,
        },
    Object.freeze({}),
  ).address;

/**
 * The prover and availability wallets' addresses, refused permanently when
 * both secrets resolve to one wallet (two encodings of one key): the two
 * actors would spend each other's UTxOs.
 */
export const resolveWatcherRuntimeWalletAddresses = async (
  input: Readonly<{ config: WatcherProcessConfig }>,
): Promise<Readonly<{ prover: string; availability: string }>> => {
  const prover = resolveWatcherRuntimeWalletAddress(
    input,
    await loadWatcherSecretText(
      input.config.watcherConfig.proverWallet.keySource,
    ),
  );
  const availability = resolveWatcherRuntimeWalletAddress(
    input,
    await loadWatcherSecretText(input.config.availability.keySource),
  );
  if (prover === availability)
    throw new WatcherPermanentRefusalError(
      "wallet_distinctness",
      new Error(
        "the prover and availability wallet secrets resolve to the same wallet",
      ),
    );
  return { prover, availability };
};

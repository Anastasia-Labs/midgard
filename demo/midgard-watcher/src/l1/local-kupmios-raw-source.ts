import { isProxy } from "node:util/types";

import {
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosHttpOgmiosRawSource,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type LocalKupmiosFraudProofRawSource,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";

export const watcherLocalKupmiosRawSourceId = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
  authorityNodeId: string,
): string =>
  [
    "watcher-native-crosscheck",
    deploymentIdentity.manifestId,
    authorityNodeId,
  ].join("/");

const releaseFinalityFromDeployment = (
  identity: VerifiedWatcherDeploymentIdentity,
): VerifiedFraudProofReleaseFinalityPolicy => {
  const policy = Object.freeze({
    confirmationDepth: 30 as const,
    automaticRecoveryMaxDepth: 2160 as const,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1" as const,
  });
  return validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: identity.manifestId,
    blueprintHash: identity.blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  });
};

/**
 * Constructs the deployment/config-bound raw Kupo/Ogmios authority before a
 * native chain-sync process is started. The runtime must subsequently bind
 * this exact source to the native authority before processing any event.
 */
export const createWatcherLocalKupmiosRawSource = (
  input: Readonly<{
    watcherConfig: unknown;
    observationDepth?: "inclusion" | "release_finality";
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    captureBounds?: Readonly<{
      signal?: AbortSignal;
      timeoutMs?: number;
      blockScanLimit?: number;
      maxResponseBytes?: number;
    }>;
  }>,
): LocalKupmiosFraudProofRawSource => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  if (
    watcherConfig.mode !== "acceptance" ||
    (watcherConfig.targetNetwork !== "Preprod" &&
      watcherConfig.targetNetwork !== "Custom") ||
    input.deploymentIdentity.network !== watcherConfig.targetNetwork ||
    watcherConfig.l1.source.sourceMode !== "local_node" ||
    watcherConfig.l1.finality.depth !== 30 ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      "local Kupo/Ogmios raw source differs from the admitted release",
    );
  }
  const source = watcherConfig.l1.source;
  const kupo = source.queryServices.find(({ kind }) => kind === "kupo");
  const ogmios = source.queryServices.find(({ kind }) => kind === "ogmios");
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("local Kupo/Ogmios raw source omitted a required service");
  }
  if (source.queryServices.some(({ kind }) => kind === "db_sync")) {
    throw new Error(
      "configured db-sync requires a concrete authenticated watcher query adapter",
    );
  }
  const captureBounds = input.captureBounds;
  if (captureBounds !== undefined) {
    const allowedKeys = [
      "signal",
      "timeoutMs",
      "blockScanLimit",
      "maxResponseBytes",
    ];
    if (
      typeof captureBounds !== "object" ||
      captureBounds === null ||
      isProxy(captureBounds) ||
      Object.getPrototypeOf(captureBounds) !== Object.prototype ||
      Reflect.ownKeys(captureBounds).some(
        (key) => typeof key !== "string" || !allowedKeys.includes(key),
      ) ||
      Object.values(Object.getOwnPropertyDescriptors(captureBounds)).some(
        (descriptor) => !("value" in descriptor) || !descriptor.enumerable,
      )
    ) {
      throw new Error(
        "local Kupo/Ogmios captureBounds must be an exact plain object",
      );
    }
    for (const [name, maximum] of [
      ["timeoutMs", watcherConfig.l1.requestTimeoutMs],
      ["blockScanLimit", 2_000],
      ["maxResponseBytes", 64 * 1024 * 1024],
    ] as const) {
      const value = captureBounds[name];
      if (
        value !== undefined &&
        (!Number.isSafeInteger(value) || value <= 0 || value > maximum)
      ) {
        throw new Error(
          `local Kupo/Ogmios capture ${name} is outside its operational bound`,
        );
      }
    }
    if (captureBounds.signal !== undefined) {
      try {
        Object.getOwnPropertyDescriptor(
          AbortSignal.prototype,
          "aborted",
        )!.get!.call(captureBounds.signal);
      } catch {
        throw new Error(
          "local Kupo/Ogmios capture signal must be a platform AbortSignal",
        );
      }
    }
  }
  return createLocalKupmiosHttpOgmiosRawSource({
    sourceId: watcherLocalKupmiosRawSourceId(
      input.deploymentIdentity,
      source.authorityNodeId,
    ),
    kupoHttpUrl: kupo.endpoint,
    ogmiosUrl: ogmios.endpoint,
    releaseFinality: releaseFinalityFromDeployment(input.deploymentIdentity),
    observationDepth: input.observationDepth ?? "release_finality",
    timeoutMs: captureBounds?.timeoutMs ?? watcherConfig.l1.requestTimeoutMs,
    ...(captureBounds?.signal === undefined
      ? {}
      : { signal: captureBounds.signal }),
    ...(captureBounds?.blockScanLimit === undefined
      ? {}
      : { blockScanLimit: captureBounds.blockScanLimit }),
    ...(captureBounds?.maxResponseBytes === undefined
      ? {}
      : { maxResponseBytes: captureBounds.maxResponseBytes }),
  });
};

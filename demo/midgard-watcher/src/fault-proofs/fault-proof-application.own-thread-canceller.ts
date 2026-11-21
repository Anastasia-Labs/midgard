import { createHash } from "node:crypto";

import { createOwnThreadCanceller } from "@al-ft/midgard-fault-proofs/own-thread-cancel";

import type { WatcherConfig } from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { watcherDeploymentAppliedScriptHashes } from "../runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import {
  bindWatcherDeploymentAuthority,
  readSecret,
} from "./fault-proof-application.bind-watcher-deployment-authority.js";
import type {
  WatcherFaultProofApplication,
  WatcherFaultProofApplicationDependencies,
  WatcherFaultProofInfrastructureAuthority,
  WatcherFaultProofL1,
} from "./fault-proof-application.production-dependencies.js";

/**
 * The application's canceller of our own computation threads. Its only
 * authority is the verified deployment and what L1 holds: the manifest's
 * script hashes say which family role a thread stands at, and the
 * deployment's published references carry the step and minting scripts. It
 * signs with the prover wallet and is paid from that wallet's coins as the
 * node's ledger and mempool show them. An application that only observes
 * opens no canceller and says so.
 */
export const openOwnThreadCancellerFrom =
  ({
    isExecutable,
    watcherConfig,
    infrastructure,
    deploymentIdentity,
    l1,
    dependencies,
    environment,
  }: {
    readonly isExecutable: () => boolean;
    readonly watcherConfig: WatcherConfig;
    readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    readonly l1: WatcherFaultProofL1;
    readonly dependencies: WatcherFaultProofApplicationDependencies;
    readonly environment: NodeJS.ProcessEnv;
  }): WatcherFaultProofApplication["openOwnThreadCanceller"] =>
  async () => {
    if (!isExecutable()) return Object.freeze({ kind: "observing" });
    const [binding, secret] = await Promise.all([
      bindWatcherDeploymentAuthority({
        watcherConfig,
        infrastructure,
        l1,
        dependencies,
      }),
      readSecret({
        source: watcherConfig.proverWallet.keySource,
        dependencies,
        environment,
        label: "watcher prover wallet",
      }),
    ]);
    if (
      createHash("sha256").update(binding.blueprintJson).digest("hex") !==
      deploymentIdentity.blueprintHash
    )
      throw new Error("watcher blueprint differs from the verified deployment");
    return Object.freeze({
      kind: "cancelling",
      canceller: createOwnThreadCanceller({
        lucid: binding.lucid,
        signer: dependencies.resolveSigner({
          network: watcherConfig.targetNetwork,
          secret,
        }),
        deployment: {
          contractScriptHashes:
            watcherDeploymentAppliedScriptHashes(deploymentIdentity),
          resolveReferenceScript: binding.resolveReferenceScript,
        },
      }),
    });
  };

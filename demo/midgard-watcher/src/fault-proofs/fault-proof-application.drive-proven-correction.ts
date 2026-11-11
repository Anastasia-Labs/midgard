import { createHash } from "node:crypto";

import {
  driveProvenCorrectionRemoval,
  excludeReservedFunding,
} from "@al-ft/midgard-fault-proofs";

import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { readAdmittedWatcherRuntimeConfig } from "../storage/retained-da-runtime.js";
import {
  bindWatcherDeploymentAuthority,
  readSecret,
} from "./fault-proof-application.bind-watcher-deployment-authority.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofInfrastructureAuthority,
  type WatcherFaultProofL1,
} from "./fault-proof-application.production-dependencies.js";

/**
 * The application's removal of a proven correction's header, started from
 * an Idle lock or finished from a Locked one. Its only authority is the
 * verified deployment and what L1 holds: the lock, the queue and the
 * record. It is paid from the prover wallet's coins no funding reservation
 * holds, so it never takes another action's inputs; a shortfall fails the
 * attempt and the next pass tries again. An application that only observes
 * starts nothing and says so.
 */
export const driveProvenCorrectionFrom =
  ({
    isExecutable,
    infrastructure,
    deploymentIdentity,
    l1,
    dependencies,
    environment,
  }: {
    readonly isExecutable: () => boolean;
    readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    readonly l1: WatcherFaultProofL1;
    readonly dependencies: WatcherFaultProofApplicationDependencies;
    readonly environment: NodeJS.ProcessEnv;
  }): WatcherFaultProofApplication["driveProvenCorrection"] =>
  async ({ runtimeConfigPath, correction, reservedOutRefs }) => {
    if (!isExecutable()) return Object.freeze({ kind: "observing" });
    const watcherConfig = await readAdmittedWatcherRuntimeConfig({
      runtimeConfigPath,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deploymentIdentity,
    });
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
    return await driveProvenCorrectionRemoval({
      lucid: binding.lucid,
      blueprint: JSON.parse(binding.blueprintJson) as unknown,
      // Builders take the verified release, as every family workflow does.
      deploymentInfo: binding.manifest,
      network: watcherConfig.targetNetwork,
      signer: excludeReservedFunding({
        signer: dependencies.resolveSigner({
          network: watcherConfig.targetNetwork,
          secret,
        }),
        reservedOutRefs,
      }),
      stateQueueMutationLeaseCoordinator: dependencies.createLeaseCoordinator(),
      correction,
    });
  };

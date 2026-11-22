import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core/deployment-manifest-identity";
import type {
  WorkflowAdapterReadinessInput,
  WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs/workflow/adapters";
import type { CompleteCanonicalReplayContext } from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import type { WorkflowRuntimeLoader } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import {
  restrictWorkflowWallet,
  type WorkflowTransactionRoster,
} from "@al-ft/midgard-fault-proofs/workflow/workflow-wallet";
import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import type { WatcherConfig } from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { watcherDeploymentAppliedScriptHashes } from "../runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import type { WatcherWorkflowInfrastructure } from "../storage/retained-da-runtime.js";
import {
  bindWatcherDeploymentAuthority,
  readSecret,
} from "./fault-proof-application.bind-watcher-deployment-authority.js";
import type {
  WatcherFaultProofApplication,
  WatcherFaultProofApplicationConstructionOptions,
  WatcherFaultProofApplicationDependencies,
  WatcherFaultProofInfrastructureAuthority,
  WatcherFaultProofL1,
} from "./fault-proof-application.production-dependencies.js";
import type { WatcherProofMaterialsViews } from "./proof-materials-views.js";

/**
 * The source id a family's L1 snapshots and signed recovery are recorded
 * under. Snapshot digests and persisted journals bind it, so it keeps the
 * shape the earlier local source wrote.
 */
export const watcherFaultProofSourceId = ({
  category,
  hubOraclePolicyId,
  localL1Source,
}: {
  readonly category: string;
  readonly hubOraclePolicyId: string;
  readonly localL1Source: Readonly<{
    authorityNodeId: string;
    chainSync: Readonly<{ genesisIdentitySha256: string }>;
  }>;
}): string =>
  [
    "watcher-fault-proof",
    category,
    hubOraclePolicyId,
    localL1Source.authorityNodeId,
    localL1Source.chainSync.genesisIdentitySha256,
  ].join("/");

/**
 * The signed deployment's governed contracts, which a workflow transaction
 * may pay (its spending validators' addresses) and embed (any applied
 * script).
 */
export const watcherWorkflowTransactionRoster = (
  identity: VerifiedWatcherDeploymentIdentity,
): WorkflowTransactionRoster => {
  const applied = Object.entries(
    watcherDeploymentAppliedScriptHashes(identity),
  );
  const members = DEPLOYMENT_MEMBERS as Readonly<
    Record<string, { readonly purpose: string } | undefined>
  >;
  return Object.freeze({
    contractAddresses: applied
      .filter(([name]) => members[name]?.purpose === "spend")
      .map(([, scriptHash]) =>
        credentialToAddress(
          identity.network,
          scriptHashToCredential(scriptHash),
        ),
      ),
    scriptHashes: applied.map(([, scriptHash]) => scriptHash),
  });
};

/**
 * Pins, for each loading invocation, the replay context its decision held
 * when its load started. A withdrawal (shutdown, rollback, another target)
 * clears the live contexts for every later run, never for a run already
 * loading: that run's revoked permit is what stops it, at its next
 * checkpoint, as it stops a run already loaded.
 */
export const replayContextsPinnedAtLoadStart = (
  live: ReadonlyMap<string, CompleteCanonicalReplayContext>,
) => {
  const started = new WeakMap<object, CompleteCanonicalReplayContext>();
  return Object.freeze({
    /** Pins the context before the wrapped loader's first await. */
    loader:
      (load: WorkflowRuntimeLoader): WorkflowRuntimeLoader =>
      async (request) => {
        const { decisionDigest } =
          request.invocation as Partial<WorkflowAdapterRunnerInput>;
        const context =
          decisionDigest === undefined ? undefined : live.get(decisionDigest);
        if (context !== undefined) started.set(request.invocation, context);
        return await load(request);
      },
    started: (invocation: object): CompleteCanonicalReplayContext | undefined =>
      started.get(invocation),
  });
};

/**
 * The watcher's one loader body for an acting invocation. On top of the
 * deployment binding it reads the prover wallet, selects the signer (for an
 * acting invocation, restricted to the deployment's transaction shape),
 * with the process's one view of its proof-materials address, and hands
 * over the optional
 * parts a family may require; the family's record then binds its own
 * manifest-bound config from these inside the shared runtime, so nothing here
 * is per family. Which optional parts a family needs is the record's
 * `requires`, refused by the shared application loop.
 */
export const buildCommonInfrastructure = async ({
  watcherConfig,
  invocation,
  infrastructure,
  deploymentIdentity,
  replayContext,
  l1,
  dependencies,
  environment,
  proofMaterials,
}: {
  readonly watcherConfig: WatcherConfig;
  /** An acting invocation carries its objective's wallet wiring. */
  readonly invocation: WorkflowAdapterReadinessInput &
    Partial<Pick<WorkflowAdapterRunnerInput, "objectiveWallet">>;
  readonly l1: WatcherFaultProofL1;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  /** The context the invocation's decision held when its load started. */
  readonly replayContext: CompleteCanonicalReplayContext | undefined;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  /** The process's proof-materials views, shared by every invocation. */
  readonly proofMaterials: WatcherProofMaterialsViews;
}): Promise<WatcherWorkflowInfrastructure> => {
  const { category } = invocation;
  const [binding, proverSecret] = await Promise.all([
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
  const keySigner = dependencies.resolveSigner({
    network: watcherConfig.targetNetwork,
    secret: proverSecret,
  });
  const resolvedSigner = {
    ...keySigner,
    proofMaterials: proofMaterials.viewFor(keySigner.proofMaterialsAddress),
  };
  const executionInvocation = invocation as Partial<WorkflowAdapterRunnerInput>;
  const decisionDigest = executionInvocation.decisionDigest;
  const signer =
    executionInvocation.objectiveWallet === undefined
      ? resolvedSigner
      : restrictWorkflowWallet({
          signer: resolvedSigner,
          objectiveWallet: executionInvocation.objectiveWallet,
          roster: watcherWorkflowTransactionRoster(deploymentIdentity),
        });
  signer.selectWallet(binding.lucid);
  return Object.freeze({
    infrastructure: Object.freeze({
      manifest: binding.manifest,
      blueprintJson: binding.blueprintJson,
      deploymentInfo: binding.deploymentInfo,
      headerHash: invocation.headerHash,
      ...(decisionDigest === undefined ? {} : { decisionDigest }),
      lucid: binding.lucid,
      signer,
      l1Source: l1.source(
        watcherFaultProofSourceId({
          category,
          hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
          localL1Source: binding.localL1Source,
        }),
      ),
      stateQueueMutationLeaseCoordinator: dependencies.createLeaseCoordinator(),
      ...(replayContext === undefined ? {} : { replayContext }),
    }),
    resolveReferenceScript: binding.resolveReferenceScript,
  });
};

export const admittedApplications = new WeakSet<object>();

/** Retains the exact runner/classifier authority captured by this module. */
export const assertWatcherFaultProofApplication = (
  application: WatcherFaultProofApplication,
): void => {
  if (!admittedApplications.has(application)) {
    throw new Error(
      "watcher fault-proof production application is not module-admitted",
    );
  }
};

export type ApplicationConstruction = {
  readonly options: WatcherFaultProofApplicationConstructionOptions;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
  readonly allowExecution: boolean;
};

/** A non-executing test application that also exposes its runtime loader. */
export type WatcherFaultProofApplicationWithLoaderForTest =
  WatcherFaultProofApplication &
    Readonly<{ unsafeLoadRuntimeForTest: WorkflowRuntimeLoader }>;

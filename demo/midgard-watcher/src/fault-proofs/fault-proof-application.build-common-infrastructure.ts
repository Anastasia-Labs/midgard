import {
  type CompleteCanonicalReplayContext,
  type FamilyValidationChallengePort,
  restrictWorkflowFundingSigner,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunnerInput,
  type WorkflowRuntimeLoader,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherConfig } from "../runtime/config.js";
import { type VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { type WatcherWorkflowInfrastructure } from "../storage/retained-da-runtime.js";
import {
  bindWatcherDeploymentAuthority,
  readSecret,
} from "./fault-proof-application.bind-watcher-deployment-authority.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofApplicationConstructionOptions,
  type WatcherFaultProofApplicationDependencies,
  type WatcherFaultProofInfrastructureAuthority,
  type WatcherHistoricalNativeScriptAuthority,
} from "./fault-proof-application.production-dependencies.js";

/**
 * The watcher's one loader body for an acting invocation. On top of the
 * deployment binding it reads the prover wallet, selects the (possibly
 * funding-restricted) signer and hands over the optional
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
  historicalNativeScriptAuthority,
  replayContexts,
  validationChallenge,
  dependencies,
  environment,
}: {
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
  readonly infrastructure: WatcherFaultProofInfrastructureAuthority;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly historicalNativeScriptAuthority: WatcherHistoricalNativeScriptAuthority;
  readonly replayContexts: ReadonlyMap<string, CompleteCanonicalReplayContext>;
  readonly validationChallenge: FamilyValidationChallengePort;
  readonly dependencies: WatcherFaultProofApplicationDependencies;
  readonly environment: NodeJS.ProcessEnv;
}): Promise<WatcherWorkflowInfrastructure> => {
  const { category } = invocation;
  const [binding, proverSecret] = await Promise.all([
    bindWatcherDeploymentAuthority({
      watcherConfig,
      infrastructure,
      dependencies,
    }),
    readSecret({
      source: watcherConfig.proverWallet.keySource,
      dependencies,
      environment,
      label: "watcher prover wallet",
    }),
  ]);
  const resolvedSigner = dependencies.resolveSigner({
    network: watcherConfig.targetNetwork,
    secret: proverSecret,
  });
  const executionInvocation = invocation as Partial<WorkflowAdapterRunnerInput>;
  const decisionDigest = executionInvocation.decisionDigest;
  const replayContext =
    decisionDigest === undefined
      ? undefined
      : replayContexts.get(decisionDigest);
  const signer =
    executionInvocation.fundingReservationPermit === undefined
      ? resolvedSigner
      : restrictWorkflowFundingSigner({
          signer: resolvedSigner,
          permit: executionInvocation.fundingReservationPermit,
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
      source: Object.freeze({
        sourceId: [
          "watcher-fault-proof",
          category,
          deploymentIdentity.manifestId,
          binding.localL1Source.authorityNodeId,
          binding.localL1Source.chainSync.genesisIdentitySha256,
        ].join("/"),
        kupoHttpUrl: binding.kupoHttpUrl,
        ogmiosUrl: binding.ogmiosUrl,
        timeoutMs: watcherConfig.l1.requestTimeoutMs,
      }),
      stateQueueMutationLeaseCoordinator: dependencies.createLeaseCoordinator(),
      historicalNativeScriptAuthority: Object.freeze({
        ...historicalNativeScriptAuthority,
        l1SourceRoster: await historicalNativeScriptAuthority.l1SourceRoster,
      }),
      ...(replayContext === undefined ? {} : { replayContext }),
      validationChallenge,
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

export const predecessorObservationForClassifier = ({
  current,
  predecessor,
}: {
  readonly current: AuthenticatedStateQueueHeaderObservation;
  readonly predecessor: WatcherStateQueueHeaderObservation;
}): AuthenticatedStateQueueHeaderObservation => {
  assertWatcherStateQueueHeaderObservation(predecessor);
  const header = Data.from(predecessor.headerCborHex, Header);
  if (
    Data.to(header, Header) !== predecessor.headerCborHex ||
    predecessor.headerHash !== current.header.prevHeaderHash
  ) {
    throw new Error(
      "watcher predecessor observation differs from the challenged HeaderV1 link",
    );
  }
  if (
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.observedSlot) ||
    !/^(?:0|[1-9][0-9]*)$/u.test(predecessor.finalityDepth)
  ) {
    throw new Error("watcher predecessor chain point is malformed");
  }
  const confirmationDepth = BigInt(predecessor.finalityDepth);
  if (confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("watcher predecessor finality depth exceeds safe range");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: current.sourceMode,
    provenance: current.provenance,
    chainPoint: Object.freeze({
      slot: BigInt(predecessor.observedSlot),
      blockHash: predecessor.observedBlockHash,
    }),
    confirmationDepth: Number(confirmationDepth),
    headerHash: predecessor.headerHash,
    header,
  });
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

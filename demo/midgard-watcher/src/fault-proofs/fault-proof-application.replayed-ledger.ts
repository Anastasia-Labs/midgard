import {
  classifyHeader as classifyProductionHeaderV1,
  createHeaderClassifier,
  headerDecisionReplayContext,
  retainedPayloadReplaySource,
} from "@al-ft/midgard-fault-proofs";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";

import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  readAdmittedWatcherRuntimeConfig,
  type WatcherRetainedDaRuntimeOwner,
} from "../storage/retained-da-runtime.js";
import { isWatcherRetainedDaTransportUnavailable } from "../storage/retained-da-transport-unavailable.js";
import {
  WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES,
  type WatcherFaultProofApplication,
} from "./fault-proof-application.production-dependencies.js";
import type { WatcherReplayedLedger } from "./replayed-ledger.js";

type ReplayApplication = Readonly<{
  deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  retainedDaOwner: WatcherRetainedDaRuntimeOwner;
  replayedLedger: WatcherReplayedLedger | undefined;
}>;

/** The checkpoint and classifier share this application's owned DA runtime. */
export const watcherReplayCheckpointAdvance = ({
  replayedLedger,
  retainedDaOwner,
  deploymentIdentity,
  isAdmitted,
}: ReplayApplication &
  Readonly<{
    isAdmitted(): boolean;
  }>): WatcherFaultProofApplication["advanceReplayedLedger"] => {
  return async ({ runtimeConfigPath, observation }) => {
    if (replayedLedger === undefined)
      throw new Error("the watcher has no replay ledger");
    const watcherConfig = await readAdmittedWatcherRuntimeConfig({
      runtimeConfigPath,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deploymentIdentity,
    });
    if (!isAdmitted()) throw new Error("watcher replay application is closed");
    try {
      const retainedDa = await retainedDaOwner.createRuntime(watcherConfig);
      try {
        return await replayedLedger.advance(observation, retainedDa.sources);
      } finally {
        await retainedDa.close();
      }
    } catch (error) {
      if (!isWatcherRetainedDaTransportUnavailable(error)) throw error;
      return [
        {
          reason: "replayed_ledger_source_pending",
          detail: error instanceof Error ? error.message : String(error),
        },
      ];
    }
  };
};

export const classifyWatcherReplayedHeader = async ({
  request: input,
  watcherConfig,
  deploymentIdentity,
  retainedDaOwner,
  replayedLedger,
  loadClassifier,
}: ReplayApplication &
  Readonly<{
    request: Parameters<WatcherFaultProofApplication["classifyHeader"]>[0];
    watcherConfig: unknown;
    loadClassifier(
      config: unknown,
      headerHash: string,
    ): ReturnType<typeof createHeaderClassifier>;
  }>) => {
  const retainedDa = await retainedDaOwner.createRuntime(watcherConfig);
  try {
    if (retainedDa.deploymentFingerprint !== deploymentIdentity.manifestId) {
      throw new Error(
        "watcher retained-DA runtime changed deployment identity",
      );
    }
    const replaySource = retainedPayloadReplaySource({
      sources: retainedDa.sources,
      retries: input.retries,
    });
    const classificationLedger = replayedLedger?.ledgerAt(
      replaySource.sources,
      input.retries,
    );
    let ledgerReadFailed = false;
    let ledgerReadFailure: unknown;
    const predecessorLedgerAt =
      classificationLedger === undefined
        ? undefined
        : async (headerHash: string) => {
            try {
              return await classificationLedger(headerHash);
            } catch (error) {
              ledgerReadFailed = true;
              ledgerReadFailure = error;
              throw error;
            }
          };
    const decision = await classifyProductionHeaderV1({
      classifier: await loadClassifier(
        watcherConfig,
        input.observation.headerHash,
      ),
      observation: input.observation,
      authenticatedObservationDigest: input.authenticatedObservationDigest,
      ...(predecessorLedgerAt === undefined ? {} : { predecessorLedgerAt }),
      sources: replaySource.sources,
      ...(input.retries === undefined ? {} : { retries: input.retries }),
    });
    // The generic post-ledger admission records unknown sources as unavailable.
    // A live follower/DA read miss must remain a retryable classification miss.
    if (ledgerReadFailed) throw ledgerReadFailure;
    const replayContext = headerDecisionReplayContext(decision);
    // A header committing a non-empty previous ledger can only be proved
    // against that ledger; a header committing the empty one is proved
    // against the empty ledger.
    if (
      decision.decision === "fault_detected" &&
      (WATCHER_PREDECESSOR_AUTHORITY_CATEGORIES as readonly string[]).includes(
        decision.category,
      ) &&
      input.observation.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT &&
      replayContext?.predecessor === undefined
    ) {
      throw new Error(
        `${decision.category} classifier decision omitted the authenticated predecessor ledger`,
      );
    }
    return decision;
  } finally {
    await retainedDa.close();
  }
};

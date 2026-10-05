import * as SDK from "@al-ft/midgard-sdk";
import { Data, Lucid, type LucidEvolution } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { createWatcherLocalKupmiosRawSource } from "../l1/local-kupmios-raw-source.js";
import { WatcherLocalKupmios } from "../l1/native-reward-account.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../runtime/process-config.js";
import { withWatcherRetainedDaReadScope } from "../storage/retained-da-runtime.read-scope.js";
import { createWatcherAvailabilityObservation } from "./observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "./published-payload.js";
import { watcherAvailabilityAttemptProvider } from "./runtime.attempt-provider.js";
import { refreshWatcherAvailabilityAttempt } from "./runtime.protocol-parameter-refresh.js";
import {
  DA_CHALLENGE_WINDOW_MS,
  required,
} from "./runtime.release-watcher-availability-workflows.js";

export const watcherAvailabilityAuthenticatedOpenDeadline = (
  header: WatcherAuthenticatedStateQueueObservation["finalizedHeaders"][number],
): number => {
  const node = Data.from(header.stateQueueNodeCborHex, SDK.StateQueueNode);
  const cutoff = node.header.endTime + DA_CHALLENGE_WINDOW_MS;
  if (cutoff < 0n || cutoff > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error("Authenticated Open cutoff is outside the clock range");
  return Number(cutoff);
};

/** All caches, provider mutations, and raw transport cancellation belong to one
 * attempt. Only the immutable signed deployment and authenticated point persist. */
export const createWatcherAvailabilityReadAttempt = (input: {
  config: WatcherProcessConfig;
  identity: VerifiedWatcherDeploymentIdentity;
  deployment: SDK.DaAvailabilityDeployment;
  observation: WatcherAuthenticatedStateQueueObservation;
  baseLucid: LucidEvolution;
  scope: SDK.DaAvailabilityReadScope;
  assertCurrent: () => void;
  selectWallet: (lucid: LucidEvolution) => void;
}) => {
  const assertCurrent = () => {
    input.assertCurrent();
    input.scope.assertCurrent();
  };
  assertCurrent();
  const source = createWatcherLocalKupmiosRawSource({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity: input.identity,
    captureBounds: {
      signal: input.scope.signal,
      timeoutMs: Math.max(
        1,
        Math.min(
          input.config.watcherConfig.l1.requestTimeoutMs,
          Math.ceil(input.scope.remainingMs()),
        ),
      ),
    },
  });
  const intake = createWatcherAvailabilityObservation({
    identity: input.identity,
    source,
    deployment: input.deployment,
    scope: input.scope,
  });
  const l1Source = createWatcherL1AvailabilityPayloadSource({
    identity: input.identity,
    deployment: input.deployment,
    rawSource: source,
    lucid: input.baseLucid,
    currentObservation: () => input.observation,
    scope: input.scope,
  });
  let instance: Promise<LucidEvolution> | undefined;
  return {
    scope: input.scope,
    intake,
    read: <T>(read: () => Promise<T>) =>
      input.scope.read(async () => {
        assertCurrent();
        const result = await read();
        assertCurrent();
        return result;
      }),
    publicRead: <T>(read: () => Promise<T>) =>
      withWatcherRetainedDaReadScope(
        {
          scope: input.scope,
          identity: input.identity,
          l1Source,
        },
        read,
      ),
    lucid: (): Promise<LucidEvolution> => {
      assertCurrent();
      instance ??= (async () => {
        const configured = input.config.watcherConfig.l1.source;
        if (configured.sourceMode !== "local_node")
          throw new Error("Attempt requires local node");
        const kupo = required(
          configured.queryServices.find(({ kind }) => kind === "kupo"),
          "Kupo source",
        );
        const ogmios = required(
          configured.queryServices.find(({ kind }) => kind === "ogmios"),
          "Ogmios source",
        );
        const provider = watcherAvailabilityAttemptProvider(
          input.scope,
          input.config.watcherConfig.l1.requestTimeoutMs,
          (timeoutMs) =>
            new WatcherLocalKupmios(
              kupo.endpoint,
              ogmios.endpoint,
              {
                watcherConfig: input.config.watcherConfig,
                binaryPath: input.config.nativeChainSyncBinaryPath,
                timeoutMs,
              },
              { requestTimeoutMs: timeoutMs },
            ),
        );
        const lucid = await Lucid(provider, input.identity.network, {
          evaluator: createScalusEvaluator(),
          slotConfig: input.config.watcherConfig.customNetwork?.slotConfig,
          presetProtocolParameters: required(
            input.baseLucid.config().protocolParameters,
            "protocol parameters",
          ),
        });
        assertCurrent();
        input.selectWallet(lucid);
        await refreshWatcherAvailabilityAttempt(
          lucid,
          input.scope,
          input.assertCurrent,
        );
        assertCurrent();
        return lucid;
      })();
      return instance;
    },
  };
};

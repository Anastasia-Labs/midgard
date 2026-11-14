import {
  type FamilyCommonInfrastructure,
  type FamilyReferenceScriptResolver,
  WORKFLOW_RUNTIME_CONFIG,
  type WorkflowAdapterReadinessInput,
  type WorkflowRuntimeLoader,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig, type WatcherConfig } from "../runtime/config.js";
import {
  createWatcherPublicDaLibp2pTransport,
  WatcherPublicDaLibp2pTransport,
} from "./public-da-libp2p-transport.js";
import {
  admitRuntimeOptions,
  RetainedDaRequestPermits,
  runtimeOwnerIdentities,
  type WatcherRetainedDaRuntime,
  type WatcherRetainedDaRuntimeOptions,
  type WatcherRetainedDaRuntimeOwner,
  type WatcherRetainedDaTransportStatus,
} from "./retained-da-runtime.retained-da-request-permits.js";
import { createRuntimeFromAdmittedConfig } from "./retained-da-runtime.watcher-retained-da-libp2p-transport.js";
import { WatcherRetainedDaTransportUnavailableError } from "./retained-da-transport-unavailable.js";

/** Spacing of transport start attempts after a failure: 1 s doubling to 60 s. */
export const watcherRetainedDaTransportRetryDelayMs = (
  consecutiveFailures: number,
): number =>
  Math.min(
    60_000,
    1_000 * 2 ** Math.min(Math.max(consecutiveFailures - 1, 0), 6),
  );

/**
 * Keeps one client identity/connection alive across bounded workflow leases.
 * Ownership is explicit: lease close revokes its requests; owner close tears
 * down the node. Repeated classifications must not redial as new peers.
 *
 * A transport that failed to start holds no state, so a later lease starts
 * it again once the retry delay has passed; until then a lease is refused
 * with the last failure. Both refusals are typed as transient, so a caller
 * waits instead of failing closed. The admitted DA configuration stays
 * pinned.
 */
export const createWatcherRetainedDaRuntimeOwner = (
  options: Omit<WatcherRetainedDaRuntimeOptions, "watcherConfig">,
): WatcherRetainedDaRuntimeOwner => {
  const admitted = admitRuntimeOptions(options);
  const controller = new AbortController();
  let configuration: string | undefined;
  let transport: Promise<WatcherPublicDaLibp2pTransport> | undefined;
  let permits: RetainedDaRequestPermits | undefined;
  let closePromise: Promise<void> | undefined;
  let consecutiveFailures = 0;
  let lastFailure: unknown;
  let retryNotBeforeMs = Number.NEGATIVE_INFINITY;
  let transportState: WatcherRetainedDaTransportStatus = Object.freeze({
    state: "idle",
    failure: null,
  });
  const start = (): Promise<WatcherPublicDaLibp2pTransport> => {
    transportState = Object.freeze({ state: "opening", failure: null });
    const attempt = (
      admitted.unsafeTransportFactoryForTest ??
      createWatcherPublicDaLibp2pTransport
    )(admitted.unsafeTransportOptionsForTest).then(
      (started) => {
        consecutiveFailures = 0;
        if (transportState.state === "opening") {
          transportState = Object.freeze({ state: "open", failure: null });
        }
        return started;
      },
      (error: unknown) => {
        if (transport === attempt) transport = undefined;
        consecutiveFailures += 1;
        lastFailure = error;
        const delayMs =
          watcherRetainedDaTransportRetryDelayMs(consecutiveFailures);
        retryNotBeforeMs = performance.now() + delayMs;
        if (transportState.state === "opening") {
          transportState = Object.freeze({
            state: "failed",
            failure: error instanceof Error ? error.message : String(error),
          });
        }
        throw new WatcherRetainedDaTransportUnavailableError(error, delayMs);
      },
    );
    return attempt;
  };
  const owner: WatcherRetainedDaRuntimeOwner = Object.freeze({
    transportStatus: () => transportState,
    createRuntime: async (config: WatcherConfig) => {
      controller.signal.throwIfAborted();
      return await createRuntimeFromAdmittedConfig(admitted, config, {
        signal: controller.signal,
        open: async () => {
          controller.signal.throwIfAborted();
          const binding = JSON.stringify({
            mode: config.mode,
            targetNetwork: config.targetNetwork,
            customNetwork: config.customNetwork,
            da: config.da,
          });
          if (configuration !== undefined && configuration !== binding) {
            throw new Error("retained-DA owner configuration changed");
          }
          if (transport === undefined) {
            const waitMs = retryNotBeforeMs - performance.now();
            if (waitMs > 0)
              throw new WatcherRetainedDaTransportUnavailableError(
                lastFailure,
                Math.ceil(waitMs),
              );
            configuration = binding;
            permits ??= new RetainedDaRequestPermits(config.da.maxConcurrency);
            transport = start();
          }
          const opened = await transport;
          controller.signal.throwIfAborted();
          return opened;
        },
        acquireRequest: async (signal) => await permits!.acquire(signal),
      });
    },
    close: () => {
      if (closePromise !== undefined) return closePromise;
      controller.abort(new Error("retained-DA runtime owner is closed"));
      transportState = Object.freeze({ state: "closed", failure: null });
      closePromise = (async () => {
        // A start that failed left nothing to stop; close must not re-throw it.
        const started = await transport?.catch(() => undefined);
        await started?.stop();
      })();
      return closePromise;
    },
  });
  runtimeOwnerIdentities.set(owner, admitted.deploymentIdentity);
  return owner;
};

/** What the watcher builds for one invocation and every family draws from. */
export type WatcherWorkflowInfrastructure = Readonly<{
  infrastructure: FamilyCommonInfrastructure;
  resolveReferenceScript: FamilyReferenceScriptResolver;
}>;

export type WatcherWorkflowInfrastructureBuilder = (input: {
  readonly watcherConfig: WatcherConfig;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<WatcherWorkflowInfrastructure>;

export const createWatcherRetainedDaRuntime = async (
  options: WatcherRetainedDaRuntimeOptions,
): Promise<WatcherRetainedDaRuntime> => {
  const { watcherConfig, ...runtimeOptions } = options;
  return await createRuntimeFromAdmittedConfig(
    admitRuntimeOptions(runtimeOptions),
    watcherConfig,
  );
};

/**
 * The watcher's one shared-workflow loader, the same for every family.
 *
 * `watcherConfig` is the configuration startup admitted. It may name public
 * network infrastructure and secret *sources*, never prepared proof evidence.
 * The loader never reads the runtime configuration path the shared runner
 * forwards: startup proved that file equal to this value and the watcher does
 * not open it again. The common infrastructure (Lucid, signer, mutation lease
 * coordinator, the optional parts a family may require) and the reference
 * resolver are constructed by the application callback after this loader has
 * independently bound public DA to the verified deployment; each family's
 * record then resolves its own roster and binds its own config from them. The
 * shared runner owns and always invokes `close`.
 */
export const createWatcherWorkflowRuntimeLoader = (
  options: WatcherRetainedDaRuntimeOptions & {
    readonly buildInfrastructure: WatcherWorkflowInfrastructureBuilder;
    readonly runtimeOwner?: WatcherRetainedDaRuntimeOwner;
  },
): WorkflowRuntimeLoader => {
  const {
    buildInfrastructure,
    runtimeOwner,
    deploymentIdentity,
    unsafeTransportFactoryForTest,
    unsafeTransportOptionsForTest,
  } = options;
  if (typeof buildInfrastructure !== "function") {
    throw new Error(
      "production workflow runtime omitted its infrastructure builder",
    );
  }
  const watcherConfig = parseWatcherConfig(options.watcherConfig);
  const admittedRuntimeOptions = admitRuntimeOptions({
    deploymentIdentity,
    ...(unsafeTransportOptionsForTest === undefined
      ? {}
      : { unsafeTransportOptionsForTest }),
    ...(unsafeTransportFactoryForTest === undefined
      ? {}
      : { unsafeTransportFactoryForTest }),
  });
  if (
    runtimeOwner !== undefined &&
    runtimeOwnerIdentities.get(runtimeOwner) !== deploymentIdentity
  ) {
    throw new Error("retained-DA owner belongs to another deployment identity");
  }
  return async ({ invocation }) => {
    if (
      invocation.deploymentFingerprint !==
      admittedRuntimeOptions.deploymentIdentity.manifestId
    ) {
      throw new Error(
        "production workflow invocation deployment differs from verified watcher authority",
      );
    }
    const retainedDa =
      runtimeOwner === undefined
        ? await createRuntimeFromAdmittedConfig(
            admittedRuntimeOptions,
            watcherConfig,
          )
        : await runtimeOwner.createRuntime(watcherConfig);
    try {
      const { infrastructure, resolveReferenceScript } =
        await buildInfrastructure({ watcherConfig, invocation });
      return {
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        infrastructure,
        resolveReferenceScript,
        retainedDaSources: retainedDa.sources,
        close: retainedDa.close,
      };
    } catch (cause) {
      await retainedDa.close();
      throw cause;
    }
  };
};

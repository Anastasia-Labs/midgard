import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Duration, Effect } from "effect";

import { startDaLibp2pRetainedPayloadServerFromEnv } from "../da/libp2p-producer.js";
import { DaPayloadsDB } from "../database/index.js";
import { isRetryableProviderError } from "../provider-retry.js";
import { retryStartupStep } from "../services/startup-waiting.js";

export const logStartupFailure = (message: string) => (error: unknown) =>
  Effect.logError(`${message}: ${formatUnknownError(error)}`);

/**
 * Runs a provider-backed startup step, waiting out a retryable provider
 * failure (`isRetryableProviderError`) every `retryDelayMs` with no deadline
 * under the named reason; any other failure fails at once.
 */
export const runStartupProviderStepWithRetry = <A, E, R>(
  key: string,
  step: Effect.Effect<A, E, R>,
  options: Readonly<{
    retryDelayMs: number;
    reason: string | ((error: E) => string);
  }>,
): Effect.Effect<A, E, R> => {
  const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
  return retryStartupStep(step, {
    key,
    reason: options.reason,
    retryable: isRetryableProviderError,
    initialMs: retryDelayMs,
    maxMs: retryDelayMs,
  });
};

/** The retained-payload server's last known state. A start failure never
 * stops the node: the thread retries forever, warning on every attempt. */
export type RetainedPayloadServerStatus = Readonly<
  | { state: "starting" }
  | { state: "serving"; since: string }
  | { state: "not_configured"; reason: string }
  | {
      state: "retrying";
      since: string;
      attempts: number;
      lastError: string;
      retryInMs: number;
    }
>;
export const RETAINED_PAYLOAD_SERVER_RETRY_INITIAL_MS = 5_000;
export const RETAINED_PAYLOAD_SERVER_RETRY_MAX_MS = 60_000;

let status: RetainedPayloadServerStatus = { state: "starting" };
/** Read by readiness and operators; see {@link RetainedPayloadServerStatus}. */
export const retainedPayloadServerStatus = (): RetainedPayloadServerStatus =>
  status;

export const retainedPayloadServerThread = (
  retrieveByHeaderHash: (
    headerHash: Buffer,
  ) => Promise<DaPayloadsDB.Row | undefined>,
  options: {
    readonly start?: typeof startDaLibp2pRetainedPayloadServerFromEnv;
    readonly retryInitialMs?: number;
    readonly retryMaxMs?: number;
  } = {},
): Effect.Effect<void, never> =>
  Effect.gen(function* () {
    const start = options.start ?? startDaLibp2pRetainedPayloadServerFromEnv;
    const initialMs =
      options.retryInitialMs ?? RETAINED_PAYLOAD_SERVER_RETRY_INITIAL_MS;
    const maxMs = options.retryMaxMs ?? RETAINED_PAYLOAD_SERVER_RETRY_MAX_MS;
    status = { state: "starting" };
    let since: string | undefined;
    for (let attempts = 1; ; attempts += 1) {
      const started = yield* Effect.either(
        Effect.tryPromise({
          try: () => start({ retrieveByHeaderHash }),
          catch: (cause) => cause,
        }),
      );
      if (started._tag === "Right") return started.right;
      since ??= new Date().toISOString();
      const retryInMs = Math.min(maxMs, initialMs * 2 ** (attempts - 1));
      const lastError = formatUnknownError(started.left, {
        includeCause: true,
      });
      status = { state: "retrying", since, attempts, lastError, retryInMs };
      yield* Effect.logWarning(
        `DA libp2p retained-payload server failed to start (attempt ${attempts.toString()}); retrying in ${retryInMs.toString()}ms: ${lastError}`,
      ).pipe(
        Effect.annotateLogs({
          event: "da_retained_payload_server_retry",
          attempts,
          retryInMs,
        }),
      );
      yield* Effect.sleep(Duration.millis(retryInMs));
    }
  }).pipe(
    Effect.tap((server) => {
      status = server.configured
        ? { state: "serving", since: new Date().toISOString() }
        : {
            state: "not_configured",
            reason: server.reason ?? "not configured",
          };
      return server.configured
        ? Effect.logInfo(
            `DA libp2p retained-payload server listening deployment_fingerprint=${server.deploymentFingerprint},local_peer_id=${server.localPeerId},listen=${server.listenMultiaddrs?.join(",") ?? ""},announce=${server.announceMultiaddrs?.join(",") ?? ""}`,
          )
        : Effect.logInfo(
            `DA libp2p retained-payload server skipped: ${server.reason ?? "not configured"}`,
          );
    }),
    Effect.flatMap((server) =>
      server.configured
        ? Effect.never.pipe(
            Effect.ensuring(
              server.close === undefined
                ? Effect.void
                : Effect.promise(() => server.close!()).pipe(
                    Effect.catchAll((error) =>
                      Effect.logWarning(
                        `DA libp2p retained-payload server stop failed: ${formatUnknownError(error)}`,
                      ),
                    ),
                  ),
            ),
          )
        : Effect.void,
    ),
  );

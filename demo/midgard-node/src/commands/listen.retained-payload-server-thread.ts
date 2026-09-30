import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Duration, Effect } from "effect";

import { startDaLibp2pRetainedPayloadServerFromEnv } from "../da/libp2p-producer.js";
import { DaPayloadsDB } from "../database/index.js";
import { isRetryableProviderError } from "../provider-retry.js";

export const logStartupFailure = (message: string) => (error: unknown) =>
  Effect.logError(`${message}: ${formatUnknownError(error)}`);

export const runStartupProviderStepWithRetry = <A, E, R>(
  label: string,
  step: Effect.Effect<A, E, R>,
  options: { readonly maxAttempts: number; readonly retryDelayMs: number },
): Effect.Effect<A, E, R> =>
  Effect.gen(function* () {
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts));
    const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
    let lastError: E | undefined;

    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      const result = yield* Effect.either(step);
      if (result._tag === "Right") {
        if (attempt > 1) {
          yield* Effect.logInfo(
            `${label} became available after ${attempt.toString()} attempt(s).`,
          );
        }
        return result.right;
      }

      lastError = result.left;
      if (!isRetryableProviderError(lastError)) {
        return yield* Effect.fail(lastError);
      }
      if (attempt < maxAttempts) {
        yield* Effect.logWarning(
          `${label} failed with a retryable provider error (attempt ${attempt.toString()}/${maxAttempts.toString()}); retrying in ${retryDelayMs.toString()}ms. cause=${formatUnknownError(lastError, { includeCause: true })}`,
        );
        if (retryDelayMs > 0) {
          yield* Effect.sleep(Duration.millis(retryDelayMs));
        }
      }
    }

    return yield* Effect.fail(lastError as E);
  });

export const retainedPayloadServerThread = (
  retrieveByHeaderHash: (
    headerHash: Buffer,
  ) => Promise<DaPayloadsDB.Row | undefined>,
): Effect.Effect<void, never> =>
  Effect.tryPromise({
    try: () =>
      startDaLibp2pRetainedPayloadServerFromEnv({ retrieveByHeaderHash }),
    catch: (cause) => cause,
  }).pipe(
    Effect.tap((server) =>
      server.configured
        ? Effect.logInfo(
            `DA libp2p retained-payload server listening deployment_fingerprint=${server.deploymentFingerprint},local_peer_id=${server.localPeerId},listen=${server.listenMultiaddrs?.join(",") ?? ""},announce=${server.announceMultiaddrs?.join(",") ?? ""}`,
          )
        : Effect.logInfo(
            `DA libp2p retained-payload server skipped: ${server.reason ?? "not configured"}`,
          ),
    ),
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
    Effect.catchAll((error) =>
      Effect.logWarning(
        `DA libp2p retained-payload server disabled after startup failure: ${formatUnknownError(error)}`,
      ),
    ),
  );

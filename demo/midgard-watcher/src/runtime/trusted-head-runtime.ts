import { createHash } from "node:crypto";

import type { WatcherFinalityPolicy } from "../l1/finality-engine.js";
import type { WatcherWalletKeySource } from "./config.js";
import {
  decodeWatcherAuthenticationKey32,
  decodeWatcherHttpBearerSecret,
  loadWatcherSecretText,
  type WatcherProcessConfig,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "./process-config.js";
import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
  type WatcherTrustedHeadAuthorityClient,
  type WatcherTrustedHeadAuthorityServer,
} from "./trusted-head-authority.js";

const sha256 = (value: Uint8Array | string): string =>
  createHash("sha256").update(value).digest("hex");

const secretCandidateIds = (value: string): ReadonlySet<string> => {
  const ids = new Set([sha256(value)]);
  if (/^[0-9a-f]{64}$/u.test(value)) {
    ids.add(sha256(Uint8Array.from(Buffer.from(value, "hex"))));
  }
  return ids;
};

const assertDistinctSecretCandidates = (
  candidates: readonly ReadonlySet<string>[],
): void => {
  const seen = new Set<string>();
  for (const ids of candidates) {
    if ([...ids].some((id) => seen.has(id))) {
      throw new Error(
        "production derived secret values must be pairwise distinct",
      );
    }
    for (const id of ids) seen.add(id);
  }
};

export type WatcherTrustedHeadAuthorityProcessRuntime = Readonly<{
  server: WatcherTrustedHeadAuthorityServer;
  close(): Promise<void>;
}>;

/**
 * Starts the append-only authority from its sidecar-only config. This process
 * loads the record key and bearer only; it has no field or loader for the
 * watcher rollback HMAC key or proof signer.
 */
export const startWatcherTrustedHeadAuthorityProcess = async (input: {
  readonly config: WatcherTrustedHeadAuthorityProcessConfig;
  readonly unsafeEnvironmentForTest?: Readonly<
    Record<string, string | undefined>
  >;
  readonly unsafeAllowEphemeralPortForTest?: true;
}): Promise<WatcherTrustedHeadAuthorityProcessRuntime> => {
  const [recordText, bearerText] = await Promise.all([
    loadWatcherSecretText(
      input.config.recordAuthenticationKeySource,
      input.unsafeEnvironmentForTest,
    ),
    loadWatcherSecretText(
      input.config.httpBearerSecretSource,
      input.unsafeEnvironmentForTest,
    ),
  ]);
  const recordAuthenticationKey = decodeWatcherAuthenticationKey32(recordText);
  const httpSecret = decodeWatcherHttpBearerSecret(bearerText);
  assertDistinctSecretCandidates([
    secretCandidateIds(recordText),
    secretCandidateIds(bearerText),
  ]);
  const store = await openWatcherTrustedHeadAuthorityStore({
    directory: input.config.directory,
    policy: input.config.policy,
    recordAuthenticationKey,
  });
  const server = await startWatcherTrustedHeadAuthorityServer({
    endpoint: input.config.endpoint,
    httpSecret,
    store,
    ...(input.unsafeAllowEphemeralPortForTest === true
      ? { unsafeAllowEphemeralPortForTest: true as const }
      : {}),
  });
  return Object.freeze({ server, close: async () => await server.close() });
};

export type WatcherTrustedHeadClientRuntime = Readonly<{
  client: WatcherTrustedHeadAuthorityClient;
  rollbackAuthenticationKey: Uint8Array;
  rollbackAuthenticationKeyId: string;
  recordAuthenticationKeyId: string;
}>;

/**
 * Starts the watcher half of the freshness boundary. It loads only the
 * rollback key and bearer. The sidecar exposes its record-key ID, never the
 * record key, so all three derived values can be collision-checked before any
 * chain event is admitted.
 */
export const createWatcherTrustedHeadClientRuntime = async (input: {
  readonly config: WatcherProcessConfig;
  readonly policy: WatcherFinalityPolicy;
  readonly additionalSecretSources?: readonly WatcherWalletKeySource[];
  readonly unsafeEnvironmentForTest?: Readonly<
    Record<string, string | undefined>
  >;
}): Promise<WatcherTrustedHeadClientRuntime> => {
  const [rollbackText, bearerText, proofSignerText, additionalSecretTexts] =
    await Promise.all([
      loadWatcherSecretText(
        input.config.watcherConfig.storage.rollbackAuthorityKeySource,
        input.unsafeEnvironmentForTest,
      ),
      loadWatcherSecretText(
        input.config.httpBearerSecretSource,
        input.unsafeEnvironmentForTest,
      ),
      loadWatcherSecretText(
        input.config.watcherConfig.proverWallet.keySource,
        input.unsafeEnvironmentForTest,
      ),
      Promise.all(
        (input.additionalSecretSources ?? []).map(
          async (source) =>
            await loadWatcherSecretText(source, input.unsafeEnvironmentForTest),
        ),
      ),
    ]);
  const rollbackAuthenticationKey =
    decodeWatcherAuthenticationKey32(rollbackText);
  const httpSecret = decodeWatcherHttpBearerSecret(bearerText);
  const rollbackAuthenticationKeyId = sha256(rollbackAuthenticationKey);
  const client = createWatcherTrustedHeadAuthorityClient({
    endpoint: input.config.trustedHeadAuthorityEndpoint,
    httpSecret,
    policy: input.policy,
    authenticationKey: rollbackAuthenticationKey,
    requestTimeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
  });
  const recordAuthenticationKeyId =
    await client.readRecordAuthenticationKeyId();
  assertDistinctSecretCandidates([
    secretCandidateIds(rollbackText),
    secretCandidateIds(bearerText),
    secretCandidateIds(proofSignerText),
    ...additionalSecretTexts.map(secretCandidateIds),
    new Set([recordAuthenticationKeyId]),
  ]);
  // An existing poisoned record-authentic but watcher-MAC-invalid head must
  // stop startup before the native chain-sync process is spawned.
  await client.readCurrent();
  return Object.freeze({
    client,
    rollbackAuthenticationKey: Uint8Array.from(rollbackAuthenticationKey),
    rollbackAuthenticationKeyId,
    recordAuthenticationKeyId,
  });
};

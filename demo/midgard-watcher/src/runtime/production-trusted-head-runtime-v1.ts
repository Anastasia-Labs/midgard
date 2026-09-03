import { createHash } from "node:crypto";

import type { WatcherFinalityPolicyV1 } from "../l1/finality-engine.js";
import type { WatcherWalletKeySource } from "./config.js";
import {
  decodeWatcherAuthenticationKey32V1,
  decodeWatcherHttpBearerSecretV1,
  loadWatcherSecretTextV1,
  type WatcherProductionProcessConfigV1,
  type WatcherTrustedHeadAuthorityProcessConfigV1,
} from "./production-process-config-v1.js";
import {
  createWatcherTrustedHeadAuthorityClientV1,
  openWatcherTrustedHeadAuthorityStoreV1,
  startWatcherTrustedHeadAuthorityServerV1,
  type WatcherTrustedHeadAuthorityClientV1,
  type WatcherTrustedHeadAuthorityServerV1,
} from "./trusted-head-authority-v1.js";

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

export type WatcherTrustedHeadAuthorityProcessRuntimeV1 = Readonly<{
  server: WatcherTrustedHeadAuthorityServerV1;
  close(): Promise<void>;
}>;

/**
 * Starts the append-only authority from its sidecar-only config. This process
 * loads the record key and bearer only; it has no field or loader for the
 * watcher rollback HMAC key or proof signer.
 */
export const startWatcherTrustedHeadAuthorityProcessV1 = async (input: {
  readonly config: WatcherTrustedHeadAuthorityProcessConfigV1;
  readonly unsafeEnvironmentForTest?: Readonly<
    Record<string, string | undefined>
  >;
  readonly unsafeAllowEphemeralPortForTest?: true;
}): Promise<WatcherTrustedHeadAuthorityProcessRuntimeV1> => {
  const [recordText, bearerText] = await Promise.all([
    loadWatcherSecretTextV1(
      input.config.recordAuthenticationKeySource,
      input.unsafeEnvironmentForTest,
    ),
    loadWatcherSecretTextV1(
      input.config.httpBearerSecretSource,
      input.unsafeEnvironmentForTest,
    ),
  ]);
  const recordAuthenticationKey =
    decodeWatcherAuthenticationKey32V1(recordText);
  const httpSecret = decodeWatcherHttpBearerSecretV1(bearerText);
  assertDistinctSecretCandidates([
    secretCandidateIds(recordText),
    secretCandidateIds(bearerText),
  ]);
  const store = await openWatcherTrustedHeadAuthorityStoreV1({
    directory: input.config.directory,
    policy: input.config.policy,
    recordAuthenticationKey,
  });
  const server = await startWatcherTrustedHeadAuthorityServerV1({
    endpoint: input.config.endpoint,
    httpSecret,
    store,
    ...(input.unsafeAllowEphemeralPortForTest === true
      ? { unsafeAllowEphemeralPortForTest: true as const }
      : {}),
  });
  return Object.freeze({ server, close: async () => await server.close() });
};

export type WatcherProductionTrustedHeadClientRuntimeV1 = Readonly<{
  client: WatcherTrustedHeadAuthorityClientV1;
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
export const createWatcherProductionTrustedHeadClientRuntimeV1 = async (input: {
  readonly config: WatcherProductionProcessConfigV1;
  readonly policy: WatcherFinalityPolicyV1;
  readonly additionalSecretSources?: readonly WatcherWalletKeySource[];
  readonly unsafeEnvironmentForTest?: Readonly<
    Record<string, string | undefined>
  >;
}): Promise<WatcherProductionTrustedHeadClientRuntimeV1> => {
  const [rollbackText, bearerText, proofSignerText, additionalSecretTexts] =
    await Promise.all([
      loadWatcherSecretTextV1(
        input.config.watcherConfig.storage.rollbackAuthorityKeySource,
        input.unsafeEnvironmentForTest,
      ),
      loadWatcherSecretTextV1(
        input.config.httpBearerSecretSource,
        input.unsafeEnvironmentForTest,
      ),
      loadWatcherSecretTextV1(
        input.config.watcherConfig.proverWallet.keySource,
        input.unsafeEnvironmentForTest,
      ),
      Promise.all(
        (input.additionalSecretSources ?? []).map(
          async (source) =>
            await loadWatcherSecretTextV1(
              source,
              input.unsafeEnvironmentForTest,
            ),
        ),
      ),
    ]);
  const rollbackAuthenticationKey =
    decodeWatcherAuthenticationKey32V1(rollbackText);
  const httpSecret = decodeWatcherHttpBearerSecretV1(bearerText);
  const rollbackAuthenticationKeyId = sha256(rollbackAuthenticationKey);
  const client = createWatcherTrustedHeadAuthorityClientV1({
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

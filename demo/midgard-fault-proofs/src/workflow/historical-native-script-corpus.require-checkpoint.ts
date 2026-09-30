import { createHash } from "node:crypto";

import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import {
  admittedCheckpointStores,
  createHistoricalNativeScriptHttpHistoryProvider,
  HISTORICAL_NATIVE_SCRIPT_CHECKPOINT,
  HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE,
  HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD,
  HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE,
  type HistoricalNativeScriptCheckpoint,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistoryProvider,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptProviderRoster,
  requireHistoricalNativeScriptProviderRoster,
} from "./historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import { admitFraudProofRawL1Point } from "./raw-l1-snapshot.js";

export const admittedHistorySources = new WeakSet<object>();

const exactHistoryRecord = ({
  value,
  provider,
  deploymentFingerprint,
  headerHash,
  providerRosterDigest,
}: {
  readonly value: unknown;
  readonly provider: HistoricalNativeScriptHistoryProvider;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly providerRosterDigest: string;
}) => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length ||
    Object.keys(value).sort().join(",") !==
      "deploymentFingerprint,headerHash,inclusionPoint,payloadEnvelopeCborHex,schemaVersion"
  ) {
    throw new Error(
      `historical source ${provider.sourceId} returned a non-record`,
    );
  }
  const parsed = value as Readonly<Record<string, unknown>>;
  if (
    Object.keys(parsed).sort().join(",") !==
      "deploymentFingerprint,headerHash,inclusionPoint,payloadEnvelopeCborHex,schemaVersion" ||
    parsed.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD ||
    parsed.deploymentFingerprint !== deploymentFingerprint ||
    parsed.headerHash !== headerHash ||
    typeof parsed.payloadEnvelopeCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(parsed.payloadEnvelopeCborHex)
  ) {
    throw new Error(
      `historical source ${provider.sourceId} changed the deployment/header or raw payload shape`,
    );
  }
  const inclusionPoint = admitFraudProofRawL1Point(
    parsed.inclusionPoint,
    `historical source ${provider.sourceId} inclusion point`,
  );
  const payloadEnvelopeCbor = Buffer.from(parsed.payloadEnvelopeCborHex, "hex");
  const candidate = Object.freeze({
    payloadEnvelopeCbor,
    inclusionPoint,
    authorityDigest: createHash("sha256")
      .update(
        JSON.stringify({
          deploymentFingerprint,
          providerRosterDigest,
          headerHash,
          payloadEnvelopeCborHex: parsed.payloadEnvelopeCborHex,
          inclusionPoint,
        }),
      )
      .digest("hex"),
  });
  return candidate;
};

/** Admits the immutable external archive quorum frozen by the application. */
export const createHistoricalNativeScriptHistorySource = ({
  providerRoster,
}: {
  readonly providerRoster: HistoricalNativeScriptProviderRoster;
}): HistoricalNativeScriptHistorySource => {
  requireHistoricalNativeScriptProviderRoster(providerRoster);
  const deploymentFingerprint = providerRoster.deploymentFingerprint;
  const providers = providerRoster.providers.map((provider) =>
    createHistoricalNativeScriptHttpHistoryProvider({
      sourceMode: "external_provider",
      sourceId: provider.sourceId,
      authorityEndpoint: provider.authorityEndpoint,
      operatorIdentitySha256: provider.operatorIdentitySha256,
    }),
  );
  const source: HistoricalNativeScriptHistorySource = Object.freeze({
    sourceVersion: HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE,
    sourceMode: "external_provider_quorum",
    deploymentFingerprint,
    providerRosterDigest: providerRoster.rosterDigest,
    fetchPayloadByHeaderHash: async ({
      headerHash,
    }: Parameters<
      HistoricalNativeScriptHistorySource["fetchPayloadByHeaderHash"]
    >[0]) => {
      if (!/^[0-9a-f]{56}$/u.test(headerHash)) {
        throw new Error("historical source header hash is invalid");
      }
      const candidates = await Promise.all(
        providers.map(async (provider) =>
          exactHistoryRecord({
            value: await provider.fetchPayloadByHeaderHash({
              deploymentFingerprint,
              headerHash,
            }),
            provider,
            deploymentFingerprint,
            headerHash,
            providerRosterDigest: providerRoster.rosterDigest,
          }),
        ),
      );
      const first = candidates[0]!;
      if (
        candidates.some(
          (candidate) => candidate.authorityDigest !== first.authorityDigest,
        )
      ) {
        throw new Error(
          "historical external providers disagree on exact raw history",
        );
      }
      return first;
    },
  });
  admittedHistorySources.add(source);
  return source;
};

/** Explicitly volatile test seam; production authority rejects this brand. */
export const unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest =
  (): HistoricalNativeScriptCheckpointStore => {
    let checkpoint: HistoricalNativeScriptCheckpoint | null = null;
    const store: HistoricalNativeScriptCheckpointStore = Object.freeze({
      storeVersion: HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE,
      durability: "unsafe_process_memory_test_v1",
      load: async () => checkpoint,
      compareAndSwap: async ({
        deploymentFingerprint,
        expectedCheckpointDigest,
        next,
      }: Parameters<
        HistoricalNativeScriptCheckpointStore["compareAndSwap"]
      >[0]) => {
        if (next.deploymentFingerprint !== deploymentFingerprint) {
          throw new Error("historical checkpoint changed deployment identity");
        }
        if (
          (checkpoint?.checkpointDigest ?? null) !== expectedCheckpointDigest
        ) {
          return "stale";
        }
        checkpoint = next;
        return "stored";
      },
    });
    admittedCheckpointStores.add(store);
    return store;
  };

export const sha256 = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const checkpointWithoutDigest = (
  checkpoint: HistoricalNativeScriptCheckpoint,
) => ({
  schemaVersion: checkpoint.schemaVersion,
  deploymentFingerprint: checkpoint.deploymentFingerprint,
  throughHeaderHash: checkpoint.throughHeaderHash,
  throughUtxosRoot: checkpoint.throughUtxosRoot,
  throughPayloadEnvelopeCborHex: checkpoint.throughPayloadEnvelopeCborHex,
  throughPayloadEnvelopeSha256: checkpoint.throughPayloadEnvelopeSha256,
  headerHashes: checkpoint.headerHashes,
  payloadEnvelopeSha256s: checkpoint.payloadEnvelopeSha256s,
  entries: checkpoint.entries,
  providerRosterDigest: checkpoint.providerRosterDigest,
  predecessorCheckpointDigest: checkpoint.predecessorCheckpointDigest,
});

const checkpointDigestV1 = (
  checkpoint: HistoricalNativeScriptCheckpoint,
): string =>
  createHash("sha256")
    .update(JSON.stringify(checkpointWithoutDigest(checkpoint)))
    .digest("hex");

export const requireCheckpoint = async ({
  value,
  deploymentFingerprint,
}: {
  readonly value: unknown;
  readonly deploymentFingerprint: string;
}): Promise<HistoricalNativeScriptCheckpoint | null> => {
  if (value === null) return null;
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length ||
    Object.keys(value).sort().join(",") !==
      "checkpointDigest,deploymentFingerprint,entries,headerHashes,payloadEnvelopeSha256s,predecessorCheckpointDigest,providerRosterDigest,schemaVersion,throughHeaderHash,throughPayloadEnvelopeCborHex,throughPayloadEnvelopeSha256,throughUtxosRoot"
  ) {
    throw new Error(
      "historical native-script checkpoint is not an exact record",
    );
  }
  const checkpoint = value as HistoricalNativeScriptCheckpoint;
  if (
    checkpoint.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_CHECKPOINT ||
    checkpoint.deploymentFingerprint !== deploymentFingerprint ||
    !/^[0-9a-f]{56}$/u.test(checkpoint.throughHeaderHash) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.throughUtxosRoot) ||
    !/^(?:[0-9a-f]{2})+$/u.test(checkpoint.throughPayloadEnvelopeCborHex) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.throughPayloadEnvelopeSha256) ||
    !Array.isArray(checkpoint.headerHashes) ||
    !Array.isArray(checkpoint.payloadEnvelopeSha256s) ||
    checkpoint.headerHashes.length === 0 ||
    checkpoint.headerHashes.length !==
      checkpoint.payloadEnvelopeSha256s.length ||
    checkpoint.headerHashes.at(-1) !== checkpoint.throughHeaderHash ||
    checkpoint.payloadEnvelopeSha256s.at(-1) !==
      checkpoint.throughPayloadEnvelopeSha256 ||
    !Array.isArray(checkpoint.entries) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.providerRosterDigest) ||
    (checkpoint.predecessorCheckpointDigest !== null &&
      !/^[0-9a-f]{64}$/u.test(checkpoint.predecessorCheckpointDigest)) ||
    checkpoint.checkpointDigest !== checkpointDigestV1(checkpoint)
  ) {
    throw new Error(
      "historical native-script checkpoint identity, shape, or digest is invalid",
    );
  }
  const uniqueHeaders = new Set(checkpoint.headerHashes);
  if (
    uniqueHeaders.size !== checkpoint.headerHashes.length ||
    checkpoint.headerHashes.some((hash) => !/^[0-9a-f]{56}$/u.test(hash)) ||
    checkpoint.payloadEnvelopeSha256s.some(
      (hash) => !/^[0-9a-f]{64}$/u.test(hash),
    )
  ) {
    throw new Error("historical native-script checkpoint chain is malformed");
  }
  const envelope = Buffer.from(checkpoint.throughPayloadEnvelopeCborHex, "hex");
  if (sha256(envelope) !== checkpoint.throughPayloadEnvelopeSha256) {
    throw new Error(
      "historical native-script checkpoint payload digest changed",
    );
  }
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: envelope,
    expectedHeaderHash: checkpoint.throughHeaderHash,
  });
  if (reconstruction.header.utxosRoot !== checkpoint.throughUtxosRoot) {
    throw new Error("historical native-script checkpoint UTxO root changed");
  }
  return checkpoint;
};

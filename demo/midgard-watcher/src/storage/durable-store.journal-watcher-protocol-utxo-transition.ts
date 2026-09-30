import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  assertReferences,
  rebuildWatcherDurableCaches,
} from "./durable-store.assert-references.js";
import {
  CANONICAL_NATURAL,
  type CanonicalJson,
  canonicalJson,
  exactLiteral,
  exactRecord,
  exactString,
  fail,
  HEX_32,
  WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
  WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
  WATCHER_DURABLE_MIGRATION_VERSION,
  WATCHER_DURABLE_STORE_SCHEMA_VERSION,
} from "./durable-store.canonical-json.js";
import {
  compareChainPointOrder,
  type WatcherDurableCacheEntry,
  type WatcherDurableCaches,
  type WatcherDurableRecords,
  type WatcherDurableStore,
  type WatcherL1ChainPoint,
  type WatcherProtocolUtxo,
  type WatcherSpentProtocolUtxo,
} from "./durable-store.parse-l1-observation.js";
import {
  parseRecords,
  parseSortedRecords,
  STORE_KEYS,
} from "./durable-store.parse-records.js";

const parseCaches = (value: unknown): WatcherDurableCaches => {
  const caches = exactRecord(value, "$.caches", [
    "schemaVersion",
    "sourceSha256",
    "entries",
  ]);
  if (caches.schemaVersion !== WATCHER_DURABLE_CACHE_SCHEMA_VERSION) {
    fail("unsupported_schema", "$.caches.schemaVersion");
  }
  const entries = parseSortedRecords(
    caches.entries,
    "$.caches.entries",
    (member, path): WatcherDurableCacheEntry => {
      const entry = exactRecord(member, path, [
        "namespace",
        "key",
        "index",
        "recordSha256",
      ]);
      return {
        namespace: exactLiteral(entry.namespace, `${path}.namespace`, [
          "chain_points",
          "confirmations",
          "correction_results",
          "da_proof_inputs",
          "deadlines",
          "decisions",
          "faults",
          "l1_observations",
          "protocol_utxos",
          "reconstructed_states",
          "retries",
          "spent_protocol_utxos",
          "submissions",
        ]),
        key: exactString(
          entry.key,
          `${path}.key`,
          /^(?:[0-9a-f#]+|[a-z0-9_.-]+)$/u,
        ),
        index: exactString(entry.index, `${path}.index`, CANONICAL_NATURAL),
        recordSha256: exactString(
          entry.recordSha256,
          `${path}.recordSha256`,
          HEX_32,
        ),
      };
    },
    (entry) => `${entry.namespace}\u0000${entry.key}`,
  );
  return {
    schemaVersion: WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
    sourceSha256: exactString(
      caches.sourceSha256,
      "$.caches.sourceSha256",
      HEX_32,
    ),
    entries,
  };
};

const sortRecords = <T>(
  records: readonly T[],
  keyOf: (record: T) => string,
): readonly T[] =>
  [...records].sort((left, right) => keyOf(left).localeCompare(keyOf(right)));

const canonicalizeRecords = (
  records: WatcherDurableRecords,
): WatcherDurableRecords => ({
  l1Observations: sortRecords(
    records.l1Observations,
    (entry) => entry.observationId,
  ),
  chainPoints: sortRecords(records.chainPoints, (entry) => entry.chainPointId),
  protocolUtxos: sortRecords(records.protocolUtxos, (entry) => entry.outRef),
  spentProtocolUtxos: sortRecords(
    records.spentProtocolUtxos,
    (entry) => entry.outRef,
  ),
  daProofInputs: sortRecords(records.daProofInputs, (entry) => entry.inputId),
  reconstructedStates: sortRecords(
    records.reconstructedStates.map((entry) => ({
      ...entry,
      inputIds: [...entry.inputIds].sort(),
    })),
    (entry) => entry.blockHash,
  ),
  decisions: sortRecords(records.decisions, (entry) => entry.blockHash),
  faults: sortRecords(records.faults, (entry) => entry.faultId),
  submissions: sortRecords(records.submissions, (entry) => entry.submissionId),
  confirmations: sortRecords(
    records.confirmations,
    (entry) => entry.confirmationId,
  ),
  retries: sortRecords(records.retries, (entry) => entry.retryId),
  deadlines: sortRecords(records.deadlines, (entry) => entry.deadlineId),
  correctionResults: sortRecords(
    records.correctionResults,
    (entry) => entry.correctionId,
  ),
});

export const parseMarker = (value: unknown): DeploymentMarker => {
  try {
    return parseDeploymentMarker(value);
  } catch {
    return fail("invalid_field", "$.deploymentMarker");
  }
};

export const parseWatcherDurableStore = (
  value: unknown,
): WatcherDurableStore => {
  const record = exactRecord(value, "$", STORE_KEYS);
  if (record.schemaVersion !== WATCHER_DURABLE_STORE_SCHEMA_VERSION) {
    fail("unsupported_schema", "$.schemaVersion");
  }
  if (record.migrationVersion !== WATCHER_DURABLE_MIGRATION_VERSION) {
    fail("unsupported_schema", "$.migrationVersion");
  }
  if (
    record.migrationManifestSha256 !== WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256
  ) {
    fail("integrity_mismatch", "$.migrationManifestSha256");
  }
  const deploymentMarker = parseMarker(record.deploymentMarker);
  const records = parseRecords(record);
  assertReferences(records);
  const caches = parseCaches(record.caches);
  const expectedCaches = rebuildWatcherDurableCaches({
    deploymentMarker,
    ...records,
  });
  if (
    canonicalJson(caches as CanonicalJson) !==
    canonicalJson(expectedCaches as CanonicalJson)
  ) {
    fail("cache_mismatch", "$.caches");
  }
  return {
    schemaVersion: WATCHER_DURABLE_STORE_SCHEMA_VERSION,
    migrationVersion: WATCHER_DURABLE_MIGRATION_VERSION,
    migrationManifestSha256: WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
    revision: exactString(record.revision, "$.revision", CANONICAL_NATURAL),
    deploymentMarker,
    ...records,
    caches,
  };
};

type WatcherDurableRecordsInput = Omit<
  WatcherDurableRecords,
  "spentProtocolUtxos"
> &
  Partial<Pick<WatcherDurableRecords, "spentProtocolUtxos">>;

export const makeWatcherDurableStore = (input: {
  readonly deploymentMarker: DeploymentMarker;
  readonly revision: string;
  readonly records: WatcherDurableRecordsInput;
}): WatcherDurableStore => {
  const deploymentMarker = parseMarker(input.deploymentMarker);
  const records = canonicalizeRecords({
    ...input.records,
    spentProtocolUtxos: input.records.spentProtocolUtxos ?? [],
  });
  const candidate = {
    schemaVersion: WATCHER_DURABLE_STORE_SCHEMA_VERSION,
    migrationVersion: WATCHER_DURABLE_MIGRATION_VERSION,
    migrationManifestSha256: WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
    revision: input.revision,
    deploymentMarker,
    ...records,
    caches: rebuildWatcherDurableCaches({
      deploymentMarker,
      ...records,
    }),
  };
  return parseWatcherDurableStore(candidate);
};

export const journalWatcherProtocolUtxoTransition = (input: {
  readonly sourceStore: unknown;
  readonly nextChainPoints: readonly WatcherL1ChainPoint[];
  readonly nextProtocolUtxos: readonly WatcherProtocolUtxo[];
  readonly spentAtChainPointId: string;
}): Readonly<{
  protocolUtxos: readonly WatcherProtocolUtxo[];
  spentProtocolUtxos: readonly WatcherSpentProtocolUtxo[];
}> => {
  const source = parseWatcherDurableStore(input.sourceStore);
  const spentAtChainPointId = exactString(
    input.spentAtChainPointId,
    "$.spentAtChainPointId",
    HEX_32,
  );
  if (
    !input.nextChainPoints.some(
      ({ chainPointId }) => chainPointId === spentAtChainPointId,
    )
  ) {
    fail("broken_reference", "$.spentAtChainPointId");
  }
  const priorActive = new Map(
    source.protocolUtxos.map((entry) => [entry.outRef, entry]),
  );
  const priorSpent = new Set(
    source.spentProtocolUtxos.map(({ outRef }) => outRef),
  );
  const sourceChainPoints = new Map(
    source.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const spentAtPoint = input.nextChainPoints.find(
    ({ chainPointId }) => chainPointId === spentAtChainPointId,
  );
  const nextActive = new Map<string, WatcherProtocolUtxo>();
  for (const entry of input.nextProtocolUtxos) {
    if (nextActive.has(entry.outRef) || priorSpent.has(entry.outRef)) {
      fail("duplicate_key", `$.protocolUtxos.${entry.outRef}`);
    }
    const prior = priorActive.get(entry.outRef);
    if (
      prior !== undefined &&
      canonicalJson(prior as CanonicalJson) !==
        canonicalJson(entry as CanonicalJson)
    ) {
      fail("integrity_mismatch", `$.protocolUtxos.${entry.outRef}`);
    }
    nextActive.set(entry.outRef, entry);
  }
  const newlySpent = source.protocolUtxos
    .filter(({ outRef }) => !nextActive.has(outRef))
    .map((entry) => {
      const creationPoint = sourceChainPoints.get(entry.chainPointId);
      if (
        creationPoint === undefined ||
        spentAtPoint === undefined ||
        compareChainPointOrder(spentAtPoint, creationPoint) < 0
      ) {
        fail("broken_reference", `$.protocolUtxos.${entry.outRef}`);
      }
      return {
        ...entry,
        spentAtChainPointId,
      };
    });
  return Object.freeze({
    protocolUtxos: Object.freeze(
      sortRecords(input.nextProtocolUtxos, (entry) => entry.outRef),
    ),
    spentProtocolUtxos: Object.freeze(
      sortRecords(
        [...source.spentProtocolUtxos, ...newlySpent],
        (entry) => entry.outRef,
      ),
    ),
  });
};

const EMPTY_RECORDS: WatcherDurableRecords = Object.freeze({
  l1Observations: [],
  chainPoints: [],
  protocolUtxos: [],
  spentProtocolUtxos: [],
  daProofInputs: [],
  reconstructedStates: [],
  decisions: [],
  faults: [],
  submissions: [],
  confirmations: [],
  retries: [],
  deadlines: [],
  correctionResults: [],
});

export const makeEmptyWatcherDurableStore = (
  deploymentMarker: DeploymentMarker,
): WatcherDurableStore =>
  makeWatcherDurableStore({
    deploymentMarker,
    revision: "0",
    records: EMPTY_RECORDS,
  });

export const UTF8_ENCODER = new TextEncoder();

export const UTF8_DECODER = new TextDecoder("utf-8", { fatal: true });

export const immutableStoreEncodings = new WeakMap<
  object,
  Readonly<{ encoded: string; caches: WatcherDurableCaches }>
>();

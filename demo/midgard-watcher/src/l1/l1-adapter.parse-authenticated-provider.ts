import { type Socket } from "node:net";
import { type TLSSocket } from "node:tls";

import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import {
  assertPublicBytesMatch,
  deriveTransaction,
} from "./l1-adapter.derive-transaction.js";
import {
  type CanonicalJson,
  digestCanonicalJson,
  exactArray,
  exactLiteral,
  exactNatural,
  exactRecord,
  exactString,
  fail,
  type ParseBudget,
  plainRecord,
} from "./l1-adapter.exact-array.js";
import {
  compareNaturalStrings,
  compareRedeemers,
  freezeSortedUnique,
  parseDatum,
  parsePublicBytes,
  parseRedeemer,
  parseScript,
  parseUtxo,
} from "./l1-adapter.parse-utxo.js";
import {
  AUTHENTICATION_KINDS,
  HEX_32,
  NETWORKS,
  PROVIDER_ID,
  transportAttestationStates,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_L1_SOURCE_MODES,
  WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_SCHEMA_VERSION,
  WATCHER_LOCAL_NODE_SURFACES,
  type WatcherL1Datum,
  type WatcherL1NormalizationSessionState,
  type WatcherL1PublicBytes,
  type WatcherL1Redeemer,
  type WatcherL1Script,
  type WatcherL1SourceIdentity,
  type WatcherL1Transaction,
  type WatcherL1TransportAttestationContext,
  type WatcherL1TransportAttestationDetails,
  type WatcherL1Utxo,
  type WatcherNormalizedAuthenticatedL1Provider,
} from "./l1-adapter.watcher-local-node-query-transport.js";

const assertWitnessViewsMatch = (
  path: string,
  claimed: Readonly<{
    witnessSet: WatcherL1PublicBytes;
    scripts: readonly WatcherL1Script[];
    datums: readonly WatcherL1Datum[];
    redeemers: readonly WatcherL1Redeemer[];
  }>,
  actual: Readonly<{
    witnessSet: WatcherL1PublicBytes;
    scripts: readonly WatcherL1Script[];
    datums: readonly WatcherL1Datum[];
    redeemers: readonly WatcherL1Redeemer[];
  }>,
): void => {
  assertPublicBytesMatch(
    claimed.witnessSet,
    actual.witnessSet,
    `${path}.witnessSet`,
  );
  for (const field of ["scripts", "datums", "redeemers"] as const) {
    if (
      claimed[field].length !== actual[field].length ||
      claimed[field].some(
        (entry, index) =>
          !watcherSameCanonicalJson(entry, actual[field][index]!),
      )
    ) {
      fail("identity_mismatch", `${path}.${field}`);
    }
  }
};

const assertUtxoViewsMatch = (
  claimed: readonly WatcherL1Utxo[],
  actual: readonly WatcherL1Utxo[],
  path: string,
): void => {
  if (
    claimed.length !== actual.length ||
    claimed.some(
      (entry, index) => !watcherSameCanonicalJson(entry, actual[index]!),
    )
  ) {
    fail("identity_mismatch", path);
  }
};

export const parseTransaction = (
  value: unknown,
  path: string,
  budget: ParseBudget,
  session: WatcherL1NormalizationSessionState | undefined,
): WatcherL1Transaction => {
  const unparsed = plainRecord(value, path);
  const hasTransactionIndex = Object.prototype.hasOwnProperty.call(
    unparsed,
    "transactionIndex",
  );
  const record = exactRecord(value, path, [
    "txHash",
    ...(hasTransactionIndex ? ["transactionIndex"] : []),
    "fullTransaction",
    "body",
    "witnessSet",
    "utxos",
    "scripts",
    "datums",
    "redeemers",
  ]);
  const transactionIndex = hasTransactionIndex
    ? exactNatural(record.transactionIndex, `${path}.transactionIndex`)
    : undefined;
  const fullTransaction = parsePublicBytes(
    record.fullTransaction,
    `${path}.fullTransaction`,
    budget,
  );
  const body = parsePublicBytes(record.body, `${path}.body`, budget);
  const derived = deriveTransaction(fullTransaction, path, session);
  assertPublicBytesMatch(
    fullTransaction,
    derived.fullTransaction,
    `${path}.fullTransaction.bytesHex`,
  );
  assertPublicBytesMatch(body, derived.body, `${path}.body.bytesHex`);
  const txHash = exactString(record.txHash, `${path}.txHash`, HEX_32);
  if (derived.txHash !== txHash) {
    fail("identity_mismatch", `${path}.txHash`);
  }
  const utxos = freezeSortedUnique(
    exactArray(record.utxos, `${path}.utxos`).map((entry, index) =>
      parseUtxo(entry, `${path}.utxos[${index.toString()}]`, txHash, budget),
    ),
    `${path}.utxos`,
    (entry) => entry.outRef,
    (left, right) => compareNaturalStrings(left.outputIndex, right.outputIndex),
  );
  const scripts = freezeSortedUnique(
    exactArray(record.scripts, `${path}.scripts`).map((entry, index) =>
      parseScript(entry, `${path}.scripts[${index.toString()}]`, budget),
    ),
    `${path}.scripts`,
    (entry) => entry.scriptHash,
  );
  const datums = freezeSortedUnique(
    exactArray(record.datums, `${path}.datums`).map((entry, index) =>
      parseDatum(entry, `${path}.datums[${index.toString()}]`, budget),
    ),
    `${path}.datums`,
    (entry) => entry.datumHash,
  );
  const redeemers = freezeSortedUnique(
    exactArray(record.redeemers, `${path}.redeemers`).map((entry, index) =>
      parseRedeemer(entry, `${path}.redeemers[${index.toString()}]`, budget),
    ),
    `${path}.redeemers`,
    (entry) => `${entry.purpose}:${entry.index}`,
    compareRedeemers,
  );
  const witnessSet = parsePublicBytes(
    record.witnessSet,
    `${path}.witnessSet`,
    budget,
  );
  assertWitnessViewsMatch(
    path,
    { witnessSet, scripts, datums, redeemers },
    {
      witnessSet: derived.witnessSet,
      scripts: derived.scripts,
      datums: derived.datums,
      redeemers: derived.redeemers,
    },
  );
  assertUtxoViewsMatch(utxos, derived.utxos, `${path}.utxos`);
  return Object.freeze({
    txHash,
    ...(transactionIndex === undefined ? {} : { transactionIndex }),
    isValid: derived.isValid,
    fullTransaction,
    body,
    witnessSet,
    utxos: derived.utxos,
    scripts,
    datums,
    redeemers,
  });
};

export const parseAuthenticatedProvider = (
  value: unknown,
): WatcherNormalizedAuthenticatedL1Provider => {
  const unparsed = plainRecord(value, "$.authenticatedProvider");
  const record = exactRecord(unparsed, "$.authenticatedProvider", [
    "schemaVersion",
    "network",
    "providerId",
    "source",
    "authentication",
  ]);
  if (
    record.schemaVersion !== WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.authenticatedProvider.schemaVersion");
  }
  const authentication = exactRecord(
    record.authentication,
    "$.authenticatedProvider.authentication",
    ["kind", "publicIdentitySha256"],
  );
  const sourceRecord = plainRecord(
    record.source,
    "$.authenticatedProvider.source",
  );
  const sourceMode = exactLiteral(
    sourceRecord.sourceMode,
    "$.authenticatedProvider.source.sourceMode",
    WATCHER_L1_SOURCE_MODES,
  );
  const authenticationKind = exactLiteral(
    authentication.kind,
    "$.authenticatedProvider.authentication.kind",
    AUTHENTICATION_KINDS,
  );
  const publicIdentitySha256 = exactString(
    authentication.publicIdentitySha256,
    "$.authenticatedProvider.authentication.publicIdentitySha256",
    HEX_32,
  );
  const source: WatcherL1SourceIdentity =
    sourceMode === "local_node"
      ? (() => {
          const local = exactRecord(
            sourceRecord,
            "$.authenticatedProvider.source",
            ["sourceMode", "authorityNodeId", "surface"],
          );
          return Object.freeze({
            sourceMode,
            authorityNodeId: exactString(
              local.authorityNodeId,
              "$.authenticatedProvider.source.authorityNodeId",
              PROVIDER_ID,
            ),
            surface: exactLiteral(
              local.surface,
              "$.authenticatedProvider.source.surface",
              WATCHER_LOCAL_NODE_SURFACES,
            ),
          });
        })()
      : (() => {
          const external = exactRecord(
            sourceRecord,
            "$.authenticatedProvider.source",
            ["sourceMode", "operatorIdentitySha256"],
          );
          return Object.freeze({
            sourceMode,
            operatorIdentitySha256: exactString(
              external.operatorIdentitySha256,
              "$.authenticatedProvider.source.operatorIdentitySha256",
              HEX_32,
            ),
          });
        })();
  if (
    source.sourceMode === "local_node" &&
    source.surface === "chain_sync" &&
    authenticationKind !== "cardano_node_genesis_v1"
  ) {
    fail("identity_mismatch", "$.authenticatedProvider.authentication.kind");
  }
  return Object.freeze({
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
    network: exactLiteral(
      record.network,
      "$.authenticatedProvider.network",
      NETWORKS,
    ),
    providerId: exactString(
      record.providerId,
      "$.authenticatedProvider.providerId",
      PROVIDER_ID,
    ),
    source,
    authentication: Object.freeze({
      kind: authenticationKind,
      publicIdentitySha256,
    }),
  });
};

export const makeTransportAttestationContext = (
  details: WatcherL1TransportAttestationDetails,
  transports: readonly (Socket | TLSSocket)[],
  ownedTransports: readonly (Socket | TLSSocket)[] = transports,
  upstreamIsLive: () => boolean = () => true,
): WatcherL1TransportAttestationContext => {
  const attestationDigest = digestCanonicalJson({
    provider: providerJson(details.provider),
    authorityBindingSha256: details.authorityBindingSha256,
    transportEndpoint: details.transportEndpoint,
  });
  const context = Object.freeze({
    schemaVersion: WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_SCHEMA_VERSION,
    attestationDigest,
  });
  transportAttestationStates.set(context, {
    details: Object.freeze(details),
    transports: Object.freeze([...transports]),
    ownedTransports: Object.freeze([...ownedTransports]),
    upstreamIsLive,
    active: true,
  });
  return context;
};

export const providerJson = (
  provider: WatcherNormalizedAuthenticatedL1Provider,
): CanonicalJson => ({
  schemaVersion: provider.schemaVersion,
  network: provider.network,
  providerId: provider.providerId,
  source: provider.source,
  authentication: provider.authentication,
});

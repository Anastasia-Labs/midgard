import { createHash } from "node:crypto";
import { createConnection as createNetConnection, type Socket } from "node:net";
import {
  connect as connectTls,
  type ConnectionOptions,
  type TLSSocket,
} from "node:tls";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import {
  watcherNativeChainSyncAuthorityDetailsV1,
  type WatcherNativeChainSyncAuthorityV1,
} from "./native-chain-sync-v1.js";

export const WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION =
  "midgard-watcher-authenticated-l1-provider-v1" as const;
export const WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-l1-block-observation-v1" as const;
export const WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION =
  "midgard-watcher-normalized-l1-block-v1" as const;
export const WATCHER_L1_NORMALIZATION_SESSION_V1_SCHEMA_VERSION =
  "midgard-watcher-l1-normalization-session-v1" as const;
export const WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_V1_SCHEMA_VERSION =
  "midgard-watcher-l1-transport-attestation-context-v1" as const;

export const WATCHER_L1_ADAPTER_V1_BOUNDS = Object.freeze({
  arrayMembers: 4_096,
  totalCollectionMembers: 65_536,
  publicBytes: 1_048_576,
  totalPublicBytes: 67_108_864,
  normalizationSessionEntries: 256,
  normalizationSessionBytes: 16_777_216,
});

export const WATCHER_L1_SCRIPT_LANGUAGES_V1 = [
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
] as const;

export const WATCHER_L1_REDEEMER_PURPOSES_V1 = [
  "spend",
  "mint",
  "certificate",
  "withdrawal",
  "vote",
  "propose",
] as const;

const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
export const WATCHER_L1_SOURCE_MODES_V1 = [
  "local_node",
  "external_providers",
] as const;
export const WATCHER_LOCAL_NODE_SURFACES_V1 = [
  "chain_sync",
  "ogmios",
  "kupo",
  "kupmios",
  "db_sync",
] as const;
const AUTHENTICATION_KINDS = [
  "https_tls_identity_v1",
  "cardano_node_genesis_v1",
  "local_endpoint_identity_v1",
] as const;
const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const PROVIDER_ID = /^[a-z][a-z0-9-]{0,62}$/u;
const UINT64_MAX = 18_446_744_073_709_551_615n;

export type WatcherL1NetworkV1 = (typeof NETWORKS)[number];
export type WatcherL1SourceModeV1 = (typeof WATCHER_L1_SOURCE_MODES_V1)[number];
export type WatcherLocalNodeSurfaceV1 =
  (typeof WATCHER_LOCAL_NODE_SURFACES_V1)[number];

export type WatcherL1SourceIdentityV1 =
  | Readonly<{
      sourceMode: "local_node";
      authorityNodeId: string;
      surface: WatcherLocalNodeSurfaceV1;
    }>
  | Readonly<{
      sourceMode: "external_providers";
      operatorIdentitySha256: string;
    }>;

/**
 * Public identity metadata established by the transport boundary. This value
 * must come from the configured TLS trust identity or a future native local
 * adapter, never from the provider response being normalized.
 */
type WatcherAuthenticatedL1ProviderBaseV1 = Readonly<{
  schemaVersion: typeof WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION;
  network: WatcherL1NetworkV1;
  providerId: string;
  authentication: Readonly<{
    kind: (typeof AUTHENTICATION_KINDS)[number];
    publicIdentitySha256: string;
  }>;
}>;

export type WatcherAuthenticatedL1ProviderV1 = Readonly<
  WatcherAuthenticatedL1ProviderBaseV1 & {
    source: WatcherL1SourceIdentityV1;
  }
>;

/**
 * Opaque authority created only after the watcher transport boundary has
 * verified an external provider TLS peer. A future local-node adapter may
 * create a local authority only after it binds peer identity to the connected
 * socket; pathname checks are intentionally not a supported authority.
 *
 * The public fields are diagnostic only. Authenticity is established by
 * module-private WeakMap membership, so a parsed or deserialized object cannot
 * be used to normalize protocol evidence.
 */
export type WatcherL1TransportAttestationContextV1 = Readonly<{
  schemaVersion: typeof WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_V1_SCHEMA_VERSION;
  attestationDigest: string;
}>;

export type WatcherLocalNodeQueryTransportV1 =
  | Readonly<{
      transportKind: "tcp";
      providerId: string;
      surface: Exclude<WatcherLocalNodeSurfaceV1, "chain_sync">;
      /** Exact W01 HTTP, WS, or PostgreSQL endpoint. */
      endpoint: string;
      connectTimeoutMs: number;
    }>
  | Readonly<{
      transportKind: "https_tls";
      providerId: string;
      surface: Exclude<WatcherLocalNodeSurfaceV1, "chain_sync">;
      /** Exact W01 HTTPS or WSS endpoint; host, port and SNI are derived. */
      endpoint: string;
      caPem: string;
      expectedTlsPublicIdentitySha256: string;
      connectTimeoutMs: number;
    }>;

export type WatcherExternalProviderTransportV1 = Readonly<{
  network: WatcherL1NetworkV1;
  providerId: string;
  operatorIdentitySha256: string;
  /** Exact W01 HTTPS endpoint; host, port and SNI are derived from it. */
  endpoint: string;
  caPem: string;
  expectedTlsPublicIdentitySha256: string;
  connectTimeoutMs: number;
}>;

export type WatcherL1TransportAttestationDetailsV1 = Readonly<{
  provider: WatcherNormalizedAuthenticatedL1ProviderV1;
  authorityBindingSha256: string | null;
  /**
   * Exact configured W01 transport location used to establish the live
   * capability. W11 compares this byte-for-byte with its configured source.
   */
  transportEndpoint: string;
}>;

export type WatcherNormalizedAuthenticatedL1ProviderV1 = Readonly<
  WatcherAuthenticatedL1ProviderBaseV1 & {
    source: WatcherL1SourceIdentityV1;
  }
>;

export type WatcherL1PublicBytesV1 = Readonly<{
  bytesHex: string;
  sha256: string;
}>;

export type WatcherL1ScriptV1 = Readonly<{
  scriptHash: string;
  language: (typeof WATCHER_L1_SCRIPT_LANGUAGES_V1)[number];
  bytes: WatcherL1PublicBytesV1;
}>;

export type WatcherL1DatumV1 = Readonly<{
  datumHash: string;
  bytes: WatcherL1PublicBytesV1;
}>;

export type WatcherL1RedeemerV1 = Readonly<{
  purpose: (typeof WATCHER_L1_REDEEMER_PURPOSES_V1)[number];
  index: string;
  bytes: WatcherL1PublicBytesV1;
}>;

export type WatcherL1UtxoV1 = Readonly<{
  outRef: string;
  outputIndex: string;
  output: WatcherL1PublicBytesV1;
  datum: WatcherL1DatumV1 | null;
  referenceScript: WatcherL1ScriptV1 | null;
}>;

export type WatcherL1TransactionV1 = Readonly<{
  txHash: string;
  transactionIndex?: string;
  isValid: boolean;
  fullTransaction: WatcherL1PublicBytesV1;
  body: WatcherL1PublicBytesV1;
  witnessSet: WatcherL1PublicBytesV1;
  utxos: readonly WatcherL1UtxoV1[];
  scripts: readonly WatcherL1ScriptV1[];
  datums: readonly WatcherL1DatumV1[];
  redeemers: readonly WatcherL1RedeemerV1[];
}>;

export type WatcherL1ChainPointV1 = Readonly<{
  chainPointId: string;
  pointDigest: string;
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  depth: string;
}>;

export type WatcherNormalizedL1BlockV1 = Readonly<{
  schemaVersion: typeof WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION;
  network: WatcherL1NetworkV1;
  provider: WatcherNormalizedAuthenticatedL1ProviderV1;
  chainPoint: WatcherL1ChainPointV1;
  transactions: readonly WatcherL1TransactionV1[];
  blockContentDigest: string;
  observationDigest: string;
}>;

export type WatcherL1NormalizationSessionV1 = Readonly<{
  schemaVersion: typeof WATCHER_L1_NORMALIZATION_SESSION_V1_SCHEMA_VERSION;
}>;

type WatcherDerivedL1TransactionV1 = Readonly<{
  fullTransaction: WatcherL1PublicBytesV1;
  body: WatcherL1PublicBytesV1;
  txHash: string;
  isValid: boolean;
  witnessSet: WatcherL1PublicBytesV1;
  utxos: readonly WatcherL1UtxoV1[];
  scripts: readonly WatcherL1ScriptV1[];
  datums: readonly WatcherL1DatumV1[];
  redeemers: readonly WatcherL1RedeemerV1[];
}>;

type WatcherL1NormalizationSessionStateV1 = {
  readonly transactions: Map<
    string,
    Readonly<{
      bytesHex: string;
      retainedBytes: number;
      derived: WatcherDerivedL1TransactionV1;
    }>
  >;
  retainedBytes: number;
};

export type WatcherL1NormalizationSessionStatsV1 = Readonly<{
  retainedEntries: number;
  retainedBytes: number;
  maximumEntries: number;
  maximumBytes: number;
}>;

const normalizationSessionStates = new WeakMap<
  WatcherL1NormalizationSessionV1,
  WatcherL1NormalizationSessionStateV1
>();
type WatcherL1TransportAttestationStateV1 = {
  details: WatcherL1TransportAttestationDetailsV1;
  transports: readonly (Socket | TLSSocket)[];
  ownedTransports: readonly (Socket | TLSSocket)[];
  upstreamIsLive: () => boolean;
  active: boolean;
};

const transportAttestationStates = new WeakMap<
  WatcherL1TransportAttestationContextV1,
  WatcherL1TransportAttestationStateV1
>();
const normalizedBlockProvenance = new WeakMap<
  object,
  WatcherL1TransportAttestationContextV1
>();

export const makeWatcherL1NormalizationSessionV1 =
  (): WatcherL1NormalizationSessionV1 => {
    const session = Object.freeze({
      schemaVersion: WATCHER_L1_NORMALIZATION_SESSION_V1_SCHEMA_VERSION,
    });
    normalizationSessionStates.set(session, {
      transactions: new Map(),
      retainedBytes: 0,
    });
    return session;
  };

export const watcherL1NormalizationSessionStatsV1 = (
  session: WatcherL1NormalizationSessionV1,
): WatcherL1NormalizationSessionStatsV1 => {
  const state = normalizationSessionStates.get(session);
  if (state === undefined) {
    throw new Error("unknown watcher L1 normalization session");
  }
  return Object.freeze({
    retainedEntries: state.transactions.size,
    retainedBytes: state.retainedBytes,
    maximumEntries: WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionEntries,
    maximumBytes: WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionBytes,
  });
};

export const isWatcherL1AdapterNormalizedBlockV1 = (
  value: unknown,
): value is WatcherNormalizedL1BlockV1 => {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const context = normalizedBlockProvenance.get(value);
  return (
    context !== undefined &&
    watcherL1TransportAttestationDetailsV1(context) !== null
  );
};

export const watcherL1TransportAttestationDetailsV1 = (
  context: unknown,
): WatcherL1TransportAttestationDetailsV1 | null => {
  if (typeof context !== "object" || context === null) {
    return null;
  }
  const state = transportAttestationStates.get(
    context as WatcherL1TransportAttestationContextV1,
  );
  return state !== undefined &&
    state.active &&
    state.upstreamIsLive() &&
    state.transports.every(
      (transport) =>
        !transport.destroyed &&
        transport.readable &&
        transport.writable &&
        transport.readyState === "open",
    )
    ? state.details
    : null;
};

export const isWatcherL1BlockAttestedByV1 = (
  block: unknown,
  context: unknown,
): boolean =>
  typeof block === "object" &&
  block !== null &&
  typeof context === "object" &&
  context !== null &&
  normalizedBlockProvenance.get(block) === context &&
  watcherL1TransportAttestationDetailsV1(context) !== null;

export const closeWatcherL1TransportAttestationContextV1 = (
  context: WatcherL1TransportAttestationContextV1,
): void => {
  const state =
    transportAttestationStates.get(context) ??
    fail("invalid_field", "$.transportAttestationContext");
  state.active = false;
  for (const transport of state.ownedTransports) {
    transport.destroy();
  }
};

export type WatcherL1AdapterErrorCode =
  | "content_digest_mismatch"
  | "duplicate_identity"
  | "identity_mismatch"
  | "invalid_field"
  | "missing_field"
  | "network_mismatch"
  | "out_of_bounds"
  | "provider_mismatch"
  | "unknown_field"
  | "unsafe_value"
  | "unsupported_schema";

export type WatcherL1AdapterDiagnostic = Readonly<{
  code: WatcherL1AdapterErrorCode;
  path: string;
  message: string;
}>;

export class WatcherL1AdapterError extends Error {
  readonly code: WatcherL1AdapterErrorCode;
  readonly path: string;

  constructor(code: WatcherL1AdapterErrorCode, path: string) {
    super(`Watcher L1 observation rejected: ${code} at ${path}`);
    this.name = "WatcherL1AdapterError";
    this.code = code;
    this.path = path;
  }
}

const fail = (code: WatcherL1AdapterErrorCode, path: string): never => {
  throw new WatcherL1AdapterError(code, path);
};

export const watcherL1AdapterDiagnostic = (
  error: unknown,
): WatcherL1AdapterDiagnostic => {
  if (error instanceof WatcherL1AdapterError) {
    return {
      code: error.code,
      path: error.path,
      message: error.message,
    };
  }
  return {
    code: "invalid_field",
    path: "$",
    message: "Watcher L1 observation rejected: invalid_field at $",
  };
};

type JsonRecord = Record<string, unknown>;
type CanonicalJson =
  | null
  | boolean
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

type ParseBudget = {
  collectionMembers: number;
  publicBytes: number;
};

const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const candidate = value as object;
  const prototype = Object.getPrototypeOf(candidate);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("unsafe_value", path);
  }
  if (Reflect.ownKeys(candidate).length !== Object.keys(candidate).length) {
    fail("unsafe_value", path);
  }
  for (const key of Object.keys(candidate)) {
    const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      fail("unsafe_value", path);
    }
  }
  return value as JsonRecord;
};

const exactRecord = (
  value: unknown,
  path: string,
  keys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const expected = new Set(keys);
  for (const key of Object.keys(record)) {
    if (!expected.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of keys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

const exactString = (value: unknown, path: string, pattern: RegExp): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
};

const exactLiteral = <T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[],
): T => {
  if (typeof value !== "string" || !allowed.includes(value as T)) {
    fail("invalid_field", path);
  }
  return value as T;
};

const exactNatural = (value: unknown, path: string): string => {
  const natural = exactString(value, path, CANONICAL_NATURAL);
  if (natural.length > 20 || BigInt(natural) > UINT64_MAX) {
    fail("out_of_bounds", path);
  }
  return natural;
};

const exactArray = (value: unknown, path: string): readonly unknown[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const values = value as readonly unknown[];
  if (
    Object.getPrototypeOf(values) !== Array.prototype ||
    Reflect.ownKeys(values).some(
      (key) =>
        typeof key !== "string" ||
        (key !== "length" &&
          (!CANONICAL_NATURAL.test(key) ||
            BigInt(key) >= BigInt(values.length))),
    ) ||
    Object.keys(values).length !== values.length
  ) {
    fail("unsafe_value", path);
  }
  for (let index = 0; index < values.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(
      values,
      index.toString(),
    );
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      fail("unsafe_value", path);
    }
  }
  if (values.length > WATCHER_L1_ADAPTER_V1_BOUNDS.arrayMembers) {
    fail("out_of_bounds", path);
  }
  return values;
};

const reserveCollectionMembers = (
  budget: ParseBudget,
  count: number,
  path: string,
): void => {
  budget.collectionMembers += count;
  if (
    budget.collectionMembers >
    WATCHER_L1_ADAPTER_V1_BOUNDS.totalCollectionMembers
  ) {
    fail("out_of_bounds", path);
  }
};

const preflightTransactionCollections = (
  value: unknown,
  budget: ParseBudget,
): readonly unknown[] => {
  const transactions = exactArray(value, "$.transactions");
  reserveCollectionMembers(budget, transactions.length, "$.transactions");
  for (let index = 0; index < transactions.length; index += 1) {
    const path = `$.transactions[${index.toString()}]`;
    const unparsed = plainRecord(transactions[index], path);
    const transaction = exactRecord(transactions[index], path, [
      "txHash",
      ...(Object.prototype.hasOwnProperty.call(unparsed, "transactionIndex")
        ? ["transactionIndex"]
        : []),
      "fullTransaction",
      "body",
      "witnessSet",
      "utxos",
      "scripts",
      "datums",
      "redeemers",
    ]);
    for (const field of ["utxos", "scripts", "datums", "redeemers"] as const) {
      const members = exactArray(transaction[field], `${path}.${field}`);
      reserveCollectionMembers(budget, members.length, "$.transactions");
    }
  }
  return transactions;
};

const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const sha256Utf8 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

const canonicalJson = (value: CanonicalJson): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map((member) => canonicalJson(member)).join(",")}]`;
  }
  const record = value as { readonly [key: string]: CanonicalJson };
  return `{${Object.keys(record)
    .sort()
    .map(
      (key) =>
        `${JSON.stringify(key)}:${canonicalJson(record[key] as CanonicalJson)}`,
    )
    .join(",")}}`;
};

const digestCanonicalJson = (value: CanonicalJson): string =>
  sha256Utf8(canonicalJson(value));

const freezePublicBytes = (
  bytesHex: string,
  sha256: string,
): WatcherL1PublicBytesV1 =>
  Object.freeze({
    bytesHex,
    sha256,
  });

const parsePublicBytes = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1PublicBytesV1 => {
  const record = exactRecord(value, path, ["bytesHex", "sha256"]);
  const bytesHex = exactString(
    record.bytesHex,
    `${path}.bytesHex`,
    LOWER_HEX_BYTES,
  );
  const byteLength = bytesHex.length / 2;
  if (byteLength > WATCHER_L1_ADAPTER_V1_BOUNDS.publicBytes) {
    fail("out_of_bounds", `${path}.bytesHex`);
  }
  budget.publicBytes += byteLength;
  if (budget.publicBytes > WATCHER_L1_ADAPTER_V1_BOUNDS.totalPublicBytes) {
    fail("out_of_bounds", "$.transactions");
  }
  const sha256 = exactString(record.sha256, `${path}.sha256`, HEX_32);
  if (sha256Bytes(Buffer.from(bytesHex, "hex")) !== sha256) {
    fail("content_digest_mismatch", `${path}.sha256`);
  }
  return freezePublicBytes(bytesHex, sha256);
};

export const makeWatcherL1PublicBytesV1 = (
  bytesHex: string,
): WatcherL1PublicBytesV1 => {
  if (
    !LOWER_HEX_BYTES.test(bytesHex) ||
    bytesHex.length / 2 > WATCHER_L1_ADAPTER_V1_BOUNDS.publicBytes
  ) {
    fail("invalid_field", "$.bytesHex");
  }
  return freezePublicBytes(bytesHex, sha256Bytes(Buffer.from(bytesHex, "hex")));
};

const parseScript = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1ScriptV1 => {
  const record = exactRecord(value, path, ["scriptHash", "language", "bytes"]);
  return Object.freeze({
    scriptHash: exactString(record.scriptHash, `${path}.scriptHash`, HEX_28),
    language: exactLiteral(
      record.language,
      `${path}.language`,
      WATCHER_L1_SCRIPT_LANGUAGES_V1,
    ),
    bytes: parsePublicBytes(record.bytes, `${path}.bytes`, budget),
  });
};

const parseDatum = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1DatumV1 => {
  const record = exactRecord(value, path, ["datumHash", "bytes"]);
  const bytes = parsePublicBytes(record.bytes, `${path}.bytes`, budget);
  const datumHash = exactString(record.datumHash, `${path}.datumHash`, HEX_32);
  if (
    computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString("hex") !==
    datumHash
  ) {
    fail("identity_mismatch", `${path}.datumHash`);
  }
  return Object.freeze({ datumHash, bytes });
};

const parseOptionalDatum = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1DatumV1 | null =>
  value === null ? null : parseDatum(value, path, budget);

const parseOptionalScript = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1ScriptV1 | null =>
  value === null ? null : parseScript(value, path, budget);

const parseRedeemer = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1RedeemerV1 => {
  const record = exactRecord(value, path, ["purpose", "index", "bytes"]);
  return Object.freeze({
    purpose: exactLiteral(
      record.purpose,
      `${path}.purpose`,
      WATCHER_L1_REDEEMER_PURPOSES_V1,
    ),
    index: exactNatural(record.index, `${path}.index`),
    bytes: parsePublicBytes(record.bytes, `${path}.bytes`, budget),
  });
};

const compareNaturalStrings = (left: string, right: string): number =>
  left.length !== right.length
    ? left.length - right.length
    : left < right
      ? -1
      : left > right
        ? 1
        : 0;

const parseUtxo = (
  value: unknown,
  path: string,
  txHash: string,
  budget: ParseBudget,
): WatcherL1UtxoV1 => {
  const record = exactRecord(value, path, [
    "outRef",
    "outputIndex",
    "output",
    "datum",
    "referenceScript",
  ]);
  const outputIndex = exactNatural(record.outputIndex, `${path}.outputIndex`);
  const outRef = exactString(
    record.outRef,
    `${path}.outRef`,
    /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u,
  );
  if (outRef !== `${txHash}#${outputIndex}`) {
    fail("identity_mismatch", `${path}.outRef`);
  }
  return Object.freeze({
    outRef,
    outputIndex,
    output: parsePublicBytes(record.output, `${path}.output`, budget),
    datum: parseOptionalDatum(record.datum, `${path}.datum`, budget),
    referenceScript: parseOptionalScript(
      record.referenceScript,
      `${path}.referenceScript`,
      budget,
    ),
  });
};

const freezeSortedUnique = <T>(
  values: readonly T[],
  path: string,
  identity: (value: T) => string,
  compare: (left: T, right: T) => number = (left, right) =>
    identity(left) < identity(right)
      ? -1
      : identity(left) > identity(right)
        ? 1
        : 0,
): readonly T[] => {
  const sorted = [...values].sort(compare);
  for (let index = 1; index < sorted.length; index += 1) {
    if (identity(sorted[index - 1] as T) === identity(sorted[index] as T)) {
      fail("duplicate_identity", `${path}[${index.toString()}]`);
    }
  }
  return Object.freeze(sorted);
};

const purposeOrder = new Map(
  WATCHER_L1_REDEEMER_PURPOSES_V1.map((purpose, index) => [purpose, index]),
);

const compareRedeemers = (
  left: WatcherL1RedeemerV1,
  right: WatcherL1RedeemerV1,
): number => {
  const purposeComparison =
    (purposeOrder.get(left.purpose) as number) -
    (purposeOrder.get(right.purpose) as number);
  return purposeComparison === 0
    ? compareNaturalStrings(left.index, right.index)
    : purposeComparison;
};

const publicBytesFromCanonicalCbor = (
  bytesHex: string,
): WatcherL1PublicBytesV1 =>
  freezePublicBytes(bytesHex, sha256Bytes(Buffer.from(bytesHex, "hex")));

type CmlWitnessScript = Readonly<{
  hash(): Readonly<{ to_hex(): string }>;
  to_canonical_cbor_hex(): string;
}>;

type CmlWitnessScriptList = Readonly<{
  get(index: number): CmlWitnessScript;
  len(): number;
}>;

const collectWitnessScripts = (
  list: CmlWitnessScriptList | undefined,
  language: WatcherL1ScriptV1["language"],
): WatcherL1ScriptV1[] => {
  const scripts: WatcherL1ScriptV1[] = [];
  if (list === undefined) {
    return scripts;
  }
  for (let index = 0; index < list.len(); index += 1) {
    const script = list.get(index);
    scripts.push(
      Object.freeze({
        scriptHash: script.hash().to_hex(),
        language,
        bytes: publicBytesFromCanonicalCbor(script.to_canonical_cbor_hex()),
      }),
    );
  }
  return scripts;
};

const redeemerPurpose = (
  tag: CML.RedeemerTag,
  path: string,
): WatcherL1RedeemerV1["purpose"] => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "certificate";
    case CML.RedeemerTag.Reward:
      return "withdrawal";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      return fail("invalid_field", path);
  }
};

const witnessRedeemer = (
  tag: CML.RedeemerTag,
  index: bigint,
  data: CML.PlutusData,
  path: string,
): WatcherL1RedeemerV1 =>
  Object.freeze({
    purpose: redeemerPurpose(tag, path),
    index: index.toString(),
    bytes: publicBytesFromCanonicalCbor(data.to_canonical_cbor_hex()),
  });

const collectWitnessRedeemers = (
  witnessSet: CML.TransactionWitnessSet,
  path: string,
): readonly WatcherL1RedeemerV1[] => {
  const redeemers = witnessSet.redeemers();
  const collected: WatcherL1RedeemerV1[] = [];
  if (redeemers === undefined) {
    return Object.freeze(collected);
  }
  const legacy = redeemers.as_arr_legacy_redeemer();
  if (legacy !== undefined) {
    for (let index = 0; index < legacy.len(); index += 1) {
      const redeemer = legacy.get(index);
      collected.push(
        witnessRedeemer(
          redeemer.tag(),
          redeemer.index(),
          redeemer.data(),
          `${path}.redeemers[${index.toString()}].purpose`,
        ),
      );
    }
  } else {
    const mapped =
      redeemers.as_map_redeemer_key_to_redeemer_val() ??
      fail("invalid_field", `${path}.witnessSet.bytesHex`);
    const keys = mapped.keys();
    for (let index = 0; index < keys.len(); index += 1) {
      const key = keys.get(index);
      const value =
        mapped.get(key) ?? fail("invalid_field", `${path}.witnessSet.bytesHex`);
      collected.push(
        witnessRedeemer(
          key.tag(),
          key.index(),
          value.data(),
          `${path}.redeemers[${index.toString()}].purpose`,
        ),
      );
    }
  }
  return freezeSortedUnique(
    collected,
    `${path}.redeemers`,
    (redeemer) => `${redeemer.purpose}:${redeemer.index}`,
    compareRedeemers,
  );
};

const collectWitnessDatums = (
  witnessSet: CML.TransactionWitnessSet,
  path: string,
): readonly WatcherL1DatumV1[] => {
  const datums = witnessSet.plutus_datums();
  const collected: WatcherL1DatumV1[] = [];
  if (datums !== undefined) {
    for (let index = 0; index < datums.len(); index += 1) {
      const bytes = publicBytesFromCanonicalCbor(
        datums.get(index).to_canonical_cbor_hex(),
      );
      collected.push(
        Object.freeze({
          datumHash: computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString(
            "hex",
          ),
          bytes,
        }),
      );
    }
  }
  return freezeSortedUnique(
    collected,
    `${path}.datums`,
    (datum) => datum.datumHash,
  );
};

const collectWitnessViews = (
  transaction: CML.Transaction,
  path: string,
): Readonly<{
  witnessSet: WatcherL1PublicBytesV1;
  scripts: readonly WatcherL1ScriptV1[];
  datums: readonly WatcherL1DatumV1[];
  redeemers: readonly WatcherL1RedeemerV1[];
}> => {
  const witnessSet = transaction.witness_set();
  const scripts = freezeSortedUnique(
    [
      ...collectWitnessScripts(witnessSet.native_scripts(), "Native"),
      ...collectWitnessScripts(witnessSet.plutus_v1_scripts(), "PlutusV1"),
      ...collectWitnessScripts(witnessSet.plutus_v2_scripts(), "PlutusV2"),
      ...collectWitnessScripts(witnessSet.plutus_v3_scripts(), "PlutusV3"),
    ],
    `${path}.scripts`,
    (script) => script.scriptHash,
  );
  const datums = collectWitnessDatums(witnessSet, path);
  const redeemers = collectWitnessRedeemers(witnessSet, path);
  if (
    (datums.length > 0 || redeemers.length > 0) &&
    transaction.body().script_data_hash() === undefined
  ) {
    fail("identity_mismatch", `${path}.body.bytesHex`);
  }
  return Object.freeze({
    witnessSet: publicBytesFromCanonicalCbor(
      witnessSet.to_canonical_cbor_hex(),
    ),
    scripts,
    datums,
    redeemers,
  });
};

const referenceScriptView = (
  script: CML.Script,
  path: string,
): WatcherL1ScriptV1 => {
  const native = script.as_native();
  const plutusV1 = script.as_plutus_v1();
  const plutusV2 = script.as_plutus_v2();
  const plutusV3 = script.as_plutus_v3();
  const selected:
    | Readonly<{
        language: WatcherL1ScriptV1["language"];
        script: CmlWitnessScript;
      }>
    | undefined =
    native === undefined
      ? plutusV1 === undefined
        ? plutusV2 === undefined
          ? plutusV3 === undefined
            ? undefined
            : { language: "PlutusV3", script: plutusV3 }
          : { language: "PlutusV2", script: plutusV2 }
        : { language: "PlutusV1", script: plutusV1 }
      : { language: "Native", script: native };
  if (selected === undefined) {
    return fail("invalid_field", path);
  }
  return Object.freeze({
    scriptHash: script.hash().to_hex(),
    language: selected.language,
    bytes: publicBytesFromCanonicalCbor(
      selected.script.to_canonical_cbor_hex(),
    ),
  });
};

const inlineDatumView = (
  output: CML.TransactionOutput,
): WatcherL1DatumV1 | null => {
  const datum = output.datum()?.as_datum();
  if (datum === undefined) {
    return null;
  }
  const bytes = publicBytesFromCanonicalCbor(datum.to_canonical_cbor_hex());
  return Object.freeze({
    datumHash: computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString(
      "hex",
    ),
    bytes,
  });
};

const collectAppliedUtxos = (
  transaction: CML.Transaction,
  txHash: string,
  path: string,
): readonly WatcherL1UtxoV1[] => {
  const body = transaction.body();
  const outputs = body.outputs();
  const collateralReturn = body.collateral_return();
  const appliedOutputCount = transaction.is_valid()
    ? outputs.len()
    : collateralReturn === undefined
      ? 0
      : 1;
  if (appliedOutputCount > WATCHER_L1_ADAPTER_V1_BOUNDS.arrayMembers) {
    fail("out_of_bounds", `${path}.utxos`);
  }
  const utxos: WatcherL1UtxoV1[] = [];
  for (let index = 0; index < appliedOutputCount; index += 1) {
    const output = transaction.is_valid()
      ? outputs.get(index)
      : (collateralReturn as CML.TransactionOutput);
    const ledgerOutputIndex = transaction.is_valid() ? index : outputs.len();
    const outputIndex = ledgerOutputIndex.toString();
    const script = output.script_ref();
    utxos.push(
      Object.freeze({
        outRef: `${txHash}#${outputIndex}`,
        outputIndex,
        output: publicBytesFromCanonicalCbor(output.to_canonical_cbor_hex()),
        datum: inlineDatumView(output),
        referenceScript:
          script === undefined
            ? null
            : referenceScriptView(
                script,
                `${path}.utxos[${outputIndex}].referenceScript`,
              ),
      }),
    );
  }
  return Object.freeze(utxos);
};

const decodeCanonicalTransaction = (
  fullTransaction: WatcherL1PublicBytesV1,
  path: string,
): CML.Transaction => {
  try {
    const transaction = CML.Transaction.from_cbor_hex(fullTransaction.bytesHex);
    if (transaction.to_canonical_cbor_hex() !== fullTransaction.bytesHex) {
      fail("identity_mismatch", `${path}.fullTransaction.bytesHex`);
    }
    return transaction;
  } catch (error) {
    if (error instanceof WatcherL1AdapterError) {
      throw error;
    }
    return fail("invalid_field", `${path}.fullTransaction.bytesHex`);
  }
};

const deriveCanonicalTransaction = (
  fullTransaction: WatcherL1PublicBytesV1,
  path: string,
  session: WatcherL1NormalizationSessionStateV1 | undefined,
): WatcherDerivedL1TransactionV1 => {
  const cached = session?.transactions.get(fullTransaction.sha256);
  if (cached !== undefined && cached.bytesHex === fullTransaction.bytesHex) {
    return cached.derived;
  }
  const transaction = decodeCanonicalTransaction(fullTransaction, path);
  const canonicalFullTransaction = publicBytesFromCanonicalCbor(
    transaction.to_canonical_cbor_hex(),
  );
  const body = publicBytesFromCanonicalCbor(
    transaction.body().to_canonical_cbor_hex(),
  );
  const txHash = computeHash32(Buffer.from(body.bytesHex, "hex")).toString(
    "hex",
  );
  const witnessViews = collectWitnessViews(transaction, path);
  const derived = Object.freeze({
    fullTransaction: canonicalFullTransaction,
    body,
    txHash,
    isValid: transaction.is_valid(),
    witnessSet: witnessViews.witnessSet,
    utxos: collectAppliedUtxos(transaction, txHash, path),
    scripts: witnessViews.scripts,
    datums: witnessViews.datums,
    redeemers: witnessViews.redeemers,
  });
  if (
    session !== undefined &&
    cached === undefined &&
    session.transactions.size <
      WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionEntries
  ) {
    const retainedBytes = Buffer.byteLength(
      canonicalJson(derived as unknown as CanonicalJson),
      "utf8",
    );
    if (
      retainedBytes <=
      WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionBytes -
        session.retainedBytes
    ) {
      session.transactions.set(
        fullTransaction.sha256,
        Object.freeze({
          bytesHex: fullTransaction.bytesHex,
          retainedBytes,
          derived,
        }),
      );
      session.retainedBytes += retainedBytes;
    }
  }
  return derived;
};

const assertPublicBytesMatch = (
  claimed: WatcherL1PublicBytesV1,
  actual: WatcherL1PublicBytesV1,
  path: string,
): void => {
  if (
    claimed.bytesHex !== actual.bytesHex ||
    claimed.sha256 !== actual.sha256
  ) {
    fail("identity_mismatch", path);
  }
};

const assertWitnessViewsMatch = (
  path: string,
  claimed: Readonly<{
    witnessSet: WatcherL1PublicBytesV1;
    scripts: readonly WatcherL1ScriptV1[];
    datums: readonly WatcherL1DatumV1[];
    redeemers: readonly WatcherL1RedeemerV1[];
  }>,
  actual: Readonly<{
    witnessSet: WatcherL1PublicBytesV1;
    scripts: readonly WatcherL1ScriptV1[];
    datums: readonly WatcherL1DatumV1[];
    redeemers: readonly WatcherL1RedeemerV1[];
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
          canonicalJson(entry) !== canonicalJson(actual[field][index]!),
      )
    ) {
      fail("identity_mismatch", `${path}.${field}`);
    }
  }
};

const assertUtxoViewsMatch = (
  claimed: readonly WatcherL1UtxoV1[],
  actual: readonly WatcherL1UtxoV1[],
  path: string,
): void => {
  if (
    claimed.length !== actual.length ||
    claimed.some(
      (entry, index) => canonicalJson(entry) !== canonicalJson(actual[index]!),
    )
  ) {
    fail("identity_mismatch", path);
  }
};

const parseTransaction = (
  value: unknown,
  path: string,
  budget: ParseBudget,
  session: WatcherL1NormalizationSessionStateV1 | undefined,
): WatcherL1TransactionV1 => {
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
  const derived = deriveCanonicalTransaction(fullTransaction, path, session);
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

const parseAuthenticatedProvider = (
  value: unknown,
): WatcherNormalizedAuthenticatedL1ProviderV1 => {
  const unparsed = plainRecord(value, "$.authenticatedProvider");
  const record = exactRecord(unparsed, "$.authenticatedProvider", [
    "schemaVersion",
    "network",
    "providerId",
    "source",
    "authentication",
  ]);
  if (
    record.schemaVersion !== WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION
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
    WATCHER_L1_SOURCE_MODES_V1,
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
  const source: WatcherL1SourceIdentityV1 =
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
              WATCHER_LOCAL_NODE_SURFACES_V1,
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
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
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

const makeTransportAttestationContext = (
  details: WatcherL1TransportAttestationDetailsV1,
  transports: readonly (Socket | TLSSocket)[],
  ownedTransports: readonly (Socket | TLSSocket)[] = transports,
  upstreamIsLive: () => boolean = () => true,
): WatcherL1TransportAttestationContextV1 => {
  const attestationDigest = digestCanonicalJson({
    provider: providerJson(details.provider),
    authorityBindingSha256: details.authorityBindingSha256,
    transportEndpoint: details.transportEndpoint,
  });
  const context = Object.freeze({
    schemaVersion: WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_V1_SCHEMA_VERSION,
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

const exactConnectionTimeout = (value: unknown, path: string): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < 1 ||
    value > 60_000
  ) {
    fail("invalid_field", path);
  }
  return value as number;
};

type ExactTlsEndpoint = Readonly<{
  endpoint: string;
  host: string;
  port: number;
  servername: string;
  caPem: string;
  expectedTlsPublicIdentitySha256: string;
  connectTimeoutMs: number;
}>;

const exactTlsEndpoint = (
  record: JsonRecord,
  path: string,
  allowedProtocols: readonly string[] = ["https:"],
): ExactTlsEndpoint => {
  const endpoint = exactString(
    record.endpoint,
    `${path}.endpoint`,
    /^(?:https|wss):\/\/.{1,2039}$/u,
  );
  let parsed: URL | null = null;
  try {
    parsed = new URL(endpoint);
  } catch {
    fail("invalid_field", `${path}.endpoint`);
  }
  if (parsed === null) {
    fail("invalid_field", `${path}.endpoint`);
  }
  const trustedEndpoint = parsed as URL;
  if (
    !allowedProtocols.includes(trustedEndpoint.protocol) ||
    trustedEndpoint.hostname.length === 0 ||
    trustedEndpoint.username.length > 0 ||
    trustedEndpoint.password.length > 0 ||
    trustedEndpoint.search.length > 0 ||
    trustedEndpoint.hash.length > 0
  ) {
    fail("invalid_field", `${path}.endpoint`);
  }
  const port =
    trustedEndpoint.port.length === 0 ? 443 : Number(trustedEndpoint.port);
  if (!Number.isInteger(port) || port < 1 || port > 65_535) {
    fail("invalid_field", `${path}.endpoint`);
  }
  if (
    typeof record.caPem !== "string" ||
    record.caPem.length === 0 ||
    record.caPem.length > 1_048_576 ||
    !record.caPem.includes("-----BEGIN CERTIFICATE-----")
  ) {
    fail("invalid_field", `${path}.caPem`);
  }
  return Object.freeze({
    endpoint,
    host: trustedEndpoint.hostname,
    port,
    servername: trustedEndpoint.hostname,
    caPem: record.caPem as string,
    expectedTlsPublicIdentitySha256: exactString(
      record.expectedTlsPublicIdentitySha256,
      `${path}.expectedTlsPublicIdentitySha256`,
      HEX_32,
    ),
    connectTimeoutMs: exactConnectionTimeout(
      record.connectTimeoutMs,
      `${path}.connectTimeoutMs`,
    ),
  });
};

type ExactTcpEndpoint = Readonly<{
  endpoint: string;
  host: string;
  port: number;
  connectTimeoutMs: number;
}>;

const exactTcpEndpoint = (
  record: JsonRecord,
  path: string,
  allowedProtocols: readonly string[],
): ExactTcpEndpoint => {
  const endpoint = exactString(
    record.endpoint,
    `${path}.endpoint`,
    /^(?:http|ws|postgresql):\/\/.{1,2039}$/u,
  );
  let parsed: URL | null = null;
  try {
    parsed = new URL(endpoint);
  } catch {
    fail("invalid_field", `${path}.endpoint`);
  }
  if (parsed === null) {
    fail("invalid_field", `${path}.endpoint`);
  }
  const trustedEndpoint = parsed as URL;
  if (
    !allowedProtocols.includes(trustedEndpoint.protocol) ||
    trustedEndpoint.hostname.length === 0 ||
    trustedEndpoint.username.length > 0 ||
    trustedEndpoint.password.length > 0 ||
    trustedEndpoint.search.length > 0 ||
    trustedEndpoint.hash.length > 0
  ) {
    fail("invalid_field", `${path}.endpoint`);
  }
  const defaultPort = trustedEndpoint.protocol === "postgresql:" ? 5_432 : 80;
  const port =
    trustedEndpoint.port.length === 0
      ? defaultPort
      : Number(trustedEndpoint.port);
  if (!Number.isInteger(port) || port < 1 || port > 65_535) {
    fail("invalid_field", `${path}.endpoint`);
  }
  return Object.freeze({
    endpoint,
    host: trustedEndpoint.hostname,
    port,
    connectTimeoutMs: exactConnectionTimeout(
      record.connectTimeoutMs,
      `${path}.connectTimeoutMs`,
    ),
  });
};

const awaitConnectedSocket = async <T extends Socket | TLSSocket>(
  socket: T,
  readyEvent: "connect" | "secureConnect",
  timeoutMs: number,
): Promise<T> =>
  await new Promise<T>((resolve, reject) => {
    let settled = false;
    const finish = (error?: Error): void => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      socket.off(readyEvent, onReady);
      socket.off("error", onError);
      if (error === undefined) resolve(socket);
      else {
        socket.destroy();
        reject(error);
      }
    };
    const onReady = (): void => finish();
    const onError = (): void =>
      finish(new Error("transport connection failed"));
    const timer = setTimeout(
      () => finish(new Error("transport connection timed out")),
      timeoutMs,
    );
    timer.unref();
    socket.once(readyEvent, onReady);
    socket.once("error", onError);
  });

const establishTcpSocket = async (
  endpoint: ExactTcpEndpoint,
  path: string,
): Promise<Readonly<{ socket: Socket; identitySha256: string }>> => {
  let socket: Socket | null = null;
  try {
    socket = await awaitConnectedSocket(
      createNetConnection({ host: endpoint.host, port: endpoint.port }),
      "connect",
      endpoint.connectTimeoutMs,
    );
  } catch {
    fail("identity_mismatch", `${path}.endpoint`);
  }
  if (socket === null) {
    fail("identity_mismatch", `${path}.endpoint`);
  }
  const trustedSocket = socket as Socket;
  trustedSocket.on("error", () => trustedSocket.destroy());
  return Object.freeze({
    socket: trustedSocket,
    identitySha256: digestCanonicalJson({
      endpoint: endpoint.endpoint,
      remoteAddress: trustedSocket.remoteAddress ?? null,
      remotePort: trustedSocket.remotePort?.toString() ?? null,
    }),
  });
};

const establishTlsSocket = async (
  endpoint: ExactTlsEndpoint,
  path: string,
): Promise<Readonly<{ socket: TLSSocket; identitySha256: string }>> => {
  const options: ConnectionOptions = {
    host: endpoint.host,
    port: endpoint.port,
    servername: endpoint.servername,
    ca: endpoint.caPem,
    rejectUnauthorized: true,
  };
  let socket: TLSSocket | null = null;
  try {
    socket = await awaitConnectedSocket(
      connectTls(options),
      "secureConnect",
      endpoint.connectTimeoutMs,
    );
  } catch {
    fail("identity_mismatch", `${path}.tlsPeer`);
  }
  if (socket === null) {
    fail("identity_mismatch", `${path}.tlsPeer`);
  }
  const trustedSocket = socket as TLSSocket;
  trustedSocket.on("error", () => trustedSocket.destroy());
  const peer = trustedSocket.getPeerCertificate();
  if (
    !trustedSocket.authorized ||
    peer.raw === undefined ||
    peer.raw.length === 0
  ) {
    trustedSocket.destroy();
    fail("identity_mismatch", `${path}.tlsPeer`);
  }
  const identitySha256 = createHash("sha256").update(peer.raw).digest("hex");
  if (identitySha256 !== endpoint.expectedTlsPublicIdentitySha256) {
    trustedSocket.destroy();
    fail("identity_mismatch", `${path}.expectedTlsPublicIdentitySha256`);
  }
  return Object.freeze({ socket: trustedSocket, identitySha256 });
};

/**
 * Mints the local chain-sync transport authority only from the opaque runtime
 * produced after the pinned native NtC helper completed its Unix-socket
 * handshake and returned the exact W01 startup identity.
 */
export const establishWatcherLocalNodeAuthorityTransportV1 = (
  nativeAuthority: WatcherNativeChainSyncAuthorityV1,
): WatcherL1TransportAttestationContextV1 => {
  const details =
    watcherNativeChainSyncAuthorityDetailsV1(nativeAuthority) ??
    fail("invalid_field", "$.nativeChainSyncAuthority");
  const provider = parseAuthenticatedProvider({
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: details.network,
    providerId: details.authorityNodeId,
    source: {
      sourceMode: "local_node",
      authorityNodeId: details.authorityNodeId,
      surface: "chain_sync",
    },
    authentication: {
      kind: "cardano_node_genesis_v1",
      publicIdentitySha256: details.genesisIdentitySha256,
    },
  });
  return makeTransportAttestationContext(
    {
      provider,
      authorityBindingSha256: details.startupDigest,
      transportEndpoint: details.socketPath,
    },
    [],
    [],
    () => watcherNativeChainSyncAuthorityDetailsV1(nativeAuthority) !== null,
  );
};

export const establishWatcherLocalNodeQueryTransportV1 = async (
  authorityContext: WatcherL1TransportAttestationContextV1,
  input: WatcherLocalNodeQueryTransportV1,
): Promise<WatcherL1TransportAttestationContextV1> => {
  const authority = watcherL1TransportAttestationDetailsV1(authorityContext);
  const authorityState = transportAttestationStates.get(authorityContext);
  if (
    authority === null ||
    authorityState === undefined ||
    authority.authorityBindingSha256 === null ||
    authority.provider.source.sourceMode !== "local_node" ||
    authority.provider.source.surface !== "chain_sync"
  ) {
    fail("invalid_field", "$.localNodeAuthorityContext");
  }
  const trustedAuthority =
    authority as WatcherL1TransportAttestationDetailsV1 & {
      readonly authorityBindingSha256: string;
      readonly provider: WatcherNormalizedAuthenticatedL1ProviderV1 & {
        readonly source: Extract<
          WatcherL1SourceIdentityV1,
          { readonly sourceMode: "local_node" }
        >;
      };
    };
  const trustedAuthorityState =
    authorityState as WatcherL1TransportAttestationStateV1;
  const base = plainRecord(input, "$.localNodeQueryTransport");
  const transportKind = exactLiteral(
    base.transportKind,
    "$.localNodeQueryTransport.transportKind",
    ["tcp", "https_tls"] as const,
  );
  const record = exactRecord(
    base,
    "$.localNodeQueryTransport",
    transportKind === "tcp"
      ? [
          "transportKind",
          "providerId",
          "surface",
          "endpoint",
          "connectTimeoutMs",
        ]
      : [
          "transportKind",
          "providerId",
          "surface",
          "endpoint",
          "caPem",
          "expectedTlsPublicIdentitySha256",
          "connectTimeoutMs",
        ],
  );
  const providerId = exactString(
    record.providerId,
    "$.localNodeQueryTransport.providerId",
    PROVIDER_ID,
  );
  const surface = exactLiteral(
    record.surface,
    "$.localNodeQueryTransport.surface",
    WATCHER_LOCAL_NODE_SURFACES_V1.filter(
      (candidate) => candidate !== "chain_sync",
    ),
  ) as Exclude<WatcherLocalNodeSurfaceV1, "chain_sync">;
  const plainProtocols =
    surface === "db_sync"
      ? ["postgresql:"]
      : surface === "kupo"
        ? ["http:"]
        : ["http:", "ws:"];
  const tlsProtocols =
    surface === "db_sync"
      ? []
      : surface === "kupo"
        ? ["https:"]
        : ["https:", "wss:"];
  const tcpEndpoint =
    transportKind === "tcp"
      ? exactTcpEndpoint(record, "$.localNodeQueryTransport", plainProtocols)
      : null;
  const tlsEndpoint =
    transportKind === "https_tls"
      ? exactTlsEndpoint(record, "$.localNodeQueryTransport", tlsProtocols)
      : null;
  const transportEndpoint =
    transportKind === "tcp" ? tcpEndpoint!.endpoint : tlsEndpoint!.endpoint;
  const established =
    transportKind === "tcp"
      ? await establishTcpSocket(tcpEndpoint!, "$.localNodeQueryTransport")
      : await establishTlsSocket(tlsEndpoint!, "$.localNodeQueryTransport");
  const provider = parseAuthenticatedProvider({
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: trustedAuthority.provider.network,
    providerId,
    source: {
      sourceMode: "local_node",
      authorityNodeId: trustedAuthority.provider.source.authorityNodeId,
      surface,
    },
    authentication: {
      kind:
        transportKind === "tcp"
          ? "local_endpoint_identity_v1"
          : "https_tls_identity_v1",
      publicIdentitySha256: established.identitySha256,
    },
  });
  return makeTransportAttestationContext(
    {
      provider,
      authorityBindingSha256: trustedAuthority.authorityBindingSha256,
      transportEndpoint,
    },
    [...trustedAuthorityState.transports, established.socket],
    [established.socket],
    () =>
      trustedAuthorityState.active && trustedAuthorityState.upstreamIsLive(),
  );
};

export const establishWatcherExternalProviderTransportV1 = async (
  input: WatcherExternalProviderTransportV1,
): Promise<WatcherL1TransportAttestationContextV1> => {
  const record = exactRecord(input, "$.externalProviderTransport", [
    "network",
    "providerId",
    "operatorIdentitySha256",
    "endpoint",
    "caPem",
    "expectedTlsPublicIdentitySha256",
    "connectTimeoutMs",
  ]);
  const endpoint = exactTlsEndpoint(record, "$.externalProviderTransport");
  const established = await establishTlsSocket(
    endpoint,
    "$.externalProviderTransport",
  );
  const provider = parseAuthenticatedProvider({
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
    network: exactLiteral(
      record.network,
      "$.externalProviderTransport.network",
      NETWORKS,
    ),
    providerId: exactString(
      record.providerId,
      "$.externalProviderTransport.providerId",
      PROVIDER_ID,
    ),
    source: {
      sourceMode: "external_providers",
      operatorIdentitySha256: exactString(
        record.operatorIdentitySha256,
        "$.externalProviderTransport.operatorIdentitySha256",
        HEX_32,
      ),
    },
    authentication: {
      kind: "https_tls_identity_v1",
      publicIdentitySha256: established.identitySha256,
    },
  });
  return makeTransportAttestationContext(
    {
      provider,
      authorityBindingSha256: null,
      transportEndpoint: endpoint.endpoint,
    },
    [established.socket],
  );
};

const transactionJson = (
  transaction: WatcherL1TransactionV1,
): CanonicalJson => ({
  txHash: transaction.txHash,
  ...(transaction.transactionIndex === undefined
    ? {}
    : { transactionIndex: transaction.transactionIndex }),
  isValid: transaction.isValid,
  fullTransaction: transaction.fullTransaction,
  body: transaction.body,
  witnessSet: transaction.witnessSet,
  utxos: transaction.utxos,
  scripts: transaction.scripts,
  datums: transaction.datums,
  redeemers: transaction.redeemers,
});

const providerJson = (
  provider: WatcherNormalizedAuthenticatedL1ProviderV1,
): CanonicalJson => ({
  schemaVersion: provider.schemaVersion,
  network: provider.network,
  providerId: provider.providerId,
  source: provider.source,
  authentication: provider.authentication,
});

const contentJson = (input: {
  network: WatcherL1NetworkV1;
  pointDigest: string;
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  transactions: readonly WatcherL1TransactionV1[];
}): CanonicalJson => ({
  network: input.network,
  pointDigest: input.pointDigest,
  blockHash: input.blockHash,
  parentBlockHash: input.parentBlockHash,
  slot: input.slot,
  blockNo: input.blockNo,
  transactions: input.transactions.map(transactionJson),
});

export const encodeWatcherNormalizedL1BlockV1 = (
  value: WatcherNormalizedL1BlockV1,
): Buffer =>
  Buffer.from(
    canonicalJson({
      schemaVersion: value.schemaVersion,
      network: value.network,
      provider: providerJson(value.provider),
      chainPoint: value.chainPoint,
      transactions: value.transactions.map(transactionJson),
      blockContentDigest: value.blockContentDigest,
      observationDigest: value.observationDigest,
    }),
    "utf8",
  );

export const normalizeWatcherL1BlockV1 = (
  transportAttestationContext: WatcherL1TransportAttestationContextV1,
  observationInput: unknown,
  normalizationSession?: WatcherL1NormalizationSessionV1,
): WatcherNormalizedL1BlockV1 => {
  /*
   * Trust boundary: the opaque context proves the configured live transport
   * location and peer identity; it does not claim that this arbitrary JS value
   * was itself read from the socket. The watcher-owned transport adapter must
   * call this function only with bytes it decoded from that connection. This is
   * an in-process capability boundary, not a remotely callable receipt API:
   * untrusted serialized callers cannot construct the WeakMap-backed context,
   * and detached/closed contexts fail below. A future wire adapter may issue
   * per-frame receipts once its framing protocol is fixed, but manufacturing a
   * receipt here would falsely attest provenance that this layer cannot see.
   */
  const session =
    normalizationSession === undefined
      ? undefined
      : (normalizationSessionStates.get(normalizationSession) ??
        fail("invalid_field", "$.normalizationSession"));
  const attestation = watcherL1TransportAttestationDetailsV1(
    transportAttestationContext,
  );
  if (attestation === null) {
    fail("invalid_field", "$.transportAttestationContext");
  }
  const provider = (attestation as WatcherL1TransportAttestationDetailsV1)
    .provider;
  const observation = exactRecord(observationInput, "$", [
    "schemaVersion",
    "network",
    "providerId",
    "chainPoint",
    "transactions",
  ]);
  if (
    observation.schemaVersion !== WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.schemaVersion");
  }
  const network = exactLiteral(observation.network, "$.network", NETWORKS);
  if (network !== provider.network) {
    fail("network_mismatch", "$.network");
  }
  const providerId = exactString(
    observation.providerId,
    "$.providerId",
    PROVIDER_ID,
  );
  if (providerId !== provider.providerId) {
    fail("provider_mismatch", "$.providerId");
  }
  const point = exactRecord(observation.chainPoint, "$.chainPoint", [
    "blockHash",
    "parentBlockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  const blockHash = exactString(
    point.blockHash,
    "$.chainPoint.blockHash",
    HEX_32,
  );
  const parentBlockHash =
    point.parentBlockHash === null
      ? null
      : exactString(
          point.parentBlockHash,
          "$.chainPoint.parentBlockHash",
          HEX_32,
        );
  const slot = exactNatural(point.slot, "$.chainPoint.slot");
  const blockNo = exactNatural(point.blockNo, "$.chainPoint.blockNo");
  const depth = exactNatural(point.depth, "$.chainPoint.depth");
  const budget: ParseBudget = { collectionMembers: 0, publicBytes: 0 };
  const transactionInputs = preflightTransactionCollections(
    observation.transactions,
    budget,
  );
  const transactions = transactionInputs.map((transaction, index) =>
    parseTransaction(
      transaction,
      `$.transactions[${index.toString()}]`,
      budget,
      session,
    ),
  );
  const transactionHashes = new Set<string>();
  for (let index = 0; index < transactions.length; index += 1) {
    const transaction = transactions[index]!;
    if (transactionHashes.has(transaction.txHash)) {
      fail("duplicate_identity", `$.transactions[${index.toString()}]`);
    }
    if (
      transaction.transactionIndex !== undefined &&
      transaction.transactionIndex !== index.toString()
    ) {
      fail(
        "identity_mismatch",
        `$.transactions[${index.toString()}].transactionIndex`,
      );
    }
    transactionHashes.add(transaction.txHash);
  }
  Object.freeze(transactions);
  const pointDigest = digestCanonicalJson({
    network,
    blockHash,
    parentBlockHash,
    slot,
    blockNo,
  });
  const chainPointId = digestCanonicalJson({
    pointDigest,
    depth,
    provider: providerJson(provider),
  });
  const chainPoint = Object.freeze({
    chainPointId,
    pointDigest,
    blockHash,
    parentBlockHash,
    slot,
    blockNo,
    depth,
  });
  const blockContentDigest = digestCanonicalJson(
    contentJson({
      network,
      pointDigest,
      blockHash,
      parentBlockHash,
      slot,
      blockNo,
      transactions,
    }),
  );
  const observationDigest = digestCanonicalJson({
    schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
    provider: providerJson(provider),
    chainPoint,
    blockContentDigest,
  });
  const normalized = Object.freeze({
    schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
    network,
    provider,
    chainPoint,
    transactions,
    blockContentDigest,
    observationDigest,
  });
  normalizedBlockProvenance.set(
    normalized,
    transportAttestationContext as WatcherL1TransportAttestationContextV1,
  );
  return normalized;
};

/**
 * Builds the exact normalized observation from canonical full transaction
 * bytes read by a watcher-owned transport adapter. All derived transaction
 * fields are reconstructed locally and then passed back through the ordinary
 * strict normalizer; callers cannot supply hashes, witnesses, outputs,
 * datums, scripts or redeemers independently of the transaction bytes.
 */
export const normalizeWatcherL1BlockFromTransactionCborsV1 = (
  transportAttestationContext: WatcherL1TransportAttestationContextV1,
  input: Readonly<{
    network: WatcherL1NetworkV1;
    chainPoint: Readonly<{
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    }>;
    transactionCbors: readonly string[];
  }>,
  normalizationSession?: WatcherL1NormalizationSessionV1,
): WatcherNormalizedL1BlockV1 => {
  const attestation = watcherL1TransportAttestationDetailsV1(
    transportAttestationContext,
  );
  if (attestation === null) {
    fail("invalid_field", "$.transportAttestationContext");
  }
  const providerId = (attestation as WatcherL1TransportAttestationDetailsV1)
    .provider.providerId;
  const transactions = input.transactionCbors.map((cborHex, index) => {
    const fullTransaction = publicBytesFromCanonicalCbor(cborHex);
    const derived = deriveCanonicalTransaction(
      fullTransaction,
      `$.transactionCbors[${index.toString()}]`,
      undefined,
    );
    return Object.freeze({
      txHash: derived.txHash,
      transactionIndex: index.toString(),
      fullTransaction: derived.fullTransaction,
      body: derived.body,
      witnessSet: derived.witnessSet,
      utxos: derived.utxos,
      scripts: derived.scripts,
      datums: derived.datums,
      redeemers: derived.redeemers,
    });
  });
  return normalizeWatcherL1BlockV1(
    transportAttestationContext,
    {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: input.network,
      providerId,
      chainPoint: input.chainPoint,
      transactions,
    },
    normalizationSession,
  );
};

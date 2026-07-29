import { createHash } from "node:crypto";

import { validatorToScriptHash } from "@lucid-evolution/lucid";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";

export const WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION =
  "midgard-watcher-authenticated-l1-provider-v1" as const;
export const WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION =
  "midgard-watcher-l1-block-observation-v1" as const;
export const WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION =
  "midgard-watcher-normalized-l1-block-v1" as const;

export const WATCHER_L1_ADAPTER_V1_BOUNDS = Object.freeze({
  arrayMembers: 4_096,
  publicBytes: 1_048_576,
  totalPublicBytes: 67_108_864,
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
 * must come from the configured TLS trust identity or Cardano node genesis,
 * never from the provider response being normalized.
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
  transactionIndex: string;
  txHash: string;
  body: WatcherL1PublicBytesV1;
  utxos: readonly WatcherL1UtxoV1[];
  scripts: readonly WatcherL1ScriptV1[];
  datums: readonly WatcherL1DatumV1[];
  redeemers: readonly WatcherL1RedeemerV1[];
}>;

export type WatcherL1ChainPointV1 = Readonly<{
  chainPointId: string;
  pointDigest: string;
  blockHash: string;
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
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

type ParseBudget = {
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

const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const sha256Utf8 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

const canonicalJson = (value: CanonicalJson): string => {
  if (value === null || typeof value === "string") {
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
  const scriptHash = exactString(
    record.scriptHash,
    `${path}.scriptHash`,
    HEX_28,
  );
  const language = exactLiteral(
    record.language,
    `${path}.language`,
    WATCHER_L1_SCRIPT_LANGUAGES_V1,
  );
  const bytes = parsePublicBytes(record.bytes, `${path}.bytes`, budget);
  const computedScriptHash = (() => {
    try {
      return validatorToScriptHash({
        type: language,
        script: bytes.bytesHex,
      });
    } catch {
      return fail("invalid_field", `${path}.bytes.bytesHex`);
    }
  })();
  if (computedScriptHash !== scriptHash) {
    fail("identity_mismatch", `${path}.scriptHash`);
  }
  return Object.freeze({
    scriptHash,
    language,
    bytes,
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

const parseTransaction = (
  value: unknown,
  path: string,
  transactionIndex: number,
  budget: ParseBudget,
): WatcherL1TransactionV1 => {
  const record = exactRecord(value, path, [
    "txHash",
    "body",
    "utxos",
    "scripts",
    "datums",
    "redeemers",
  ]);
  const body = parsePublicBytes(record.body, `${path}.body`, budget);
  const txHash = exactString(record.txHash, `${path}.txHash`, HEX_32);
  if (
    computeHash32(Buffer.from(body.bytesHex, "hex")).toString("hex") !== txHash
  ) {
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
  const purposeOrder = new Map(
    WATCHER_L1_REDEEMER_PURPOSES_V1.map((purpose, index) => [purpose, index]),
  );
  const redeemers = freezeSortedUnique(
    exactArray(record.redeemers, `${path}.redeemers`).map((entry, index) =>
      parseRedeemer(entry, `${path}.redeemers[${index.toString()}]`, budget),
    ),
    `${path}.redeemers`,
    (entry) => `${entry.purpose}:${entry.index}`,
    (left, right) => {
      const purposeComparison =
        (purposeOrder.get(left.purpose) as number) -
        (purposeOrder.get(right.purpose) as number);
      return purposeComparison === 0
        ? compareNaturalStrings(left.index, right.index)
        : purposeComparison;
    },
  );
  return Object.freeze({
    transactionIndex: transactionIndex.toString(),
    txHash,
    body,
    utxos,
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

const transactionJson = (
  transaction: WatcherL1TransactionV1,
): CanonicalJson => ({
  transactionIndex: transaction.transactionIndex,
  txHash: transaction.txHash,
  body: transaction.body,
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
  slot: string;
  blockNo: string;
  transactions: readonly WatcherL1TransactionV1[];
}): CanonicalJson => ({
  network: input.network,
  pointDigest: input.pointDigest,
  blockHash: input.blockHash,
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
  authenticatedProviderInput: unknown,
  observationInput: unknown,
): WatcherNormalizedL1BlockV1 => {
  const provider = parseAuthenticatedProvider(authenticatedProviderInput);
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
    "slot",
    "blockNo",
    "depth",
  ]);
  const blockHash = exactString(
    point.blockHash,
    "$.chainPoint.blockHash",
    HEX_32,
  );
  const slot = exactNatural(point.slot, "$.chainPoint.slot");
  const blockNo = exactNatural(point.blockNo, "$.chainPoint.blockNo");
  const depth = exactNatural(point.depth, "$.chainPoint.depth");
  const budget: ParseBudget = { publicBytes: 0 };
  const transactions = Object.freeze(
    exactArray(observation.transactions, "$.transactions").map(
      (transaction, index) =>
        parseTransaction(
          transaction,
          `$.transactions[${index.toString()}]`,
          index,
          budget,
        ),
    ),
  );
  const transactionHashes = new Set<string>();
  for (const transaction of transactions) {
    if (transactionHashes.has(transaction.txHash)) {
      fail(
        "duplicate_identity",
        `$.transactions[${transaction.transactionIndex}]`,
      );
    }
    transactionHashes.add(transaction.txHash);
  }
  const pointDigest = digestCanonicalJson({
    network,
    blockHash,
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
    slot,
    blockNo,
    depth,
  });
  const blockContentDigest = digestCanonicalJson(
    contentJson({
      network,
      pointDigest,
      blockHash,
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
  return Object.freeze({
    schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
    network,
    provider,
    chainPoint,
    transactions,
    blockContentDigest,
    observationDigest,
  });
};

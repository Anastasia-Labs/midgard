import { isAbsolute, normalize } from "node:path";

export const WATCHER_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-config-v1" as const;

export const WATCHER_CARDANO_SECURITY_PARAMETER_K = 2_160 as const;

export const WATCHER_CONFIG_BOUNDS = {
  configJsonBytes: { min: 2, max: 262_144 },
  externalProviders: { min: 2, max: 4 },
  localNodeQueryServices: { min: 2, max: 3 },
  daPeers: { min: 1, max: 32 },
  requestTimeoutMs: { min: 100, max: 120_000 },
  concurrency: { min: 1, max: 64 },
  finalityDepth: { min: 1, max: 2_160 },
  rollbackDepth: { min: 1, max: 2_160 },
  postFinalityRecoveryDepth: {
    min: 1,
    max: WATCHER_CARDANO_SECURITY_PARAMETER_K,
  },
  deadlineMs: { min: 1_000, max: 86_400_000 },
} as const;

export type WatcherConfigMode = "development" | "acceptance";
export type WatcherTargetNetwork = "Mainnet" | "Preprod" | "Preview";
export type WatcherL1SourceMode = "local_node" | "external_providers";

export type WatcherL1ProviderConfig = Readonly<{
  identity: string;
  operatorIdentitySha256: string;
  endpoint: string;
}>;

export type WatcherLocalNodeQueryServiceConfig = Readonly<{
  kind: "ogmios" | "kupo" | "db_sync";
  identity: string;
  endpoint: string;
}>;

export type WatcherL1SourceConfig =
  | Readonly<{
      sourceMode: "local_node";
      authorityNodeId: string;
      chainSync: Readonly<{
        kind: "cardano_node_socket";
        socketPath: string;
        nodeConfigPath: string;
        genesisConfigPath: string;
        genesisIdentitySha256: string;
      }>;
      queryServices: readonly WatcherLocalNodeQueryServiceConfig[];
    }>
  | Readonly<{
      sourceMode: "external_providers";
      providers: readonly WatcherL1ProviderConfig[];
    }>;

export type WatcherL1Config = Readonly<{
  source: WatcherL1SourceConfig;
  requestTimeoutMs: number;
  maxConcurrency: number;
  finality: Readonly<{
    depth: number;
    rollback: Readonly<{
      beforeFinality: "rewind";
      afterFinality: "quarantine";
      maxDepth: number;
      postFinalityRecoveryMaxDepth: typeof WATCHER_CARDANO_SECURITY_PARAMETER_K;
    }>;
  }>;
}>;

export type WatcherDaPeerConfig = Readonly<{
  identity: string;
  /** Peer id embedded in, and authenticated against, `multiaddr`. */
  peerId: string;
  multiaddr: string;
}>;

export type WatcherWalletKeySource =
  | Readonly<{ kind: "environment"; variable: string }>
  | Readonly<{ kind: "file"; path: string }>;

export type WatcherRollbackAuthorityKeySource = WatcherWalletKeySource;

export type WatcherConfig = Readonly<{
  schemaVersion: typeof WATCHER_CONFIG_SCHEMA_VERSION;
  mode: WatcherConfigMode;
  targetNetwork: WatcherTargetNetwork;
  l1: WatcherL1Config;
  da: Readonly<{
    peers: readonly WatcherDaPeerConfig[];
    requestTimeoutMs: number;
    maxConcurrency: number;
  }>;
  storage: Readonly<{
    driver: "sqlite";
    path: string;
    rollbackAuthorityKeySource: WatcherRollbackAuthorityKeySource;
  }>;
  proverWallet: Readonly<{
    keySource: WatcherWalletKeySource;
  }>;
  deadlines: Readonly<{
    daFetchMs: number;
    daPublishMs: number;
    proofConstructMs: number;
    proofSubmitMs: number;
  }>;
}>;

export type WatcherConfigErrorCode =
  | "duplicate_field"
  | "inline_secret_forbidden"
  | "invalid_configuration"
  | "invalid_endpoint"
  | "invalid_value"
  | "malformed_json"
  | "missing_required_field"
  | "non_string_key"
  | "out_of_bounds"
  | "provider_alias"
  | "secret_source_alias"
  | "unsafe_path"
  | "unsafe_value"
  | "unknown_field";

export type WatcherConfigDiagnostic = Readonly<{
  code: WatcherConfigErrorCode;
  path: string;
  message: string;
}>;

export class WatcherConfigError extends Error {
  readonly code: WatcherConfigErrorCode;
  readonly path: string;

  constructor(code: WatcherConfigErrorCode, path: string) {
    super(`Watcher configuration rejected: ${code} at ${path}`);
    this.name = "WatcherConfigError";
    this.code = code;
    this.path = path;
  }
}

const admittedWatcherConfigs = new WeakSet<object>();

function fail(code: WatcherConfigErrorCode, path: string): never {
  throw new WatcherConfigError(code, path);
}

export const watcherConfigDiagnostic = (
  error: unknown,
): WatcherConfigDiagnostic => {
  if (error instanceof WatcherConfigError) {
    return {
      code: error.code,
      path: error.path,
      message: error.message,
    };
  }
  return {
    code: "invalid_configuration",
    path: "$",
    message: "Watcher configuration rejected: invalid_configuration at $",
  };
};

const INLINE_SECRET_FIELD =
  /^(?:api[-_]?key|key|mnemonic|passphrase|password|private[-_]?key|secret|seed|seed[-_]?phrase|token|value)$/iu;

const rejectInlineSecretFields = (
  value: Record<string, unknown>,
  path: string,
): void => {
  if (Object.keys(value).some((key) => INLINE_SECRET_FIELD.test(key))) {
    fail("inline_secret_forbidden", path);
  }
};

const plainRecord = (value: unknown, path: string): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_value", path);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("unsafe_value", path);
  }
  const record = value as Record<string, unknown>;
  if (Reflect.ownKeys(record).length !== Object.keys(record).length) {
    fail("non_string_key", path);
  }
  for (const key of Object.keys(record)) {
    const descriptor = Object.getOwnPropertyDescriptor(record, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      fail("unsafe_value", path);
    }
  }
  rejectInlineSecretFields(record, path);
  return record;
};

const exactRecord = (
  value: unknown,
  path: string,
  keys: readonly string[],
): Record<string, unknown> => {
  const record = plainRecord(value, path);
  const allowed = new Set(keys);
  if (Object.keys(record).some((key) => !allowed.has(key))) {
    fail("unknown_field", path);
  }
  for (const key of keys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_required_field", `${path}.${key}`);
    }
  }
  return record;
};

const exactString = (
  value: unknown,
  path: string,
  {
    minLength = 1,
    maxLength,
    pattern,
  }: {
    readonly minLength?: number;
    readonly maxLength: number;
    readonly pattern?: RegExp;
  },
): string => {
  if (
    typeof value !== "string" ||
    value !== value.trim() ||
    value.length < minLength ||
    value.length > maxLength ||
    (pattern !== undefined && !pattern.test(value))
  ) {
    fail("invalid_value", path);
  }
  return value;
};

const enumValue = <const Values extends readonly string[]>(
  value: unknown,
  path: string,
  values: Values,
): Values[number] => {
  if (
    typeof value !== "string" ||
    !(values as readonly string[]).includes(value)
  ) {
    fail("invalid_value", path);
  }
  return value as Values[number];
};

const boundedInteger = (
  value: unknown,
  path: string,
  bounds: Readonly<{ min: number; max: number }>,
): number => {
  if (!Number.isSafeInteger(value)) {
    fail("invalid_value", path);
  }
  const integer = value as number;
  if (integer < bounds.min || integer > bounds.max) {
    fail("out_of_bounds", path);
  }
  return integer;
};

const boundedArray = (
  value: unknown,
  path: string,
  bounds: Readonly<{ min: number; max: number }>,
): readonly unknown[] => {
  if (!Array.isArray(value)) {
    fail("invalid_value", path);
  }
  if (value.length < bounds.min || value.length > bounds.max) {
    fail("out_of_bounds", path);
  }
  return value;
};

const IDENTITY_PATTERN = /^[a-z][a-z0-9-]{2,31}$/u;
const HEX_32_PATTERN = /^[0-9a-f]{64}$/u;
const ENVIRONMENT_VARIABLE_PATTERN = /^[A-Z][A-Z0-9_]{2,127}$/u;

const isPrivateIpv4 = (hostname: string): boolean => {
  const match = /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/u.exec(hostname);
  if (match === null) {
    return false;
  }
  const octets = match.slice(1).map(Number);
  if (octets.some((octet) => octet < 0 || octet > 255)) {
    return true;
  }
  const [first = 0, second = 0] = octets;
  return (
    first === 0 ||
    first === 10 ||
    first === 127 ||
    (first === 169 && second === 254) ||
    (first === 172 && second >= 16 && second <= 31) ||
    (first === 192 && second === 168) ||
    first >= 224
  );
};

const isPublicHostname = (hostname: string): boolean => {
  const normalized = hostname
    .toLowerCase()
    .replace(/^\[|\]$/gu, "")
    .replace(/\.$/u, "");
  const isIpv6 = normalized.includes(":");
  const isPrivateIpv6 =
    isIpv6 &&
    (normalized === "::1" ||
      normalized.startsWith("fc") ||
      normalized.startsWith("fd") ||
      normalized.startsWith("fe8") ||
      normalized.startsWith("fe9") ||
      normalized.startsWith("fea") ||
      normalized.startsWith("feb"));
  const isIpv4 = /^\d{1,3}(?:\.\d{1,3}){3}$/u.test(normalized);
  return !(
    normalized === "localhost" ||
    normalized.endsWith(".localhost") ||
    normalized.endsWith(".local") ||
    normalized.endsWith(".internal") ||
    normalized.endsWith(".lan") ||
    isPrivateIpv6 ||
    (isIpv4 && isPrivateIpv4(normalized)) ||
    (!isIpv4 && !isIpv6 && !normalized.includes("."))
  );
};

const parseExternalProviderEndpoint = (
  value: unknown,
  path: string,
  mode: WatcherConfigMode,
): Readonly<{ endpoint: string; aliasKey: string }> => {
  const endpoint = exactString(value, path, { maxLength: 2_048 });
  let url: URL;
  try {
    url = new URL(endpoint);
  } catch {
    fail("invalid_endpoint", path);
  }
  if (
    url.protocol !== "https:" ||
    url.username.length > 0 ||
    url.password.length > 0 ||
    url.search.length > 0 ||
    url.hash.length > 0 ||
    (mode === "acceptance" && !isPublicHostname(url.hostname))
  ) {
    fail("invalid_endpoint", path);
  }
  url.hostname = url.hostname.toLowerCase().replace(/\.$/u, "");
  const aliasKey = `${url.protocol}//${url.host.toLowerCase()}${url.pathname.replace(/\/+$/u, "")}`;
  return { endpoint, aliasKey };
};

const DA_MULTIADDR_PATTERN =
  /^\/dns(4|6)\/([a-z0-9](?:[a-z0-9.-]{0,251}[a-z0-9])?)\/tcp\/([1-9][0-9]{0,4})\/p2p\/([1-9A-HJ-NP-Za-km-z]{20,128})$/u;

const parsePublicDaMultiaddr = (
  value: unknown,
  path: string,
): Readonly<{ multiaddr: string; aliasKey: string }> => {
  const multiaddr = exactString(value, path, { maxLength: 512 });
  const match = DA_MULTIADDR_PATTERN.exec(multiaddr);
  if (match === null) {
    fail("invalid_endpoint", path);
  }
  const [, , hostname = "", portText = "0", peerId = ""] = match;
  const port = Number(portText);
  if (
    port < 1 ||
    port > 65_535 ||
    hostname !== hostname.toLowerCase() ||
    !hostname.includes(".") ||
    !isPublicHostname(hostname)
  ) {
    fail("invalid_endpoint", path);
  }
  return {
    multiaddr,
    aliasKey: `${hostname}:${port.toString()}:${peerId}`,
  };
};

const parseProviders = (
  value: unknown,
  mode: WatcherConfigMode,
): readonly WatcherL1ProviderConfig[] => {
  const values = boundedArray(
    value,
    "$.l1.source.providers",
    WATCHER_CONFIG_BOUNDS.externalProviders,
  );
  const identities = new Set<string>();
  const operatorIdentities = new Set<string>();
  const endpoints = new Set<string>();
  return Object.freeze(
    values.map((entry, index) => {
      const path = `$.l1.source.providers[${index.toString()}]`;
      const record = exactRecord(entry, path, [
        "identity",
        "operatorIdentitySha256",
        "endpoint",
      ]);
      const identity = exactString(record.identity, `${path}.identity`, {
        maxLength: 32,
        pattern: IDENTITY_PATTERN,
      });
      const operatorIdentitySha256 = exactString(
        record.operatorIdentitySha256,
        `${path}.operatorIdentitySha256`,
        {
          maxLength: 64,
          pattern: HEX_32_PATTERN,
        },
      );
      const endpoint = parseExternalProviderEndpoint(
        record.endpoint,
        `${path}.endpoint`,
        mode,
      );
      if (
        identities.has(identity) ||
        operatorIdentities.has(operatorIdentitySha256) ||
        endpoints.has(endpoint.aliasKey)
      ) {
        fail("provider_alias", path);
      }
      identities.add(identity);
      operatorIdentities.add(operatorIdentitySha256);
      endpoints.add(endpoint.aliasKey);
      return Object.freeze({
        identity,
        operatorIdentitySha256,
        endpoint: endpoint.endpoint,
      });
    }),
  );
};

const parseLocalNodeEndpoint = (
  value: unknown,
  path: string,
  kind: WatcherLocalNodeQueryServiceConfig["kind"],
): string => {
  const endpoint = exactString(value, path, { maxLength: 2_048 });
  let url: URL;
  try {
    url = new URL(endpoint);
  } catch {
    fail("invalid_endpoint", path);
  }
  const protocols =
    kind === "ogmios"
      ? ["http:", "ws:"]
      : kind === "kupo"
        ? ["http:"]
        : ["postgresql:"];
  const hostname = url.hostname.toLowerCase().replace(/^\[|\]$/gu, "");
  if (
    !protocols.includes(url.protocol) ||
    !["127.0.0.1", "localhost", "::1"].includes(hostname) ||
    url.username.length > 0 ||
    url.password.length > 0 ||
    url.search.length > 0 ||
    url.hash.length > 0
  ) {
    fail("invalid_endpoint", path);
  }
  return endpoint;
};

const parseLocalNodeQueryServices = (
  value: unknown,
): readonly WatcherLocalNodeQueryServiceConfig[] => {
  const services = boundedArray(
    value,
    "$.l1.source.queryServices",
    WATCHER_CONFIG_BOUNDS.localNodeQueryServices,
  );
  const identities = new Set<string>();
  const kinds = new Set<string>();
  const endpoints = new Set<string>();
  const parsed = services.map((entry, index) => {
    const path = `$.l1.source.queryServices[${index.toString()}]`;
    const service = exactRecord(entry, path, ["kind", "identity", "endpoint"]);
    const kind = enumValue(service.kind, `${path}.kind`, [
      "ogmios",
      "kupo",
      "db_sync",
    ] as const);
    const identity = exactString(service.identity, `${path}.identity`, {
      maxLength: 32,
      pattern: IDENTITY_PATTERN,
    });
    const endpoint = parseLocalNodeEndpoint(
      service.endpoint,
      `${path}.endpoint`,
      kind,
    );
    if (
      identities.has(identity) ||
      kinds.has(kind) ||
      endpoints.has(endpoint)
    ) {
      fail("provider_alias", path);
    }
    identities.add(identity);
    kinds.add(kind);
    endpoints.add(endpoint);
    return Object.freeze({ kind, identity, endpoint });
  });
  if (!["ogmios", "kupo"].every((kind) => kinds.has(kind))) {
    fail("missing_required_field", "$.l1.source.queryServices");
  }
  return Object.freeze(parsed);
};

const parseDaPeers = (value: unknown): readonly WatcherDaPeerConfig[] => {
  const values = boundedArray(
    value,
    "$.da.peers",
    WATCHER_CONFIG_BOUNDS.daPeers,
  );
  const identities = new Set<string>();
  const endpoints = new Set<string>();
  return Object.freeze(
    values.map((entry, index) => {
      const path = `$.da.peers[${index.toString()}]`;
      const record = exactRecord(entry, path, ["identity", "multiaddr"]);
      const identity = exactString(record.identity, `${path}.identity`, {
        maxLength: 32,
        pattern: IDENTITY_PATTERN,
      });
      const endpoint = parsePublicDaMultiaddr(
        record.multiaddr,
        `${path}.multiaddr`,
      );
      if (identities.has(identity) || endpoints.has(endpoint.aliasKey)) {
        fail("provider_alias", path);
      }
      identities.add(identity);
      endpoints.add(endpoint.aliasKey);
      const peerId = endpoint.aliasKey.split(":")[2]!;
      return Object.freeze({ identity, peerId, multiaddr: endpoint.multiaddr });
    }),
  );
};

const requireAbsoluteFilePath = (
  value: unknown,
  path: string,
  durable: boolean,
): string => {
  const filePath = exactString(value, path, { maxLength: 4_096 });
  const normalized = normalize(filePath);
  if (
    !isAbsolute(filePath) ||
    normalized !== filePath ||
    normalized === "/" ||
    normalized.includes("\0") ||
    normalized === "/tmp" ||
    normalized.startsWith("/tmp/")
  ) {
    fail("unsafe_path", path);
  }
  if (
    durable &&
    (normalized === "/dev" ||
      normalized.startsWith("/dev/") ||
      normalized === "/proc" ||
      normalized.startsWith("/proc/") ||
      normalized === "/run" ||
      normalized.startsWith("/run/") ||
      normalized === "/sys" ||
      normalized.startsWith("/sys/"))
  ) {
    fail("unsafe_path", path);
  }
  return filePath;
};

const parseKeySource = (
  value: unknown,
  path: string,
): WatcherWalletKeySource => {
  if (typeof value === "string") {
    fail("inline_secret_forbidden", path);
  }
  const preliminary = plainRecord(value, path);
  const kind = preliminary.kind;
  if (kind === "environment") {
    const record = exactRecord(value, path, ["kind", "variable"]);
    return Object.freeze({
      kind,
      variable: exactString(record.variable, `${path}.variable`, {
        maxLength: 128,
        pattern: ENVIRONMENT_VARIABLE_PATTERN,
      }),
    });
  }
  if (kind === "file") {
    const record = exactRecord(value, path, ["kind", "path"]);
    return Object.freeze({
      kind,
      path: requireAbsoluteFilePath(record.path, `${path}.path`, false),
    });
  }
  fail("invalid_value", `${path}.kind`);
};

const parseL1Source = (
  value: unknown,
  mode: WatcherConfigMode,
): WatcherL1SourceConfig => {
  const preliminary = plainRecord(value, "$.l1.source");
  if (preliminary.sourceMode === "local_node") {
    const source = exactRecord(value, "$.l1.source", [
      "sourceMode",
      "authorityNodeId",
      "chainSync",
      "queryServices",
    ]);
    const chainSync = exactRecord(source.chainSync, "$.l1.source.chainSync", [
      "kind",
      "socketPath",
      "nodeConfigPath",
      "genesisConfigPath",
      "genesisIdentitySha256",
    ]);
    if (chainSync.kind !== "cardano_node_socket") {
      fail("invalid_value", "$.l1.source.chainSync.kind");
    }
    return Object.freeze({
      sourceMode: "local_node",
      authorityNodeId: exactString(
        source.authorityNodeId,
        "$.l1.source.authorityNodeId",
        { maxLength: 32, pattern: IDENTITY_PATTERN },
      ),
      chainSync: Object.freeze({
        kind: "cardano_node_socket",
        socketPath: requireAbsoluteFilePath(
          chainSync.socketPath,
          "$.l1.source.chainSync.socketPath",
          false,
        ),
        nodeConfigPath: requireAbsoluteFilePath(
          chainSync.nodeConfigPath,
          "$.l1.source.chainSync.nodeConfigPath",
          false,
        ),
        genesisConfigPath: requireAbsoluteFilePath(
          chainSync.genesisConfigPath,
          "$.l1.source.chainSync.genesisConfigPath",
          false,
        ),
        genesisIdentitySha256: exactString(
          chainSync.genesisIdentitySha256,
          "$.l1.source.chainSync.genesisIdentitySha256",
          { maxLength: 64, pattern: HEX_32_PATTERN },
        ),
      }),
      queryServices: parseLocalNodeQueryServices(source.queryServices),
    });
  }
  if (preliminary.sourceMode === "external_providers") {
    const source = exactRecord(value, "$.l1.source", [
      "sourceMode",
      "providers",
    ]);
    return Object.freeze({
      sourceMode: "external_providers",
      providers: parseProviders(source.providers, mode),
    });
  }
  fail("invalid_value", "$.l1.source.sourceMode");
};

export const parseWatcherConfig = (value: unknown): WatcherConfig => {
  if (
    typeof value === "object" &&
    value !== null &&
    admittedWatcherConfigs.has(value)
  ) {
    return value as WatcherConfig;
  }
  const root = exactRecord(value, "$", [
    "schemaVersion",
    "mode",
    "targetNetwork",
    "l1",
    "da",
    "storage",
    "proverWallet",
    "deadlines",
  ]);
  if (root.schemaVersion !== WATCHER_CONFIG_SCHEMA_VERSION) {
    fail("invalid_value", "$.schemaVersion");
  }
  const mode = enumValue(root.mode, "$.mode", [
    "development",
    "acceptance",
  ] as const);
  const targetNetwork = enumValue(root.targetNetwork, "$.targetNetwork", [
    "Mainnet",
    "Preprod",
    "Preview",
  ] as const);

  const l1 = exactRecord(root.l1, "$.l1", [
    "source",
    "requestTimeoutMs",
    "maxConcurrency",
    "finality",
  ]);
  const finality = exactRecord(l1.finality, "$.l1.finality", [
    "depth",
    "rollback",
  ]);
  const rollback = exactRecord(finality.rollback, "$.l1.finality.rollback", [
    "beforeFinality",
    "afterFinality",
    "maxDepth",
  ]);
  const finalityDepth = boundedInteger(
    finality.depth,
    "$.l1.finality.depth",
    WATCHER_CONFIG_BOUNDS.finalityDepth,
  );
  const rollbackMaxDepth = boundedInteger(
    rollback.maxDepth,
    "$.l1.finality.rollback.maxDepth",
    WATCHER_CONFIG_BOUNDS.rollbackDepth,
  );
  if (rollbackMaxDepth > finalityDepth) {
    fail("out_of_bounds", "$.l1.finality.rollback.maxDepth");
  }
  const l1RequestTimeoutMs = boundedInteger(
    l1.requestTimeoutMs,
    "$.l1.requestTimeoutMs",
    WATCHER_CONFIG_BOUNDS.requestTimeoutMs,
  );

  const da = exactRecord(root.da, "$.da", [
    "peers",
    "requestTimeoutMs",
    "maxConcurrency",
  ]);
  const daRequestTimeoutMs = boundedInteger(
    da.requestTimeoutMs,
    "$.da.requestTimeoutMs",
    WATCHER_CONFIG_BOUNDS.requestTimeoutMs,
  );

  const storage = exactRecord(root.storage, "$.storage", [
    "driver",
    "path",
    "rollbackAuthorityKeySource",
  ]);
  if (storage.driver !== "sqlite") {
    fail("invalid_value", "$.storage.driver");
  }

  const proverWallet = exactRecord(root.proverWallet, "$.proverWallet", [
    "keySource",
  ]);

  const deadlines = exactRecord(root.deadlines, "$.deadlines", [
    "daFetchMs",
    "daPublishMs",
    "proofConstructMs",
    "proofSubmitMs",
  ]);
  const daFetchMs = boundedInteger(
    deadlines.daFetchMs,
    "$.deadlines.daFetchMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const daPublishMs = boundedInteger(
    deadlines.daPublishMs,
    "$.deadlines.daPublishMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const proofConstructMs = boundedInteger(
    deadlines.proofConstructMs,
    "$.deadlines.proofConstructMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  const proofSubmitMs = boundedInteger(
    deadlines.proofSubmitMs,
    "$.deadlines.proofSubmitMs",
    WATCHER_CONFIG_BOUNDS.deadlineMs,
  );
  if (daFetchMs < daRequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.daFetchMs");
  }
  if (daPublishMs < daRequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.daPublishMs");
  }
  if (proofSubmitMs < l1RequestTimeoutMs) {
    fail("out_of_bounds", "$.deadlines.proofSubmitMs");
  }
  const rollbackAuthorityKeySource = parseKeySource(
    storage.rollbackAuthorityKeySource,
    "$.storage.rollbackAuthorityKeySource",
  );
  const proverWalletKeySource = parseKeySource(
    proverWallet.keySource,
    "$.proverWallet.keySource",
  );
  if (
    rollbackAuthorityKeySource.kind === proverWalletKeySource.kind &&
    (rollbackAuthorityKeySource.kind === "environment"
      ? rollbackAuthorityKeySource.variable ===
        (
          proverWalletKeySource as Extract<
            WatcherWalletKeySource,
            { readonly kind: "environment" }
          >
        ).variable
      : rollbackAuthorityKeySource.path ===
        (
          proverWalletKeySource as Extract<
            WatcherWalletKeySource,
            { readonly kind: "file" }
          >
        ).path)
  ) {
    fail("secret_source_alias", "$.storage.rollbackAuthorityKeySource");
  }

  const admitted = Object.freeze({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode,
    targetNetwork,
    l1: Object.freeze({
      source: parseL1Source(l1.source, mode),
      requestTimeoutMs: l1RequestTimeoutMs,
      maxConcurrency: boundedInteger(
        l1.maxConcurrency,
        "$.l1.maxConcurrency",
        WATCHER_CONFIG_BOUNDS.concurrency,
      ),
      finality: Object.freeze({
        depth: finalityDepth,
        rollback: Object.freeze({
          beforeFinality: enumValue(
            rollback.beforeFinality,
            "$.l1.finality.rollback.beforeFinality",
            ["rewind"] as const,
          ),
          afterFinality: enumValue(
            rollback.afterFinality,
            "$.l1.finality.rollback.afterFinality",
            ["quarantine"] as const,
          ),
          maxDepth: rollbackMaxDepth,
          postFinalityRecoveryMaxDepth: WATCHER_CARDANO_SECURITY_PARAMETER_K,
        }),
      }),
    }),
    da: Object.freeze({
      peers: parseDaPeers(da.peers),
      requestTimeoutMs: daRequestTimeoutMs,
      maxConcurrency: boundedInteger(
        da.maxConcurrency,
        "$.da.maxConcurrency",
        WATCHER_CONFIG_BOUNDS.concurrency,
      ),
    }),
    storage: Object.freeze({
      driver: "sqlite",
      path: requireAbsoluteFilePath(storage.path, "$.storage.path", true),
      rollbackAuthorityKeySource,
    }),
    proverWallet: Object.freeze({
      keySource: proverWalletKeySource,
    }),
    deadlines: Object.freeze({
      daFetchMs,
      daPublishMs,
      proofConstructMs,
      proofSubmitMs,
    }),
  });
  admittedWatcherConfigs.add(admitted);
  return admitted;
};

class StrictJsonReader {
  readonly #source: string;
  #position = 0;

  constructor(source: string) {
    this.#source = source;
  }

  parse(): unknown {
    this.#skipWhitespace();
    const value = this.#parseValue("$");
    this.#skipWhitespace();
    if (this.#position !== this.#source.length) {
      fail("malformed_json", "$");
    }
    return value;
  }

  #parseValue(path: string): unknown {
    this.#skipWhitespace();
    const character = this.#source[this.#position];
    if (character === "{") {
      return this.#parseObject(path);
    }
    if (character === "[") {
      return this.#parseArray(path);
    }
    if (character === '"') {
      return this.#parseString(path);
    }
    if (character === "t") {
      return this.#parseLiteral("true", true, path);
    }
    if (character === "f") {
      return this.#parseLiteral("false", false, path);
    }
    if (character === "n") {
      return this.#parseLiteral("null", null, path);
    }
    if (
      character === "-" ||
      (character !== undefined && /\d/u.test(character))
    ) {
      return this.#parseNumber(path);
    }
    fail("malformed_json", path);
  }

  #parseObject(path: string): Record<string, unknown> {
    this.#position += 1;
    this.#skipWhitespace();
    const result: Record<string, unknown> = Object.create(null) as Record<
      string,
      unknown
    >;
    const keys = new Set<string>();
    if (this.#source[this.#position] === "}") {
      this.#position += 1;
      return result;
    }
    while (this.#position < this.#source.length) {
      if (this.#source[this.#position] !== '"') {
        fail("malformed_json", path);
      }
      const key = this.#parseString(path);
      if (keys.has(key)) {
        fail("duplicate_field", path);
      }
      keys.add(key);
      this.#skipWhitespace();
      if (this.#source[this.#position] !== ":") {
        fail("malformed_json", path);
      }
      this.#position += 1;
      result[key] = this.#parseValue(path);
      this.#skipWhitespace();
      const delimiter = this.#source[this.#position];
      if (delimiter === "}") {
        this.#position += 1;
        return result;
      }
      if (delimiter !== ",") {
        fail("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    fail("malformed_json", path);
  }

  #parseArray(path: string): readonly unknown[] {
    this.#position += 1;
    this.#skipWhitespace();
    const result: unknown[] = [];
    if (this.#source[this.#position] === "]") {
      this.#position += 1;
      return result;
    }
    while (this.#position < this.#source.length) {
      result.push(this.#parseValue(`${path}[${result.length.toString()}]`));
      this.#skipWhitespace();
      const delimiter = this.#source[this.#position];
      if (delimiter === "]") {
        this.#position += 1;
        return result;
      }
      if (delimiter !== ",") {
        fail("malformed_json", path);
      }
      this.#position += 1;
      this.#skipWhitespace();
    }
    fail("malformed_json", path);
  }

  #parseString(path: string): string {
    const start = this.#position;
    this.#position += 1;
    let escaped = false;
    while (this.#position < this.#source.length) {
      const character = this.#source[this.#position]!;
      if (!escaped && character === '"') {
        this.#position += 1;
        try {
          return JSON.parse(
            this.#source.slice(start, this.#position),
          ) as string;
        } catch {
          fail("malformed_json", path);
        }
      }
      if (!escaped && character.charCodeAt(0) < 0x20) {
        fail("malformed_json", path);
      }
      if (!escaped && character === "\\") {
        escaped = true;
      } else {
        escaped = false;
      }
      this.#position += 1;
    }
    fail("malformed_json", path);
  }

  #parseNumber(path: string): number {
    const match = /^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?/u.exec(
      this.#source.slice(this.#position),
    );
    if (match === null) {
      fail("malformed_json", path);
    }
    this.#position += match[0].length;
    const number = Number(match[0]);
    if (!Number.isFinite(number)) {
      fail("unsafe_value", path);
    }
    return number;
  }

  #parseLiteral<T>(literal: string, value: T, path: string): T {
    if (!this.#source.startsWith(literal, this.#position)) {
      fail("malformed_json", path);
    }
    this.#position += literal.length;
    return value;
  }

  #skipWhitespace(): void {
    while (
      this.#position < this.#source.length &&
      /\s/u.test(this.#source[this.#position]!)
    ) {
      this.#position += 1;
    }
  }
}

export const parseWatcherConfigJson = (source: string): WatcherConfig => {
  if (
    typeof source !== "string" ||
    source.length < WATCHER_CONFIG_BOUNDS.configJsonBytes.min ||
    Buffer.byteLength(source, "utf8") >
      WATCHER_CONFIG_BOUNDS.configJsonBytes.max
  ) {
    fail("out_of_bounds", "$");
  }
  return parseWatcherConfig(new StrictJsonReader(source).parse());
};

/** Duplicate-key rejecting JSON admission shared by production identity files. */
export const parseWatcherStrictJsonValue = (source: string): unknown =>
  new StrictJsonReader(source).parse();

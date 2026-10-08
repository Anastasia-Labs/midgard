import { type L1Origin } from "@al-ft/midgard-core/l1-origin";

import { type WatcherCustomNetwork } from "./custom-network.js";

export const WATCHER_CONFIG_SCHEMA_VERSION =
  "midgard-watcher-config-v1" as const;

export const WATCHER_CARDANO_SECURITY_PARAMETER_K = 2_160 as const;

export const WATCHER_CONFIG_BOUNDS = {
  configJsonBytes: { min: 2, max: 262_144 },
  daPeers: { min: 1, max: 32 },
  requestTimeoutMs: { min: 100, max: 120_000 },
  concurrency: { min: 1, max: 64 },
  finalityDepth: { min: 1, max: 2_160 },
  deadlineMs: { min: 1_000, max: 86_400_000 },
  l1OriginSlot: { min: 0, max: Number.MAX_SAFE_INTEGER },
} as const;

export type WatcherConfigMode = "development" | "acceptance";

export type WatcherTargetNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

/** The watcher's L1 source: its own cardano-node, read through the follower. */
export type WatcherL1SourceConfig = Readonly<{
  sourceMode: "local_node";
  authorityNodeId: string;
  chainSync: Readonly<{
    kind: "cardano_node_socket";
    socketPath: string;
    nodeConfigPath: string;
    genesisConfigPath: string;
    genesisIdentitySha256: string;
  }>;
}>;

export type WatcherL1Config = Readonly<{
  source: WatcherL1SourceConfig;
  /**
   * Operator override of the deployment's L1 origin: the point immediately
   * before the block holding the prepareHubOracleNonce tx, where the L1
   * follower starts. Absent means the deployment's own origin applies.
   */
  origin?: L1Origin;
  requestTimeoutMs: number;
  maxConcurrency: number;
  finality: Readonly<{
    depth: number;
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
  customNetwork?: WatcherCustomNetwork;
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

export const admittedWatcherConfigs = new WeakSet<object>();

export function fail(code: WatcherConfigErrorCode, path: string): never {
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

export const plainRecord = (
  value: unknown,
  path: string,
): Record<string, unknown> => {
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

export const exactRecord = (
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

export const exactString = (
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

export const enumValue = <const Values extends readonly string[]>(
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

export const boundedInteger = (
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

export const boundedArray = (
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

export const IDENTITY_PATTERN = /^[a-z][a-z0-9-]{2,31}$/u;

export const HEX_32_PATTERN = /^[0-9a-f]{64}$/u;

export const ENVIRONMENT_VARIABLE_PATTERN = /^[A-Z][A-Z0-9_]{2,127}$/u;

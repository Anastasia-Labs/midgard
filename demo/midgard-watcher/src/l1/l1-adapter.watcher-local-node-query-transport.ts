import { type Socket } from "node:net";
import { type TLSSocket } from "node:tls";

export const WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION =
  "midgard-watcher-authenticated-l1-provider-v1" as const;

export const WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-l1-block-observation-v1" as const;

export const WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION =
  "midgard-watcher-normalized-l1-block-v1" as const;

export const WATCHER_L1_NORMALIZATION_SESSION_SCHEMA_VERSION =
  "midgard-watcher-l1-normalization-session-v1" as const;

export const WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_SCHEMA_VERSION =
  "midgard-watcher-l1-transport-attestation-context-v1" as const;

export const WATCHER_L1_ADAPTER_BOUNDS = Object.freeze({
  arrayMembers: 4_096,
  totalCollectionMembers: 65_536,
  publicBytes: 1_048_576,
  totalPublicBytes: 67_108_864,
  normalizationSessionEntries: 256,
  normalizationSessionBytes: 16_777_216,
});

export const WATCHER_L1_SCRIPT_LANGUAGES = [
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
] as const;

export const WATCHER_L1_REDEEMER_PURPOSES = [
  "spend",
  "mint",
  "certificate",
  "withdrawal",
  "vote",
  "propose",
] as const;

export const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;

export const WATCHER_L1_SOURCE_MODES = [
  "local_node",
  "external_providers",
] as const;

export const WATCHER_LOCAL_NODE_SURFACES = [
  "chain_sync",
  "ogmios",
  "kupo",
  "kupmios",
  "db_sync",
] as const;

export const AUTHENTICATION_KINDS = [
  "https_tls_identity_v1",
  "cardano_node_genesis_v1",
  "local_endpoint_identity_v1",
  "local_capture_identity_v1",
] as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const PROVIDER_ID = /^[a-z][a-z0-9-]{0,62}$/u;

export const UINT64_MAX = 18_446_744_073_709_551_615n;

export type WatcherL1Network = (typeof NETWORKS)[number];

export type WatcherL1SourceModeV1 = (typeof WATCHER_L1_SOURCE_MODES)[number];

export type WatcherLocalNodeSurface =
  (typeof WATCHER_LOCAL_NODE_SURFACES)[number];

export type WatcherL1SourceIdentity =
  | Readonly<{
      sourceMode: "local_node";
      authorityNodeId: string;
      surface: WatcherLocalNodeSurface;
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
type WatcherAuthenticatedL1ProviderBase = Readonly<{
  schemaVersion: typeof WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION;
  network: WatcherL1Network;
  providerId: string;
  authentication: Readonly<{
    kind: (typeof AUTHENTICATION_KINDS)[number];
    publicIdentitySha256: string;
  }>;
}>;

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
export type WatcherL1TransportAttestationContext = Readonly<{
  schemaVersion: typeof WATCHER_L1_TRANSPORT_ATTESTATION_CONTEXT_SCHEMA_VERSION;
  attestationDigest: string;
}>;

export type WatcherLocalNodeQueryTransport =
  | Readonly<{
      transportKind: "tcp";
      providerId: string;
      surface: Exclude<WatcherLocalNodeSurface, "chain_sync">;
      /** Exact W01 HTTP, WS, or PostgreSQL endpoint. */
      endpoint: string;
      connectTimeoutMs: number;
    }>
  | Readonly<{
      transportKind: "https_tls";
      providerId: string;
      surface: Exclude<WatcherLocalNodeSurface, "chain_sync">;
      /** Exact W01 HTTPS or WSS endpoint; host, port and SNI are derived. */
      endpoint: string;
      caPem: string;
      expectedTlsPublicIdentitySha256: string;
      connectTimeoutMs: number;
    }>;

export type WatcherExternalProviderTransport = Readonly<{
  network: WatcherL1Network;
  providerId: string;
  operatorIdentitySha256: string;
  /** Exact W01 HTTPS endpoint; host, port and SNI are derived from it. */
  endpoint: string;
  caPem: string;
  expectedTlsPublicIdentitySha256: string;
  connectTimeoutMs: number;
}>;

export type WatcherL1TransportAttestationDetails = Readonly<{
  provider: WatcherNormalizedAuthenticatedL1Provider;
  authorityBindingSha256: string | null;
  /**
   * Exact configured W01 transport location used to establish the live
   * capability. W11 compares this byte-for-byte with its configured source.
   */
  transportEndpoint: string;
}>;

export type WatcherNormalizedAuthenticatedL1Provider = Readonly<
  WatcherAuthenticatedL1ProviderBase & {
    source: WatcherL1SourceIdentity;
  }
>;

export type WatcherL1PublicBytes = Readonly<{
  bytesHex: string;
  sha256: string;
}>;

export type WatcherL1Script = Readonly<{
  scriptHash: string;
  language: (typeof WATCHER_L1_SCRIPT_LANGUAGES)[number];
  bytes: WatcherL1PublicBytes;
}>;

export type WatcherL1Datum = Readonly<{
  datumHash: string;
  bytes: WatcherL1PublicBytes;
}>;

export type WatcherL1Redeemer = Readonly<{
  purpose: (typeof WATCHER_L1_REDEEMER_PURPOSES)[number];
  index: string;
  bytes: WatcherL1PublicBytes;
}>;

export type WatcherL1Utxo = Readonly<{
  outRef: string;
  outputIndex: string;
  output: WatcherL1PublicBytes;
  datum: WatcherL1Datum | null;
  referenceScript: WatcherL1Script | null;
}>;

export type WatcherL1Transaction = Readonly<{
  txHash: string;
  transactionIndex?: string;
  isValid: boolean;
  /** Exact accepted frame with retained ledger child encodings. */
  fullTransaction: WatcherL1PublicBytes;
  /** Exact decoded body encoding committed to by txHash. */
  body: WatcherL1PublicBytes;
  /** Exact decoded witness-set encoding. */
  witnessSet: WatcherL1PublicBytes;
  /** Canonical derived views of applied outputs and witness material. */
  utxos: readonly WatcherL1Utxo[];
  scripts: readonly WatcherL1Script[];
  datums: readonly WatcherL1Datum[];
  redeemers: readonly WatcherL1Redeemer[];
}>;

export type WatcherL1ChainPoint = Readonly<{
  chainPointId: string;
  pointDigest: string;
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  depth: string;
}>;

export type WatcherNormalizedL1Block = Readonly<{
  schemaVersion: typeof WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION;
  network: WatcherL1Network;
  provider: WatcherNormalizedAuthenticatedL1Provider;
  chainPoint: WatcherL1ChainPoint;
  transactions: readonly WatcherL1Transaction[];
  blockContentDigest: string;
  observationDigest: string;
}>;

export type WatcherL1NormalizationSession = Readonly<{
  schemaVersion: typeof WATCHER_L1_NORMALIZATION_SESSION_SCHEMA_VERSION;
}>;

export type WatcherDerivedL1Transaction = Readonly<{
  fullTransaction: WatcherL1PublicBytes;
  body: WatcherL1PublicBytes;
  txHash: string;
  isValid: boolean;
  witnessSet: WatcherL1PublicBytes;
  utxos: readonly WatcherL1Utxo[];
  scripts: readonly WatcherL1Script[];
  datums: readonly WatcherL1Datum[];
  redeemers: readonly WatcherL1Redeemer[];
}>;

export type WatcherL1NormalizationSessionState = {
  readonly transactions: Map<
    string,
    Readonly<{
      bytesHex: string;
      retainedBytes: number;
      derived: WatcherDerivedL1Transaction;
    }>
  >;
  retainedBytes: number;
};

export type WatcherL1NormalizationSessionStats = Readonly<{
  retainedEntries: number;
  retainedBytes: number;
  maximumEntries: number;
  maximumBytes: number;
}>;

export const normalizationSessionStates = new WeakMap<
  WatcherL1NormalizationSession,
  WatcherL1NormalizationSessionState
>();

export type WatcherL1TransportAttestationState = {
  details: WatcherL1TransportAttestationDetails;
  transports: readonly (Socket | TLSSocket)[];
  ownedTransports: readonly (Socket | TLSSocket)[];
  upstreamIsLive: () => boolean;
  active: boolean;
  renewalTimer?: ReturnType<typeof setTimeout>;
};

export const transportAttestationStates = new WeakMap<
  WatcherL1TransportAttestationContext,
  WatcherL1TransportAttestationState
>();

export const normalizedBlockProvenance = new WeakMap<
  object,
  WatcherL1TransportAttestationContext
>();

export const makeWatcherL1NormalizationSession =
  (): WatcherL1NormalizationSession => {
    const session = Object.freeze({
      schemaVersion: WATCHER_L1_NORMALIZATION_SESSION_SCHEMA_VERSION,
    });
    normalizationSessionStates.set(session, {
      transactions: new Map(),
      retainedBytes: 0,
    });
    return session;
  };

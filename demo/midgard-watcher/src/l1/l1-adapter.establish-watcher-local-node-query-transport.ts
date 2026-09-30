import { type Socket } from "node:net";
import { type TLSSocket } from "node:tls";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  type CanonicalJson,
  closeWatcherL1TransportAttestationContext,
  exactLiteral,
  exactRecord,
  exactString,
  fail,
  plainRecord,
  watcherL1TransportAttestationDetails,
} from "./l1-adapter.exact-array.js";
import {
  establishTcpSocket,
  establishTlsSocket,
  type ExactTcpEndpoint,
  exactTcpEndpoint,
  exactTlsEndpoint,
  LOCAL_QUERY_TRANSPORT_RENEWAL_MS,
} from "./l1-adapter.exact-tls-endpoint.js";
import {
  makeTransportAttestationContext,
  parseAuthenticatedProvider,
  providerJson,
} from "./l1-adapter.parse-authenticated-provider.js";
import {
  HEX_32,
  NETWORKS,
  PROVIDER_ID,
  transportAttestationStates,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_LOCAL_NODE_SURFACES,
  type WatcherExternalProviderTransport,
  type WatcherL1Network,
  type WatcherL1SourceIdentity,
  type WatcherL1Transaction,
  type WatcherL1TransportAttestationContext,
  type WatcherL1TransportAttestationDetails,
  type WatcherL1TransportAttestationState,
  type WatcherLocalNodeQueryTransport,
  type WatcherLocalNodeSurface,
  type WatcherNormalizedAuthenticatedL1Provider,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.watcher-local-node-query-transport.js";

const maintainLocalQueryTransport = (
  context: WatcherL1TransportAttestationContext,
  endpoint: ExactTcpEndpoint,
  upstreamTransports: readonly (Socket | TLSSocket)[],
  identitySha256: string,
): void => {
  const state = transportAttestationStates.get(context)!;
  const schedule = () => {
    if (!state.active) return;
    state.renewalTimer = setTimeout(() => {
      void renew().catch(() =>
        closeWatcherL1TransportAttestationContext(context),
      );
    }, LOCAL_QUERY_TRANSPORT_RENEWAL_MS);
    state.renewalTimer.unref();
  };
  const renew = async () => {
    if (watcherL1TransportAttestationDetails(context) === null) {
      closeWatcherL1TransportAttestationContext(context);
      return;
    }
    const previous = state.ownedTransports[0]!;
    const candidate = await establishTcpSocket(
      endpoint,
      "$.localNodeQueryTransport",
      (socket) => {
        // Owner close also owns a candidate that has not connected yet.
        state.ownedTransports = Object.freeze([previous, socket]);
      },
    );
    if (
      watcherL1TransportAttestationDetails(context) === null ||
      candidate.identitySha256 !== identitySha256 ||
      candidate.socket.destroyed ||
      !candidate.socket.readable ||
      !candidate.socket.writable ||
      candidate.socket.readyState !== "open"
    ) {
      closeWatcherL1TransportAttestationContext(context);
      return;
    }
    state.transports = Object.freeze([...upstreamTransports, candidate.socket]);
    // Retain the retired socket until physical close, bounding ownership to
    // two sockets even when close delivery is delayed. No next rollover starts
    // before this close acknowledgement.
    previous.once("close", () => {
      state.ownedTransports = Object.freeze([candidate.socket]);
      schedule();
    });
    previous.destroy();
  };
  schedule();
};

export const establishWatcherLocalNodeQueryTransport = async (
  authorityContext: WatcherL1TransportAttestationContext,
  input: WatcherLocalNodeQueryTransport,
): Promise<WatcherL1TransportAttestationContext> => {
  const authority = watcherL1TransportAttestationDetails(authorityContext);
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
  const trustedAuthority = authority as WatcherL1TransportAttestationDetails & {
    readonly authorityBindingSha256: string;
    readonly provider: WatcherNormalizedAuthenticatedL1Provider & {
      readonly source: Extract<
        WatcherL1SourceIdentity,
        { readonly sourceMode: "local_node" }
      >;
    };
  };
  const trustedAuthorityState =
    authorityState as WatcherL1TransportAttestationState;
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
    WATCHER_LOCAL_NODE_SURFACES.filter(
      (candidate) => candidate !== "chain_sync",
    ),
  ) as Exclude<WatcherLocalNodeSurface, "chain_sync">;
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
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
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
  const context = makeTransportAttestationContext(
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
  if (tcpEndpoint !== null) {
    maintainLocalQueryTransport(
      context,
      tcpEndpoint,
      trustedAuthorityState.transports,
      established.identitySha256,
    );
  }
  return context;
};

export const establishWatcherExternalProviderTransport = async (
  input: WatcherExternalProviderTransport,
): Promise<WatcherL1TransportAttestationContext> => {
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
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
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

const transactionJson = (transaction: WatcherL1Transaction): CanonicalJson => ({
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

export const contentJson = (input: {
  network: WatcherL1Network;
  pointDigest: string;
  blockHash: string;
  parentBlockHash: string | null;
  slot: string;
  blockNo: string;
  transactions: readonly WatcherL1Transaction[];
}): CanonicalJson => ({
  network: input.network,
  pointDigest: input.pointDigest,
  blockHash: input.blockHash,
  parentBlockHash: input.parentBlockHash,
  slot: input.slot,
  blockNo: input.blockNo,
  transactions: input.transactions.map(transactionJson),
});

export const encodeWatcherNormalizedL1Block = (
  value: WatcherNormalizedL1Block,
): Buffer =>
  Buffer.from(
    watcherCanonicalJson({
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

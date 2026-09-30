import "./l1-adapter.parse-utxo.js";

import { createHash } from "node:crypto";
import { createConnection as createNetConnection, type Socket } from "node:net";
import {
  connect as connectTls,
  type ConnectionOptions,
  type TLSSocket,
} from "node:tls";

import {
  digestCanonicalJson,
  exactString,
  fail,
  type JsonRecord,
} from "./l1-adapter.exact-array.js";
import {
  makeTransportAttestationContext,
  parseAuthenticatedProvider,
} from "./l1-adapter.parse-authenticated-provider.js";
import {
  HEX_32,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
} from "./l1-adapter.watcher-local-node-query-transport.js";
import {
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
} from "./native-chain-sync.js";

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

export const exactTlsEndpoint = (
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

export type ExactTcpEndpoint = Readonly<{
  endpoint: string;
  host: string;
  port: number;
  connectTimeoutMs: number;
}>;

export const exactTcpEndpoint = (
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
      socket.off("close", onClose);
      if (error === undefined) resolve(socket);
      else {
        socket.destroy();
        reject(error);
      }
    };
    const onReady = (): void => finish();
    const onError = (): void =>
      finish(new Error("transport connection failed"));
    const onClose = (): void =>
      finish(new Error("transport closed before connection"));
    const timer = setTimeout(
      () => finish(new Error("transport connection timed out")),
      timeoutMs,
    );
    timer.unref();
    socket.once(readyEvent, onReady);
    socket.once("error", onError);
    socket.once("close", onClose);
  });

export const establishTcpSocket = async (
  endpoint: ExactTcpEndpoint,
  path: string,
  onConnecting?: (socket: Socket) => void,
): Promise<Readonly<{ socket: Socket; identitySha256: string }>> => {
  let socket: Socket | null = null;
  try {
    const connecting = createNetConnection({
      host: endpoint.host,
      port: endpoint.port,
    });
    onConnecting?.(connecting);
    socket = await awaitConnectedSocket(
      connecting,
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

export const establishTlsSocket = async (
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
export const establishWatcherLocalNodeAuthorityTransport = (
  nativeAuthority: WatcherNativeChainSyncAuthority,
): WatcherL1TransportAttestationContext => {
  const details =
    watcherNativeChainSyncAuthorityDetails(nativeAuthority) ??
    fail("invalid_field", "$.nativeChainSyncAuthority");
  const provider = parseAuthenticatedProvider({
    schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
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
    () => watcherNativeChainSyncAuthorityDetails(nativeAuthority) !== null,
  );
};

// The endpoint-identity sockets are independent of individual HTTP reads.
// Rotate while both connections are live, before an idle HTTP server retires
// the old socket. This maintains authority; it never revives a lost context.
export const LOCAL_QUERY_TRANSPORT_RENEWAL_MS = 30_000;

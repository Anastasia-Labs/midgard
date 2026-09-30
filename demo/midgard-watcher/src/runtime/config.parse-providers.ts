import { isIP } from "node:net";

import {
  boundedArray,
  enumValue,
  exactRecord,
  exactString,
  fail,
  HEX_32_PATTERN,
  IDENTITY_PATTERN,
  WATCHER_CONFIG_BOUNDS,
  type WatcherConfigMode,
  type WatcherDaPeerConfig,
  type WatcherL1ProviderConfig,
  type WatcherLocalNodeQueryServiceConfig,
  type WatcherTargetNetwork,
} from "./config.watcher-config.js";

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

export const DA_MULTIADDR_PATTERN =
  /^\/dns(4|6)\/([a-z0-9](?:[a-z0-9.-]{0,251}[a-z0-9])?)\/tcp\/([1-9][0-9]{0,4})\/p2p\/([1-9A-HJ-NP-Za-km-z]{20,128})$/u;

const parseDaMultiaddr = (
  value: unknown,
  path: string,
  network: WatcherTargetNetwork,
): Readonly<{ multiaddr: string; aliasKey: string; peerId: string }> => {
  const multiaddr = exactString(value, path, { maxLength: 512 });
  if (network === "Custom") {
    const local =
      /^\/ip4\/([0-9.]+)\/tcp\/([1-9][0-9]{0,4})\/p2p\/([1-9A-HJ-NP-Za-km-z]{20,128})$/u.exec(
        multiaddr,
      );
    if (local !== null) {
      const [, address, portText, peerId] = local;
      const port = Number(portText);
      if (isIP(address!) !== 4 || port > 65_535) fail("invalid_endpoint", path);
      return {
        multiaddr,
        peerId: peerId!,
        aliasKey: `${address}:${port}:${peerId}`,
      };
    }
  }
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
    peerId,
    aliasKey: `${hostname}:${port.toString()}:${peerId}`,
  };
};

export const parseProviders = (
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

export const parseLocalNodeQueryServices = (
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

export const parseDaPeers = (
  value: unknown,
  network: WatcherTargetNetwork,
): readonly WatcherDaPeerConfig[] => {
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
      const endpoint = parseDaMultiaddr(
        record.multiaddr,
        `${path}.multiaddr`,
        network,
      );
      if (identities.has(identity) || endpoints.has(endpoint.aliasKey)) {
        fail("provider_alias", path);
      }
      identities.add(identity);
      endpoints.add(endpoint.aliasKey);
      const peerId = endpoint.peerId;
      return Object.freeze({ identity, peerId, multiaddr: endpoint.multiaddr });
    }),
  );
};

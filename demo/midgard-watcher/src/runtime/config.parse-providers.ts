import { isIP } from "node:net";

import {
  boundedArray,
  exactRecord,
  exactString,
  fail,
  IDENTITY_PATTERN,
  WATCHER_CONFIG_BOUNDS,
  type WatcherDaPeerConfig,
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

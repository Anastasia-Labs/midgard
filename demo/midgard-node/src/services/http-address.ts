/**
 * IP addresses as the HTTP ingress keys them, and the trusted proxy ranges
 * (HTTP_TRUSTED_PROXY_PEERS) whose connections may name another client.
 */
import { isIP } from "node:net";

const ipv6Words = (part: string): number[] =>
  part === ""
    ? []
    : part.split(":").flatMap((group) => {
        if (!group.includes(".")) return [Number.parseInt(group, 16)];
        const [a = 0, b = 0, c = 0, d = 0] = group.split(".").map(Number);
        return [a * 256 + b, c * 256 + d];
      });

/**
 * The 4 bytes of an IPv4 address or the 16 of an IPv6 address. An
 * IPv4-mapped IPv6 address is the IPv4 address it maps.
 */
export const httpAddressBytes = (address: string): Uint8Array => {
  const family = isIP(address);
  if (family === 4) return Uint8Array.from(address.split(".").map(Number));
  if (family !== 6) throw new Error(`Not an IP address: ${address}`);
  const [head = "", tail] = address.split("%", 1)[0]!.split("::");
  const left = ipv6Words(head);
  const right = tail === undefined ? [] : ipv6Words(tail);
  const words = [
    ...left,
    ...Array<number>(8 - left.length - right.length).fill(0),
    ...right,
  ];
  const bytes = new Uint8Array(16);
  words.forEach((word, index) => {
    bytes[2 * index] = word >> 8;
    bytes[2 * index + 1] = word & 255;
  });
  const mapped =
    bytes.subarray(0, 10).every((byte) => byte === 0) &&
    bytes[10] === 255 &&
    bytes[11] === 255;
  return mapped ? bytes.slice(12) : bytes;
};

/** Entries HTTP_TRUSTED_PROXY_PEERS may list: room for a cloud provider's published load-balancer ranges. */
export const HTTP_TRUSTED_PROXY_MAX_ENTRIES = 64;
/**
 * The widest range an entry may name. A wider one would let much of the
 * internet name its own client and mint quotas.
 */
const MIN_PREFIX_BITS = { ipv4: 8, ipv6: 24 } as const;

type ProxyRange = { readonly bytes: Uint8Array; readonly bits: number };

/** Whether the first `bits` bits of `address` equal those of `range`. */
const inRange = (address: Uint8Array, range: ProxyRange): boolean => {
  if (address.length !== range.bytes.length) return false;
  const whole = range.bits >> 3;
  for (let index = 0; index < whole; index += 1)
    if (address[index] !== range.bytes[index]) return false;
  const rest = range.bits & 7;
  if (rest === 0) return true;
  const mask = (0xff << (8 - rest)) & 0xff;
  return (address[whole]! & mask) === (range.bytes[whole]! & mask);
};

/** One entry: an address, or a range written with no host bits set. */
const proxyRange = (entry: string): ProxyRange | undefined => {
  const parts = entry.split("/");
  const [address = "", length] = parts;
  const family = isIP(address);
  if (parts.length > 2 || family === 0 || address.includes("%"))
    return undefined;
  const bytes = httpAddressBytes(address);
  // An IPv4-mapped IPv6 entry is written as the IPv4 address or range instead.
  if (family === 6 && bytes.length === 4) return undefined;
  const width = bytes.length * 8;
  const bits =
    length === undefined
      ? width
      : /^[0-9]{1,3}$/u.test(length)
        ? Number(length)
        : Number.NaN;
  const floor =
    bytes.length === 4 ? MIN_PREFIX_BITS.ipv4 : MIN_PREFIX_BITS.ipv6;
  if (!(bits >= floor && bits <= width)) return undefined;
  const range = { bytes, bits };
  // No host bits: the range starts at the address written.
  const start = new Uint8Array(bytes.length);
  start.set(bytes.subarray(0, bits >> 3));
  if (bits & 7)
    start[bits >> 3] = bytes[bits >> 3]! & ((0xff << (8 - (bits & 7))) & 0xff);
  return start.every((byte, index) => byte === bytes[index])
    ? range
    : undefined;
};

/**
 * Parses HTTP_TRUSTED_PROXY_PEERS: up to 64 distinct comma-separated IPv4 or
 * IPv6 addresses or CIDR ranges, such as a private ingress address or a cloud
 * load balancer's published ranges. Empty trusts no proxy.
 */
export const parseHttpTrustedProxyPeers = (
  input: string,
): readonly string[] => {
  if (input.trim().length === 0) return [];
  const entries = input.split(",").map((entry) => entry.trim().toLowerCase());
  if (
    entries.length > HTTP_TRUSTED_PROXY_MAX_ENTRIES ||
    new Set(entries).size !== entries.length ||
    entries.some((entry) => proxyRange(entry) === undefined)
  )
    throw new Error(
      `HTTP_TRUSTED_PROXY_PEERS requires 1 to ${HTTP_TRUSTED_PROXY_MAX_ENTRIES.toString()} distinct IPv4 or IPv6 addresses or CIDR ranges, each at least /${MIN_PREFIX_BITS.ipv4.toString()} for IPv4 or /${MIN_PREFIX_BITS.ipv6.toString()} for IPv6 and with no host bits set`,
    );
  return entries;
};

/** Parsed once per configured list. */
const parsedPeers = new WeakMap<readonly string[], readonly ProxyRange[]>();

/** Whether the IP address `address` lies in a configured trusted proxy entry. */
export const isTrustedHttpProxy = (
  address: string,
  peers: readonly string[],
): boolean => {
  let ranges = parsedPeers.get(peers);
  if (ranges === undefined) {
    ranges = peers.flatMap((entry) => proxyRange(entry) ?? []);
    parsedPeers.set(peers, ranges);
  }
  if (ranges.length === 0) return false;
  const bytes = httpAddressBytes(address);
  return ranges.some((range) => inRange(bytes, range));
};

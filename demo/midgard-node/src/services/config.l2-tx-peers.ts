/**
 * `L2_TX_PEERS`: the base URLs of the other operators' nodes that this node
 * shares its accepted L2 transactions with (`fibers/l2-tx-peer-forwarder.ts`),
 * comma separated. Empty (the default) turns sharing off.
 */
import { Config } from "effect";

/** At most this many peers; a curated operator set is small. */
export const L2_TX_PEERS_MAX = 16;

/**
 * A peer base URL in one spelling: scheme and host lowercased, the default
 * port and trailing slashes dropped. Undefined for anything but a plain
 * http(s) base URL.
 */
const normalisePeerUrl = (peer: string): string | undefined => {
  try {
    const url = new URL(peer);
    if (
      (url.protocol !== "http:" && url.protocol !== "https:") ||
      url.username !== "" ||
      url.password !== "" ||
      // Even an empty query or fragment, which `URL` reports as "".
      peer.includes("?") ||
      peer.includes("#")
    )
      return undefined;
    return `${url.origin}${url.pathname}`.replace(/\/+$/u, "");
  } catch {
    return undefined;
  }
};

export const l2TxPeersConfig = Config.string("L2_TX_PEERS").pipe(
  Config.withDefault(""),
  Config.mapAttempt((input) => {
    if (input.trim().length === 0) return [] as readonly string[];
    const peers = input.split(",").map((peer) => normalisePeerUrl(peer.trim()));
    // Distinct after normalising: `http://A:80/` and `http://a` are one peer.
    if (
      peers.length > L2_TX_PEERS_MAX ||
      peers.some((peer) => peer === undefined) ||
      new Set(peers).size !== peers.length
    )
      throw new Error(
        `L2_TX_PEERS needs 1 to ${L2_TX_PEERS_MAX.toString()} distinct http(s) base URLs, comma separated`,
      );
    return peers as readonly string[];
  }),
);

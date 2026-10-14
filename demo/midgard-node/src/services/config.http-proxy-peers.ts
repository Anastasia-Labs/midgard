import { isIP } from "node:net";

import { Config } from "effect";

/** A small explicit private proxy set; unset means socket identity, never header trust. */
export const httpTrustedProxyPeersConfig = Config.string(
  "HTTP_TRUSTED_PROXY_PEERS",
).pipe(
  Config.withDefault(""),
  Config.mapAttempt((input) => {
    if (input.trim().length === 0) return [] as readonly string[];
    const peers = input.split(",").map((peer) => peer.trim());
    if (
      peers.length > 8 ||
      new Set(peers).size !== peers.length ||
      peers.some((peer) => {
        if (isIP(peer) !== 4) return true;
        const [first, second] = peer.split(".").map(Number);
        return !(
          first === 10 ||
          (first === 192 && second === 168) ||
          (first === 172 && second! >= 16 && second! <= 31)
        );
      })
    )
      throw new Error(
        "HTTP_TRUSTED_PROXY_PEERS requires 1 to 8 distinct private IPv4 addresses",
      );
    return peers;
  }),
);

/** HTTP configuration is one component; preserve existing admin-key decoding. */
export const httpIngressConfig = Config.all({
  ADMIN_API_KEY: Config.string("ADMIN_API_KEY").pipe(Config.withDefault("")),
  HTTP_TRUSTED_PROXY_PEERS: httpTrustedProxyPeersConfig,
});

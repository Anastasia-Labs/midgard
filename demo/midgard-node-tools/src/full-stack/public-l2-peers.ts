/**
 * L2 transaction sharing between a public stack and the instance attached to
 * it on the same host (attached-config.ts), whose producer the stack's
 * committee admits on the loopback. Neither node has a TCP listener, and both
 * TLS ingresses serve the stack's one certificate, so each node dials the
 * other's ingress at the certificate's host name: one Docker network per
 * direction holds the receiving ingress under that name and the sending
 * node, which trusts the stack CA. The stack's Compose project creates both
 * networks, named for the stack; the attached instance's project joins them.
 */
import { createHash } from "node:crypto";

import { attachedProducers } from "./attached-config.js";
import type { StackConfig } from "./config.js";
import { publicOriginHost } from "./node-tls.js";

/** The port each TLS ingress serves on inside its container. */
const INGRESS_PORT = 8443;

export type L2PeerRoute = NonNullable<ReturnType<typeof l2PeerRoute>>;

/** The two networks and each service's side; undefined with no instance attached on the host. */
export function l2PeerRoute(config: StackConfig) {
  const isStack = config.attachedTo === undefined;
  const stack = config.attachedTo ?? config;
  const attached = attachedProducers(stack);
  if (attached.length === 0) return undefined;
  if (attached.length > 1)
    throw new Error(
      `A public stack shares L2 transactions with at most one instance attached on its host: their ingresses serve the stack's one certificate host name. da.admittedProducers names ${attached.length.toString()} on the loopback`,
    );
  const prefix = `midgard-l2-peers-${createHash("sha256").update(stack.runDirectory).digest("hex").slice(0, 12)}`;
  const network = (direction: string) => ({
    name: `${prefix}-to-${direction}`,
    ...(isStack ? {} : { external: true }),
  });
  return {
    networks: {
      l2_peers_to_stack: network("stack"),
      l2_peers_to_attached: network("attached"),
    },
    ingressNetwork: isStack ? "l2_peers_to_stack" : "l2_peers_to_attached",
    nodeNetwork: isStack ? "l2_peers_to_attached" : "l2_peers_to_stack",
  };
}

/**
 * The node's L2_TX_PEERS with the attached instance's peer added: any peer the
 * operator set, other than that one, stays.
 */
function withL2Peer(configured: string | undefined, peer: string) {
  const origin = (value: string) => {
    try {
      return new URL(value).origin;
    } catch {
      return value;
    }
  };
  const others = (configured ?? "")
    .split(",")
    .map((value) => value.trim())
    .filter((value) => value !== "" && origin(value) !== origin(peer));
  return [...others, peer].join(",");
}

/**
 * The node environment that reaches the other instance's ingress: the peer
 * added to `configuredPeers`, and `caFile`, the stack CA as the container
 * mounts it, trusted.
 */
export function l2PeerEnvironment(
  config: StackConfig,
  stackEnv: Record<string, string>,
  configuredPeers: string | undefined,
  caFile: string,
): Record<string, string> {
  if (l2PeerRoute(config) === undefined) return {};
  return {
    L2_TX_PEERS: withL2Peer(
      configuredPeers,
      `https://${publicOriginHost(stackEnv)}:${INGRESS_PORT.toString()}`,
    ),
    NODE_EXTRA_CA_CERTS: caFile,
  };
}

/** The ingress's place on its peer network, under the certificate's host name. */
export const l2PeerIngressNetworks = (
  route: L2PeerRoute | undefined,
  stackEnv: Record<string, string>,
) =>
  route === undefined
    ? {}
    : { [route.ingressNetwork]: { aliases: [publicOriginHost(stackEnv)] } };

/** The node's peer network, from which it dials the other ingress. */
export const l2PeerNodeNetworks = (route: L2PeerRoute | undefined) =>
  route === undefined ? {} : { networks: { [route.nodeNetwork]: {} } };

/** The stack CA's mount (`volume`) when the node has a peer to trust. */
export const l2PeerCaVolumes = (
  route: L2PeerRoute | undefined,
  volume: string,
) => (route === undefined ? [] : [volume]);

import { multiaddr } from "@multiformats/multiaddr";

/**
 * Further producers a committee admits: canonical multiaddrs, each ending in
 * the one `/p2p/<peer id>` of a distinct peer that is none of the
 * deployment's own identities (`reservedPeerIds`).
 */
export const parseAdmittedProducerMultiaddrs = (
  values: readonly string[],
  reservedPeerIds: readonly string[],
): string[] => {
  const seen = new Set(reservedPeerIds);
  return values.map((value) => {
    let address;
    try {
      address = multiaddr(value);
    } catch {
      throw new Error(`admitted producer ${value} is not a multiaddr`);
    }
    const peerIds = address
      .getComponents()
      .filter((component) => component.name === "p2p")
      .map((component) => component.value);
    const peerId = peerIds[0];
    if (
      peerIds.length !== 1 ||
      peerId === undefined ||
      !value.endsWith(`/p2p/${peerId}`)
    ) {
      throw new Error(
        `admitted producer ${value} must end in exactly one /p2p/<peer id>`,
      );
    }
    if (seen.has(peerId)) {
      throw new Error(
        `admitted producer ${value} repeats a peer the deployment already names`,
      );
    }
    seen.add(peerId);
    return address.toString();
  });
};

/**
 * The producers a committee target admits besides the deployment's own, each
 * by the public multiaddr it listens on, never by its key. They join the
 * committee's bootstrap peers, which are the peers it accepts payloads from.
 */
export const admittedProducers = (
  target: string,
  values: readonly string[] = [],
  reservedPeerIds: readonly string[],
): string[] => {
  if (target !== "committee" && values.length !== 0) {
    throw new Error("only a committee target admits further producers");
  }
  return parseAdmittedProducerMultiaddrs(values, reservedPeerIds);
};

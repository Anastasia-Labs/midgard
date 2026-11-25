/**
 * `LEDGER_SNAPSHOT_PEERS`: the nodes a node without a ledger asks for the
 * ledger snapshot it starts from (`ledger-snapshot-start.ts`), in the order
 * they are asked, comma separated. Each is a libp2p multiaddr ending in
 * `/p2p/<peer id>`, of a distinct peer. Empty (the default) is
 * valid: a node on a deployment whose confirmed ledger is empty needs none.
 */
import { peerIdFromString } from "@libp2p/peer-id";
import { multiaddr } from "@multiformats/multiaddr";
import { Config } from "effect";

/** One snapshot peer: its peer id and the multiaddr it is dialed on. */
export type LedgerSnapshotPeerAddress = Readonly<{
  peerId: string;
  multiaddr: string;
}>;

const peerOf = (value: string): LedgerSnapshotPeerAddress => {
  let address;
  try {
    address = multiaddr(value);
  } catch {
    throw new Error(`LEDGER_SNAPSHOT_PEERS: ${value} is not a multiaddr`);
  }
  const peerId = address
    .getComponents()
    .filter((component) => component.name === "p2p")
    .at(-1)?.value;
  if (peerId === undefined || !value.endsWith(`/p2p/${peerId}`))
    throw new Error(
      `LEDGER_SNAPSHOT_PEERS: ${value} must end in /p2p/<peer id>`,
    );
  try {
    peerIdFromString(peerId);
  } catch {
    throw new Error(
      `LEDGER_SNAPSHOT_PEERS: ${value} names no valid libp2p peer id`,
    );
  }
  return { peerId, multiaddr: value };
};

/** The peers `input` names, refusing anything but distinct peers. */
export const parseLedgerSnapshotPeers = (
  input: string,
): readonly LedgerSnapshotPeerAddress[] => {
  if (input.trim().length === 0) return [];
  const peers = input.split(",").map((value) => peerOf(value.trim()));
  if (new Set(peers.map((peer) => peer.peerId)).size !== peers.length)
    throw new Error("LEDGER_SNAPSHOT_PEERS names a peer id more than once");
  return peers;
};

export const ledgerSnapshotPeersConfig = Config.string(
  "LEDGER_SNAPSHOT_PEERS",
).pipe(Config.withDefault(""), Config.mapAttempt(parseLedgerSnapshotPeers));

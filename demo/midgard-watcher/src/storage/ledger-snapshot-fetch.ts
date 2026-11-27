/**
 * The configured DA peers, as sources of ledger snapshots
 * (`fetchDaLedgerSnapshot` over the watcher's libp2p transport).
 *
 * The protocol is not in `DA_PUBLIC_RETAINED_DA_PROTOCOLS`, so the
 * transport never repeats a request: a failed page fails the peer for that
 * transfer.
 */
import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  fetchDaLedgerSnapshot,
} from "@al-ft/midgard-core/da-transport";
import type { LedgerEntry } from "@al-ft/midgard-fault-proofs/ledger-at";

import type { WatcherConfig } from "../runtime/config.js";
import type {
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequest,
} from "./public-da-client.js";

/** One configured DA peer, as a source of ledger snapshots. */
export type WatcherLedgerSnapshotPeer = Readonly<{
  /** The peer's configured identity (`da.peers[].identity`). */
  identity: string;
  /**
   * The entries of the ledger with root `utxosRoot`, page by page. Rejects
   * with `DaLedgerSnapshotUnserved` or `DaLedgerSnapshotMalformed`, or with
   * the abort reason once `signal` is aborted.
   */
  entries(utxosRoot: string): AsyncIterable<LedgerEntry>;
}>;

/**
 * The snapshot sources of `peers`, in configuration order. Each request
 * gets `timeoutMs` and ends with `signal`; `acquireRequest`, when given,
 * bounds it with the runtime's other DA requests.
 */
export const watcherLedgerSnapshotPeers = (
  input: Readonly<{
    transport: WatcherPublicDaLibp2pTransportV1;
    peers: readonly WatcherConfig["da"]["peers"][number][];
    /** The deployment fingerprint, hex. */
    deploymentFingerprint: string;
    timeoutMs: number;
    signal: AbortSignal;
    admission?: WatcherPublicDaRequest["admission"];
    acquireRequest?: (signal: AbortSignal) => Promise<() => void>;
  }>,
): readonly WatcherLedgerSnapshotPeer[] => {
  const protocol = DaRequestResponseProtocol.ledgerSnapshot;
  const protocolId = daRequestResponseProtocolId(
    input.deploymentFingerprint,
    protocol,
  );
  return input.peers.map((peer) => {
    const request = async (requestCbor: Buffer): Promise<Uint8Array> => {
      const signal = AbortSignal.any([
        input.signal,
        AbortSignal.timeout(input.timeoutMs),
      ]);
      let release: (() => void) | undefined;
      try {
        release = await input.acquireRequest?.(signal);
        return await input.transport.request({
          peerIdentity: peer.identity,
          peerId: peer.peerId,
          multiaddr: peer.multiaddr,
          protocol,
          protocolId,
          requestCbor,
          timeoutMs: input.timeoutMs,
          signal,
          ...(input.admission === undefined
            ? {}
            : { admission: input.admission }),
        });
      } finally {
        release?.();
      }
    };
    return Object.freeze({
      identity: peer.identity,
      entries: (utxosRoot: string) =>
        fetchDaLedgerSnapshot({
          peerIdentity: peer.identity,
          deploymentFingerprint: input.deploymentFingerprint,
          utxosRoot,
          request,
          signal: input.signal,
        }),
    });
  });
};

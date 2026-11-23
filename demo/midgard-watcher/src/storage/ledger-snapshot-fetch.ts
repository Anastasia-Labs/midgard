/**
 * Asking the configured DA peers for a ledger snapshot (`ledger-snapshot`).
 *
 * A peer answers one page per request, in the ledger's trie path order; the
 * next page is asked for after the last out-ref of the page before, until a
 * page says it reaches the end of the ledger. One page is held at a time.
 * Nothing a page carries is trusted: the reader checks the whole stream
 * against the root it asked for (`LedgerStore.admit`).
 *
 * The protocol is not in `DA_PUBLIC_RETAINED_DA_PROTOCOLS`, so the
 * transport never repeats a request: a failed page fails the peer for that
 * transfer.
 */
import {
  type DaLedgerSnapshotRequest,
  type DaLedgerSnapshotResponse,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaLedgerSnapshotResponseCbor,
  encodeDaLedgerSnapshotRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import type { LedgerEntry } from "@al-ft/midgard-fault-proofs/ledger-at";

import type { WatcherConfig } from "../runtime/config.js";
import type {
  WatcherPublicDaLibp2pTransportV1,
  WatcherPublicDaRequest,
} from "./public-da-client.js";

/**
 * The peer did not serve the snapshot this time: it does not speak the
 * protocol, the request failed or timed out, or it answered `not_held` or
 * `rejected`. It may be asked again later.
 */
export class WatcherLedgerSnapshotUnserved extends Error {
  constructor(
    readonly peerIdentity: string,
    detail: string,
    options?: ErrorOptions,
  ) {
    super(
      `DA peer ${peerIdentity} did not serve the ledger snapshot: ${detail}`,
      options,
    );
    this.name = "WatcherLedgerSnapshotUnserved";
  }
}

/**
 * The peer answered what no honest peer sends: a response that does not
 * decode, or an `ok` page with no entry that does not reach the end.
 */
export class WatcherLedgerSnapshotMalformed extends Error {
  constructor(
    readonly peerIdentity: string,
    detail: string,
    options?: ErrorOptions,
  ) {
    super(
      `DA peer ${peerIdentity} served a malformed ledger snapshot: ${detail}`,
      options,
    );
    this.name = "WatcherLedgerSnapshotMalformed";
  }
}

/** One configured DA peer, as a source of ledger snapshots. */
export type WatcherLedgerSnapshotPeer = Readonly<{
  /** The peer's configured identity (`da.peers[].identity`). */
  identity: string;
  /**
   * The entries of the ledger with root `utxosRoot`, page by page. Rejects
   * with `WatcherLedgerSnapshotUnserved` or `WatcherLedgerSnapshotMalformed`,
   * or with the abort reason once `signal` is aborted.
   */
  entries(utxosRoot: string): AsyncIterable<LedgerEntry>;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

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
  const deploymentFingerprint = Buffer.from(input.deploymentFingerprint, "hex");
  return input.peers.map((peer) => {
    const ask = async (
      request: DaLedgerSnapshotRequest,
    ): Promise<DaLedgerSnapshotResponse> => {
      const signal = AbortSignal.any([
        input.signal,
        AbortSignal.timeout(input.timeoutMs),
      ]);
      let release: (() => void) | undefined;
      let response: Uint8Array;
      try {
        release = await input.acquireRequest?.(signal);
        response = await input.transport.request({
          peerIdentity: peer.identity,
          peerId: peer.peerId,
          multiaddr: peer.multiaddr,
          protocol,
          protocolId,
          requestCbor: encodeDaLedgerSnapshotRequestCbor(request),
          timeoutMs: input.timeoutMs,
          signal,
          ...(input.admission === undefined
            ? {}
            : { admission: input.admission }),
        });
      } catch (error) {
        // The runtime closing ends the transfer; any other failure is this
        // peer's, for this transfer.
        input.signal.throwIfAborted();
        throw new WatcherLedgerSnapshotUnserved(
          peer.identity,
          `the request failed: ${message(error)}`,
          { cause: error },
        );
      } finally {
        release?.();
      }
      try {
        return decodeDaLedgerSnapshotResponseCbor(response);
      } catch (error) {
        throw new WatcherLedgerSnapshotMalformed(
          peer.identity,
          `the response does not decode: ${message(error)}`,
          { cause: error },
        );
      }
    };
    return Object.freeze({
      identity: peer.identity,
      entries: async function* (utxosRoot: string) {
        let after: Buffer | null = null;
        for (;;) {
          const page = await ask({
            deploymentFingerprint,
            utxosRoot: Buffer.from(utxosRoot, "hex"),
            after,
          });
          if (page.status !== "ok")
            throw new WatcherLedgerSnapshotUnserved(
              peer.identity,
              `it answered ${page.status}`,
            );
          const last = page.entries.at(-1);
          // Asking after the same out-ref again would get the same page.
          if (last === undefined && !page.done)
            throw new WatcherLedgerSnapshotMalformed(
              peer.identity,
              "a page with no entry does not reach the end of the ledger",
            );
          for (const entry of page.entries)
            yield {
              outRef: Buffer.from(entry.outRef),
              output: Buffer.from(entry.output),
            };
          if (page.done || last === undefined) return;
          after = Buffer.from(last.outRef);
        }
      },
    });
  });
};

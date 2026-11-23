/**
 * A watcher taking in the confirmed head's ledger from a DA peer
 * (`ReplayedLedger.ensure`'s `bootstrap`): when it has no verified head, and
 * when the head's parent ledger is neither held nor derivable.
 *
 * Where the retained DA no longer serves a chain's old payloads, a watcher
 * that never verified a confirmed head, or was down longer than they are
 * retained, cannot derive the head's ledger by replay. It asks its DA peers,
 * in configuration order, for the ledger with the confirmed root it read
 * from L1, and the store checks the whole stream against that root
 * (`LedgerStore.admit`), so no peer is trusted. A failed `admit` leaves
 * nothing retained, and the next `retain` deletes what it wrote.
 *
 * A peer that does not serve the snapshot this time is asked again on a
 * later call. A peer that served a wrong one (a stream the root check, the
 * duplicate check or the entry check refuses, or a malformed answer) is not
 * asked again by this process, and is logged once.
 */
import { LedgerRootMismatchError } from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  LedgerEntryNotCanonical,
  LedgerNotRetained,
  LedgerSnapshotRefused,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";

import { WATCHER_PACKAGE_NAME } from "../runtime/package-name.js";
import {
  WatcherLedgerSnapshotMalformed,
  type WatcherLedgerSnapshotPeer,
  WatcherLedgerSnapshotUnserved,
} from "../storage/ledger-snapshot-fetch.js";

/** The warn-level log reason of a DA peer that served a wrong snapshot. */
export const WATCHER_LEDGER_SNAPSHOT_PEER_EXCLUDED =
  "replayed_ledger_snapshot_peer_excluded";

/** A stream no honest peer serves for the root asked for. */
const servedWrong = (error: unknown): boolean =>
  error instanceof WatcherLedgerSnapshotMalformed ||
  error instanceof LedgerRootMismatchError ||
  error instanceof LedgerSnapshotRefused ||
  error instanceof LedgerEntryNotCanonical;

export type WatcherLedgerSnapshotBootstrap = (
  peers: readonly WatcherLedgerSnapshotPeer[],
  utxosRoot: string,
) => Promise<boolean>;

/**
 * Makes `store` hold the ledger of `utxosRoot`, true once it does. A root
 * the store already holds (an earlier `admit` whose verified-head write did
 * not happen) asks no peer. False when no peer served it. A failure of the
 * store, or the transfer's abort, rejects.
 */
export const createWatcherLedgerSnapshotBootstrap = (
  input: Readonly<{
    store: LedgerStore;
    log?: (line: string) => void;
  }>,
): WatcherLedgerSnapshotBootstrap => {
  const log =
    input.log ?? ((line: string) => process.stderr.write(`${line}\n`));
  /** The identities of peers that served a wrong snapshot. */
  const excluded = new Set<string>();
  const held = async (root: string): Promise<boolean> => {
    try {
      await input.store.at(root);
      return true;
    } catch (error) {
      if (error instanceof LedgerNotRetained) return false;
      throw error;
    }
  };
  return async (peers, utxosRoot) => {
    if (await held(utxosRoot)) return true;
    for (const peer of peers) {
      if (excluded.has(peer.identity)) continue;
      try {
        await input.store.admit(peer.entries(utxosRoot), utxosRoot);
        return true;
      } catch (error) {
        if (error instanceof WatcherLedgerSnapshotUnserved) continue;
        if (!servedWrong(error)) throw error;
        excluded.add(peer.identity);
        log(
          JSON.stringify({
            packageName: WATCHER_PACKAGE_NAME,
            level: "warn",
            reason: WATCHER_LEDGER_SNAPSHOT_PEER_EXCLUDED,
            peerIdentity: peer.identity,
            utxosRoot,
            cause: error instanceof Error ? error.message : String(error),
          }),
        );
      }
    }
    return false;
  };
};

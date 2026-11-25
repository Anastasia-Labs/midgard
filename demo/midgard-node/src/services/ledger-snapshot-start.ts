/**
 * How a node without a ledger stamp takes in its ledger: the ledger at the
 * landed queue's confirmed header H, whose post-state root is R.
 *
 * 1. The staging table is emptied: a start without a stamp runs the whole
 *    take-in again, whatever the ledger store holds.
 * 2. R the empty ledger's root: the empty ledger. Otherwise the configured
 *    snapshot peers are asked for R in order (`fetchDaLedgerSnapshot`); the
 *    stream is staged and fed to the ledger store's `admit`, which checks
 *    it against R and moves the store's root marker to it. A peer whose
 *    stream fails the check is not asked again by this process; a peer that
 *    does not serve R now is asked again on the next attempt. With no peer
 *    serving, the take-in fails with `LedgerSnapshotPending`, which the
 *    driver's recompute holds as `ledger_snapshot_pending` and retries.
 * 3. One transaction installs it: the staged rows become `confirmed_ledger`
 *    and `mempool_ledger`, the frontier is set to (H, R), the ledger is
 *    stamped at R and the anchor is recorded. The stamp is the commit
 *    point: a crash before it leaves an unstamped node, whose next start
 *    takes the ledger in again (an `admit` of a root the store holds only
 *    moves its marker).
 *
 * The node ingests no L1 event before the install (`snapshot-anchor.ts`).
 */
import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  DaLedgerSnapshotMalformed,
  DaLedgerSnapshotUnserved,
  fetchDaLedgerSnapshot,
} from "@al-ft/midgard-core/da-transport";
import { LedgerRootMismatchError } from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  EMPTY_LEDGER_ROOT,
  LedgerEntryNotCanonical,
  LedgerSnapshotRefused,
  type LedgerStore,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/state-queue";
import { Cause, Context, Data, Effect, Exit, Runtime } from "effect";

import { MpfEngineStateDB } from "../database/index.js";
import * as Staging from "../database/ledger-snapshot-staging.js";
import * as SnapshotAnchor from "../database/snapshot-anchor.js";
import type * as Ledger from "../database/utils/ledger.js";
import type { LandedStateQueue } from "../l1-state-queue/landed.js";
import { Frontier } from "../landed-blocks/store.js";
import type { Database } from "./database.js";

/** The node has no ledger yet and no configured peer served its snapshot. */
export const LEDGER_SNAPSHOT_PENDING = "ledger_snapshot_pending";

export class LedgerSnapshotPending extends Data.TaggedError(
  "LedgerSnapshotPending",
)<{ readonly detail: string }> {}

/** One snapshot peer. */
export type LedgerSnapshotPeer = Readonly<{
  /** The peer's name in logs and holds. */
  identity: string;
  /** Sends one encoded `ledger-snapshot` request; its encoded response. */
  request: (requestCbor: Buffer, signal: AbortSignal) => Promise<Uint8Array>;
}>;

/** The snapshot peers, in the order they are asked. */
export type LedgerSnapshotPeerSource = Readonly<{
  /** The deployment fingerprint, hex. */
  deploymentFingerprint: string;
  peers: readonly LedgerSnapshotPeer[];
  /** The identities of the peers that served a wrong stream, for the
   * process's lifetime. */
  excluded: Set<string>;
}>;

/** The snapshot peers, when given in place of the configured ones. */
export class LedgerSnapshotPeers extends Context.Tag("LedgerSnapshotPeers")<
  LedgerSnapshotPeers,
  LedgerSnapshotPeerSource
>() {}

/** A stream no honest peer serves for the root asked for. */
const servedWrong = (error: unknown): boolean =>
  error instanceof DaLedgerSnapshotMalformed ||
  error instanceof LedgerRootMismatchError ||
  error instanceof LedgerSnapshotRefused ||
  error instanceof LedgerEntryNotCanonical;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** Rows staged per write. */
const STAGE_BATCH = 1024;

/** `entry` as a `confirmed_ledger` row; an output that does not decode is
 * an entry no honest peer serves. */
const rowOf = (entry: {
  outRef: Buffer;
  output: Buffer;
}): Ledger.EntryNoTimeStamp => {
  try {
    return {
      tx_id: Buffer.from(decodeMidgardSpendInputItem(entry.outRef).txId),
      outref: entry.outRef,
      output: entry.output,
      address: encodeMidgardAddressText(
        decodeMidgardTxOutput(entry.output).address,
      ),
    };
  } catch (cause) {
    throw new LedgerEntryNotCanonical(entry.outRef, cause);
  }
};

/** H, R and H's end time: the landed queue root's confirmed state. */
export const ledgerAnchorOf = (queue: LandedStateQueue) =>
  Effect.gen(function* () {
    if (queue.root === null)
      return yield* Effect.fail(
        new Error("The landed state queue has no root to take a ledger at"),
      );
    const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
      queue.root.element.datum,
    );
    return {
      headerHash: data.headerHash,
      utxosRoot: data.utxoRoot,
      endTimeMs: data.endTime,
    } satisfies SnapshotAnchor.SnapshotAnchor;
  });

/** The install: one transaction, in the caller's `write`. */
const install = (anchor: SnapshotAnchor.SnapshotAnchor) =>
  Effect.gen(function* () {
    yield* Staging.moveIntoLedgers;
    yield* Frontier.upsert({
      headerHash: anchor.headerHash,
      utxosRoot: anchor.utxosRoot,
    });
    yield* MpfEngineStateDB.stampLedgerMigration(anchor.utxosRoot);
    yield* SnapshotAnchor.record(anchor);
  });

export const takeInLedger = <R>(input: {
  readonly store: LedgerStore;
  readonly anchor: SnapshotAnchor.SnapshotAnchor;
  /** The peers; read only when R is not the empty ledger's root. */
  readonly peers: Effect.Effect<LedgerSnapshotPeerSource, unknown, R>;
  /** Runs one write transaction (the follower write gate). */
  readonly write: <A, E>(
    work: Effect.Effect<A, E, Database>,
  ) => Effect.Effect<A, unknown, Database>;
}): Effect.Effect<void, unknown, Database | R> =>
  Effect.gen(function* () {
    const { store, anchor, write } = input;
    const root = anchor.utxosRoot;
    yield* write(Staging.clear);
    if (root === EMPTY_LEDGER_ROOT)
      yield* Effect.tryPromise({
        try: () => store.admit([], root, { current: true }),
        catch: (cause) => cause,
      });
    else {
      const source = yield* input.peers;
      const runtime = yield* Effect.runtime<Database>();
      const stage = async (rows: readonly Ledger.EntryNoTimeStamp[]) => {
        const exit = await Runtime.runPromiseExit(runtime)(
          write(Staging.insert(rows)),
        );
        if (Exit.isFailure(exit)) throw Cause.squash(exit.cause);
      };
      async function* staged(
        entries: AsyncIterable<{ outRef: Buffer; output: Buffer }>,
      ) {
        let rows: Ledger.EntryNoTimeStamp[] = [];
        for await (const entry of entries) {
          rows.push(rowOf(entry));
          yield entry;
          if (rows.length === STAGE_BATCH) {
            await stage(rows);
            rows = [];
          }
        }
        await stage(rows);
      }
      const unserved: string[] =
        source.peers.length === 0 ? ["LEDGER_SNAPSHOT_PEERS names none"] : [];
      let taken = false;
      for (const peer of source.peers) {
        if (source.excluded.has(peer.identity)) {
          unserved.push(`${peer.identity} served a wrong ledger before`);
          continue;
        }
        const admitted = yield* Effect.either(
          Effect.tryPromise({
            try: (signal) =>
              store.admit(
                staged(
                  fetchDaLedgerSnapshot({
                    peerIdentity: peer.identity,
                    deploymentFingerprint: source.deploymentFingerprint,
                    utxosRoot: root,
                    request: (requestCbor) => peer.request(requestCbor, signal),
                    signal,
                  }),
                ),
                root,
                { current: true },
              ),
            catch: (cause) => cause,
          }),
        );
        if (admitted._tag === "Right") {
          taken = true;
          break;
        }
        const error = admitted.left;
        if (error instanceof DaLedgerSnapshotUnserved)
          unserved.push(message(error));
        else if (servedWrong(error)) {
          source.excluded.add(peer.identity);
          unserved.push(
            `${peer.identity} is not asked again: ${message(error)}`,
          );
          yield* Effect.logWarning(
            `ledger take-in: snapshot peer ${peer.identity} served a wrong ledger for ${root}; it is not asked again: ${message(error)}`,
          );
        } else return yield* Effect.fail(error);
        yield* write(Staging.clear);
      }
      if (!taken)
        return yield* Effect.fail(
          new LedgerSnapshotPending({
            detail: `no snapshot peer served the ledger of ${anchor.headerHash} (${root}): ${unserved.join("; ")}`,
          }),
        );
    }
    yield* write(install(anchor));
  });

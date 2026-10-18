/**
 * The ledger a watcher keeps by replaying blocks (DS-3).
 *
 * No DA payload carries a UTxO set, so the ledger after a block is derived by
 * replaying that block's retained payload over its parent's ledger with the
 * block replay the node also runs (`replayBlockEvents`). Ledgers are
 * content-addressed: each is keyed by the header hash it follows and admitted
 * only when it opens that header's `utxos_root`. The owning runtime fences
 * these ephemeral derivations when mutable L1 admission facts are rewound;
 * only its stable, root-checked checkpoint survives that generation change.
 *
 * What is kept: one checkpoint (the ledger after the L1 ConfirmedState head,
 * persisted and re-checked against ConfirmedState's `utxos_root` on load) and
 * a bounded in-memory LRU of ledgers above it. A ledger missing from both is
 * rebuilt by walking `prev_header_hash` back to a kept ledger (or genesis) and
 * replaying forward.
 *
 * A block whose replay is refused, or whose replayed ledger does not open its
 * `utxos_root`, is reported through `onRefusal`. Its own header still gets the
 * replayed entries on a root mismatch, unchecked, so a classifier records the
 * post-ledger mismatch; nothing above it is derived from them. A payload no
 * attested availability commitment binds yet (`ReplayedLedgerSource.binding`)
 * is not refused that way: its copy is released and the read waits.
 */
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Exit } from "effect";

import { isReplayLocalFault } from "./block-replay.faults.js";
import { type BlockReplayContext, replayBlockEvents } from "./block-replay.js";
import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  type LedgerAt,
  ledgerEntriesRoot,
  type LedgerEntry,
  LedgerRootMismatchError,
} from "./ledger-at.js";
import { RetainedDaPayloadUnavailableError } from "./transition-trace/fetch.js";

/** The L1 facts a block's user events and forced orders replay against. */
export type BlockReplayMaterial = Pick<
  BlockReplayContext,
  "replayUserEvent" | "verifyForcedSource" | "blockTimeMs"
>;

/** Where one operation reads blocks and their L1 material. */
export type ReplayedLedgerSource = {
  /**
   * The retained DA payload of the block `headerHash`, or `undefined` when no
   * source retains it. The payload's own header must hash to `headerHash`.
   */
  readonly payloadAt: (
    headerHash: string,
  ) => Promise<SDK.DaPayload | undefined>;
  /** Throws when the facts are not available yet (the caller retries). */
  readonly materialFor: (
    payload: SDK.DaPayload,
  ) => Promise<BlockReplayMaterial>;
  readonly l1SlotConfig: L1SlotConfig;
  /**
   * Which served payloads no attested availability commitment binds yet (the
   * block's node is Unattested), and their release. Such a payload's replay
   * refusal or root mismatch is not the block's: the copy is released and
   * the read waits (`RetainedDaPayloadUnavailableError`). Without it every
   * payload is taken as bound.
   */
  readonly binding?: Readonly<{
    unbound: (headerHash: string) => boolean;
    release: (headerHash: string) => void;
  }>;
};

/** The ledger after the block `headerHash`. */
export type LedgerCheckpoint = {
  readonly headerHash: string;
  readonly entries: readonly LedgerEntry[];
};

export type LedgerCheckpointStore = {
  readonly read: () => Promise<LedgerCheckpoint | undefined>;
  readonly write: (checkpoint: LedgerCheckpoint) => Promise<void>;
};

/** A block whose ledger could not be derived or does not open its root. */
export type ReplayedLedgerRefusal = {
  readonly headerHash: string;
  readonly kind: "replay-refused" | "root-mismatch" | "parent-mismatch";
  readonly detail: string;
};

export type ReplayedLedger = {
  /** The `LedgerAt` over one operation's sources. */
  readonly ledgerAt: (source: ReplayedLedgerSource) => LedgerAt;
  /**
   * Loads the persisted checkpoint. One for the ConfirmedState head must open
   * its `utxos_root`, or it is refused and not used. One for an older head is
   * kept as a candidate, checked against the first child that reaches it.
   */
  readonly restore: (confirmed: {
    readonly headerHash: string;
    readonly utxosRoot: string;
  }) => Promise<"restored" | "candidate" | "absent" | "refused">;
  /**
   * Moves the checkpoint to the ConfirmedState head. False, and the old
   * checkpoint kept, when that ledger cannot be derived yet.
   */
  readonly advance: (
    confirmed: { readonly headerHash: string; readonly utxosRoot: string },
    source: ReplayedLedgerSource,
  ) => Promise<boolean>;
};

type Kept = { readonly headerHash: string; readonly ledger: CheckedLedger };

type Replayed =
  | { readonly kind: "checked"; readonly ledger: CheckedLedger }
  | { readonly kind: "mismatch"; readonly entries: readonly LedgerEntry[] }
  | { readonly kind: "refused" };

const label = (headerHash: string) => `replayed ledger after ${headerHash}`;

/**
 * A payload no commitment binds yet that does not replay says nothing of
 * its block: the copy is released and the read waits for another.
 */
const unboundWait = (
  source: ReplayedLedgerSource,
  headerHash: string,
  detail: string,
) => {
  if (source.binding?.unbound(headerHash) !== true) return;
  source.binding.release(headerHash);
  throw new RetainedDaPayloadUnavailableError(
    headerHash,
    `the payload of ${headerHash} is not bound to an attested commitment yet, and this copy does not replay: ${detail}`,
    "unreachable",
  );
};

export const createReplayedLedger = ({
  checkpoints,
  capacity = 64,
  onRefusal,
}: {
  readonly checkpoints: LedgerCheckpointStore;
  /** Ledgers kept in memory above the checkpoint. */
  readonly capacity?: number;
  readonly onRefusal?: (refusal: ReplayedLedgerRefusal) => void;
}): ReplayedLedger => {
  if (!Number.isSafeInteger(capacity) || capacity < 1)
    throw new Error("replayed ledger capacity must be a positive integer");
  const recent = new Map<string, CheckedLedger>();
  let checkpoint: Kept | undefined;
  let candidate: LedgerCheckpoint | undefined;

  const refuse = (refusal: ReplayedLedgerRefusal) => onRefusal?.(refusal);

  const remember = (headerHash: string, ledger: CheckedLedger) => {
    recent.delete(headerHash);
    recent.set(headerHash, ledger);
    while (recent.size > capacity)
      recent.delete(recent.keys().next().value as string);
  };

  const kept = (headerHash: string): CheckedLedger | undefined => {
    const hit = recent.get(headerHash);
    if (hit !== undefined) {
      remember(headerHash, hit);
      return hit;
    }
    return checkpoint?.headerHash === headerHash
      ? checkpoint.ledger
      : undefined;
  };

  /** The candidate checkpoint, admitted against the root its child binds. */
  const admitCandidate = async (
    headerHash: string,
    utxosRoot: string,
  ): Promise<CheckedLedger | undefined> => {
    if (candidate?.headerHash !== headerHash) return undefined;
    const entries = candidate.entries;
    candidate = undefined;
    try {
      const ledger = await checkLedgerAgainstRoot({
        entries,
        utxosRoot,
        label: label(headerHash),
      });
      checkpoint = { headerHash, ledger };
      return ledger;
    } catch (error) {
      if (!(error instanceof LedgerRootMismatchError)) throw error;
      return undefined;
    }
  };

  const replay = async (
    headerHash: string,
    payload: SDK.DaPayload,
    parent: CheckedLedger,
    source: ReplayedLedgerSource,
  ): Promise<Replayed> => {
    const header = payload.block_body.header;
    if (parent.utxosRoot !== header.prevUtxosRoot) {
      refuse({
        headerHash,
        kind: "parent-mismatch",
        detail: `the block starts from ${header.prevUtxosRoot}, its parent's ledger opens ${parent.utxosRoot}`,
      });
      return { kind: "refused" };
    }
    const material = await source.materialFor(payload);
    const exit = await Effect.runPromiseExit(
      replayBlockEvents({
        header,
        parentEntries: parent.entries,
        ledgerRoot: (entries) =>
          Effect.tryPromise(() => ledgerEntriesRoot(entries)),
        expectedNetworkId: header.expectedNetworkId,
        minFeeA: header.minFeeA,
        minFeeB: header.minFeeB,
        blockSlot: header.blockSlot,
        l1SlotConfig: source.l1SlotConfig,
        payload,
        ...material,
      }),
    );
    if (Exit.isFailure(exit)) {
      // A local fault says nothing about the block: it is raised, so the
      // block is replayed again, never refused.
      const local = [
        ...Cause.defects(exit.cause),
        ...Cause.failures(exit.cause),
      ].find(isReplayLocalFault);
      if (local !== undefined) throw local;
      const detail = String(exit.cause);
      unboundWait(source, headerHash, detail);
      refuse({ headerHash, kind: "replay-refused", detail });
      return { kind: "refused" };
    }
    try {
      return {
        kind: "checked",
        ledger: await checkLedgerAgainstRoot({
          entries: exit.value,
          utxosRoot: header.utxosRoot,
          label: label(headerHash),
        }),
      };
    } catch (error) {
      if (!(error instanceof LedgerRootMismatchError)) throw error;
      unboundWait(source, headerHash, error.message);
      refuse({ headerHash, kind: "root-mismatch", detail: error.message });
      return { kind: "mismatch", entries: exit.value };
    }
  };

  /** The authenticated payload of `headerHash`, or `undefined`. */
  const payloadAt = async (
    source: ReplayedLedgerSource,
    headerHash: string,
  ): Promise<SDK.DaPayload | undefined> => {
    const payload = await source.payloadAt(headerHash);
    if (payload === undefined) return undefined;
    const hash = await Effect.runPromise(
      SDK.hashBlockHeader(payload.block_body.header),
    );
    if (hash !== headerHash || payload.block_body.header_hash !== headerHash)
      throw new Error(
        `the retained payload served for ${headerHash} is another block's`,
      );
    return payload;
  };

  const derive = async (
    headerHash: string,
    source: ReplayedLedgerSource,
  ): Promise<readonly LedgerEntry[] | undefined> => {
    if (headerHash === SDK.GENESIS_HEADER_HASH) return [];
    const hit = kept(headerHash);
    if (hit !== undefined) return hit.entries;
    // Walk back to a kept ledger, then replay forward.
    const chain: { headerHash: string; payload: SDK.DaPayload }[] = [];
    let cursor = headerHash;
    let base: CheckedLedger | undefined;
    while (base === undefined) {
      const payload = await payloadAt(source, cursor);
      if (payload === undefined) return undefined;
      chain.push({ headerHash: cursor, payload });
      const { prevHeaderHash, prevUtxosRoot } = payload.block_body.header;
      if (prevHeaderHash === SDK.GENESIS_HEADER_HASH)
        base = await checkLedgerAgainstRoot({
          entries: [],
          utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          label: "genesis ledger",
        });
      else
        base =
          kept(prevHeaderHash) ??
          (await admitCandidate(prevHeaderHash, prevUtxosRoot));
      cursor = prevHeaderHash;
    }
    let parent = base;
    for (const block of chain.reverse()) {
      const replayed = await replay(
        block.headerHash,
        block.payload,
        parent,
        source,
      );
      if (replayed.kind !== "checked")
        return replayed.kind === "mismatch" && block.headerHash === headerHash
          ? replayed.entries
          : undefined;
      remember(block.headerHash, replayed.ledger);
      parent = replayed.ledger;
    }
    return parent.entries;
  };

  return {
    ledgerAt: (source) => (headerHash) => derive(headerHash, source),
    restore: async ({ headerHash, utxosRoot }) => {
      let stored: LedgerCheckpoint | undefined;
      try {
        stored = await checkpoints.read();
      } catch (error) {
        // An unreadable checkpoint is rebuilt from retained payloads.
        refuse({ headerHash, kind: "replay-refused", detail: String(error) });
        return "refused";
      }
      if (stored === undefined) return "absent";
      if (stored.headerHash !== headerHash) {
        candidate = stored;
        return "candidate";
      }
      try {
        checkpoint = {
          headerHash,
          ledger: await checkLedgerAgainstRoot({
            entries: stored.entries,
            utxosRoot,
            label: label(headerHash),
          }),
        };
        return "restored";
      } catch (error) {
        if (!(error instanceof LedgerRootMismatchError)) throw error;
        refuse({ headerHash, kind: "root-mismatch", detail: error.message });
        return "refused";
      }
    },
    advance: async ({ headerHash, utxosRoot }, source) => {
      if (checkpoint?.headerHash === headerHash) {
        if (checkpoint.ledger.utxosRoot === utxosRoot) return true;
        refuse({
          headerHash,
          kind: "root-mismatch",
          detail: "the confirmed root differs from the checked checkpoint",
        });
        return false;
      }
      const entries = await derive(headerHash, source);
      if (entries === undefined) return false;
      let ledger: CheckedLedger;
      try {
        ledger = await checkLedgerAgainstRoot({
          entries,
          utxosRoot,
          label: label(headerHash),
        });
      } catch (error) {
        if (!(error instanceof LedgerRootMismatchError)) throw error;
        return false;
      }
      await checkpoints.write({ headerHash, entries: ledger.entries });
      checkpoint = { headerHash, ledger };
      recent.delete(headerHash);
      return true;
    },
  };
};

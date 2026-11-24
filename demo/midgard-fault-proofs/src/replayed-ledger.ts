/**
 * The ledger a watcher keeps by replaying blocks.
 *
 * No DA payload carries a UTxO set, so the ledger after a block is derived by
 * replaying that block's retained payload over its parent's ledger with the
 * block replay the node also runs (`replayBlockEvents`). Every derived ledger
 * lives in one ledger store, addressed by its root: the ledger after a block
 * is the store's ledger at the block's `utxos_root`, and a block's ledger is
 * persisted only once its replay opens that root.
 *
 * A lookup the store cannot answer walks `prev_header_hash` back, holding
 * headers only, to the first block whose `prev_utxos_root` the store holds
 * (the empty ledger at genesis is always held), then replays forward one
 * block at a time: fetch its payload, replay it over the parent's ledger,
 * check the root, persist, let the payload go. Each persisted block is a
 * resume point, so a crash, a retry or a payload not served yet resumes from
 * the highest one, and a fresh start over an empty store is the same walk.
 *
 * The store holds what `retain` names, set by the caller from its window of
 * blocks; see `ReplayedLedger.retain`. How overlapping lookups, replays and
 * retention stay out of each other's way is `replayed-ledger.holds.ts`.
 *
 * A lookup is answered by root, whichever block's replay produced the
 * ledger. Whether a block was verified is a fact about the block: its events
 * replayed over its parent's ledger open the root its header commits. The
 * confirmed head is therefore not taken on its root (`ReplayedLedger.ensure`):
 * every block from the durable verified head (`ReplayedLedgerVerifiedHead`)
 * up to it is replayed unless this process already replayed that block.
 *
 * A block whose replay is refused, whose replayed ledger does not open its
 * `utxos_root`, or that does not start from its parent's ledger is reported
 * through `onRefusal` and never persisted. Its own lookup still gets the
 * replayed ledger on a root mismatch, held in memory over its parent, so a
 * classifier records the post-ledger mismatch; nothing above it is derived
 * from it. A payload no
 * attested availability commitment binds yet (`ReplayedLedgerSource.binding`)
 * is not refused that way: its copy is released and the read waits.
 */
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";

import { type BlockReplayContext } from "./block-replay.js";
import { type LedgerAt, LedgerRootMismatchError } from "./ledger-at.js";
import {
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStore,
} from "./ledger-store/index.js";
import {
  type ChainBlock,
  confirmedChainVerifier,
  memoryVerifiedHead,
  type ReplayedLedgerUnverified,
  type ReplayedLedgerVerifiedHead,
} from "./replayed-ledger.confirmed.js";
import { createLedgerHolds, type HeldLedger } from "./replayed-ledger.holds.js";
import {
  authenticHeader,
  authenticPayload,
  replayBlock,
  unboundWait,
} from "./replayed-ledger.replay-block.js";

export {
  memoryVerifiedHead,
  type ReplayedLedgerUnverified,
  type ReplayedLedgerVerifiedHead,
} from "./replayed-ledger.confirmed.js";

/** The L1 facts a block's user events and forced orders replay against. */
export type BlockReplayMaterial = Pick<
  BlockReplayContext,
  "replayUserEvent" | "verifyForcedSource" | "blockTimeMs"
>;

/** Where one operation reads blocks and their L1 material. */
export type ReplayedLedgerSource = {
  /**
   * The header of the block `headerHash`, or `undefined` when the source
   * does not know the block. It must hash to `headerHash`.
   */
  readonly headerAt: (headerHash: string) => Promise<SDK.Header | undefined>;
  /**
   * The retained DA payload of the block `headerHash`, or `undefined` when no
   * source retains it. The payload's own header must hash to `headerHash`.
   */
  readonly payloadAt: (
    headerHash: string,
  ) => Promise<SDKDa.DaPayload | undefined>;
  /** Throws when the facts are not available yet (the caller retries). */
  readonly materialFor: (
    payload: SDKDa.DaPayload,
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

/** A block whose ledger could not be derived or does not open its root. */
export type ReplayedLedgerRefusal = {
  readonly headerHash: string;
  readonly kind: "replay-refused" | "root-mismatch" | "parent-mismatch";
  readonly detail: string;
};

/** The roots of one block, as its header commits them. */
export type ReplayedLedgerBlockRoots = {
  readonly utxosRoot: string;
  readonly prevUtxosRoot: string;
};

/** What the store keeps: see `ReplayedLedger.retain`. */
export type ReplayedLedgerWindow = {
  /** Every block inside the retention window. */
  readonly blocks: Iterable<ReplayedLedgerBlockRoots>;
  /** The root after the newest block the caller knows. */
  readonly newestRoot: string;
  /** The root of the stable confirmed head. */
  readonly confirmedRoot: string;
};

export type ReplayedLedger = {
  /**
   * The `LedgerAt` over one operation's sources. A ledger it answers is not
   * held: the root stays readable while `retain` keeps it, and a read after
   * that is this process's own fault (`readingLedger`).
   */
  readonly ledgerAt: (source: ReplayedLedgerSource) => LedgerAt;
  /**
   * Verifies the confirmed chain up to the confirmed head and makes the
   * store hold the head's ledger. Every block above the verified head is
   * replayed over its parent's ledger, whether or not the store already
   * holds the root it commits, unless this process replayed that block
   * itself; the verified head moves up block by block. True once the head
   * is verified and its ledger persisted. False when the source does not
   * know the head or serves no payload of it, when a block is refused (its
   * replay, its root or its parent's root, reported through `onRefusal`),
   * or when the head's own header commits another root than the confirmed
   * one. A payload a source failed to fetch rejects with
   * `RetainedDaPayloadUnavailableError`, except as below.
   *
   * A block below the head that cannot be replayed any more (no source
   * retains its payload, or the ledger it starts from cannot be derived) is
   * passed and reported through `onUnverified`. The head's payload is always
   * waited for. `confirmed` must be a stable head
   * (`replayed-ledger.confirmed.ts`).
   *
   * A head that is not genesis is offered to `bootstrap`, at most once per
   * call: before the walk when there is no verified head, and when the walk
   * finds neither the head's ledger nor its parent's held or derivable. When
   * it makes the store hold the head's ledger, the head is the verified head
   * and is reported through `onUnverified` (`ledger-snapshot`), with no
   * replay of it.
   *
   * `retainTurn` is the caller's turn to retain, awaited after each block of
   * the walk is passed, so a walk over many blocks need not hold every
   * ledger it derives until it ends. The roots the walk holds are pinned, so
   * a `retain` run from it never drops the ledger the walk continues from.
   */
  readonly ensure: (
    confirmed: { readonly headerHash: string; readonly utxosRoot: string },
    source: ReplayedLedgerSource,
    retainTurn?: () => Promise<void>,
  ) => Promise<boolean>;
  /**
   * Keeps every window block's `utxos_root` and `prev_utxos_root`, the newest
   * root, the confirmed root and the roots running replays hold, and drops
   * every other ledger. Resolves to the roots of that set this keeper had
   * seen held in this process that are not held any more, which calls for an
   * operations alert; a root never held is only not derived yet.
   */
  readonly retain: (
    window: ReplayedLedgerWindow,
  ) => Promise<ReadonlySet<string>>;
};

type Step =
  | { readonly kind: "held"; readonly held: HeldLedger }
  | { readonly kind: "mismatch"; readonly ledger: Ledger }
  /** The block is refused, as `onRefusal` was told. */
  | { readonly kind: "refused" }
  /** A header, payload or parent ledger is not available. */
  | { readonly kind: "absent" };

const label = (headerHash: string) => `replayed ledger after ${headerHash}`;

export const createReplayedLedger = ({
  store,
  verifiedHead = memoryVerifiedHead(),
  onRefusal,
  onUnverified,
  onHeld,
  bootstrap,
}: {
  readonly store: LedgerStore;
  /** Where the verified head is kept; in this process only by default. */
  readonly verifiedHead?: ReplayedLedgerVerifiedHead;
  readonly onRefusal?: (refusal: ReplayedLedgerRefusal) => void;
  /** Told each confirmed block `ensure` passes without replaying it. */
  readonly onUnverified?: (block: ReplayedLedgerUnverified) => void;
  /** Told each header whose ledger the store is found to hold, with the
   * ledger's root. */
  readonly onHeld?: (headerHash: string, root: string) => void;
  /** Makes the store hold the ledger of `utxosRoot` without replaying to
   * it, true when it holds it afterwards; asked by `ensure` for the
   * confirmed head when there is no verified head, or when the head's parent
   * ledger is neither held nor derivable (`replayed-ledger.confirmed.ts`). */
  readonly bootstrap?: (utxosRoot: string) => Promise<boolean>;
}): ReplayedLedger => {
  const holds = createLedgerHolds(store);
  const refuse = (refusal: ReplayedLedgerRefusal) => onRefusal?.(refusal);
  /** The blocks this process replayed whose ledgers opened their roots,
   * each with that root. One leaves once the verified head passed it or
   * `retain` no longer names its root. */
  const replayed = new Map<string, string>();

  /**
   * Replays `block` over `parent`, persisting the ledger when it opens the
   * block's root. Called by one caller at a time per block. A lookup
   * (`verify` false) takes a root the store already holds, which a caller
   * this one waited on may have persisted; a verification always replays.
   */
  const replayStep = async (
    block: ChainBlock,
    parent: HeldLedger,
    source: ReplayedLedgerSource,
    mode: { readonly target: boolean; readonly verify: boolean },
  ): Promise<Step> => {
    const { headerHash, header } = block;
    const { target } = mode;
    if (!mode.verify) {
      const done = await holds.open(header.utxosRoot);
      if (done !== undefined) {
        onHeld?.(headerHash, header.utxosRoot);
        return { kind: "held", held: done };
      }
    }
    const release = holds.reserve(header.utxosRoot);
    try {
      const payload = await authenticPayload(source, headerHash);
      if (payload === undefined) {
        release();
        return { kind: "absent" };
      }
      const ledger = await replayBlock({
        headerHash,
        payload,
        parent: parent.ledger,
        source,
        refuse,
      });
      if (ledger === undefined) {
        release();
        return { kind: "refused" };
      }
      if (ledger.root !== header.utxosRoot) {
        const { message } = new LedgerRootMismatchError(
          label(headerHash),
          header.utxosRoot,
          ledger.root,
        );
        unboundWait(source, headerHash, message);
        refuse({ headerHash, kind: "root-mismatch", detail: message });
        release();
        return target ? { kind: "mismatch", ledger } : { kind: "refused" };
      }
      await holds.persist(ledger);
      replayed.set(headerHash, header.utxosRoot);
      onHeld?.(headerHash, header.utxosRoot);
      return { kind: "held", held: { ledger, release } };
    } catch (error) {
      release();
      throw error;
    }
  };

  /** The ledger after `headerHash`, held until released; the replayed
   * ledger of a target that does not open its root; or nothing. */
  const derive = async (
    headerHash: string,
    source: ReplayedLedgerSource,
  ): Promise<Step> => {
    if (headerHash === SDK.GENESIS_HEADER_HASH) {
      const empty = await holds.open(EMPTY_LEDGER_ROOT);
      return empty === undefined
        ? { kind: "absent" }
        : { kind: "held", held: empty };
    }
    const target = await authenticHeader(source, headerHash);
    if (target === undefined) return { kind: "absent" };
    const hit = await holds.open(target.utxosRoot);
    if (hit !== undefined) {
      onHeld?.(headerHash, target.utxosRoot);
      return { kind: "held", held: hit };
    }
    // Walk back by headers to a block whose parent's ledger is held.
    const chain = [{ headerHash, header: target }];
    let parent: HeldLedger | undefined;
    for (;;) {
      const top = chain[chain.length - 1]!.header;
      parent = await holds.open(top.prevUtxosRoot);
      if (parent !== undefined) break;
      if (top.prevHeaderHash === SDK.GENESIS_HEADER_HASH) {
        parent = await holds.open(EMPTY_LEDGER_ROOT);
        if (parent === undefined) return { kind: "absent" };
        break;
      }
      const below = await authenticHeader(source, top.prevHeaderHash);
      if (below === undefined) return { kind: "absent" };
      chain.push({ headerHash: top.prevHeaderHash, header: below });
    }
    try {
      for (const block of chain.reverse()) {
        if (block.header.prevUtxosRoot !== parent.ledger.root) {
          refuse({
            headerHash: block.headerHash,
            kind: "parent-mismatch",
            detail: `the block starts from ${block.header.prevUtxosRoot}, its parent's ledger opens ${parent.ledger.root}`,
          });
          return { kind: "refused" };
        }
        const from: HeldLedger = parent;
        const step = await holds.exclusive(block.headerHash, () =>
          replayStep(block, from, source, {
            target: block.headerHash === headerHash,
            verify: false,
          }),
        );
        if (step.kind !== "held") return step;
        parent.release();
        parent = step.held;
      }
      const held = parent;
      parent = undefined;
      return { kind: "held", held };
    } finally {
      parent?.release();
    }
  };

  const ensure = confirmedChainVerifier({
    holds,
    verifiedHead,
    ledgerAfter: async (headerHash, source) => {
      const step = await derive(headerHash, source);
      return step.kind === "held" ? step.held : undefined;
    },
    replay: async (block, parent, source) => {
      const step = await replayStep(block, parent, source, {
        target: false,
        verify: true,
      });
      return step.kind === "held"
        ? step.held
        : step.kind === "absent"
          ? "absent"
          : "refused";
    },
    replayedHere: (headerHash) => replayed.has(headerHash),
    passedBy: (headerHash) => replayed.delete(headerHash),
    refuse,
    ...(onUnverified === undefined ? {} : { onUnverified }),
    ...(onHeld === undefined ? {} : { onHeld }),
    ...(bootstrap === undefined ? {} : { bootstrap }),
  });

  return {
    ledgerAt: (source) => async (headerHash) => {
      const step = await derive(headerHash, source);
      if (step.kind === "mismatch") return step.ledger;
      if (step.kind !== "held") return undefined;
      step.held.release();
      return step.held.ledger;
    },
    ensure,
    retain: ({ blocks, newestRoot, confirmedRoot }) => {
      const roots = new Set([newestRoot, confirmedRoot]);
      for (const block of blocks) {
        roots.add(block.utxosRoot);
        roots.add(block.prevUtxosRoot);
      }
      for (const [headerHash, root] of replayed)
        if (!roots.has(root)) replayed.delete(headerHash);
      return holds.retain(roots);
    },
  };
};

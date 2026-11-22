/**
 * The ledger a watcher keeps by replaying blocks (DS-3).
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
 * A block whose replay is refused, whose replayed ledger does not open its
 * `utxos_root`, or that does not start from its parent's ledger is reported
 * through `onRefusal` and never persisted. Its own lookup still gets the
 * replayed entries on a root mismatch, unchecked, so a classifier records the
 * post-ledger mismatch; nothing above it is derived from them. A payload no
 * attested availability commitment binds yet (`ReplayedLedgerSource.binding`)
 * is not refused that way: its copy is released and the read waits.
 */
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";

import { type BlockReplayContext } from "./block-replay.js";
import {
  type LedgerAt,
  type LedgerEntry,
  LedgerRootMismatchError,
} from "./ledger-at.js";
import {
  EMPTY_LEDGER_ROOT,
  type Ledger,
  type LedgerStore,
} from "./ledger-store/index.js";
import {
  createLedgerHolds,
  type HeldLedger,
  storeCall,
} from "./replayed-ledger.holds.js";
import {
  authenticHeader,
  authenticPayload,
  replayBlock,
  unboundWait,
} from "./replayed-ledger.replay-block.js";

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
  /** The `LedgerAt` over one operation's sources. */
  readonly ledgerAt: (source: ReplayedLedgerSource) => LedgerAt;
  /**
   * Makes the store hold the ledger after the confirmed head, replaying up
   * to it when needed. True once that ledger is persisted; false when it
   * cannot be derived yet, or when the head's own header commits another
   * root (refused as a root mismatch).
   */
  readonly ensure: (
    confirmed: { readonly headerHash: string; readonly utxosRoot: string },
    source: ReplayedLedgerSource,
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
  | { readonly kind: "mismatch"; readonly entries: readonly LedgerEntry[] }
  | { readonly kind: "absent" };

const label = (headerHash: string) => `replayed ledger after ${headerHash}`;

export const createReplayedLedger = ({
  store,
  onRefusal,
  onHeld,
}: {
  readonly store: LedgerStore;
  readonly onRefusal?: (refusal: ReplayedLedgerRefusal) => void;
  /** Told each header whose ledger the store is found to hold, with the
   * ledger's root. */
  readonly onHeld?: (headerHash: string, root: string) => void;
}): ReplayedLedger => {
  const holds = createLedgerHolds(store);
  const refuse = (refusal: ReplayedLedgerRefusal) => onRefusal?.(refusal);

  /** The ledger's entries: the one place a `LedgerAt` answer is listed. */
  const listed = (ledger: Ledger): Promise<readonly LedgerEntry[]> =>
    storeCall(`list ledger ${ledger.root}`, async () => {
      const entries: LedgerEntry[] = [];
      for await (const entry of ledger.entries()) entries.push(entry);
      return entries;
    });

  /** Replays `block` over `parent`, persisting the ledger when it opens the
   * block's root. Called by one caller at a time per block. */
  const replayStep = async (
    block: { readonly headerHash: string; readonly header: SDK.Header },
    parent: HeldLedger,
    source: ReplayedLedgerSource,
    target: boolean,
  ): Promise<Step> => {
    const { headerHash, header } = block;
    // A caller this one waited on may have persisted it.
    const done = await holds.open(header.utxosRoot);
    if (done !== undefined) {
      onHeld?.(headerHash, header.utxosRoot);
      return { kind: "held", held: done };
    }
    const release = holds.reserve(header.utxosRoot);
    try {
      const payload = await authenticPayload(source, headerHash);
      if (payload === undefined) {
        release();
        return { kind: "absent" };
      }
      const replayed = await replayBlock({
        headerHash,
        payload,
        parent: parent.ledger,
        source,
        refuse,
      });
      if (replayed === undefined) {
        release();
        return { kind: "absent" };
      }
      if (replayed.root !== header.utxosRoot) {
        const { message } = new LedgerRootMismatchError(
          label(headerHash),
          header.utxosRoot,
          replayed.root,
        );
        unboundWait(source, headerHash, message);
        refuse({ headerHash, kind: "root-mismatch", detail: message });
        const entries = target ? await listed(replayed) : [];
        release();
        return target ? { kind: "mismatch", entries } : { kind: "absent" };
      }
      await holds.persist(replayed);
      onHeld?.(headerHash, header.utxosRoot);
      return { kind: "held", held: { ledger: replayed, release } };
    } catch (error) {
      release();
      throw error;
    }
  };

  /** The ledger after `headerHash`, held until released; the replayed
   * entries of a target that does not open its root; or nothing. */
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
          return { kind: "absent" };
        }
        const from: HeldLedger = parent;
        const step = await holds.exclusive(block.headerHash, () =>
          replayStep(block, from, source, block.headerHash === headerHash),
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

  return {
    ledgerAt: (source) => async (headerHash) => {
      const step = await derive(headerHash, source);
      if (step.kind === "absent") return undefined;
      if (step.kind === "mismatch") return step.entries;
      try {
        return await listed(step.held.ledger);
      } finally {
        step.held.release();
      }
    },
    ensure: async ({ headerHash, utxosRoot }, source) => {
      const header =
        headerHash === SDK.GENESIS_HEADER_HASH
          ? { utxosRoot: EMPTY_LEDGER_ROOT }
          : await authenticHeader(source, headerHash);
      if (header === undefined) return false;
      if (header.utxosRoot !== utxosRoot) {
        refuse({
          headerHash,
          kind: "root-mismatch",
          detail: `the confirmed root ${utxosRoot} differs from the root ${header.utxosRoot} its header commits`,
        });
        return false;
      }
      const step = await derive(headerHash, source);
      if (step.kind !== "held") return false;
      step.held.release();
      return true;
    },
    retain: ({ blocks, newestRoot, confirmedRoot }) => {
      const roots = new Set([newestRoot, confirmedRoot]);
      for (const block of blocks) {
        roots.add(block.utxosRoot);
        roots.add(block.prevUtxosRoot);
      }
      return holds.retain(roots);
    },
  };
};

/**
 * Verifying the confirmed chain by block (`ReplayedLedger.ensure`).
 *
 * The store answers by root, and a held root says only that some block's
 * replay opened it: an empty block commits its parent's root, and so can a
 * block whose events do not replay to it. A confirmed block is verified when
 * its own events, replayed over its parent's ledger, open the root its
 * header commits. The verified head records how far up the confirmed chain
 * that was done, across restarts, so each confirmed head is checked from the
 * last one: every block between them is replayed, unless this process
 * already replayed that block.
 *
 * The confirmed head handed in must be stable. One that is not above the
 * verified head on the same chain is verified from the lowest block the
 * source still knows.
 */
import * as SDK from "@al-ft/midgard-sdk/core";

import { EMPTY_LEDGER_ROOT } from "./ledger-store/index.js";
import {
  type createLedgerHolds,
  type HeldLedger,
  storeCall,
} from "./replayed-ledger.holds.js";
import type {
  ReplayedLedgerRefusal,
  ReplayedLedgerSource,
} from "./replayed-ledger.js";
import { authenticHeader } from "./replayed-ledger.replay-block.js";
import { isRetainedDaPayloadUnavailableError } from "./transition-trace/fetch.retained-da-payload-unavailable.js";

/**
 * The confirmed block up to which this watcher has verified the confirmed
 * chain, kept across restarts: every confirmed block at or below it was
 * replayed over its parent's ledger and opened its root, or could not be
 * replayed any more (`ReplayedLedgerUnverified`).
 */
export type ReplayedLedgerVerifiedHead = {
  /** The header hash last written, or `undefined` when none was. */
  readonly read: () => Promise<string | undefined>;
  readonly write: (headerHash: string) => Promise<void>;
};

/** A verified head that lasts as long as the process. */
export const memoryVerifiedHead = (): ReplayedLedgerVerifiedHead => {
  let headerHash: string | undefined;
  return {
    read: async () => headerHash,
    write: async (next) => {
      headerHash = next;
    },
  };
};

/**
 * A confirmed block `ensure` passed without replaying it.
 * `payload-not-retained`: every source answered that it holds no payload of
 * the block. `parent-ledger-not-held`: the ledger the block starts from is
 * neither held nor derivable. `header-not-held`: the source no longer knows
 * the block, so neither it nor any confirmed block below it down to the
 * verified head was replayed.
 */
export type ReplayedLedgerUnverified = {
  readonly headerHash: string;
  readonly cause:
    | "payload-not-retained"
    | "parent-ledger-not-held"
    | "header-not-held";
};

export type ChainBlock = {
  readonly headerHash: string;
  readonly header: SDK.Header;
};

/** One confirmed block's ledger, when the store holds it; or `ensure` stops. */
type Outcome = { readonly held: HeldLedger | undefined } | "stop";

/** Every source answered, and none holds the payload. */
const isPayloadNotRetained = (error: unknown): boolean =>
  isRetainedDaPayloadUnavailableError(error) &&
  error.availability === "not_found";

export const confirmedChainVerifier = (input: {
  readonly holds: Pick<
    ReturnType<typeof createLedgerHolds>,
    "open" | "exclusive"
  >;
  readonly verifiedHead: ReplayedLedgerVerifiedHead;
  /** The ledger after `headerHash` by a root lookup, replaying up to it
   * when the store does not hold it; `undefined` when it cannot be derived. */
  readonly ledgerAfter: (
    headerHash: string,
    source: ReplayedLedgerSource,
  ) => Promise<HeldLedger | undefined>;
  /** Replays `block` over `parent` and persists the ledger when it opens the
   * block's root. `refused`: the block is refused, as `refuse` was told.
   * `absent`: its payload is not served. */
  readonly replay: (
    block: ChainBlock,
    parent: HeldLedger,
    source: ReplayedLedgerSource,
  ) => Promise<HeldLedger | "refused" | "absent">;
  /** Whether this process replayed `headerHash` and its ledger opened its
   * root. */
  readonly replayedHere: (headerHash: string) => boolean;
  /** Told each block the verified head moved up to. */
  readonly passedBy: (headerHash: string) => void;
  readonly refuse: (refusal: ReplayedLedgerRefusal) => void;
  readonly onUnverified?: (block: ReplayedLedgerUnverified) => void;
  readonly onHeld?: (headerHash: string, root: string) => void;
}) => {
  const { holds, refuse, onUnverified, onHeld } = input;
  /** The verified head as last read or written; `null` before the read. */
  let through: string | undefined | null = null;
  const verifiedThrough = async (): Promise<string | undefined> => {
    if (through === null)
      through = await storeCall("read the verified head", () =>
        input.verifiedHead.read(),
      );
    return through;
  };
  const verifyThrough = async (headerHash: string): Promise<void> => {
    await storeCall("write the verified head", () =>
      input.verifiedHead.write(headerHash),
    );
    through = headerHash;
  };

  /** A block that cannot be replayed is passed and reported; the head only
   * when the store holds the ledger it commits. */
  const pass = async (
    { headerHash, header }: ChainBlock,
    cause: ReplayedLedgerUnverified["cause"],
    head: boolean,
  ): Promise<Outcome> => {
    const own = await holds.open(header.utxosRoot);
    if (own === undefined && head) return "stop";
    try {
      onUnverified?.({ headerHash, cause });
      if (own !== undefined) onHeld?.(headerHash, header.utxosRoot);
    } catch (error) {
      own?.release();
      throw error;
    }
    return { held: own };
  };

  /**
   * The ledger `block` starts from: held, or, for the lowest block of the
   * walk, derived by a root lookup. Above it, a parent that left no ledger
   * was passed, so its ledger cannot be derived either.
   */
  const parentLedger = async (
    { header }: ChainBlock,
    source: ReplayedLedgerSource,
    lowest: boolean,
  ): Promise<HeldLedger | undefined> => {
    const held = await holds.open(header.prevUtxosRoot);
    if (held !== undefined || !lowest) return held;
    try {
      return await input.ledgerAfter(header.prevHeaderHash, source);
    } catch (error) {
      if (isPayloadNotRetained(error)) return undefined;
      throw error;
    }
  };

  /**
   * Verifies the confirmed block `block`. `parentRoot` is the root its
   * parent's header commits, when the source knows the parent; `below` is
   * the ledger after the parent, when the block under it left one.
   */
  const verifyBlock = async (
    block: ChainBlock,
    parentRoot: string | undefined,
    below: HeldLedger | undefined,
    source: ReplayedLedgerSource,
    { head, lowest }: { readonly head: boolean; readonly lowest: boolean },
  ): Promise<Outcome> => {
    const { headerHash, header } = block;
    if (parentRoot !== undefined && parentRoot !== header.prevUtxosRoot) {
      refuse({
        headerHash,
        kind: "parent-mismatch",
        detail: `the block starts from ${header.prevUtxosRoot}, its parent's ledger opens ${parentRoot}`,
      });
      return "stop";
    }
    if (input.replayedHere(headerHash)) {
      const own = await holds.open(header.utxosRoot);
      if (own !== undefined) {
        try {
          onHeld?.(headerHash, header.utxosRoot);
        } catch (error) {
          own.release();
          throw error;
        }
        return { held: own };
      }
    }
    const opened =
      below === undefined
        ? await parentLedger(block, source, lowest)
        : undefined;
    try {
      const parent = below ?? opened;
      if (parent === undefined)
        return await pass(block, "parent-ledger-not-held", head);
      let replayed: HeldLedger | "refused" | "absent";
      try {
        replayed = await holds.exclusive(headerHash, () =>
          input.replay(block, parent, source),
        );
      } catch (error) {
        // The head's payload is always waited for.
        if (head || !isPayloadNotRetained(error)) throw error;
        replayed = "absent";
      }
      if (replayed === "refused") return "stop";
      if (replayed !== "absent") return { held: replayed };
      return head ? "stop" : await pass(block, "payload-not-retained", false);
    } finally {
      opened?.release();
    }
  };

  return async (
    confirmed: { readonly headerHash: string; readonly utxosRoot: string },
    source: ReplayedLedgerSource,
  ): Promise<boolean> => {
    const { headerHash, utxosRoot } = confirmed;
    const genesis = headerHash === SDK.GENESIS_HEADER_HASH;
    const header = genesis
      ? undefined
      : await authenticHeader(source, headerHash);
    const committed = genesis ? EMPTY_LEDGER_ROOT : header?.utxosRoot;
    if (committed === undefined) return false;
    if (committed !== utxosRoot) {
      refuse({
        headerHash,
        kind: "root-mismatch",
        detail: `the confirmed root ${utxosRoot} differs from the root ${committed} its header commits`,
      });
      return false;
    }
    // Walk back by headers to the verified head.
    const verified = await verifiedThrough();
    if (header === undefined || headerHash === verified) {
      const held = await input.ledgerAfter(headerHash, source);
      held?.release();
      return held !== undefined;
    }
    const chain: ChainBlock[] = [{ headerHash, header }];
    /** The root the lowest block's parent commits, when it is known. */
    let parentRoot: string | undefined;
    /** The first header below the chain the source does not know. */
    let unknown: string | undefined;
    for (;;) {
      const below = chain[chain.length - 1]!.header.prevHeaderHash;
      if (below === SDK.GENESIS_HEADER_HASH) {
        parentRoot = EMPTY_LEDGER_ROOT;
        break;
      }
      const next = await authenticHeader(source, below);
      if (below === verified) {
        parentRoot = next?.utxosRoot;
        break;
      }
      if (next === undefined) {
        unknown = below;
        break;
      }
      chain.push({ headerHash: below, header: next });
    }
    let carry: HeldLedger | undefined;
    try {
      const lowest = chain[chain.length - 1]!.headerHash;
      for (const block of chain.reverse()) {
        const outcome = await verifyBlock(block, parentRoot, carry, source, {
          head: block.headerHash === headerHash,
          lowest: block.headerHash === lowest,
        });
        if (outcome === "stop") return false;
        carry?.release();
        carry = outcome.held;
        await verifyThrough(block.headerHash);
        input.passedBy(block.headerHash);
        if (unknown !== undefined)
          onUnverified?.({ headerHash: unknown, cause: "header-not-held" });
        unknown = undefined;
        parentRoot = block.header.utxosRoot;
      }
      return true;
    } finally {
      carry?.release();
    }
  };
};

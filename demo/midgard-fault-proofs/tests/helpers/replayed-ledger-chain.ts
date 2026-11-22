/** A chain of committed blocks that each spend one passing-script seed, for
 * the replayed ledger's tests: every block replays over its parent's ledger
 * and opens its own `utxos_root`, so each has a distinct ledger. The chain
 * starts above a ledger that holds every seed (`base`), as the ledger after a
 * block no fixture header names. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect } from "effect";

import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import type { ReplayedLedgerSource } from "../../src/replayed-ledger.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import {
  depositSeed,
  plutusExecutionBlockFixture,
  scenarioSeedLovelace,
  scriptCredential,
} from "./plutus-execution-block-fixture.js";

export type ReplayedLedgerChainBlock = Readonly<{
  headerHash: string;
  header: SDK.Header;
  payload: SDKDa.DaPayload;
  /** The block's retained-DA payload envelope. */
  payloadEnvelopeCbor: Buffer;
  /** The ledger after the block. */
  entries: readonly LedgerEntry[];
}>;

/** The header hash the first block names as its parent. */
export const REPLAYED_LEDGER_CHAIN_PARENT = "11".repeat(28);

const FIRST_SEED_BYTE = 0x30;

const seedOf = (byte: number) =>
  depositSeed(
    byte,
    scriptCredential("passing"),
    scenarioSeedLovelace(false, undefined, undefined),
  );

/**
 * `length` blocks in order, each above the one before it, and `forks`: for
 * each index `i` given, one more block above block `i - 1` (the base for 0)
 * that spends another seed than block `i`, so it commits another root.
 */
export const replayedLedgerChain = async (
  length: number,
  forks: readonly number[] = [],
) => {
  const bytes = Array.from(
    { length: length + forks.length },
    (_, index) => FIRST_SEED_BYTE + index,
  );
  const base: readonly LedgerEntry[] = bytes.map(seedOf);
  const build = async (
    seedByte: number,
    parent: { headerHash: string; entries: readonly LedgerEntry[] },
  ): Promise<ReplayedLedgerChainBlock> => {
    const fixture = await plutusExecutionBlockFixture("passing", {
      seedByte,
      predecessorEntries: parent.entries,
      header: { prevHeaderHash: parent.headerHash },
    });
    const { payload } = await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    });
    return {
      headerHash: payload.block_body.header_hash,
      header: payload.block_body.header,
      payload,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      entries: fixture.postEntries,
    };
  };
  const blocks: ReplayedLedgerChainBlock[] = [];
  let parent = { headerHash: REPLAYED_LEDGER_CHAIN_PARENT, entries: base };
  for (let index = 0; index < length; index += 1) {
    const block = await build(bytes[index]!, parent);
    blocks.push(block);
    parent = block;
  }
  const forked: ReplayedLedgerChainBlock[] = [];
  for (const [offset, index] of forks.entries()) {
    const below =
      index === 0
        ? { headerHash: REPLAYED_LEDGER_CHAIN_PARENT, entries: base }
        : blocks[index - 1]!;
    forked.push(await build(bytes[length + offset]!, below));
  }
  return {
    base,
    baseRoot: await ledgerEntriesRoot(base),
    blocks,
    forks: forked,
  };
};

/**
 * A source serving `blocks`' headers and payloads, recording each payload
 * request in `asked`; `serves` decides whether a payload is served (a payload
 * not served is not retained yet).
 */
export const replayedLedgerChainSource = (
  blocks: readonly ReplayedLedgerChainBlock[],
  options: Readonly<{
    asked?: string[];
    serves?: (headerHash: string) => boolean;
    onRequest?: (headerHash: string) => void | Promise<void>;
  }> = {},
): ReplayedLedgerSource => {
  const byHash = new Map(blocks.map((block) => [block.headerHash, block]));
  return {
    headerAt: async (headerHash) => byHash.get(headerHash)?.header,
    payloadAt: async (headerHash) => {
      options.asked?.push(headerHash);
      await options.onRequest?.(headerHash);
      if (options.serves !== undefined && !options.serves(headerHash))
        return undefined;
      return byHash.get(headerHash)?.payload;
    },
    // The blocks carry only an L2 transaction.
    materialFor: async (payload) => ({
      blockTimeMs: Number(payload.block_body.header.endTime),
      replayUserEvent: () => Effect.die("the block has no user event"),
      verifyForcedSource: () => Effect.die("the block has no forced order"),
    }),
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  };
};

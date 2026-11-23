/**
 * Test ledger source. DA payloads carry no UTxO set, so fixture builders
 * record the ledger each block commits to, keyed by that block's
 * `utxos_root`, and tests read it back by root or by header, admitted to a
 * memory ledger store against that root.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";

import {
  type LedgerAt,
  ledgerEntriesRoot,
  type LedgerEntry,
} from "../../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
} from "../../src/ledger-store/index.js";
import type { RetainedDaPayloadSource } from "../../src/transition-trace/fetch.admit-retained-da-provenance.js";
import { fetchRetainedDaPayloadByHeaderHash } from "../../src/transition-trace/fetch.fetch-retained-da-payload-by-header-hash.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.reconstruct-da-payload.js";

type FixtureLedgerEntry =
  | SDKDa.DaPayloadEntry
  | Readonly<{ key: Uint8Array; value: Uint8Array }>;

const ledgersByRoot = new Map<string, readonly SDKDa.DaPayloadEntry[]>();

const hexEntry = (entry: FixtureLedgerEntry): SDKDa.DaPayloadEntry =>
  Array.isArray(entry)
    ? [entry[0], entry[1]]
    : [
        Buffer.from((entry as { key: Uint8Array }).key).toString("hex"),
        Buffer.from((entry as { value: Uint8Array }).value).toString("hex"),
      ];

/** `entries` admitted against `root` to a memory store of their own. */
export const memoryLedger = async (
  entries: Iterable<LedgerEntry>,
  root: string,
): Promise<Ledger> =>
  await createLedgerStore(createMemoryLedgerStorage()).admit(entries, root);

/** Every entry `ledger` lists, in key order. */
export const entriesOf = async (ledger: Ledger): Promise<LedgerEntry[]> => {
  const entries: LedgerEntry[] = [];
  for await (const entry of ledger.entries()) entries.push(entry);
  return entries;
};

/** The root of the entries `ledger` lists, hashed afresh. */
export const entriesRoot = async (ledger: Ledger): Promise<string> =>
  await ledgerEntriesRoot(await entriesOf(ledger));

/** Records `entries` as the ledger whose root is `utxosRoot`. */
export const recordFixtureLedger = (
  utxosRoot: string,
  entries: readonly FixtureLedgerEntry[],
): void => {
  ledgersByRoot.set(
    utxosRoot,
    entries
      .map(hexEntry)
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0)),
  );
};

/** The recorded ledger with root `utxosRoot`, as `[out-ref, output]` hex. */
export const fixtureLedgerEntries = (
  utxosRoot: string,
): SDKDa.DaPayloadEntry[] => {
  if (utxosRoot === SDK.EMPTY_MERKLE_TREE_ROOT) return [];
  const entries = ledgersByRoot.get(utxosRoot);
  if (entries === undefined)
    throw new Error(`no fixture ledger recorded for root ${utxosRoot}`);
  return entries.map(([key, value]) => [key, value]);
};

/** The recorded ledger after a block with header `header`. */
export const fixtureLedgerAfter = (header: {
  readonly utxosRoot: string;
}): SDKDa.DaPayloadEntry[] => fixtureLedgerEntries(header.utxosRoot);

/** The recorded ledger with root `utxosRoot`, as `LedgerEntry` values. */
export const fixtureLedgerEntriesByRoot = (utxosRoot: string): LedgerEntry[] =>
  fixtureLedgerEntries(utxosRoot).map(([key, value]) => ({
    outRef: Buffer.from(key, "hex"),
    output: Buffer.from(value, "hex"),
  }));

/**
 * A `LedgerAt` over retained fixture payloads: it reads the header of the
 * requested block from `sources` and returns the ledger recorded for its
 * `utxos_root`, or `undefined` when none was recorded.
 */
export const fixtureLedgerAt =
  ({
    sources,
  }: {
    readonly sources: readonly RetainedDaPayloadSource[];
  }): LedgerAt =>
  async (headerHash) => {
    const fetched = await fetchRetainedDaPayloadByHeaderHash({
      headerHash,
      sources,
    });
    const block = await reconstructDaPayload({
      payloadEnvelopeCbor: fetched.payloadEnvelopeCbor,
      expectedHeaderHash: headerHash,
    });
    const root = block.header.utxosRoot;
    if (root !== SDK.EMPTY_MERKLE_TREE_ROOT && !ledgersByRoot.has(root))
      return undefined;
    return await memoryLedger(fixtureLedgerEntriesByRoot(root), root);
  };

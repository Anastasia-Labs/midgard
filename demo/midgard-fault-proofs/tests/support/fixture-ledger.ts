/**
 * Test ledger source. DA payloads carry no UTxO set, so fixture builders
 * record the ledger each block commits to, keyed by that block's
 * `utxos_root`, and tests read it back by root or by header. Production code
 * under test admits any ledger only through `checkLedgerAgainstRoot`.
 */
import * as SDK from "@al-ft/midgard-sdk";

import type { LedgerAt, LedgerEntry } from "../../src/ledger-at.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../../src/transition-trace/fetch.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";

type FixtureLedgerEntry =
  | SDK.DaPayloadEntry
  | Readonly<{ key: Uint8Array; value: Uint8Array }>;

const ledgersByRoot = new Map<string, readonly SDK.DaPayloadEntry[]>();

const hexEntry = (entry: FixtureLedgerEntry): SDK.DaPayloadEntry =>
  Array.isArray(entry)
    ? [entry[0], entry[1]]
    : [
        Buffer.from((entry as { key: Uint8Array }).key).toString("hex"),
        Buffer.from((entry as { value: Uint8Array }).value).toString("hex"),
      ];

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
): SDK.DaPayloadEntry[] => {
  if (utxosRoot === SDK.EMPTY_MERKLE_TREE_ROOT) return [];
  const entries = ledgersByRoot.get(utxosRoot);
  if (entries === undefined)
    throw new Error(`no fixture ledger recorded for root ${utxosRoot}`);
  return entries.map(([key, value]) => [key, value]);
};

/** The recorded ledger after a block with header `header`. */
export const fixtureLedgerAfter = (header: {
  readonly utxosRoot: string;
}): SDK.DaPayloadEntry[] => fixtureLedgerEntries(header.utxosRoot);

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
    return fixtureLedgerEntriesByRoot(root);
  };

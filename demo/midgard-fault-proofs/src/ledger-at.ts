/**
 * The UTxO ledger after a block, looked up by header hash.
 *
 * DA payloads carry no UTxO set: every party derives the ledger by replaying
 * blocks. A `LedgerAt` source returns the ledger after the block a header hash
 * names, and nothing it returns is trusted. `checkLedgerAgainstRoot` recomputes
 * the MPF root of the entries' canonical ledger descriptors (the same
 * descriptor the node commits, `buildCanonicalMidgardLedgerEntryOutputMaterial`)
 * and refuses a ledger that does not open the header's `utxos_root`. A family
 * takes only a `CheckedLedger`, which only this module constructs, so an
 * unchecked or substituted ledger never reaches a proof builder.
 */

import { readFile } from "node:fs/promises";

import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";

import { parseHex, parseJsonText } from "./json-file.js";
import {
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

/** One ledger entry: the §5.3 out-ref key bytes and the full output CBOR. */
export type LedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
};

/**
 * The ledger after the block `headerHash`, or `undefined` when the source does
 * not hold it. The result is untrusted until `checkLedgerAgainstRoot` admits it.
 */
export type LedgerAt = (
  headerHash: string,
) => Promise<readonly LedgerEntry[] | undefined>;

declare const checkedLedgerBrand: unique symbol;

/** A ledger whose descriptor trie opens `utxosRoot`. */
export type CheckedLedger = {
  readonly [checkedLedgerBrand]: true;
  readonly utxosRoot: string;
  /** Ascending by out-ref key bytes. */
  readonly entries: readonly LedgerEntry[];
  /** Out-ref key to canonical descriptor: the trie the root commits. */
  readonly trie: KeyValuePhasRoot;
  /** Out-ref key (hex) to full output CBOR. */
  readonly outputs: ReadonlyMap<string, Buffer>;
};

export class LedgerRootMismatchError extends Error {
  constructor(
    readonly label: string,
    readonly expectedRoot: string,
    readonly actualRoot: string,
  ) {
    super(
      `${label}: the ledger's root ${actualRoot} differs from the committed utxos_root ${expectedRoot}`,
    );
    this.name = "LedgerRootMismatchError";
  }
}

/** Refuses `entries` unless their descriptor trie opens `utxosRoot`. */
export const checkLedgerAgainstRoot = async ({
  entries,
  utxosRoot,
  label,
}: {
  readonly entries: readonly LedgerEntry[];
  readonly utxosRoot: string;
  readonly label: string;
}): Promise<CheckedLedger> => {
  const copied = entries.map(({ outRef, output }) => ({
    outRef: Buffer.from(outRef),
    output: Buffer.from(output),
  }));
  let trie: KeyValuePhasRoot;
  try {
    trie = await keyValuePhasRootWithCount(
      copied.map(({ outRef, output }) => ({
        key: outRef,
        value: buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef,
          outputCbor: output,
        }).descriptorCbor,
      })),
    );
  } catch (cause) {
    throw new Error(
      `${label}: the ledger has an entry that is not a canonical ledger entry`,
      { cause },
    );
  }
  if (trie.root !== utxosRoot)
    throw new LedgerRootMismatchError(label, utxosRoot, trie.root);
  const sorted = copied.sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  );
  return Object.freeze({
    utxosRoot,
    entries: Object.freeze(sorted),
    trie,
    outputs: new Map(
      sorted.map(({ outRef, output }) => [outRef.toString("hex"), output]),
    ),
  }) as unknown as CheckedLedger;
};

/** The ledger after the authenticated header `{ headerHash, utxosRoot }`. */
export const ledgerAfter = async ({
  ledgerAt,
  headerHash,
  utxosRoot,
  label,
}: {
  readonly ledgerAt: LedgerAt;
  readonly headerHash: string;
  readonly utxosRoot: string;
  readonly label: string;
}): Promise<CheckedLedger> => {
  const entries = await ledgerAt(headerHash);
  if (entries === undefined)
    throw new Error(`${label}: the ledger after ${headerHash} is unavailable`);
  return await checkLedgerAgainstRoot({ entries, utxosRoot, label });
};

/**
 * The ledger a block starts from, named by its authenticated header's
 * `prev_header_hash` and `prev_utxos_root`. Without a source only the empty
 * genesis ledger is available: it is the one ledger that opens the empty root.
 */
export const predecessorLedger = async ({
  prevHeaderHash,
  prevUtxosRoot,
  ledgerAt,
  label,
}: {
  readonly prevHeaderHash?: string;
  readonly prevUtxosRoot: string;
  readonly ledgerAt?: LedgerAt;
  readonly label: string;
}): Promise<CheckedLedger> => {
  if (ledgerAt !== undefined && prevHeaderHash !== undefined)
    return await ledgerAfter({
      ledgerAt,
      headerHash: prevHeaderHash,
      utxosRoot: prevUtxosRoot,
      label,
    });
  if (ledgerAt !== undefined)
    throw new Error(
      `${label}: a ledger source needs the predecessor's header hash`,
    );
  if (prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
    throw new Error(
      `${label}: the block starts from a non-empty ledger (root ${prevUtxosRoot}); supply the ledger after its predecessor`,
    );
  return await checkLedgerAgainstRoot({
    entries: [],
    utxosRoot: prevUtxosRoot,
    label,
  });
};

/**
 * Binds a checked ledger to the root a block commits as its starting ledger.
 * With no ledger, only the empty genesis root is satisfied.
 */
export const requireLedgerOpening = async ({
  ledger,
  utxosRoot,
  label,
}: {
  readonly ledger: CheckedLedger | undefined;
  readonly utxosRoot: string;
  readonly label: string;
}): Promise<CheckedLedger> => {
  if (ledger === undefined)
    return await predecessorLedger({ prevUtxosRoot: utxosRoot, label });
  if (ledger.utxosRoot !== utxosRoot)
    throw new LedgerRootMismatchError(label, utxosRoot, ledger.utxosRoot);
  return ledger;
};

/** A source holding exactly one block's ledger. */
export const singleLedgerAt =
  (headerHash: string, entries: readonly LedgerEntry[]): LedgerAt =>
  async (requested) =>
    requested === headerHash ? entries : undefined;

/**
 * A ledger file: `{ "headerHash": <28-byte hex>, "entries": [[<out-ref hex>,
 * <output CBOR hex>], ...] }`, the ledger after the block `headerHash`. The
 * returned source answers only for that header; its entries are checked like
 * any other source's.
 */
export const readLedgerFile = async (path: string): Promise<LedgerAt> => {
  const label = `ledger file ${path}`;
  const json = parseJsonText(
    await readFile(path, "utf8"),
    () => `${label} is not JSON`,
  );
  if (
    typeof json !== "object" ||
    json === null ||
    Object.keys(json).sort().join(",") !== "entries,headerHash"
  )
    throw new Error(`${label} must hold exactly headerHash and entries`);
  const { headerHash, entries } = json as Record<string, unknown>;
  if (!Array.isArray(entries))
    throw new Error(`${label}: entries must be an array`);
  return singleLedgerAt(
    parseHex(headerHash, `${label} headerHash`, 28),
    entries.map((entry: unknown, index) => {
      if (!Array.isArray(entry) || entry.length !== 2)
        throw new Error(`${label}: entry ${index.toString()} must be a pair`);
      return {
        outRef: Buffer.from(
          parseHex(entry[0], `${label} entry ${index.toString()} out-ref`),
          "hex",
        ),
        output: Buffer.from(
          parseHex(entry[1], `${label} entry ${index.toString()} output`),
          "hex",
        ),
      };
    }),
  );
};

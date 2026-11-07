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

import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";

import { undecodableNativeOutputDescriptor } from "./evidence/retained-ledger-output.js";
import { parseHex, parseJsonText } from "./json-file.js";
import {
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "./transition-trace/phas.js";

/** One ledger entry: the §5.3 out-ref key bytes and the full output CBOR. */
export type LedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
  /**
   * Optional. For an output with no canonical descriptor it must equal the
   * bytes-only derivation; otherwise it is ignored (see `withDescriptor`).
   */
  readonly descriptor?: Buffer | undefined;
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

/**
 * The entry with the descriptor its trie leaf holds. An output whose native
 * script_ref does not decode has no canonical descriptor; then its descriptor
 * is derived from the output bytes alone, a source-supplied one must equal it,
 * and the root check authenticates it. A canonical descriptor is not kept on
 * the entry.
 */
const withDescriptor = ({
  outRef,
  output,
  descriptor,
}: LedgerEntry): { entry: LedgerEntry; leaf: Buffer } => {
  const entry = { outRef: Buffer.from(outRef), output: Buffer.from(output) };
  try {
    const leaf = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: entry.outRef,
      outputCbor: entry.output,
    }).descriptorCbor;
    return { entry, leaf };
  } catch (cause) {
    let leaf: Buffer;
    try {
      leaf = undecodableNativeOutputDescriptor({
        key: entry.outRef,
        output: entry.output,
      });
    } catch {
      throw cause;
    }
    if (descriptor !== undefined && !leaf.equals(Buffer.from(descriptor)))
      throw new Error("ledger entry descriptor is not the output's own");
    return { entry: { ...entry, descriptor: leaf }, leaf };
  }
};

/**
 * `withDescriptor`, reusing `from`'s entry and leaf for an entry with the
 * same out-ref and output bytes: the leaf is a function of those bytes alone,
 * and `from`'s root check already admitted it. A source-supplied descriptor
 * that is not that leaf takes the full derivation, as without `from`.
 */
const reusingDescriptors = (
  from: CheckedLedger | undefined,
): ((entry: LedgerEntry) => { entry: LedgerEntry; leaf: Buffer }) => {
  if (from === undefined) return withDescriptor;
  const indexByKey = new Map(
    from.entries.map(({ outRef }, index) => [outRef.toString("hex"), index]),
  );
  return (entry) => {
    const index = indexByKey.get(Buffer.from(entry.outRef).toString("hex"));
    const prior = index === undefined ? undefined : from.entries[index];
    const leaf = index === undefined ? undefined : from.trie.entries[index];
    return prior !== undefined &&
      leaf !== undefined &&
      leaf.key.equals(prior.outRef) &&
      prior.output.equals(entry.output) &&
      (entry.descriptor === undefined ||
        leaf.value.equals(Buffer.from(entry.descriptor)))
      ? { entry: prior, leaf: leaf.value }
      : withDescriptor(entry);
  };
};

const ledgerTrie = async (
  entries: readonly LedgerEntry[],
  label: string,
  from?: CheckedLedger,
): Promise<{ copied: LedgerEntry[]; trie: KeyValuePhasRoot }> => {
  try {
    const leaves = entries.map(reusingDescriptors(from));
    return {
      copied: leaves.map(({ entry }) => entry),
      trie: await keyValuePhasRootWithCount(
        leaves.map(({ entry, leaf }) => ({ key: entry.outRef, value: leaf })),
      ),
    };
  } catch (cause) {
    throw new Error(
      `${label}: the ledger has an entry that is not a canonical ledger entry`,
      { cause },
    );
  }
};

/**
 * The root the entries' descriptor trie commits: the root a replay compares
 * with a header's `utxos_root`. Admits nothing; see `checkLedgerAgainstRoot`.
 */
export const ledgerEntriesRoot = async (
  entries: readonly LedgerEntry[],
): Promise<string> => (await ledgerTrie(entries, "ledger root")).trie.root;

/**
 * Refuses `entries` unless their descriptor trie opens `utxosRoot`. With
 * `from`, a ledger the entries were derived from, an entry `from` already
 * holds byte for byte keeps its descriptor rather than deriving it again; the
 * result and every refusal are the same as without it.
 */
export const checkLedgerAgainstRoot = async ({
  entries,
  utxosRoot,
  label,
  from,
}: {
  readonly entries: readonly LedgerEntry[];
  readonly utxosRoot: string;
  readonly label: string;
  readonly from?: CheckedLedger;
}): Promise<CheckedLedger> => {
  const { copied, trie } = await ledgerTrie(entries, label, from);
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
  return await checkLedgerAgainstRoot({
    entries,
    utxosRoot,
    label,
  });
};

const emptyLedger = Object.freeze({
  utxosRoot: EMPTY_MERKLE_TREE_ROOT,
  entries: Object.freeze([]),
  trie: Object.freeze({
    root: EMPTY_MERKLE_TREE_ROOT,
    count: 0n,
    entries: Object.freeze([]),
  }),
  outputs: new Map(),
}) as unknown as CheckedLedger;

const genesisLedger = (prevUtxosRoot: string, label: string): CheckedLedger => {
  if (prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
    throw new Error(
      `${label}: the block starts from a non-empty ledger (root ${prevUtxosRoot}); supply the ledger after its predecessor`,
    );
  return emptyLedger;
};

/**
 * The ledger a block starts from, named by its authenticated header's
 * `prev_header_hash` and `prev_utxos_root`. The known genesis ledger needs no
 * source. Other supplied sources are always consulted, including for an empty
 * root, and every returned ledger must open that root.
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
  if (
    prevHeaderHash === GENESIS_HEADER_HASH &&
    prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT
  )
    return emptyLedger;
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
  return genesisLedger(prevUtxosRoot, label);
};

/**
 * Binds a checked ledger to the root a block commits as its starting ledger.
 * With no ledger, only the empty genesis root is satisfied.
 */
export const requireLedgerOpening = ({
  ledger,
  utxosRoot,
  label,
}: {
  readonly ledger: CheckedLedger | undefined;
  readonly utxosRoot: string;
  readonly label: string;
}): CheckedLedger => {
  if (ledger === undefined) return genesisLedger(utxosRoot, label);
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
 * A ledger as JSON: `[[<out-ref hex>, <output CBOR hex>], ...]`; an entry
 * whose leaf is not canonical carries its descriptor hex third.
 */
export type LedgerJson = readonly (
  | readonly [string, string]
  | readonly [string, string, string]
)[];

export const ledgerJson = (ledger: CheckedLedger): LedgerJson =>
  ledger.entries.map(({ outRef, output, descriptor }) =>
    descriptor === undefined
      ? ([outRef.toString("hex"), output.toString("hex")] as const)
      : ([
          outRef.toString("hex"),
          output.toString("hex"),
          descriptor.toString("hex"),
        ] as const),
  );

/** Parses `LedgerJson` into (unchecked) ledger entries. */
export const parseLedgerJson = (
  value: unknown,
  label: string,
): LedgerEntry[] => {
  if (!Array.isArray(value))
    throw new Error(`${label}: entries must be an array`);
  return value.map((entry: unknown, index) => {
    if (!Array.isArray(entry) || (entry.length !== 2 && entry.length !== 3))
      throw new Error(
        `${label}: entry ${index.toString()} must be a pair or a triple`,
      );
    return {
      ...(entry.length === 3
        ? {
            descriptor: Buffer.from(
              parseHex(
                entry[2],
                `${label} entry ${index.toString()} descriptor`,
              ),
              "hex",
            ),
          }
        : {}),
      outRef: Buffer.from(
        parseHex(entry[0], `${label} entry ${index.toString()} out-ref`),
        "hex",
      ),
      output: Buffer.from(
        parseHex(entry[1], `${label} entry ${index.toString()} output`),
        "hex",
      ),
    };
  });
};

/** Re-admits a persisted `LedgerJson` against the root it must open. */
export const checkLedgerJson = async ({
  value,
  utxosRoot,
  label,
}: {
  readonly value: unknown;
  readonly utxosRoot: string;
  readonly label: string;
}): Promise<CheckedLedger> =>
  await checkLedgerAgainstRoot({
    entries: parseLedgerJson(value, label),
    utxosRoot,
    label,
  });

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
  return singleLedgerAt(
    parseHex(headerHash, `${label} headerHash`, 28),
    parseLedgerJson(entries, label),
  );
};

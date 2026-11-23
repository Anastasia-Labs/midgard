/**
 * The UTxO ledger after a block, looked up by header hash.
 *
 * DA payloads carry no UTxO set: every party derives the ledger by replaying
 * blocks. A `LedgerAt` source returns the ledger after the block a header hash
 * names, as a ledger store `Ledger`. `ledgerAfter` binds the answer to the
 * authenticated header: a ledger whose root is not the header's `utxos_root`
 * is refused, so a substituted ledger never reaches a proof builder. Hashes
 * are checked where entries enter a store (`LedgerStore.admit`), not here.
 *
 * `ledgerEntriesRoot` computes the MPF root of entries' canonical ledger
 * descriptors (the same descriptor the node commits,
 * `buildCanonicalMidgardLedgerEntryOutputMaterial`).
 */

import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk/core";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";

import { undecodableNativeOutputDescriptor } from "./evidence/retained-ledger-output.js";
import {
  EMPTY_LEDGER_ROOT,
  type Ledger,
  ledgerAt,
} from "./ledger-store/index.js";
import { keyValuePhasRootWithCount } from "./transition-trace/phas.js";

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
 * not hold it. The result is bound to the block only by `ledgerAfter`.
 */
export type LedgerAt = (headerHash: string) => Promise<Ledger | undefined>;

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

const notCanonical = (label: string, cause: unknown) =>
  new Error(
    `${label}: the ledger has an entry that is not a canonical ledger entry`,
    { cause },
  );

/** `entry` and its trie leaf, derived as every ledger root check derives
 * them; refused under `label` when it is not a canonical ledger entry. */
export const ledgerEntryLeaf = (
  entry: LedgerEntry,
  label: string,
): { entry: LedgerEntry; leaf: Buffer } => {
  try {
    return withDescriptor(entry);
  } catch (cause) {
    throw notCanonical(label, cause);
  }
};

/** The root the entries' descriptor trie commits: the root a replay compares
 * with a header's `utxos_root`. */
export const ledgerEntriesRoot = async (
  entries: readonly LedgerEntry[],
): Promise<string> => {
  const leaves = entries.map((entry) => ledgerEntryLeaf(entry, "ledger root"));
  try {
    return (
      await keyValuePhasRootWithCount(
        leaves.map(({ entry, leaf }) => ({ key: entry.outRef, value: leaf })),
      )
    ).root;
  } catch (cause) {
    throw notCanonical("ledger root", cause);
  }
};

/** The ledger after the authenticated header `{ headerHash, utxosRoot }`:
 * refused when the source does not hold it or holds a ledger with another
 * root. */
export const ledgerAfter = async ({
  ledgerAt: source,
  headerHash,
  utxosRoot,
  label,
}: {
  readonly ledgerAt: LedgerAt;
  readonly headerHash: string;
  readonly utxosRoot: string;
  readonly label: string;
}): Promise<Ledger> => {
  const ledger = await source(headerHash);
  if (ledger === undefined)
    throw new Error(`${label}: the ledger after ${headerHash} is unavailable`);
  if (ledger.root !== utxosRoot)
    throw new LedgerRootMismatchError(label, utxosRoot, ledger.root);
  return ledger;
};

/** The empty genesis ledger: nothing to read, nothing to persist. */
export const emptyLedger = (): Ledger =>
  ledgerAt(EMPTY_LEDGER_ROOT, {
    node: async () => undefined,
    outputs: async (outRefs) => outRefs.map(() => undefined),
  });

const genesisLedger = (prevUtxosRoot: string, label: string): Ledger => {
  if (prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
    throw new Error(
      `${label}: the block starts from a non-empty ledger (root ${prevUtxosRoot}); supply the ledger after its predecessor`,
    );
  return emptyLedger();
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
  ledgerAt: source,
  label,
}: {
  readonly prevHeaderHash?: string;
  readonly prevUtxosRoot: string;
  readonly ledgerAt?: LedgerAt;
  readonly label: string;
}): Promise<Ledger> => {
  if (
    prevHeaderHash === GENESIS_HEADER_HASH &&
    prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT
  )
    return emptyLedger();
  if (source !== undefined && prevHeaderHash !== undefined)
    return await ledgerAfter({
      ledgerAt: source,
      headerHash: prevHeaderHash,
      utxosRoot: prevUtxosRoot,
      label,
    });
  if (source !== undefined)
    throw new Error(
      `${label}: a ledger source needs the predecessor's header hash`,
    );
  return genesisLedger(prevUtxosRoot, label);
};

/**
 * Binds a ledger to the root a block commits as its starting ledger. With no
 * ledger, only the empty genesis root is satisfied.
 */
export const requireLedgerOpening = ({
  ledger,
  utxosRoot,
  label,
}: {
  readonly ledger: Ledger | undefined;
  readonly utxosRoot: string;
  readonly label: string;
}): Ledger => {
  if (ledger === undefined) return genesisLedger(utxosRoot, label);
  if (ledger.root !== utxosRoot)
    throw new LedgerRootMismatchError(label, utxosRoot, ledger.root);
  return ledger;
};

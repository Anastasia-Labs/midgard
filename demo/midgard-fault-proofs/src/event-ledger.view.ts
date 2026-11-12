/**
 * The ledger before one step of an event-ledger walk, as a checked ledger:
 * the predecessor's entries with the changes of the steps before it, merged
 * only when read.
 */
import { type CheckedLedger, type LedgerEntry } from "./ledger-at.js";
import {
  type KeyValuePhasEntry,
  type KeyValuePhasRoot,
  type mutableKeyValuePhasTrie,
} from "./transition-trace/phas.js";

/** What step `step` did to one out-ref: created `created`, or spent it. */
export type Change = Readonly<{
  step: number;
  created?: Readonly<{ entry: LedgerEntry; leaf: Buffer }>;
}>;

/** The outputs of the ledger before step `before`, read through `latest`. */
class LedgerOutputs implements ReadonlyMap<string, Buffer> {
  constructor(
    private readonly predecessor: CheckedLedger,
    private readonly latest: (key: string) => Change | undefined,
    private readonly sorted: () => readonly LedgerEntry[],
    readonly size: number,
  ) {}

  get(key: string): Buffer | undefined {
    const change = this.latest(key);
    return change === undefined
      ? this.predecessor.outputs.get(key)
      : change.created?.entry.output;
  }

  has(key: string): boolean {
    return this.get(key) !== undefined;
  }

  *entries(): MapIterator<[string, Buffer]> {
    for (const { outRef, output } of this.sorted())
      yield [outRef.toString("hex"), output];
  }

  *keys(): MapIterator<string> {
    for (const [key] of this.entries()) yield key;
  }

  *values(): MapIterator<Buffer> {
    for (const [, output] of this.entries()) yield output;
  }

  [Symbol.iterator](): MapIterator<[string, Buffer]> {
    return this.entries();
  }

  forEach(
    callback: (
      value: Buffer,
      key: string,
      map: ReadonlyMap<string, Buffer>,
    ) => void,
  ): void {
    for (const [key, output] of this.entries()) callback(output, key, this);
  }
}

/** The block's steps applied from `predecessor` up to `next`. */
export type Pass = {
  readonly predecessor: CheckedLedger;
  /** Each out-ref's changes, in step order. */
  readonly changes: Map<string, Change[]>;
  /** `roots[k]` and `sizes[k]`: the ledger before step `k`. */
  readonly roots: string[];
  readonly sizes: number[];
  /** `firstAfter[rank]`: the first applied step of a phase after `rank`. */
  readonly firstAfter: (number | undefined)[];
  next: number;
  priorRank: number;
  /** The step that failed to apply, and why. */
  failure?: Readonly<{ step: number; error: unknown }>;
  trie?: Promise<Awaited<ReturnType<typeof mutableKeyValuePhasTrie>>>;
};

export const latestBefore = (pass: Pass, key: string, step: number) => {
  const changes = pass.changes.get(key);
  if (changes === undefined) return undefined;
  for (let index = changes.length - 1; index >= 0; index -= 1)
    if (changes[index]!.step < step) return changes[index];
  return undefined;
};

export const record = (walk: Pass, key: string, change: Change) => {
  const changes = walk.changes.get(key);
  if (changes === undefined) walk.changes.set(key, [change]);
  else changes.push(change);
};

/** The ledger before step `step` of `pass`, as a checked ledger. Its entries
 * are merged from the predecessor's and the changes only when read. */
export const view = (pass: Pass, step: number): CheckedLedger => {
  const { predecessor } = pass;
  let merged:
    | { entries: readonly LedgerEntry[]; leaves: readonly KeyValuePhasEntry[] }
    | undefined;
  const sorted = () => {
    if (merged !== undefined) return merged;
    const kept: { entry: LedgerEntry; leaf: KeyValuePhasEntry }[] = [];
    const leaves = predecessor.trie.entries;
    for (const [index, entry] of predecessor.entries.entries())
      if (latestBefore(pass, entry.outRef.toString("hex"), step) === undefined)
        kept.push({ entry, leaf: leaves[index]! });
    const created: typeof kept = [];
    for (const key of pass.changes.keys()) {
      const leaf = latestBefore(pass, key, step)?.created;
      if (leaf !== undefined)
        created.push({
          entry: leaf.entry,
          leaf: { key: leaf.entry.outRef, value: leaf.leaf },
        });
    }
    created.sort((left, right) =>
      Buffer.compare(left.entry.outRef, right.entry.outRef),
    );
    const all: typeof kept = [];
    let [k, c] = [0, 0];
    while (k < kept.length || c < created.length)
      all.push(
        c === created.length ||
          (k < kept.length &&
            Buffer.compare(kept[k]!.entry.outRef, created[c]!.entry.outRef) < 0)
          ? kept[k++]!
          : created[c++]!,
      );
    merged = {
      entries: Object.freeze(all.map(({ entry }) => entry)),
      leaves: Object.freeze(all.map(({ leaf }) => leaf)),
    };
    return merged;
  };
  const trie: KeyValuePhasRoot = Object.freeze({
    root: pass.roots[step]!,
    count: BigInt(pass.sizes[step]!),
    get entries() {
      return sorted().leaves;
    },
  });
  return Object.freeze({
    utxosRoot: pass.roots[step]!,
    get entries() {
      return sorted().entries;
    },
    trie,
    outputs: new LedgerOutputs(
      predecessor,
      (key) => latestBefore(pass, key, step),
      () => sorted().entries,
      pass.sizes[step]!,
    ),
  }) as unknown as CheckedLedger;
};

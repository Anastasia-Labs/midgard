/**
 * The fork simulator's settlement record (`landed-blocks-sim.ts`): which
 * block on the model's processed chain includes each transaction, and the
 * folded and released ones the batch closure reads.
 */
import type { SimIncluded } from "./landed-blocks-sim.mempool.js";
import { rootLineage } from "./landed-blocks-sim.model.js";
import type { LandedSimEnv } from "./landed-blocks-sim.ports.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export const simRecords = (env: LandedSimEnv) => {
  const { book } = env;
  /** The transactions a block includes (an own block's journal, a foreign block's replay). */
  const includesOf = (header: string): readonly Buffer[] =>
    env.registry.get(header)!.own
      ? book.blocks.get(header)!.txIds
      : (env.includes.get(header) ?? []);

  /**
   * The model's settlement record at `model`: the block on the processed
   * chain that includes each transaction, the kind of the folded block (at
   * or below the frontier) for those a folded block includes, and those
   * whose folded block is no longer among the `retained` folds (its fold
   * is final, so its rows are gone).
   */
  const recordAt = (
    model: Readonly<{ frontier: string; tip: string }>,
    retained: ReadonlySet<string>,
  ) => {
    const chain = rootLineage(env.registry, model.tip);
    const foldedThrough = chain.indexOf(model.frontier);
    const settledBy = new Map<string, string>();
    const folded = new Map<string, "own" | "foreign">();
    const released = new Set<string>();
    chain.forEach((header, at) => {
      for (const id of includesOf(header)) {
        settledBy.set(hex(id), header);
        if (at > foldedThrough) continue;
        folded.set(hex(id), env.registry.get(header)!.own ? "own" : "foreign");
        if (!retained.has(header)) released.add(hex(id));
      }
    });
    return { settledBy, folded, released };
  };

  type Record = ReturnType<typeof recordAt>;

  /** The record and the live own block's members, as the batch closure reads them. */
  const includedBy = (
    record: Record,
    live: Readonly<{ txIds: readonly Buffer[] }> | undefined,
  ): SimIncluded => ({
    settled: new Set([
      ...record.settledBy.keys(),
      ...(live?.txIds ?? []).map(hex),
    ]),
    folded: record.folded,
    released: record.released,
  });

  return { includesOf, recordAt, includedBy };
};

export type SimRecord = ReturnType<ReturnType<typeof simRecords>["recordAt"]>;

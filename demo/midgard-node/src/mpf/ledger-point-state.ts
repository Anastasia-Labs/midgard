/**
 * A block's ledger as its own changes so far leave it, read by key: the
 * block's base ledger under the out-refs the block deleted and the outputs
 * it inserted. Validation resolves only the inputs a transaction names, so
 * a commit reads those keys and never the whole ledger.
 */
import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";
import type { UTxOStatePatch } from "@al-ft/midgard-validation/phase-b";
import { Effect } from "effect";

import { DatabaseError } from "../database/utils/common.js";

export type LedgerPointState = {
  /**
   * The outputs the ledger holds at `outRefHexes`, keyed by out-ref hex; an
   * out-ref it does not hold has no key. The map is the caller's own.
   */
  readonly read: (
    outRefHexes: Iterable<string>,
  ) => Effect.Effect<Map<string, Buffer>, DatabaseError>;
  /** Records the block deleting `outRefHex`. */
  readonly delete: (outRefHex: string) => void;
  /** Records the block inserting `output` at `outRefHex`. */
  readonly insert: (outRefHex: string, output: Buffer) => void;
  /** Records a state patch of a transaction the block applied. */
  readonly apply: (patch: UTxOStatePatch) => void;
};

/**
 * Point reads of `ledger` with the block's changes over it. A read the
 * ledger cannot answer (a node or output this node no longer holds) is this
 * node's fault, a `DatabaseError` on `table`, so the build holds.
 */
export const ledgerPointState = (
  ledger: Ledger,
  table: string,
): LedgerPointState => {
  const changes = new Map<string, Buffer | null>();
  return {
    read: (outRefHexes) =>
      Effect.gen(function* () {
        const keys = [...new Set(outRefHexes)];
        const unchanged = keys.filter((key) => !changes.has(key));
        const read =
          unchanged.length === 0
            ? new Map()
            : yield* Effect.tryPromise({
                try: () =>
                  ledger.read(unchanged.map((key) => Buffer.from(key, "hex"))),
                catch: (cause) =>
                  new DatabaseError({
                    table,
                    message: "Failed to read the block's base ledger by key",
                    cause,
                  }),
              });
        const state = new Map<string, Buffer>();
        for (const key of keys) {
          const output = changes.has(key)
            ? changes.get(key)
            : read.get(key)?.output;
          if (output !== undefined && output !== null)
            state.set(key, Buffer.from(output));
        }
        return state;
      }),
    delete: (outRefHex) => {
      changes.set(outRefHex, null);
    },
    insert: (outRefHex, output) => {
      changes.set(outRefHex, Buffer.from(output));
    },
    apply: (patch) => {
      for (const outRefHex of patch.deletedOutRefs)
        changes.set(outRefHex, null);
      for (const [outRefHex, output] of patch.upsertedOutRefs)
        changes.set(outRefHex, Buffer.from(output));
    },
  };
};

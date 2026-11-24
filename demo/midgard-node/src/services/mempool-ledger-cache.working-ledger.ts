import type { UTxOStatePatch } from "@al-ft/midgard-validation/phase-b";

import { MempoolLedgerDB } from "../database/index.js";
import type { MempoolLedgerDelta } from "./globals.next-l1-provider-health-evidence.js";
import type { MempoolLedgerState } from "./mempool-ledger-cache.mempool-ledger-cache-service.js";

type LivePatch = {
  readonly sequence: bigint;
  readonly deletes: ReadonlySet<string>;
  readonly upserts: ReadonlyMap<string, Buffer>;
};

/**
 * The validation cache's one in-memory ledger: the durable entries, with the
 * accepted Phase B patches not yet promoted laid over them. It is loaded in
 * full once and repaired at keys afterwards; it is never copied. Every write
 * returns how many entries it wrote.
 */
export const makeWorkingLedger = () => {
  const durable = new Map<string, Buffer>();
  // In sequence order.
  const livePatches: LivePatch[] = [];

  // Newest patch first, an upsert before a delete (as Phase B's own overlay
  // answers), then the durable entries.
  const lookup: MempoolLedgerState = {
    get: (outRefHex) => {
      for (let index = livePatches.length - 1; index >= 0; index -= 1) {
        const patch = livePatches[index]!;
        const upserted = patch.upserts.get(outRefHex);
        if (upserted !== undefined) return upserted;
        if (patch.deletes.has(outRefHex)) return undefined;
      }
      return durable.get(outRefHex);
    },
  };

  return {
    lookup,

    /** Replaces the durable entries with `entries`. */
    loadAll: (entries: readonly MempoolLedgerDB.EntryWithTimeStamp[]) => {
      durable.clear();
      for (const entry of entries) {
        durable.set(
          entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
          Buffer.from(entry[MempoolLedgerDB.Columns.OUTPUT]),
        );
      }
      return entries.length;
    },

    applyDelta: (delta: MempoolLedgerDelta) => {
      for (const outRefHex of delta.deletes) durable.delete(outRefHex);
      for (const [outRefHex, output] of delta.upserts)
        durable.set(outRefHex, Buffer.from(output));
      return delta.deletes.length + delta.upserts.length;
    },

    /** Sets each of `keys` to its output in `spendable`, or removes it. */
    repair: (
      keys: ReadonlySet<string>,
      spendable: ReadonlyMap<string, Buffer>,
    ) => {
      for (const key of keys) {
        const output = spendable.get(key);
        if (output === undefined) durable.delete(key);
        else durable.set(key, Buffer.from(output));
      }
      return keys.size;
    },

    /** Lays `patch` over the ledger as sequence `sequence`'s. */
    setPatch: (sequence: bigint, patch: UTxOStatePatch): void => {
      const live: LivePatch = {
        sequence,
        deletes: new Set(patch.deletedOutRefs),
        upserts: new Map(
          patch.upsertedOutRefs.map(([outRefHex, output]) => [
            outRefHex,
            Buffer.from(output),
          ]),
        ),
      };
      const at = livePatches.findIndex((other) => other.sequence >= sequence);
      if (at < 0) livePatches.push(live);
      else
        livePatches.splice(
          at,
          livePatches[at]!.sequence === sequence ? 1 : 0,
          live,
        );
    },

    /** Moves sequence `sequence`'s patch into the durable entries. */
    promote: (sequence: bigint) => {
      const at = livePatches.findIndex((other) => other.sequence === sequence);
      if (at < 0) return 0;
      const [patch] = livePatches.splice(at, 1);
      for (const outRefHex of patch!.deletes) durable.delete(outRefHex);
      for (const [outRefHex, output] of patch!.upserts)
        durable.set(outRefHex, output);
      return patch!.deletes.size + patch!.upserts.size;
    },

    /** Drops every live patch; the keys they named. */
    dropPatches: (): ReadonlySet<string> => {
      const keys = new Set<string>();
      for (const patch of livePatches) {
        for (const key of patch.deletes) keys.add(key);
        for (const key of patch.upserts.keys()) keys.add(key);
      }
      livePatches.length = 0;
      return keys;
    },
  };
};

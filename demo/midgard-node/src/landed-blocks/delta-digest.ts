/**
 * The digest of a landed row's stored delta: its header, the parent header
 * and root it builds on, its own root, and the outrefs it spends and the
 * entries it produces, in order. A verification run records it for each row
 * it checked to the tail, and a fold advances the verified frontier only on
 * a row whose digest is the one recorded (`confirmed-ledger-advance.ts`), so
 * a delta changed after the run is never taken for the one it checked.
 */
import { createHash } from "node:crypto";

import type { LandedBlockRow } from "./store.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

/** SHA-256 of `row`'s delta, as 64 hex. */
export const landedDeltaDigest = (
  row: Pick<
    LandedBlockRow,
    | "headerHash"
    | "parentHeaderHash"
    | "parentUtxosRoot"
    | "utxosRoot"
    | "spent"
    | "produced"
  >,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify([
        row.headerHash,
        row.parentHeaderHash,
        row.parentUtxosRoot,
        row.utxosRoot,
        row.spent.map(hex),
        row.produced.map((entry) => [hex(entry.outref), hex(entry.output)]),
      ]),
    )
    .digest("hex");

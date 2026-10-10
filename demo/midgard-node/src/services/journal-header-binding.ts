import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";

const C = Pending.Columns;

/** Why a journal's own header bytes do not bind its replay base and
 * candidate root, or undefined when they do. A block's header hash commits
 * to its predecessor hash and its two UTxO roots; the journal's header bytes
 * must hash to its header hash, and its replay base (base tail header hash
 * and base root) and candidate root must be that header's own. Header bytes
 * that do not decode or hash bind nothing. Callers read this only when no
 * retained parent journal (one with the base tail header hash) binds the
 * base instead. */
export const unboundJournalReason = (record: Pending.Record) =>
  Effect.gen(function* () {
    const headerHash = record[C.HEADER_HASH].toString("hex");
    const decoded = yield* Effect.try(
      () =>
        Data.from(
          record[C.HEADER_CBOR].toString("hex"),
          SDK.Header,
        ) as SDK.Header,
    ).pipe(
      Effect.flatMap((header) =>
        Effect.map(SDK.hashBlockHeader(header), (hash) => ({ header, hash })),
      ),
      Effect.catchAllDefect(Effect.fail),
      Effect.option,
    );
    if (Option.isNone(decoded))
      return `block ${headerHash}'s journal header bytes do not decode to a hashable header`;
    const { header, hash } = decoded.value;
    if (hash !== headerHash)
      return `block ${headerHash}'s journal header bytes hash to ${hash}`;
    const baseTail = record[C.BASE_TAIL_HEADER_HASH].toString("hex");
    const baseRoot = record[C.BASE_UTXOS_ROOT];
    const candidateRoot = record[C.EXPECTED_UTXOS_ROOT];
    if (
      header.prevHeaderHash !== baseTail ||
      header.prevUtxosRoot !== baseRoot ||
      header.utxosRoot !== candidateRoot
    )
      return `block ${headerHash}'s journal replay base ${baseTail}/${baseRoot} and candidate root ${candidateRoot} are not its header's predecessor ${header.prevHeaderHash}/${header.prevUtxosRoot} and root ${header.utxosRoot}`;
    return undefined;
  });

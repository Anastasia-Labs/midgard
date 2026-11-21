/**
 * An own block's status derived from the facts (plan §8.2, §8.3,
 * `pendingBlockFinalizations.derived-status.ts`), never stored:
 *
 * - unlanded with no landed row, landed with a processed row the working
 *   ledger has not taken in, applied once it has, folded at the frontier or
 *   as a retained fold;
 * - folded once the merge prune dropped its fold, from its merged queue
 *   terminal while no landed row is left, and not while one is;
 * - the frontier and a retained fold win over a landed row left behind;
 * - a rollback that takes its landing off the chain makes it unlanded
 *   again, and nothing is written to its journal.
 */
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { Frontier, setState } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { insertOwnJournal } from "./helpers/landed-blocks-sim.own.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  journalRowOf,
  land,
  ownBlockStatuses,
  ownLandedRow,
  retainFold,
} from "./helpers/own-block-facts.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";
import { inNode } from "./landed-blocks-own.fixture.js";

const S = PendingBlockFinalizationsDB.OwnBlockStatus;
const ZERO_ROOT = "00".repeat(32);
const header = (label: string) =>
  simDigest(`own-block-status:${label}`).subarray(0, 28).toString("hex");
const G = header("genesis");
const bytes = (headerHash: string) => Buffer.from(headerHash, "hex");
/** A distinct landed parent per block: one processed child per parent. */
const parentOf = (headerHash: string) => header(`parent:${headerHash}`);

/** The journal of own block `headerHash` on G. */
const journal = (headerHash: string, at: number) =>
  insertOwnJournal({
    headerHash,
    baseHeaderHash: G,
    baseUtxosRoot: ZERO_ROOT,
    expectedUtxosRoot: ZERO_ROOT,
    spent: [],
    produced: [],
    txIds: [],
    at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 60_000),
  });

const frontierAt = (headerHash: string) =>
  withFollowerWrite(Frontier.upsert({ headerHash, utxosRoot: ZERO_ROOT }));

const merged = (headerHash: string) =>
  insertQueueTerminal({
    headerHash: bytes(headerHash),
    outcome: "merged",
    height: 7,
  });

describe("an own block's derived status", () => {
  it("is unlanded, landed, applied or folded by the facts", () =>
    inNode(
      Effect.gen(function* () {
        const [unlanded, landed, applied, frontier, retained] = [
          "unlanded",
          "landed",
          "applied",
          "frontier",
          "retained",
        ].map(header);
        for (const [at, headerHash] of [
          unlanded,
          landed,
          applied,
          frontier,
          retained,
        ].entries())
          yield* journal(headerHash!, at);
        yield* land(ownLandedRow(landed!, parentOf(landed!)));
        yield* land(
          ownLandedRow(applied!, parentOf(applied!), { applied: true }),
        );
        yield* frontierAt(frontier!);
        yield* retainFold(retained!, parentOf(retained!));
        expect(Object.fromEntries(yield* ownBlockStatuses)).toEqual({
          [unlanded!]: S.Unlanded,
          [landed!]: S.Landed,
          [applied!]: S.Applied,
          [frontier!]: S.Folded,
          [retained!]: S.Folded,
        });
      }),
    ));

  it("is folded from its merged terminal once no landed row is left, and not while one is", () =>
    inNode(
      Effect.gen(function* () {
        const pruned = header("pruned");
        const leftover = header("leftover");
        const removed = header("removed");
        yield* journal(pruned, 0);
        yield* journal(leftover, 1);
        yield* journal(removed, 2);
        for (const headerHash of [pruned, leftover, removed])
          yield* merged(headerHash);
        yield* land(ownLandedRow(leftover, parentOf(leftover)));
        yield* land(
          ownLandedRow(removed, parentOf(removed), {
            state: "removed",
            applied: true,
          }),
        );
        expect(Object.fromEntries(yield* ownBlockStatuses)).toEqual({
          [pruned]: S.Folded,
          [leftover]: S.Landed,
          [removed]: S.Unlanded,
        });
      }),
    ));

  it("is folded at the frontier or a retained fold whatever landed row is left", () =>
    inNode(
      Effect.gen(function* () {
        const frontier = header("frontier");
        const retained = header("retained");
        yield* journal(frontier, 0);
        yield* journal(retained, 1);
        yield* land(
          ownLandedRow(frontier, parentOf(frontier), { applied: true }),
        );
        yield* land(ownLandedRow(retained, parentOf(retained)));
        yield* frontierAt(frontier);
        yield* retainFold(retained, parentOf(retained));
        expect(Object.fromEntries(yield* ownBlockStatuses)).toEqual({
          [frontier]: S.Folded,
          [retained]: S.Folded,
        });
      }),
    ));

  it("is unlanded again after a rollback, with its journal untouched", () =>
    inNode(
      Effect.gen(function* () {
        const own = header("own");
        yield* journal(own, 0);
        yield* land(ownLandedRow(own, parentOf(own), { applied: true }));
        const row = yield* journalRowOf(bytes(own));
        expect((yield* ownBlockStatuses).get(own)).toBe(S.Applied);
        yield* setState([own], "removed");
        expect((yield* ownBlockStatuses).get(own)).toBe(S.Unlanded);
        expect(yield* journalRowOf(bytes(own))).toEqual(row);
      }),
    ));
});

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  replacedSameBaseJournals,
  sameBaseJournals,
} from "../src/services/history-expired-intent-release.base-spend.js";
import { replacedBlockRevivalDisposition } from "../src/services/history-expired-intent-release.prepare-replaced-block-revival.js";
import {
  BASE_HEADER,
  BASE_OUT,
  change,
  insertJournal,
  signedCommit,
  TTL,
  UTXOS_ROOT,
} from "./helpers/history-expired-intent-release-before-ttl.js";
import { authority } from "./helpers/history-expired-intent-release-displaced-sibling.js";
import {
  observerSees,
  onNode,
} from "./helpers/history-expired-intent-release-preparation.js";

// One above the bind-parameter limit, plus the query's other predicates. Only
// two journal rows are needed: the growing lists, not a large table, cause it.
const headers = Array.from({ length: 65_535 }, (_, index) => {
  const header = Buffer.alloc(28);
  header.writeUInt32BE(index + 1, 24);
  return header;
});
const selected = headers[headers.length - 1]!;
const other = Buffer.alloc(28, 0xff);
const base = {
  outRef: BASE_OUT,
  headerHash: BASE_HEADER,
  utxosRoot: UTXOS_ROOT,
};
const journal = (header: Buffer, abandonment: "replacement" | "correction") =>
  Effect.gen(function* () {
    yield* insertJournal({
      header,
      status: Pending.Status.Abandoned,
      commit: signedCommit(BASE_OUT, TTL),
      baseOut: BASE_OUT,
      baseHeader: BASE_HEADER,
      createdAt: new Date(0),
      abandonment,
    });
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET block_end_time = block_start_time + INTERVAL '1 second'
      WHERE header_hash = ${header}`;
  });

describe("signed-intent revival SQL has a constant bind budget", () => {
  it("selects a replacement at the end of more than 65,534 observer headers", async () => {
    const result = await onNode(undefined, (node) =>
      Effect.gen(function* () {
        yield* journal(selected, "replacement");
        yield* journal(other, "replacement");
        yield* observerSees(
          headers.map((header) => ({
            headerHash: header.toString("hex"),
            outRef: `${header.toString("hex").padStart(64, "0")}#0`,
          })),
        );
        return yield* replacedBlockRevivalDisposition({
          change: change("forward", 10),
          deferral: node.deferral,
          rewindAuthority: authority,
        });
      }),
    );
    expect(result?.status).toBe("pending");
    expect(result?.reason).toContain(selected.toString("hex"));
    expect(result?.reason).not.toContain(other.toString("hex"));
  });

  it("excludes every listed header beyond the parameter limit and preserves empty-list semantics", async () => {
    const result = await onNode(undefined, () =>
      Effect.gen(function* () {
        yield* journal(selected, "replacement");
        yield* journal(other, "replacement");
        return {
          excluded: yield* sameBaseJournals(base, headers),
          all: yield* sameBaseJournals(base, []),
        };
      }),
    );
    expect(result.excluded.map((row) => row.header_hash)).toEqual([other]);
    expect(result.all.map((row) => row.header_hash)).toEqual([selected, other]);
  });

  it("uses the replacement array query without reviving correction-abandoned siblings", async () => {
    const result = await onNode(undefined, () =>
      Effect.gen(function* () {
        yield* journal(selected, "replacement");
        yield* journal(other, "correction");
        const sql = yield* SqlClient.SqlClient;
        return {
          replaced: yield* replacedSameBaseJournals(base, []),
          untouched: yield* sql<{ status: string }>`SELECT status
            FROM pending_block_finalizations ORDER BY header_hash`,
        };
      }),
    );
    expect(result.replaced.map((row) => row.header_hash)).toEqual([selected]);
    expect(result.untouched.map((row) => row.status)).toEqual([
      Pending.Status.Abandoned,
      Pending.Status.Abandoned,
    ]);
  });
});

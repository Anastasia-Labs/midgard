import "./utils.js";

import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { HttpRouter } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { withHttpReadSql } from "../src/commands/listen-http-read.js";
import { encodeReadPage } from "../src/commands/listen-read-page.js";
import { getTxHandler } from "../src/commands/listen-router.get-tx-handler.js";
import {
  PublicHttpReadLimitError,
  requirePublicReadPayload,
} from "../src/database/http-read-byte-budget.js";
import { publicUtxosByOutRefs } from "../src/database/http-read-lookups.js";
import { addressHistoryPage } from "../src/database/http-read-pages.js";
import { readHttpTxStatusFacts } from "../src/database/http-tx-status-facts.js";
import {
  HTTP_PUBLIC_READ_DATA_MAX_BYTES,
  HTTP_RESPONSE_MAX_BYTES,
} from "../src/services/http-resource-limits.js";
import { address1 } from "./database.test/fixtures.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// Opaque storage-only bytes cover SQL transfer/hex expansion before validation.
// These cases make no genuine transaction/admission or block-production claim.
describe("public SQL materialization byte budget", () => {
  it("bounds transferred page payload bytes without silently dropping indexed cursor members", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const sql = yield* SqlClient.SqlClient;
          for (let index = 1; index <= 4; index += 1) {
            const txId = Buffer.alloc(32, index);
            yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${txId}, ${Buffer.alloc(200_000, index)})`;
            yield* sql`INSERT INTO address_history (tx_id, address) VALUES (${txId}, ${address1})`;
          }
          const request = { limit: 4, cursor: undefined };
          const page = yield* addressHistoryPage(address1, request);
          expect(page.map((row) => row.tx_id)).toEqual(
            [1, 2, 3, 4].map((index) => Buffer.alloc(32, index)),
          );
          expect(
            page.reduce((sum, row) => sum + (row.tx?.byteLength ?? 0), 0),
          ).toBeLessThanOrEqual(HTTP_RESPONSE_MAX_BYTES / 2);
          const encode = (row: (typeof page)[number]) =>
            requirePublicReadPayload(row.tx).toString("hex");
          const first = encodeReadPage(
            page,
            request,
            (row) => row.tx_id,
            encode,
          );
          expect(first.entries).toHaveLength(2);
          expect(first.nextCursor).toBe(Buffer.alloc(32, 2).toString("hex"));
          const resumedRequest = {
            limit: 4,
            cursor: Buffer.from(first.nextCursor!, "hex"),
          };
          const resumedRows = yield* addressHistoryPage(
            address1,
            resumedRequest,
          );
          expect(resumedRows.map((row) => row.tx_id)).toEqual(
            [3, 4].map((index) => Buffer.alloc(32, index)),
          );
          const resumed = encodeReadPage(
            resumedRows,
            resumedRequest,
            (row) => row.tx_id,
            encode,
          );
          expect(resumed.entries).toHaveLength(2);
          expect(resumed.nextCursor).toBeNull();
        }),
      ),
    );
  });

  it("holds at an oversized first row instead of skipping it to a later small row", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const sql = yield* SqlClient.SqlClient;
          for (const [index, size] of [
            [1, HTTP_PUBLIC_READ_DATA_MAX_BYTES + 1],
            [2, 16],
          ]) {
            const txId = Buffer.alloc(32, index);
            yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${txId}, ${Buffer.alloc(size, index)})`;
            yield* sql`INSERT INTO address_history (tx_id, address) VALUES (${txId}, ${address1})`;
          }
          const request = { limit: 2, cursor: undefined };
          const page = yield* addressHistoryPage(address1, request);
          expect(page.map((row) => row.tx_id)).toEqual(
            [1, 2].map((index) => Buffer.alloc(32, index)),
          );
          expect(page.every((row) => row.tx === null)).toBe(true);
          expect(() =>
            encodeReadPage(
              page,
              request,
              (row) => row.tx_id,
              (row) => requirePublicReadPayload(row.tx),
            ),
          ).toThrow(PublicHttpReadLimitError);
          expect(
            (yield* sql<{
              count: number;
            }>`SELECT COUNT(*)::int AS count FROM address_history`)[0]!.count,
          ).toBe(2);
        }),
      ),
    );
  });

  it("fails an over-budget outref batch as a whole while bounded splits preserve caller order and misses", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const sql = yield* SqlClient.SqlClient;
          const outrefs = [1, 2, 3].map((index) =>
            encodeMidgardSpendInputItem({
              txId: Buffer.alloc(32, index),
              outputIndex: 0,
            }),
          );
          for (let index = 0; index < outrefs.length; index += 1)
            yield* sql`INSERT INTO mempool_ledger (tx_id, outref, output, address) VALUES (${Buffer.alloc(32, index + 1)}, ${outrefs[index]!}, ${Buffer.alloc(200_000, index + 1)}, ${address1})`;
          const excess = yield* Effect.either(
            publicUtxosByOutRefs([outrefs[2]!, outrefs[0]!, outrefs[1]!]),
          );
          expect(excess._tag).toBe("Left");
          if (excess._tag === "Left")
            expect(excess.left._tag).toBe("PublicHttpReadLimitError");
          const missing = encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, 9),
            outputIndex: 0,
          });
          const bounded = yield* publicUtxosByOutRefs([
            outrefs[2]!,
            missing,
            outrefs[0]!,
          ]);
          expect(bounded.map((row) => row.outref)).toEqual([
            outrefs[2],
            outrefs[0],
          ]);
          expect(bounded.map((row) => row.output.byteLength)).toEqual([
            200_000, 200_000,
          ]);
          expect(bounded[0]!.output.equals(Buffer.alloc(200_000, 3))).toBe(
            true,
          );
        }),
      ),
    );
  });

  it("reads status from metadata-only tables and withholds private rejection details at SQL projection", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql.withTransaction(
            Effect.gen(function* () {
              // Deliberately no transaction, canonical payload or proof-sidecar columns.
              yield* sql`CREATE TEMP TABLE immutable (tx_id bytea) ON COMMIT DROP`;
              yield* sql`CREATE TEMP TABLE mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
              yield* sql`CREATE TEMP TABLE processed_mempool (tx_id bytea, included_by bytea) ON COMMIT DROP`;
              yield* sql`CREATE TEMP TABLE tx_admissions (tx_id bytea, status text) ON COMMIT DROP`;
              yield* sql`CREATE TEMP TABLE tx_rejections (tx_id bytea, reject_code text, reject_detail text, created_at timestamptz) ON COMMIT DROP`;
              const txId = Buffer.alloc(32, 7);
              const privateDetail = "provider password=arbitrary-base64/OPAQUE";
              yield* sql`INSERT INTO tx_rejections VALUES (${txId}, 'withdrawal_input', ${privateDetail}, '2026-10-13T20:00:00Z')`;
              yield* sql`INSERT INTO tx_admissions VALUES (${txId}, 'rejected')`;
              const publicFacts = yield* readHttpTxStatusFacts(txId, false);
              expect(publicFacts.reject_code).toBe("withdrawal_input");
              expect(publicFacts.reject_detail).toBeNull();
              expect(publicFacts.admission_status).toBe("rejected");
              expect(publicFacts.in_immutable).toBe(false);
              const privateFacts = yield* readHttpTxStatusFacts(txId, true);
              expect(privateFacts.reject_detail).toBe(privateDetail);
              expect(privateFacts.created_at).toEqual(publicFacts.created_at);
            }),
          );
        }),
      ),
    );
  });

  it("serves the actual transaction route within its response cap and refuses the adjacent stored-row boundary", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const sql = yield* SqlClient.SqlClient;
          for (const [index, size] of [
            [1, HTTP_PUBLIC_READ_DATA_MAX_BYTES],
            [2, HTTP_PUBLIC_READ_DATA_MAX_BYTES + 1],
          ])
            yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${Buffer.alloc(32, index)}, ${Buffer.alloc(size, index)})`;
          const handler = yield* NodeHttpServer.makeHandler(
            HttpRouter.empty.pipe(
              HttpRouter.get("/tx", withHttpReadSql(getTxHandler)),
            ),
          );
          const server = createServer(handler);
          server.listen(0, "127.0.0.1");
          yield* Effect.promise(() => once(server, "listening"));
          yield* Effect.addFinalizer(() =>
            Effect.promise(async () => {
              server.closeAllConnections();
              await new Promise<void>((resolve) =>
                server.close(() => resolve()),
              );
            }),
          );
          const endpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port}/tx?tx_hash=`;
          const accepted = yield* Effect.promise(() =>
            fetch(endpoint + Buffer.alloc(32, 1).toString("hex"), {
              signal: AbortSignal.timeout(5_000),
            }),
          );
          expect(accepted.status).toBe(200);
          const text = yield* Effect.promise(() => accepted.text());
          expect(Buffer.byteLength(text)).toBeLessThan(HTTP_RESPONSE_MAX_BYTES);
          expect((JSON.parse(text) as { tx: string }).tx.length).toBe(
            HTTP_PUBLIC_READ_DATA_MAX_BYTES * 2,
          );
          const refused = yield* Effect.promise(() =>
            fetch(endpoint + Buffer.alloc(32, 2).toString("hex"), {
              signal: AbortSignal.timeout(5_000),
            }),
          );
          expect(refused.status).toBe(503);
          expect(refused.headers.get("retry-after")).toBe("5");
          expect(yield* Effect.promise(() => refused.json())).toEqual({
            error: "Stored data exceeds public HTTP read capacity",
            error_code: "E_HTTP_READ_DATA_LIMIT",
          });
        }).pipe(Effect.scoped),
      ),
    );
  });
});

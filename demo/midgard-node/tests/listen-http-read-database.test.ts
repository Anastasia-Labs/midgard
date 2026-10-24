import "./utils.js";

import { createHash } from "node:crypto";
import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { HttpRouter, HttpServerResponse } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { withHttpReadSql } from "../src/commands/listen-http-read.js";
import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import {
  AdmissionSql,
  HttpReadSql,
  HttpReadSqlLive,
} from "../src/services/database.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// This regression needs real sockets and independently named PostgreSQL pools:
// neither a semaphore unit test nor a mocked SQL handler catches pool sharing,
// queued overflow or failure to cancel a disconnected client's query.
describe("public HTTP database capacity", () => {
  it("refuses a cap-sized burst without queuing while durable admission progresses, then releases aborted readers", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* resetApplicationTables;
          const observer = yield* SqlClient.SqlClient;
          const admission = yield* AdmissionSql;
          const readSql = yield* HttpReadSql;
          const countReaders = observer<{
            count: number;
          }>`SELECT COUNT(*)::int AS count FROM pg_stat_activity
        WHERE datname = current_database() AND application_name = 'midgard-node-http-read'
          AND state = 'active' AND wait_event = 'PgSleep'`;
          const awaitCount = (expected: number) =>
            Effect.gen(function* () {
              while ((yield* countReaders)[0]!.count !== expected)
                yield* Effect.sleep(10);
            }).pipe(
              Effect.timeoutFail({
                duration: 5_000,
                onTimeout: () =>
                  new Error(
                    `HTTP read pool did not reach ${expected} active queries`,
                  ),
              }),
            );
          const delayed = Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql`SELECT pg_sleep(20)`;
            return yield* HttpServerResponse.json({ ok: true });
          });
          const fast = Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const [row] = yield* sql<{
              application_name: string;
            }>`SELECT current_setting('application_name') AS application_name`;
            return yield* HttpServerResponse.json(row);
          });
          const route = HttpRouter.empty.pipe(
            HttpRouter.get("/tx", withHttpReadSql(delayed, 30_000)),
            HttpRouter.get("/tx-status", withHttpReadSql(fast)),
            Effect.provideService(HttpReadSql, readSql),
          );
          const handler = yield* NodeHttpServer.makeHandler(route);
          const server = createServer(handler);
          server.listen(0, "127.0.0.1");
          yield* Effect.promise(() => once(server, "listening"));
          const endpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port}`;
          const controllers = Array.from(
            { length: 4 },
            () => new AbortController(),
          );
          const pending = controllers.map((controller) =>
            fetch(`${endpoint}/tx`, { signal: controller.signal }).catch(
              () => undefined,
            ),
          );
          yield* Effect.addFinalizer(() =>
            Effect.promise(async () => {
              controllers.forEach((controller) => controller.abort());
              await Promise.all(pending);
              server.closeAllConnections();
              await new Promise<void>((resolve) =>
                server.close(() => resolve()),
              );
            }),
          );
          yield* awaitCount(4);
          const started = performance.now();
          const overflow = yield* Effect.promise(() =>
            Promise.all(
              Array.from({ length: 32 }, async () => {
                const response = await fetch(`${endpoint}/tx`, {
                  signal: AbortSignal.timeout(2_000),
                });
                return {
                  status: response.status,
                  retry: response.headers.get("retry-after"),
                  body: await response.json(),
                };
              }),
            ),
          );
          expect(overflow).toHaveLength(32);
          expect(
            overflow.every(
              (response) =>
                response.status === 503 &&
                response.retry === "5" &&
                response.body.error_code === "E_HTTP_READ_UNAVAILABLE",
            ),
          ).toBe(true);
          expect(performance.now() - started).toBeLessThan(2_000);
          expect((yield* countReaders)[0]!.count).toBe(4);
          const txId = createHash("sha256")
            .update("r7-public-read-admission-progress")
            .digest();
          const admitted = yield* TxAdmissionsDB.admitReservedBatch([
            {
              txId,
              txCanonicalCbor: Buffer.from("r7-testing-admission"),
              submitSource: "native",
            },
          ]).pipe(
            Effect.provideService(SqlClient.SqlClient, admission),
            Effect.timeoutFail({
              duration: 2_000,
              onTimeout: () =>
                new Error("HTTP readers starved durable admission"),
            }),
          );
          expect(admitted[0]?._tag).toBe("Success");
          expect((yield* TxAdmissionsDB.getByTxId(txId))?.tx_id).toEqual(txId);
          controllers.forEach((controller) => controller.abort());
          yield* Effect.promise(() => Promise.all(pending));
          yield* awaitCount(0);
          const recovered = yield* Effect.promise(() =>
            fetch(`${endpoint}/tx-status`, {
              signal: AbortSignal.timeout(2_000),
            }),
          );
          expect(recovered.status).toBe(200);
          expect(yield* Effect.promise(() => recovered.json())).toEqual({
            application_name: "midgard-node-http-read",
          });
        }).pipe(Effect.provide(HttpReadSqlLive), Effect.scoped),
      ),
    );
  }, 20_000);
});

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient, Statement } from "@effect/sql";
import type { Connection } from "@effect/sql/SqlConnection";
import { PgClient } from "@effect/sql-pg";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  getBlockHandler,
  getBlocksHandler,
} from "../src/commands/http/public-info.js";
import {
  getTxHandler,
  getTxStatusHandler,
  postTxStatusBatchHandler,
} from "../src/commands/http/transaction-status.js";
import {
  getTxsOfAddressHandler,
  getUtxoHandler,
  getUtxosHandler,
} from "../src/commands/http/utxo-reads.js";
import { redactPublicLogger } from "../src/commands/listen-public-logging.js";
import {
  decodeRecentBlockCursor,
  encodeRecentBlockCursor,
  type RecentBlockPageRow,
} from "../src/database/http-read-pages.js";

const makeFakeSql = (
  rowsFor: (query: string, params: readonly unknown[]) => readonly unknown[],
) => {
  const queries: { sql: string; params: readonly unknown[] }[] = [];
  const compiler = PgClient.makeCompiler();
  const connection: Connection = {
    execute: (compiled, params) => {
      queries.push({ sql: compiled, params });
      return Effect.succeed(rowsFor(compiled, params) as readonly any[]);
    },
    executeRaw: () => Effect.die("unsupported"),
    executeStream: () => {
      throw new Error("unsupported");
    },
    executeValues: () => Effect.die("unsupported"),
    executeUnprepared: (compiled, params) => {
      queries.push({ sql: compiled, params });
      return Effect.succeed(rowsFor(compiled, params) as readonly any[]);
    },
  };
  const sql = Statement.make(
    Effect.succeed(connection),
    compiler,
    [],
    undefined,
  ) as unknown as SqlClient.SqlClient;
  return { sql, queries };
};

const runJson = async <E, R>(
  effect: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
) => {
  const response = await Effect.runPromise(
    effect as Effect.Effect<HttpServerResponse.HttpServerResponse, E, never>,
  );
  const web = HttpServerResponse.toWeb(response);
  return {
    status: web.status,
    body: (await web.json()) as Record<string, any>,
  };
};

describe("recent block cursor codec", () => {
  it("encodes and decodes 36-byte big-endian epoch-ms + 28-byte header hash", () => {
    const endTime = new Date("2026-11-25T12:34:56.789Z");
    const headerHash = Buffer.from("ab".repeat(28), "hex");
    const encoded = encodeRecentBlockCursor({
      block_end_time: endTime,
      header_hash: headerHash,
    });
    expect(encoded.length).toBe(36);
    expect(encoded.toString("hex")).toHaveLength(72);
    expect(decodeRecentBlockCursor(encoded)).toEqual({
      blockEndTime: endTime,
      headerHash,
    });
  });

  it("rejects malformed length or out-of-range timestamps", () => {
    expect(() => decodeRecentBlockCursor(Buffer.alloc(32))).toThrow();
    const negative = Buffer.alloc(36);
    negative.writeBigInt64BE(-1n, 0);
    expect(() => decodeRecentBlockCursor(negative)).toThrow();
    const overflow = Buffer.alloc(36);
    overflow.writeBigInt64BE(9_000_000_000_000_000n, 0);
    expect(() => decodeRecentBlockCursor(overflow)).toThrow();
  });
});

describe("GET /blocks", () => {
  const sampleRow = (
    byte: number,
    endIso: string,
    l2Count: number,
  ): RecentBlockPageRow => ({
    header_hash: Buffer.alloc(28, byte),
    block_start_time: new Date(Date.parse(endIso) - 5_000),
    block_end_time: new Date(endIso),
    block_time: new Date(Date.parse(endIso) + 1_000),
    l2_transaction_count: BigInt(l2Count),
    deposit_count: 1n,
    withdrawal_count: 2n,
    forced_transaction_count: 0n,
    total_event_count: BigInt(l2Count + 3),
  });

  it("returns recent block metadata ordered DESC with keyset pagination and never selects payload_cbor", async () => {
    const rows = [
      sampleRow(0xbb, "2026-11-25T12:00:10.000Z", 5),
      sampleRow(0xaa, "2026-11-25T12:00:05.000Z", 2),
      sampleRow(0x99, "2026-11-25T12:00:00.000Z", 1),
    ];
    const { sql, queries } = makeFakeSql(() => rows);

    const first = await runJson(
      getBlocksHandler.pipe(
        Effect.provideService(ParsedSearchParams, { limit: "2" }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );

    expect(first.status).toBe(200);
    expect(first.body.blocks).toEqual([
      {
        headerHash: "bb".repeat(28),
        blockStartTime: "2026-11-25T12:00:05.000Z",
        blockEndTime: "2026-11-25T12:00:10.000Z",
        blockTime: "2026-11-25T12:00:11.000Z",
        l2TransactionCount: 5,
        depositCount: 1,
        withdrawalCount: 2,
        forcedTransactionCount: 0,
        totalEventCount: 8,
      },
      {
        headerHash: "aa".repeat(28),
        blockStartTime: "2026-11-25T12:00:00.000Z",
        blockEndTime: "2026-11-25T12:00:05.000Z",
        blockTime: "2026-11-25T12:00:06.000Z",
        l2TransactionCount: 2,
        depositCount: 1,
        withdrawalCount: 2,
        forcedTransactionCount: 0,
        totalEventCount: 5,
      },
    ]);
    const expectedCursor = encodeRecentBlockCursor(rows[1]!).toString("hex");
    expect(first.body.nextCursor).toBe(expectedCursor);
    expect(queries[0]!.sql).not.toContain("payload_cbor");
    expect(queries[0]!.sql).toContain("removed");
    expect(queries[0]!.sql).toContain(
      "ORDER BY da_payloads.block_end_time DESC, da_payloads.header_hash DESC",
    );

    const { sql: page2Sql, queries: page2Queries } = makeFakeSql(() => [
      rows[2]!,
    ]);
    const second = await runJson(
      getBlocksHandler.pipe(
        Effect.provideService(ParsedSearchParams, {
          limit: "2",
          cursor: expectedCursor,
        }),
        Effect.provideService(SqlClient.SqlClient, page2Sql),
      ),
    );
    expect(second.status).toBe(200);
    expect(second.body.blocks).toHaveLength(1);
    expect(second.body.blocks[0].headerHash).toBe("99".repeat(28));
    expect(second.body.nextCursor).toBeNull();
    expect(page2Queries[0]!.sql).toContain(
      "(da_payloads.block_end_time, da_payloads.header_hash) <",
    );
    expect(page2Queries[0]!.params).toContainEqual(rows[1]!.block_end_time);
    expect(page2Queries[0]!.params).toContainEqual(rows[1]!.header_hash);
  });

  it("rejects invalid limit or cursor with 400", async () => {
    const { sql } = makeFakeSql(() => []);
    const badTimestamp = Buffer.alloc(36);
    badTimestamp.writeBigInt64BE(9_000_000_000_000_000n, 0);
    const cases: readonly Record<string, string>[] = [
      { limit: "0" },
      { limit: "101" },
      { cursor: "aa".repeat(32) },
      { cursor: "AA".repeat(36) },
      { cursor: badTimestamp.toString("hex") },
    ];
    for (const params of cases) {
      const reply = await runJson(
        getBlocksHandler.pipe(
          Effect.provideService(ParsedSearchParams, params),
          Effect.provideService(SqlClient.SqlClient, sql),
        ),
      );
      expect(reply).toEqual({ status: 400, body: { error: "Invalid page" } });
    }
  });
});

describe("public handler structured logging and clean error messages", () => {
  it("logs whitelisted header_hash and count on GET /block without hardcoded [redacted]", async () => {
    const headerHashHex = "ca".repeat(28);
    const { sql } = makeFakeSql(() => [{ tx_id: Buffer.alloc(32, 7) }]);
    const logs: string[] = [];
    const logger = redactPublicLogger(
      Logger.make((options) => {
        logs.push(JSON.stringify([...options.annotations]));
      }),
      {},
    );
    const reply = await runJson(
      getBlockHandler.pipe(
        Effect.provideService(ParsedSearchParams, {
          header_hash: headerHashHex,
        }),
        Effect.provideService(SqlClient.SqlClient, sql),
        Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
      ),
    );
    expect(reply.status).toBe(200);
    expect(logs.join(" ")).toContain(headerHashHex);
    expect(logs.join(" ")).toContain('["count",1]');
  });

  it("returns clean error messages without [redacted] suffixes", async () => {
    const { sql } = makeFakeSql(() => []);
    const badBlock = await runJson(
      getBlockHandler.pipe(
        Effect.provideService(ParsedSearchParams, { header_hash: "bad" }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badBlock).toEqual({
      status: 400,
      body: { error: "Invalid block hash" },
    });

    const badTx = await runJson(
      getTxHandler.pipe(
        Effect.provideService(ParsedSearchParams, { tx_hash: "bad" }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badTx).toEqual({
      status: 404,
      body: { error: "Invalid transaction hash" },
    });

    const badTxStatus = await runJson(
      getTxStatusHandler.pipe(
        Effect.provideService(ParsedSearchParams, { tx_hash: "bad" }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badTxStatus).toEqual({
      status: 400,
      body: { error: "Invalid transaction hash" },
    });

    const badBatch = await runJson(
      postTxStatusBatchHandler.pipe(
        Effect.provideService(
          HttpServerRequest.HttpServerRequest,
          HttpServerRequest.fromWeb(
            new Request("http://node.test/tx-status", {
              method: "POST",
              headers: { "content-type": "application/json" },
              body: JSON.stringify({ txHashes: ["not-hex"] }),
            }),
          ),
        ),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badBatch).toEqual({
      status: 400,
      body: { error: "Invalid transaction hash" },
    });

    const badTxsAddr = await runJson(
      getTxsOfAddressHandler.pipe(
        Effect.provideService(ParsedSearchParams, { address: "not-an-addr" }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badTxsAddr).toEqual({
      status: 400,
      body: { error: "Invalid address" },
    });

    const badUtxosAddr = await runJson(
      getUtxosHandler.pipe(
        Effect.provideService(ParsedSearchParams, {}),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(badUtxosAddr).toEqual({
      status: 400,
      body: { error: "Invalid address" },
    });

    const missingUtxo = await runJson(
      getUtxoHandler.pipe(
        Effect.provideService(ParsedSearchParams, {
          txOutRef: Buffer.from(
            encodeMidgardSpendInputItem({
              txId: Buffer.alloc(32, 0x11),
              outputIndex: 0,
            }),
          ).toString("hex"),
        }),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
    expect(missingUtxo).toEqual({
      status: 404,
      body: { error: "UTxO not found" },
    });
  });
});

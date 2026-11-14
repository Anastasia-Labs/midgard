import { SqlClient } from "@effect/sql";
import { Effect, Layer } from "effect";

import { MempoolLedgerDB } from "../database/index.js";
import { Globals } from "./globals.js";
import { makeMempoolLedgerCacheService } from "./mempool-ledger-cache.make-mempool-ledger-cache-service.js";
import { MempoolLedgerCache } from "./mempool-ledger-cache.mempool-ledger-cache-service.js";

const makeMempoolLedgerCache = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient.SqlClient;
  return yield* makeMempoolLedgerCacheService(
    globals,
    MempoolLedgerDB.retrieveSpendable.pipe(
      Effect.provideService(SqlClient.SqlClient, sql),
    ),
  );
});

export const mempoolLedgerCacheLayer = Layer.effect(
  MempoolLedgerCache,
  makeMempoolLedgerCache,
);

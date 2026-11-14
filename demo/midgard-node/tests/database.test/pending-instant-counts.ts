import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  type ProjectedEventTable,
  retrievePendingHeaderInstantCounts,
} from "../../src/database/utils/projected-events.js";
import { isolatedDb } from "./fixtures.js";
export const registerPendingInstantCountCase = () => {
  it.effect(
    "retrieves complete pending inclusion instants before applying the group limit",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql.withTransaction(
            Effect.gen(function* () {
              yield* sql`CREATE TEMP TABLE p1_instant_query (id TEXT PRIMARY KEY, inclusion_time TIMESTAMPTZ NOT NULL, assigned BYTEA, status TEXT NOT NULL) ON COMMIT DROP`;
              const config: ProjectedEventTable = {
                tableName: "p1_instant_query",
                idColumn: "id",
                inclusionTimeColumn: "inclusion_time",
                projectedHeaderHashColumn: "assigned",
                statusColumn: "status",
                awaitingStatus: "awaiting",
                projectedStatus: "projected",
                terminalStatus: "final",
                entitySingular: "event",
                entityPlural: "events",
                idLabel: "id",
              };
              const rows = [
                {
                  id: "a",
                  inclusion_time: new Date(1_000),
                  assigned: null,
                  status: "awaiting",
                },
                {
                  id: "b",
                  inclusion_time: new Date(1_000),
                  assigned: null,
                  status: "projected",
                },
                {
                  id: "c",
                  inclusion_time: new Date(1_000),
                  assigned: null,
                  status: "final",
                },
                {
                  id: "d",
                  inclusion_time: new Date(1_000),
                  assigned: Buffer.from("11", "hex"),
                  status: "projected",
                },
                {
                  id: "e",
                  inclusion_time: new Date(2_000),
                  assigned: null,
                  status: "projected",
                },
                {
                  id: "f",
                  inclusion_time: new Date(3_000),
                  assigned: null,
                  status: "projected",
                },
              ];
              yield* sql`INSERT INTO p1_instant_query ${sql.insert(rows)}`;
              expect(
                yield* retrievePendingHeaderInstantCounts(
                  config,
                  new Date(2_000),
                  1,
                ),
              ).toEqual([{ inclusionTimeMs: 1_000, count: 3 }]);
              expect(
                yield* retrievePendingHeaderInstantCounts(
                  config,
                  new Date(2_000),
                  2,
                  ["awaiting", "projected"],
                ),
              ).toEqual([
                { inclusionTimeMs: 1_000, count: 2 },
                { inclusionTimeMs: 2_000, count: 1 },
              ]);
            }),
          );
        }),
      ),
  );
};

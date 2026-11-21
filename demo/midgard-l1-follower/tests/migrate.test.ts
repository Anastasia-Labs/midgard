import { describe, expect, it } from "vitest";

import {
  applyMigrations,
  followerMigrations,
  openSqliteBackend,
} from "../src/index.js";

describe("migration runner (catalog)", () => {
  it("records each table's class, and refuses a table without its header", async () => {
    const backend = openSqliteBackend(":memory:");
    try {
      await applyMigrations(backend, [
        followerMigrations("sqlite"),
        {
          namespace: "role",
          migrations: [
            {
              id: "0001",
              sql: "-- class: B; retention: forever\nCREATE TABLE role_signed (id integer PRIMARY KEY);",
            },
          ],
        },
      ]);
      const catalog = await backend.transaction("read", (tx) =>
        tx.query(
          "SELECT table_name, table_class FROM l1_follower_tables ORDER BY table_name",
        ),
      );
      expect(
        Object.fromEntries(
          catalog.map((row) => [row.table_name, row.table_class]),
        ),
      ).toMatchObject({ l1_outputs: "A", l1_scripts: "C", role_signed: "B" });
      await expect(
        applyMigrations(backend, [
          {
            namespace: "role",
            migrations: [
              {
                id: "0002",
                sql: "CREATE TABLE role_unclassified (id integer);",
              },
            ],
          },
        ]),
      ).rejects.toThrow(
        /role\/0002 table role_unclassified: missing `-- class:/u,
      );
    } finally {
      await backend.close();
    }
  });
});

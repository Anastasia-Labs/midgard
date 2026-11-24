import { describe, expect, it } from "vitest";

import {
  applySchemas,
  followerSchema,
  openSqliteBackend,
} from "../src/index.js";

describe("schema apply (catalog)", () => {
  it("records each table's class, and refuses a table without its header", async () => {
    const backend = openSqliteBackend(":memory:");
    try {
      await applySchemas(backend, [
        followerSchema("sqlite"),
        {
          namespace: "role",
          sql: "-- class: B; retention: forever\nCREATE TABLE role_signed (id integer PRIMARY KEY);",
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
        applySchemas(backend, [
          {
            namespace: "unclassified",
            sql: "CREATE TABLE role_unclassified (id integer);",
          },
        ]),
      ).rejects.toThrow(
        /schema unclassified table role_unclassified: missing `-- class:/u,
      );
    } finally {
      await backend.close();
    }
  });
});

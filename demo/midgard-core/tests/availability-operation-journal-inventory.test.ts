import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { describe, expect, it } from "vitest";

import { openAvailabilityOperationJournal } from "../src/availability-operation-journal.js";
import {
  type AvailabilityJournalInventoryPage,
  openAvailabilityJournalInventory,
} from "../src/availability-operation-journal-inventory.js";
import { inventorySelection } from "../src/availability-operation-journal-inventory.projection.js";

const fixture = () => {
  const directory = mkdtempSync(join(tmpdir(), "availability-inventory-"));
  const path = join(directory, "journal.sqlite");
  const journal = openAvailabilityOperationJournal(path);
  return { directory, path, journal };
};

describe("read-only complete availability journal holds inventory", () => {
  const inspect = (path: string, limits = {}) => {
    const reader = openAvailabilityJournalInventory(path, limits);
    const pages: AvailabilityJournalInventoryPage[] = [];
    try {
      for (;;) {
        const page = reader.nextPage();
        if (page.event === "availability_journal_inventory_complete")
          return { pages, footer: page };
        pages.push(page);
      }
    } finally {
      reader.close();
    }
  };
  it("enumerates pending-only and lease-only work omitted by actor status, every state and orphan edges without private bytes", () => {
    const f = fixture();
    const db = new DatabaseSync(f.path);
    try {
      const lease = f.journal.acquire("actor", "builder", 0, 100);
      f.journal.acquire("unsigned", "old-builder", 0, 10);
      for (const state of [
        "pending",
        "included",
        "confirmed",
        "expired",
        "conflict",
      ] as const) {
        f.journal.persist(
          lease,
          {
            id: state,
            actor: "actor",
            deploymentIdentity: "deployment",
            headerHash: "header",
            action: "publish",
            signedCbor: "PRIVATE-SIGNED-CBOR",
            txHash: `${state}-tx`,
            spentOutRefs: [`${state}-external#0`],
            collateralOutRefs: [],
            expectedOutRefs: [],
            validUntilSlot: 100,
            completesWorkflow: false,
          },
          1,
        );
        if (state === "pending") {
          // Genuine old status gap: durable intent and resources exist, but its journal subset is empty.
          expect(f.journal.unfinalized("deployment", "actor")).toEqual([]);
          expect(f.journal.reservedOutRefs("actor")).toContain(
            "pending-external#0",
          );
        }
        if (state !== "pending")
          f.journal.transition(
            lease,
            state,
            state,
            state === "included" || state === "confirmed" ? "10.block" : null,
            "PRIVATE-PROVIDER-ERROR",
            2,
          );
      }
      expect(
        f.journal.unfinalized("deployment", "actor").map((r) => r.intent.id),
      ).toEqual(["included"]);
      expect(
        f.journal.pending("deployment", "actor").map((r) => r.intent.id),
      ).toEqual(["conflict", "pending"]);
      db.exec(`INSERT INTO availability_journal_metadata VALUES ('halt', 'PRIVATE-HALT');
        INSERT INTO availability_operation_resources VALUES ('orphan#0', 'missing', 'spend', 'foreign');
        INSERT INTO availability_operation_dependencies VALUES ('external', 'missing');
        INSERT INTO availability_operation_workflows VALUES ('foreign','other-deployment','other-header','missing',
          '{"reason":"challenge-closed","txHash":"terminal","spendPoint":"20.block"}');`);
      const raw = db
        .prepare(
          "SELECT record FROM availability_operation_intents ORDER BY id",
        )
        .all();
      const observed = inspect(f.path, { pageRows: 1 });
      const family = (name: string) =>
        observed.pages.filter((p) => p.family === name).flatMap((p) => p.rows);
      expect(observed.footer).toMatchObject({
        complete: true,
        authority: "stored_unreobserved",
        familyRows: {
          metadata: 2,
          leases: 2,
          intents: 5,
          resources: 5,
          dependencies: 6,
          workflows: 1,
        },
      });
      expect(
        family("intents")
          .map((r) => r.state)
          .sort(),
      ).toEqual(["confirmed", "conflict", "expired", "included", "pending"]);
      expect(family("leases")).toContainEqual(
        expect.objectContaining({
          actor: "unsigned",
          owner: "old-builder",
          expiresAtMs: 10,
        }),
      );
      expect(family("resources")).toContainEqual(
        expect.objectContaining({ intentId: "missing", intentPresent: false }),
      );
      expect(family("dependencies")).toContainEqual(
        expect.objectContaining({
          childId: "missing",
          childPresent: false,
          parentReference: "external_or_unresolved",
        }),
      );
      expect(family("workflows")).toContainEqual(
        expect.objectContaining({
          deployment: "other-deployment",
          retiredBy: "missing",
          retiredIntentPresent: false,
          release: {
            reason: "challenge-closed",
            txHash: "terminal",
            spendPoint: "20.block",
            authority: "stored_unreobserved",
          },
        }),
      );
      expect(JSON.stringify(observed)).not.toContain("PRIVATE-");
      expect(
        db
          .prepare(
            "SELECT record FROM availability_operation_intents ORDER BY id",
          )
          .all(),
      ).toEqual(raw);
      expect(
        db
          .prepare(
            "SELECT value FROM availability_journal_metadata WHERE key = 'halt'",
          )
          .get()?.value,
      ).toBe("PRIVATE-HALT");
      expect(() => f.journal.acquire("actor", "intruder", 1, 10)).toThrow(
        "already leased",
      );
    } finally {
      db.close();
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("pins every family across a real WAL writer between pages and sees later rows only in a new snapshot", () => {
    const f = fixture();
    const db = new DatabaseSync(f.path);
    const reader = openAvailabilityJournalInventory(f.path, { pageRows: 1 });
    try {
      expect(reader.nextPage()).toMatchObject({ family: "metadata" });
      db.exec("BEGIN");
      const later = {
        intent: {
          id: "later",
          actor: "later",
          deploymentIdentity: "deployment",
          headerHash: "header",
          action: "open",
          signedCbor: "PRIVATE-CBOR",
          txHash: "later-tx",
          spentOutRefs: ["later#0"],
          collateralOutRefs: [],
          expectedOutRefs: [],
          validUntilSlot: 100,
          completesWorkflow: false,
        },
        state: "pending",
        inclusionPoint: null,
        detail: null,
      };
      db.prepare(
        "INSERT INTO availability_operation_intents VALUES (?,?,?,?,?,?)",
      ).run(
        "later",
        "deployment",
        "later",
        JSON.stringify(later),
        "pending",
        "later-tx",
      );
      db.exec(`INSERT INTO availability_operation_leases VALUES ('later', 'writer', 1, 999);
        INSERT INTO availability_operation_resources VALUES ('later#0','later','spend','later');
        INSERT INTO availability_operation_dependencies VALUES ('external','later');
        INSERT INTO availability_operation_workflows VALUES ('later','deployment','header',NULL,NULL);
        COMMIT;`);
      const pages: AvailabilityJournalInventoryPage[] = [];
      for (;;) {
        const page = reader.nextPage();
        if (page.event === "availability_journal_inventory_complete") {
          expect(page).toMatchObject({ complete: true, rows: 1 });
          break;
        }
        pages.push(page);
      }
      expect(pages).toEqual([]);
      expect(inspect(f.path).footer).toMatchObject({ complete: true, rows: 6 });
    } finally {
      reader.close();
      db.close();
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it.each(["1", "2", "4"])(
    "refuses historical or unknown schema %s without migration",
    (schema) => {
      const f = fixture();
      const db = new DatabaseSync(f.path);
      try {
        db.prepare(
          "UPDATE availability_journal_metadata SET value = ? WHERE key = 'schema'",
        ).run(schema);
        expect(() => inspect(f.path)).toThrow(
          schema === "4"
            ? "inventory_schema_unsupported"
            : "historical_schema_inventory_unsupported",
        );
        expect(
          db
            .prepare(
              "SELECT value FROM availability_journal_metadata WHERE key = 'schema'",
            )
            .get()?.value,
        ).toBe(schema);
      } finally {
        db.close();
        f.journal.close();
        rmSync(f.directory, { recursive: true, force: true });
      }
    },
  );
  it("returns incomplete for row/output/UTF-8 byte bounds, and never creates missing files", () => {
    const f = fixture();
    const db = new DatabaseSync(f.path);
    try {
      db.exec(
        "INSERT INTO availability_operation_leases VALUES ('one','builder',1,0),('two','builder',1,0)",
      );
      expect(inspect(f.path, { totalRows: 1 }).footer).toMatchObject({
        complete: false,
        code: "inventory_row_limit_exceeded",
      });
      expect(inspect(f.path, { outputBytes: 1024 }).footer).toMatchObject({
        complete: false,
        code: "inventory_output_limit_exceeded",
      });
      db.prepare(
        "UPDATE availability_operation_leases SET owner = ? WHERE scope = 'one'",
      ).run("é\0".repeat(1024));
      expect(inspect(f.path).footer).toMatchObject({
        complete: false,
        code: "inventory_field_unsafe_or_oversized",
      });
      expect(() => inspect(join(f.directory, "missing.sqlite"))).toThrow(
        "inventory_existing_file_unavailable",
      );
      expect(
        () =>
          new DatabaseSync(join(f.directory, "missing.sqlite"), {
            readOnly: true,
          }),
      ).toThrow();
    } finally {
      db.close();
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it("refuses oversized retained JSON and malformed release evidence without deleting either", () => {
    const f = fixture();
    const db = new DatabaseSync(f.path);
    try {
      db.prepare(
        "INSERT INTO availability_operation_intents VALUES (?,?,?,?,?,?)",
      ).run(
        "large",
        "deployment",
        "actor",
        "é\0".repeat(1024),
        "pending",
        "tx",
      );
      expect(inspect(f.path, { recordBytes: 64 }).footer).toMatchObject({
        complete: false,
        code: "inventory_record_oversized_or_malformed",
      });
      expect(
        db
          .prepare(
            "SELECT length(CAST(record AS BLOB)) AS n FROM availability_operation_intents",
          )
          .get()?.n,
      ).toBe(3072);
      db.exec(
        "DELETE FROM availability_operation_intents; INSERT INTO availability_operation_workflows VALUES ('actor','deployment','header','missing','not-json')",
      );
      expect(inspect(f.path).footer).toMatchObject({
        complete: false,
        code: "inventory_record_malformed",
      });
      expect(
        db.prepare("SELECT release FROM availability_operation_workflows").get()
          ?.release,
      ).toBe("not-json");
      db.exec("DROP TABLE availability_operation_dependencies");
      expect(() => inspect(f.path)).toThrow("inventory_schema_incomplete");
    } finally {
      db.close();
      f.journal.close();
      rmSync(f.directory, { recursive: true, force: true });
    }
  });
  it.each([
    ["generation", "text"],
    ["expires_at", "blob"],
  ] as const)(
    "refuses huge nonnumeric lease %s (%s) before returning its bytes to JS",
    (column, storage) => {
      const f = fixture();
      const db = new DatabaseSync(f.path);
      try {
        // INTEGER affinity in the genuine writer's non-STRICT table accepts TEXT/BLOB.
        db.exec(
          "INSERT INTO availability_operation_leases VALUES ('actor','owner',1,0)",
        );
        db.prepare(
          `UPDATE availability_operation_leases SET ${column} = ?`,
        ).run(
          storage === "text"
            ? "x".repeat(8 * 1024 * 1024)
            : Buffer.alloc(8 * 1024 * 1024, 120),
        );
        expect(
          db
            .prepare(
              `SELECT typeof(${column}) AS storage, length(${column}) AS size FROM availability_operation_leases`,
            )
            .get(),
        ).toMatchObject({ storage, size: 8 * 1024 * 1024 });
        const bounded = db
          .prepare(
            `SELECT ${inventorySelection("leases", 1024, 262144)} FROM availability_operation_leases r`,
          )
          .get();
        expect(bounded?.[column]).toBeNull();
        expect(inspect(f.path).footer).toMatchObject({
          complete: false,
          code: "inventory_record_malformed",
        });
        expect(
          db
            .prepare(
              `SELECT length(${column}) AS size FROM availability_operation_leases`,
            )
            .get()?.size,
        ).toBe(8 * 1024 * 1024);
      } finally {
        db.close();
        f.journal.close();
        rmSync(f.directory, { recursive: true, force: true });
      }
    },
  );
});

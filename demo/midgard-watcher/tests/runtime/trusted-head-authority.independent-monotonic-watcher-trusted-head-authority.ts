import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
} from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  authenticationKey,
  head,
  hex32,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

describe("independent monotonic watcher trusted-head authority", () => {
  it("keeps exact expected-head authority across reopen and rejects stale writes", async () => {
    const scene = await sqliteScene();
    try {
      const current = await scene.advance(12);
      const stale = head(scene.input.policy, 0, "77");
      expect(
        await scene.store.compareAndSwap({
          expectedTrustedHead: stale,
          nextTrustedHead: head(scene.input.policy, 1, "88"),
        }),
      ).toEqual({ committed: false, head: current });
      const reopened = await openWatcherTrustedHeadAuthorityStore(scene.input);
      try {
        expect(await reopened.readCurrent()).toEqual(current);
      } finally {
        reopened.close();
      }
    } finally {
      scene.store.close();
    }
  });
  it.each([
    "record-MAC",
    "record-encoding",
    "record-substitution",
    "record-gap",
    "extra-record",
    "current-MAC",
    "checkpoint-MAC",
    "initialization-MAC",
    "unexpected-schema",
  ])(
    "rechecks every live dependency on the same open store: %s",
    async (mode) => {
      const scene = await sqliteScene();
      try {
        await scene.advance(12);
        scene.mutate((db) => {
          if (mode === "record-gap") {
            db.exec(
              "DELETE FROM authority_records WHERE revision='00000000000000000005'",
            );
            return;
          }
          if (mode === "extra-record") {
            db.exec(
              "INSERT INTO authority_records(revision,bytes) SELECT '00000000000000000012',bytes FROM authority_records LIMIT 1",
            );
            return;
          }
          if (mode === "unexpected-schema") {
            db.exec("CREATE TABLE unknown(x)");
            return;
          }
          if (mode.startsWith("record-")) {
            const row = db
              .prepare(
                "SELECT revision,bytes FROM authority_records ORDER BY revision LIMIT 1",
              )
              .get()!;
            const original = Buffer.from(row.bytes as Uint8Array).toString(
              "utf8",
            );
            let changed: string;
            if (mode === "record-encoding") changed = original + " ";
            else if (mode === "record-substitution")
              changed = Buffer.from(
                db
                  .prepare(
                    "SELECT bytes FROM authority_records ORDER BY revision DESC LIMIT 1",
                  )
                  .get()!.bytes as Uint8Array,
              ).toString("utf8");
            else
              changed = original.replace(
                /"recordMac":"[0-9a-f]+"/,
                '"recordMac":"' + hex32("00") + '"',
              );
            db.prepare(
              "UPDATE authority_records SET bytes=? WHERE revision=?",
            ).run(Buffer.from(changed), row.revision!);
            return;
          }
          const table =
            mode === "current-MAC"
              ? "authority_current"
              : mode === "checkpoint-MAC"
                ? "authority_checkpoint"
                : "authority_initialization";
          const old = Buffer.from(
            db.prepare(`SELECT bytes FROM ${table}`).get()!.bytes as Uint8Array,
          ).toString("utf8");
          db.prepare(`UPDATE ${table} SET bytes=?`).run(
            Buffer.from(
              old.replace(
                /"envelopeMac":"[0-9a-f]+"/,
                '"envelopeMac":"' + hex32("00") + '"',
              ),
            ),
          );
        });
        await expect(scene.store.readCurrent()).rejects.toThrow();
      } finally {
        scene.store.close();
      }
    },
  );
  it("exposes only authenticated loopback read and expected-prior CAS with read-back", async () => {
    const scene = await sqliteScene();
    const finalityPolicy = scene.input.policy;
    const store = scene.store;
    const server = await startWatcherTrustedHeadAuthorityServer({
      endpoint: "http://127.0.0.1:0",
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      store,
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const client = createWatcherTrustedHeadAuthorityClient({
        endpoint: server.endpoint,
        httpSecret: "authority-http-secret-with-sufficient-entropy",
        policy: finalityPolicy,
        authenticationKey,
        requestTimeoutMs: 2_000,
      });
      const first = head(finalityPolicy, 0, "10");
      expect(await client.readRecordAuthenticationKeyId()).toBe(
        createHash("sha256").update(recordAuthenticationKey).digest("hex"),
      );
      expect(await client.readCurrent()).toBeNull();
      expect(
        await client.compareAndSwap({
          expectedTrustedHead: null,
          nextTrustedHead: first,
        }),
      ).toBe(true);
      expect(await client.readCurrent()).toEqual(first);
      const poisoned = {
        ...head(finalityPolicy, 1, "20"),
        headMac: hex32("ff"),
      };
      const poisonedResponse = await fetch(
        `${server.endpoint}/v1/trusted-head/cas`,
        {
          method: "POST",
          headers: {
            authorization:
              "Bearer authority-http-secret-with-sufficient-entropy",
            "content-type": "application/json",
          },
          body: watcherCanonicalJson({
            expectedTrustedHead: first,
            nextTrustedHead: poisoned,
          }),
        },
      );
      expect(poisonedResponse.status).toBe(200);
      await expect(client.readCurrent()).rejects.toThrow("invalid head");
      await expect(
        fetch(`${server.endpoint}/v1/trusted-head`, {
          headers: { authorization: "Bearer wrong-secret-never-authorized" },
        }),
      ).resolves.toMatchObject({ status: 401 });
      await expect(
        fetch(`${server.endpoint}/v1/trusted-head`, {
          method: "DELETE",
          headers: {
            authorization:
              "Bearer authority-http-secret-with-sufficient-entropy",
          },
        }),
      ).resolves.toMatchObject({ status: 404 });
    } finally {
      await server.close();
      store.close();
    }
  });

  it("reports persistence failures as 500 without returning internal details", async () => {
    const server = await startWatcherTrustedHeadAuthorityServer({
      endpoint: "http://127.0.0.1:0",
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      store: {
        readRecordAuthenticationKeyId: async () => hex32("99"),
        readCurrent: async () => {
          throw new Error("sensitive filesystem path and cause");
        },
        compareAndSwap: async () => ({ committed: false, head: null }),
      },
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const response = await fetch(`${server.endpoint}/v1/trusted-head`, {
        headers: {
          authorization: "Bearer authority-http-secret-with-sufficient-entropy",
        },
      });
      expect(response.status).toBe(500);
      expect(await response.json()).toEqual({ error: "persistence_failure" });
    } finally {
      await server.close();
    }
  });
});

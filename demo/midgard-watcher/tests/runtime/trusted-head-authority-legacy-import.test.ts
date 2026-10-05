import { randomUUID } from "node:crypto";
import { writeFileSync } from "node:fs";
import { readFile, rm, symlink, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { expect, it, vi } from "vitest";

import {
  makeAuthorityRecord,
  sha256,
} from "../../src/runtime/trusted-head-authority.exact-record.js";
import {
  auditLegacyWatcherTrustedHeadAuthority,
  importLegacyAuthorityStore,
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import { legacyScene } from "./trusted-head-authority.legacy-fixture.js";
import { directory, head } from "./trusted-head-authority.policy.js";

it.each([1, 2, 8])(
  "imports exact legacy head/revision with K=%s and retires archive authority without an implicit fallback",
  async (liveRecordLimit) => {
    const legacy = await legacyScene(12);
    const input = {
      directory: await directory(),
      policy: legacy.policy,
      recordAuthenticationKey: legacy.recordAuthenticationKey,
      liveRecordLimit,
      generation: `generation-${randomUUID()}`,
      legacyDirectory: legacy.path,
    };
    await importLegacyAuthorityStore(input);
    const store = await openWatcherTrustedHeadAuthorityStore(input);
    try {
      const exact = head(legacy.policy, 11, "77");
      expect(await store.readCurrent()).toEqual(exact);
      await writeFile(
        join(legacy.path, "00000000000000000000.json"),
        "retired archive corruption",
      );
      expect(await store.readCurrent()).toEqual(exact);
      await expect(
        auditLegacyWatcherTrustedHeadAuthority({
          ...input,
          directory: legacy.path,
        }),
      ).rejects.toThrow();
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: exact,
          nextTrustedHead: head(legacy.policy, 12, "88"),
        }),
      ).toMatchObject({ committed: true });
    } finally {
      store.close();
    }
    const databasePath = join(
      input.directory,
      input.generation,
      "authority.sqlite",
    );
    await rm(databasePath);
    await expect(openWatcherTrustedHeadAuthorityStore(input)).rejects.toThrow();
    await expect(readFile(databasePath)).rejects.toThrow();
    await expect(importLegacyAuthorityStore(input)).rejects.toThrow();
  },
);
it.each(["empty-final", "torn-final", "unknown-file", "gap", "symlink"])(
  "never repairs or selects malformed legacy source: %s",
  async (mode) => {
    const legacy = await legacyScene(3),
      last = join(legacy.path, "00000000000000000002.json");
    if (mode === "empty-final") await writeFile(last, "");
    if (mode === "torn-final") await writeFile(last, '{"head":');
    if (mode === "unknown-file")
      await writeFile(join(legacy.path, "unexpected"), "unknown");
    if (mode === "gap")
      await rm(join(legacy.path, "00000000000000000001.json"));
    if (mode === "symlink") {
      await rm(last);
      await symlink(join(legacy.path, "00000000000000000000.json"), last);
    }
    const input = {
      directory: await directory(),
      policy: legacy.policy,
      recordAuthenticationKey: legacy.recordAuthenticationKey,
      liveRecordLimit: 2,
      generation: `generation-${randomUUID()}`,
      legacyDirectory: legacy.path,
    };
    await expect(importLegacyAuthorityStore(input)).rejects.toThrow();
    await expect(
      readFile(join(input.directory, "authority-backend.json")),
    ).rejects.toThrow();
    if (mode === "empty-final") expect((await readFile(last)).length).toBe(0);
    if (mode === "torn-final")
      expect(await readFile(last, "utf8")).toBe('{"head":');
  },
);
it("never interprets a legacy directory as fresh initialization", async () => {
  const legacy = await legacyScene(1);
  await expect(
    initializeSelectedAuthorityStore({
      directory: legacy.path,
      policy: legacy.policy,
      recordAuthenticationKey: legacy.recordAuthenticationKey,
      liveRecordLimit: 1,
      generation: `generation-${randomUUID()}`,
    }),
  ).rejects.toThrow(/genuinely new/);
});
it("refuses a correctly signed legacy successor appearing after preparation and before selection", async () => {
  const legacy = await legacyScene(3),
    last = await readFile(join(legacy.path, "00000000000000000002.json"));
  const input = {
    directory: await directory(),
    policy: legacy.policy,
    recordAuthenticationKey: legacy.recordAuthenticationKey,
    liveRecordLimit: 2,
    generation: `generation-${randomUUID()}`,
    legacyDirectory: legacy.path,
  };
  const next = makeAuthorityRecord({
    head: head(legacy.policy, 3, "88"),
    priorRecordSha256: sha256(last),
    recordAuthenticationKey: legacy.recordAuthenticationKey,
  });
  const raw = watcherCanonicalJson(next),
    path = join(legacy.path, "00000000000000000003.json");
  const original = DatabaseSync.prototype.exec;
  let appended = false;
  const changed = vi
    .spyOn(DatabaseSync.prototype, "exec")
    .mockImplementation(function (this: DatabaseSync, sql: string) {
      original.call(this, sql);
      if (sql.startsWith("COMMIT;") && !appended) {
        appended = true;
        writeFileSync(path, raw);
      }
    });
  try {
    await expect(importLegacyAuthorityStore(input)).rejects.toThrow(
      /source changed/,
    );
  } finally {
    changed.mockRestore();
  }
  expect(appended).toBe(true);
  expect(await readFile(path, "utf8")).toBe(raw);
  await expect(
    readFile(join(input.directory, "authority-backend.json")),
  ).rejects.toThrow();
  const intent = await readFile(
    join(input.directory, input.generation, "initialization-intent.json"),
  );
  await expect(importLegacyAuthorityStore(input)).rejects.toThrow(
    /initialization intent differs/,
  );
  expect(
    await readFile(
      join(input.directory, input.generation, "initialization-intent.json"),
    ),
  ).toEqual(intent);
});

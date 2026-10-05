import { randomUUID } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { expect, it, vi } from "vitest";

import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import {
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

it.each(["before", "after"])(
  "resumes the original authenticated initialization after %s SQLite commit acknowledgement loss",
  async (phase) => {
    const input = {
      directory: await directory(),
      policy: policy(),
      recordAuthenticationKey,
      liveRecordLimit: 2,
      generation: `generation-${randomUUID()}`,
    };
    const original = DatabaseSync.prototype.exec;
    let fired = false;
    const fault = vi
      .spyOn(DatabaseSync.prototype, "exec")
      .mockImplementation(function (this: DatabaseSync, sql: string) {
        if (sql.startsWith("COMMIT;") && !fired) {
          fired = true;
          if (phase === "after") original.call(this, sql);
          throw new Error("synthetic initialization commit loss");
        }
        return original.call(this, sql);
      });
    try {
      await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
        "synthetic initialization commit loss",
      );
    } finally {
      fault.mockRestore();
    }
    expect(fired).toBe(true);
    await expect(
      readFile(join(input.directory, "authority-backend.json")),
    ).rejects.toThrow();
    const intentPath = join(
      input.directory,
      input.generation,
      "initialization-intent.json",
    );
    const intent = await readFile(intentPath);
    await initializeSelectedAuthorityStore(input);
    expect(await readFile(intentPath)).toEqual(intent);
    const store = await openWatcherTrustedHeadAuthorityStore(input);
    try {
      expect(await store.readCurrent()).toBeNull();
    } finally {
      store.close();
    }
  },
);
it.each(["before", "after"])(
  "recovers exactly one complete authority after %s CAS commit loss through a checkpoint boundary",
  async (phase) => {
    const scene = await sqliteScene(1),
      prior = await scene.advance(1),
      next = head(scene.input.policy, 1, "88");
    const original = DatabaseSync.prototype.exec;
    let fired = false;
    const fault = vi
      .spyOn(DatabaseSync.prototype, "exec")
      .mockImplementation(function (this: DatabaseSync, sql: string) {
        if (sql === "COMMIT" && !fired) {
          fired = true;
          if (phase === "after") original.call(this, sql);
          throw new Error("synthetic CAS commit loss");
        }
        return original.call(this, sql);
      });
    try {
      await expect(
        scene.store.compareAndSwap({
          expectedTrustedHead: prior,
          nextTrustedHead: next,
        }),
      ).rejects.toThrow("synthetic CAS commit loss");
    } finally {
      fault.mockRestore();
      scene.store.close();
    }
    expect(fired).toBe(true);
    const reopened = await openWatcherTrustedHeadAuthorityStore(scene.input);
    try {
      expect(await reopened.readCurrent()).toEqual(
        phase === "before" ? prior : next,
      );
      if (phase === "after")
        expect(
          await reopened.compareAndSwap({
            expectedTrustedHead: prior,
            nextTrustedHead: next,
          }),
        ).toEqual({ committed: false, head: next });
    } finally {
      reopened.close();
    }
  },
);

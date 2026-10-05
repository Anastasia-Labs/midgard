import { createHmac, randomUUID } from "node:crypto";
import { readFile, rename, symlink, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { expect, it } from "vitest";

import {
  makeAuthorityRecord,
  sha256,
} from "../../src/runtime/trusted-head-authority.exact-record.js";
import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  authenticationKey,
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

it.each([undefined, 0, -1, 1.5, 4097])(
  "refuses initialization without supported explicit K=%s",
  async (liveRecordLimit) => {
    const root = await directory();
    await expect(
      initializeSelectedAuthorityStore({
        directory: root,
        policy: policy(),
        recordAuthenticationKey,
        liveRecordLimit: liveRecordLimit as number,
        generation: `generation-${randomUUID()}`,
      }),
    ).rejects.toThrow(/liveRecordLimit/);
    await expect(
      readFile(join(root, "authority-backend.json")),
    ).rejects.toThrow();
  },
);
it.each(["key", "policy", "limit"])(
  "binds selected reads to configured %s identity",
  async (mode) => {
    const scene = await sqliteScene(2);
    scene.store.close();
    const input =
      mode === "key"
        ? {
            ...scene.input,
            recordAuthenticationKey: new Uint8Array(32).fill(17),
          }
        : mode === "policy"
          ? {
              ...scene.input,
              policy: { ...scene.input.policy, policyDigest: "11".repeat(32) },
            }
          : { ...scene.input, liveRecordLimit: 1 };
    await expect(openWatcherTrustedHeadAuthorityStore(input)).rejects.toThrow();
  },
);
it.each(["selector", "database"])(
  "rejects a symlink for the selected %s",
  async (mode) => {
    const scene = await sqliteScene(2);
    scene.store.close();
    const path =
      mode === "selector"
        ? join(scene.input.directory, "authority-backend.json")
        : scene.databasePath;
    await rename(path, path + ".original");
    await symlink(path + ".original", path);
    await expect(
      openWatcherTrustedHeadAuthorityStore(scene.input),
    ).rejects.toThrow(/identity/);
  },
);
it.each(["current", "record"])(
  "rejects oversized retained %s bytes without admitting partial material",
  async (mode) => {
    const scene = await sqliteScene(1);
    await scene.advance(2);
    scene.mutate((db) => {
      db.exec("PRAGMA ignore_check_constraints=ON");
      db.prepare(
        mode === "current"
          ? "UPDATE authority_current SET bytes=?"
          : "UPDATE authority_records SET bytes=?",
      ).run(Buffer.alloc(32769));
    });
    try {
      await expect(scene.store.readCurrent()).rejects.toThrow(/bytes/);
    } finally {
      scene.store.close();
    }
  },
);
it("does not reset an advanced selected authority when the original initialization is retried", async () => {
  const scene = await sqliteScene(1);
  const current = await scene.advance(3);
  const selector = await readFile(
    join(scene.input.directory, "authority-backend.json"),
  );
  await initializeSelectedAuthorityStore(scene.input);
  try {
    expect(await scene.store.readCurrent()).toEqual(current);
    expect(
      await readFile(join(scene.input.directory, "authority-backend.json")),
    ).toEqual(selector);
  } finally {
    scene.store.close();
  }
});
it("rejects a changed authenticated initialization intent without deleting its bytes", async () => {
  const scene = await sqliteScene(1);
  scene.store.close();
  const path = join(scene.input.directory, "authority-backend.json");
  const original = await readFile(path);
  await writeFile(path, "{}");
  await expect(initializeSelectedAuthorityStore(scene.input)).rejects.toThrow();
  expect(await readFile(path, "utf8")).toBe("{}");
  await writeFile(path, original);
  await expect(
    initializeSelectedAuthorityStore({
      ...scene.input,
      generation: `generation-${randomUUID()}`,
    }),
  ).rejects.toThrow();
});
it("uses exact uint64 tail geometry and refuses wraparound at the maximum revision", async () => {
  const scene = await sqliteScene(1),
    maximum = (1n << 64n) - 1n;
  const at = (revision: bigint) => {
    const { headMac: _ignored, ...original } = head(
      scene.input.policy,
      0,
      "77",
    );
    const body = { ...original, revision: revision.toString() };
    return {
      ...body,
      headMac: createHmac("sha256", authenticationKey)
        .update(`${body.schemaVersion}:${watcherCanonicalJson(body)}`)
        .digest("hex"),
    };
  };
  const boundary = makeAuthorityRecord({
    head: at(maximum - 1n),
    priorRecordSha256: "12".repeat(32),
    recordAuthenticationKey,
  });
  const boundaryBytes = Buffer.from(watcherCanonicalJson(boundary));
  const last = makeAuthorityRecord({
    head: at(maximum),
    priorRecordSha256: sha256(boundaryBytes),
    recordAuthenticationKey,
  });
  const lastBytes = Buffer.from(watcherCanonicalJson(last));
  const checkpoint = scene.envelopes.encode("checkpoint", {
    boundaryRecord: boundary,
  });
  scene.mutate((db) => {
    const initial = db
      .prepare("SELECT bytes FROM authority_initialization")
      .get()!.bytes as Uint8Array;
    db.prepare("INSERT INTO authority_checkpoint VALUES(1,?)").run(checkpoint);
    db.prepare("INSERT INTO authority_records VALUES(?,?)").run(
      maximum.toString().padStart(20, "0"),
      lastBytes,
    );
    db.prepare("UPDATE authority_current SET bytes=?").run(
      scene.envelopes.encode("current", {
        head: last.head,
        recordSha256: sha256(lastBytes),
        checkpointSha256: sha256(checkpoint),
        initializationSha256: sha256(initial),
      }),
    );
  });
  try {
    expect(await scene.store.readCurrent()).toEqual(last.head);
    await expect(
      scene.store.compareAndSwap({
        expectedTrustedHead: last.head,
        nextTrustedHead: at(maximum + 1n),
      }),
    ).rejects.toThrow(/revision/);
    expect(
      await scene.store.compareAndSwap({
        expectedTrustedHead: last.head,
        nextTrustedHead: at(0n),
      }),
    ).toEqual({ committed: false, head: last.head });
    expect(await scene.store.readCurrent()).toEqual(last.head);
  } finally {
    scene.store.close();
  }
});

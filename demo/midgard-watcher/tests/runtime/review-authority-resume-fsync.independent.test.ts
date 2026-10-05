import { randomUUID } from "node:crypto";
import { access, readFile } from "node:fs/promises";
import { join } from "node:path";

import { expect, it, vi } from "vitest";

import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import * as exclusive from "../../src/storage/exclusive-record-file.js";
import {
  directory,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";

it("refuses successful same-attempt resume until a previously linked selector receives directory fsync", async () => {
  const input = {
    directory: await directory(),
    policy: policy(),
    recordAuthenticationKey,
    liveRecordLimit: 1,
    generation: `generation-${randomUUID()}`,
  };
  const selector = join(input.directory, "authority-backend.json");
  const realSync = exclusive.syncDirectory;
  let failedSyncs = 0;
  const fault = vi
    .spyOn(exclusive, "syncDirectory")
    .mockImplementation(async (path) => {
      if (path === input.directory) {
        const exists = await access(selector).then(
          () => true,
          () => false,
        );
        if (exists) {
          failedSyncs++;
          throw new Error("independent selector directory fsync refusal");
        }
      }
      await realSync(path);
    });
  try {
    await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
      "independent selector directory fsync refusal",
    );
    const linkedBytes = await readFile(selector);
    expect(failedSyncs).toBe(1);
    await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
      "independent selector directory fsync refusal",
    );
    expect(failedSyncs).toBe(2);
    expect(await readFile(selector)).toEqual(linkedBytes);
  } finally {
    fault.mockRestore();
  }
  const store = await openWatcherTrustedHeadAuthorityStore(input);
  store.close();
});

it("does not acknowledge a new volume retry while the failed authority-directory creation parent sync remains refused", async () => {
  const parent = await directory();
  const input = {
    directory: join(parent, "new-authority"),
    policy: policy(),
    recordAuthenticationKey,
    liveRecordLimit: 1,
    generation: `generation-${randomUUID()}`,
  };
  const realSync = exclusive.syncDirectory;
  let failedSyncs = 0;
  const fault = vi
    .spyOn(exclusive, "syncDirectory")
    .mockImplementation(async (path) => {
      if (path === parent) {
        failedSyncs++;
        throw new Error("independent authority parent directory fsync refusal");
      }
      await realSync(path);
    });
  try {
    await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
      "independent authority parent directory fsync refusal",
    );
    expect(failedSyncs).toBe(1);
    // Existing mkdir entries after acknowledgement loss are not evidence that
    // their parent received fsync. Public retry must discharge that debt.
    await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
      "independent authority parent directory fsync refusal",
    );
    expect(failedSyncs).toBe(2);
  } finally {
    fault.mockRestore();
  }
});

it("reports the actual selected-retry synchronization footprint independently of two helper calls", async () => {
  const input = {
    directory: await directory(),
    policy: policy(),
    recordAuthenticationKey,
    liveRecordLimit: 1,
    generation: `generation-${randomUUID()}`,
  };
  const selector = join(input.directory, "authority-backend.json");
  const realSync = exclusive.syncDirectory;
  const initialFault = vi
    .spyOn(exclusive, "syncDirectory")
    .mockImplementation(async (path) => {
      if (
        path === input.directory &&
        (await access(selector).then(
          () => true,
          () => false,
        ))
      )
        throw new Error("independent first publication refused");
      await realSync(path);
    });
  try {
    await expect(initializeSelectedAuthorityStore(input)).rejects.toThrow(
      "independent first publication refused",
    );
  } finally {
    initialFault.mockRestore();
  }
  const before = await readFile(selector);
  const calls: string[] = [];
  const trace = vi
    .spyOn(exclusive, "syncDirectory")
    .mockImplementation(async (path) => {
      calls.push(path);
      await realSync(path);
    });
  try {
    await initializeSelectedAuthorityStore(input);
  } finally {
    trace.mockRestore();
  }
  expect(await readFile(selector)).toEqual(before);
  console.log(
    "INDEPENDENT_SELECTED_RETRY_SYNC " +
      JSON.stringify({
        acknowledged: true,
        syncDirectories: calls,
        selectorUnchanged: true,
      }),
  );
  expect(calls).toContain(input.directory);
});

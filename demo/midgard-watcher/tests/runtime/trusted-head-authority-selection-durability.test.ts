import { dirname, join } from "node:path";

import { expect, it, vi } from "vitest";

import {
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import * as exclusive from "../../src/storage/exclusive-record-file.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

it.each(["generation", "authority", "parent"])(
  "refuses an already selected same-attempt retry while %s publication sync fails",
  async (kind) => {
    const scene = await sqliteScene(1),
      head = await scene.advance(3);
    scene.store.close();
    const path =
      kind === "generation"
        ? join(scene.input.directory, scene.input.generation)
        : kind === "authority"
          ? scene.input.directory
          : dirname(scene.input.directory);
    const original = exclusive.syncDirectory;
    let refused = 0;
    const fault = vi
      .spyOn(exclusive, "syncDirectory")
      .mockImplementation(async (directory) => {
        if (directory === path) {
          refused++;
          throw new Error("synthetic required publication sync refusal");
        }
        await original(directory);
      });
    try {
      await expect(
        initializeSelectedAuthorityStore(scene.input),
      ).rejects.toThrow(/publication sync refusal/);
      expect(refused).toBe(1);
    } finally {
      fault.mockRestore();
    }
    await initializeSelectedAuthorityStore(scene.input);
    const reopened = await openWatcherTrustedHeadAuthorityStore(scene.input);
    try {
      expect(await reopened.readCurrent()).toEqual(head);
    } finally {
      reopened.close();
    }
  },
);
it("reasserts every namespace layer without replacing selected intent or advanced state", async () => {
  const scene = await sqliteScene(1),
    head = await scene.advance(3);
  scene.store.close();
  const original = exclusive.syncDirectory,
    calls: string[] = [];
  const trace = vi
    .spyOn(exclusive, "syncDirectory")
    .mockImplementation(async (path) => {
      calls.push(path);
      await original(path);
    });
  try {
    await initializeSelectedAuthorityStore(scene.input);
  } finally {
    trace.mockRestore();
  }
  expect(calls).toContain(join(scene.input.directory, scene.input.generation));
  expect(calls).toContain(scene.input.directory);
  expect(calls).toContain(dirname(scene.input.directory));
  const reopened = await openWatcherTrustedHeadAuthorityStore(scene.input);
  try {
    expect(await reopened.readCurrent()).toEqual(head);
  } finally {
    reopened.close();
  }
});
it("ordinary selected open does not acquire provisioning directory-sync behavior", async () => {
  const scene = await sqliteScene(1);
  scene.store.close();
  const fault = vi
    .spyOn(exclusive, "syncDirectory")
    .mockRejectedValue(new Error("no ordinary-open provisioning"));
  try {
    const reopened = await openWatcherTrustedHeadAuthorityStore(scene.input);
    try {
      expect(await reopened.readCurrent()).toBeNull();
    } finally {
      reopened.close();
    }
    expect(fault).not.toHaveBeenCalled();
  } finally {
    fault.mockRestore();
  }
});

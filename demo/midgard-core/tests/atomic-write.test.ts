import { mkdirSync } from "node:fs";
import { mkdtemp, readdir, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  writeJsonFileAtomic,
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
  writeTextFileAtomicSync,
} from "../src/atomic-write.js";

const tempFiles = async (directory: string): Promise<string[]> =>
  (await readdir(directory, { recursive: true })).filter((name) =>
    name.includes(".tmp-"),
  );

describe("durable atomic writes", () => {
  it("publishes immutable evidence atomically without replacing a racing writer", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-no-replace-"));
    try {
      const path = join(directory, "evidence.json");
      const outcomes = await Promise.allSettled([
        writeTextFileAtomicNoReplace(path, "first\n", { mode: 0o600 }),
        writeTextFileAtomicNoReplace(path, "second\n", { mode: 0o600 }),
      ]);
      expect(
        outcomes.filter(({ status }) => status === "fulfilled"),
      ).toHaveLength(1);
      expect(
        outcomes.filter(({ status }) => status === "rejected"),
      ).toHaveLength(1);
      expect(["first\n", "second\n"]).toContain(await readFile(path, "utf8"));
      expect(
        (await readdir(directory)).filter((name) => name.includes(".tmp-")),
      ).toHaveLength(0);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("writes content and applies the requested private mode without leaving a temp file", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-atomic-write-"));
    try {
      const textPath = join(directory, "state.json");
      await writeTextFileAtomic(textPath, "journal\n", { mode: 0o600 });
      expect(await readFile(textPath, "utf8")).toBe("journal\n");
      const jsonPath = join(directory, "summary.json");
      await writeJsonFileAtomic(jsonPath, { durable: true }, { mode: 0o640 });
      expect(await readFile(jsonPath, "utf8")).toBe(
        '{\n  "durable": true\n}\n',
      );
      expect(
        (await readdir(directory)).filter((name) => name.includes(".tmp-"))
          .length,
      ).toBe(0);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("creates missing directories with the requested directory mode", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-atomic-dirs-"));
    try {
      const asyncPath = join(directory, "async", "nested", "state.json");
      await writeTextFileAtomic(asyncPath, "async\n", { directoryMode: 0o700 });
      const syncPath = join(directory, "sync", "nested", "state.json");
      writeTextFileAtomicSync(syncPath, "sync\n", { directoryMode: 0o700 });
      for (const created of ["async", "async/nested", "sync", "sync/nested"])
        expect((await stat(join(directory, created))).mode & 0o777).toBe(0o700);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});

describe("synchronous durable atomic writes", () => {
  it("replaces the file with string or byte content in the requested mode", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-atomic-sync-"));
    try {
      const path = join(directory, "state.json");
      writeTextFileAtomicSync(path, "first\n", { mode: 0o600 });
      expect(await readFile(path, "utf8")).toBe("first\n");
      expect((await stat(path)).mode & 0o777).toBe(0o600);
      // The mode is applied exactly, not narrowed by the process umask.
      writeTextFileAtomicSync(path, new TextEncoder().encode("second\n"), {
        mode: 0o666,
      });
      expect(await readFile(path, "utf8")).toBe("second\n");
      expect((await stat(path)).mode & 0o777).toBe(0o666);
      expect(await tempFiles(directory)).toEqual([]);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("leaves the target and no temp file behind when the write fails", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-atomic-sync-"));
    try {
      // A directory cannot be replaced by a file: the rename fails.
      const path = join(directory, "occupied");
      mkdirSync(path);
      expect(() => writeTextFileAtomicSync(path, "content\n")).toThrow();
      expect((await stat(path)).isDirectory()).toBe(true);
      expect(await tempFiles(directory)).toEqual([]);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});

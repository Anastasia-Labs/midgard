import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  writeJsonFileAtomic,
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "@/files/atomic-write.js";

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
});

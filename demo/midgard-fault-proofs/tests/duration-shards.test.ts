import { readdirSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  planDurationShards,
  readShardDurationTable,
} from "@al-ft/midgard-test-support/duration-shards";
import { describe, expect, it } from "vitest";

import { EmulatorSequencer } from "./support/emulator-sequencer.js";

const packageRoot = fileURLToPath(new URL("..", import.meta.url));
const tablePath = fileURLToPath(
  new URL("./support/ci-file-durations.json", import.meta.url),
);
const table = readShardDurationTable(tablePath);
// The same files the Vitest include globs (`./tests/**/*.test.{ts,tsx}`) name.
const testFiles = readdirSync(new URL(".", import.meta.url), {
  recursive: true,
  encoding: "utf8",
})
  .filter((name) => /\.test\.tsx?$/u.test(name))
  .map((name) => `tests/${name.split("\\").join("/")}`);

const shardsOf = (
  entries: readonly { id: string; file: string }[],
  count: number,
) => {
  const plan = planDurationShards({ entries, count, table });
  return Array.from({ length: count }, (_, index) =>
    entries.filter(({ id }) => plan.get(id) === index + 1).map(({ id }) => id),
  );
};

describe("duration-aware CI shards", () => {
  it("runs every current test file in exactly one of three shards", () => {
    const entries = testFiles.map((file) => ({ id: file, file }));
    const shards = shardsOf(entries, 3);
    expect(shards.flat().sort()).toEqual(entries.map(({ id }) => id).sort());
    for (const shard of shards) expect(shard.length).toBeGreaterThan(0);
  });

  it("places files the table does not know, and ignores input order", () => {
    const entries = [
      ...testFiles.slice(0, 40).map((file) => ({ id: file, file })),
      ...Array.from({ length: 25 }, (_, index) => ({
        id: `tests/new-${index}.test.ts`,
        file: `tests/new-${index}.test.ts`,
      })),
    ];
    for (const count of [1, 2, 3, 5]) {
      const forward = shardsOf(entries, count);
      const backward = shardsOf([...entries].reverse(), count);
      expect(backward.map((shard) => [...shard].sort())).toEqual(
        forward.map((shard) => [...shard].sort()),
      );
      expect(forward.flat().sort()).toEqual(entries.map(({ id }) => id).sort());
    }
  });

  it("shards Vitest specifications through the configured sequencer", async () => {
    const specs = testFiles.map((file, index) => ({
      moduleId: `${packageRoot}${file}`,
      project: { name: index % 7 === 0 ? "interactive-emulator" : "testing" },
    }));
    const seen: string[] = [];
    for (let index = 1; index <= 3; index++) {
      const sequencer = new EmulatorSequencer({
        config: {
          root: packageRoot.replace(/\/$/u, ""),
          shard: { index, count: 3 },
        },
      } as never) as unknown as {
        shard: (files: typeof specs) => Promise<typeof specs>;
        sort: (files: typeof specs) => Promise<typeof specs>;
      };
      const mine = await sequencer.sort(await sequencer.shard(specs));
      const seconds = mine.map(
        ({ moduleId }) =>
          table.files.get(moduleId.slice(packageRoot.length)) ??
          table.defaultSeconds,
      );
      expect(seconds).toEqual([...seconds].sort((a, b) => b - a));
      seen.push(...mine.map(({ moduleId }) => moduleId));
    }
    expect(seen.sort()).toEqual(specs.map(({ moduleId }) => moduleId).sort());
  });
});

/**
 * The partition contract every package that shards by duration
 * (`durationShards` in `../duration-shards.js`) pins in its own suite: on
 * every shard count, each current test file runs in exactly one shard,
 * whatever the table knows about it and whatever order Vitest lists files in.
 * A package calls {@link describeDurationShards} from one test file.
 */

import { readdirSync } from "node:fs";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  durationShardSequencer,
  planDurationShards,
  readShardDurationTable,
} from "../duration-shards.js";

type Entry = { readonly id: string; readonly file: string };

type SequencerProbe = {
  shard: (files: readonly Spec[]) => Promise<Spec[]>;
  sort: (files: readonly Spec[]) => Promise<Spec[]>;
};
type Spec = { readonly moduleId: string; readonly project: { name: string } };

export const describeDurationShards = ({
  packageRoot,
  tablePath,
  ciShardCount,
  testFile = /\.test\.tsx?$/u,
}: {
  /** Absolute package directory, the Vitest root. */
  readonly packageRoot: string;
  /** The package's committed duration table. */
  readonly tablePath: string;
  /** The `--shard=i/<count>` count Node CI runs the package with. */
  readonly ciShardCount: number;
  /** Test files under `tests/`, as the package's Vitest include globs name them. */
  readonly testFile?: RegExp;
}): void => {
  const table = readShardDurationTable(tablePath);
  const testFiles = readdirSync(join(packageRoot, "tests"), {
    recursive: true,
    encoding: "utf8",
  })
    .filter((name) => testFile.test(name))
    .map((name) => `tests/${name.split("\\").join("/")}`)
    .sort();
  const shardsOf = (entries: readonly Entry[], count: number) => {
    const plan = planDurationShards({ entries, count, table });
    return Array.from({ length: count }, (_, index) =>
      entries
        .filter(({ id }) => plan.get(id) === index + 1)
        .map(({ id }) => id),
    );
  };
  const counts = Array.from({ length: ciShardCount + 3 }, (_, i) => i + 1);

  describe("duration-aware CI shards", () => {
    it("runs every current test file in exactly one shard, on every shard count", () => {
      expect(testFiles.length).toBeGreaterThan(0);
      const entries = testFiles.map((file) => ({ id: file, file }));
      for (const count of counts) {
        const shards = shardsOf(entries, count);
        expect(shards.flat().sort()).toEqual(testFiles);
        if (count <= ciShardCount)
          for (const shard of shards) expect(shard.length).toBeGreaterThan(0);
      }
    });

    it("places files the table does not know, and ignores input order", () => {
      const entries = [
        ...testFiles.slice(0, 40).map((file) => ({ id: file, file })),
        ...Array.from({ length: 25 }, (_, index) => ({
          id: `tests/new-${index}.test.ts`,
          file: `tests/new-${index}.test.ts`,
        })),
      ];
      for (const count of counts) {
        const forward = shardsOf(entries, count);
        const backward = shardsOf([...entries].reverse(), count);
        expect(backward.map((shard) => [...shard].sort())).toEqual(
          forward.map((shard) => [...shard].sort()),
        );
        expect(forward.flat().sort()).toEqual(
          entries.map(({ id }) => id).sort(),
        );
      }
    });

    it("shards Vitest specifications through the sequencer, longest first", async () => {
      const Sequencer = durationShardSequencer({ tablePath });
      const root = packageRoot.replace(/\/$/u, "");
      const specs: Spec[] = testFiles.map((file, index) => ({
        moduleId: `${root}/${file}`,
        project: { name: index % 7 === 0 ? "interactive" : "testing" },
      }));
      for (const count of counts) {
        const seen: string[] = [];
        for (let index = 1; index <= count; index++) {
          const sequencer = new Sequencer({
            config: { root, shard: { index, count } },
          } as never) as unknown as SequencerProbe;
          const mine = await sequencer.sort(await sequencer.shard(specs));
          const seconds = mine.map(
            ({ moduleId }) =>
              table.files.get(moduleId.slice(root.length + 1)) ??
              table.defaultSeconds,
          );
          expect(seconds).toEqual([...seconds].sort((a, b) => b - a));
          seen.push(...mine.map(({ moduleId }) => moduleId));
        }
        expect(seen.sort()).toEqual(specs.map(({ moduleId }) => moduleId));
      }
    });
  });
};

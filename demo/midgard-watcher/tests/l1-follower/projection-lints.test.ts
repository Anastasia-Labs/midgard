import { readFile } from "node:fs/promises";

import { createTemporalRegistry } from "@al-ft/midgard-l1-follower";
import {
  declaredTables,
  lintDeterminism,
  lintSchema,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

import {
  WATCHER_DA_ATTESTATIONS_TABLE,
  WATCHER_QUEUE_CHECKPOINTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  watcherProjection,
} from "../../src/l1-follower/projection.js";
import { SIM_WATCHER_DEPLOYMENT } from "../support/l1-follower-state-queue-traffic.js";

const projection = watcherProjection(SIM_WATCHER_DEPLOYMENT);

/**
 * The projection's sources, and the old decoders it reuses with their local
 * imports (the lint is per file, not transitive).
 */
const PROJECTION_SOURCES = [
  "src/l1-follower/projection.ts",
  "src/l1-follower/view.ts",
  "src/l1-follower/checkpoints.ts",
  "src/l1-follower/checkpoints.read.ts",
  "src/l1-follower/projection.schema.ts",
  "src/l1-follower/projection.da-attestations.ts",
  "src/l1-follower/raw-reads.ts",
  "src/l1-follower/raw-reads.types.ts",
  "src/l1-follower/reads.ts",
  "src/l1-follower/tables.ts",
  "src/indexers/authenticated-state-queue-observation.correction-lock-witness.ts",
  "src/indexers/authenticated-state-queue-observation.reconstruct-queue.ts",
  "src/indexers/authenticated-state-queue-observation.queue-output.ts",
  "src/indexers/authenticated-state-queue-observation.parse-persisted-header.ts",
  "src/indexers/authenticated-state-queue-observation.parse-persisted-observation.ts",
];

describe("watcher projection lints (F2 schema, F4 determinism)", () => {
  it("declares four class D-t tables, registered as temporal, on both dialects", () => {
    const registry = createTemporalRegistry(projection.temporalTables);
    for (const dialect of ["postgres", "sqlite"] as const) {
      const sets = [projection.migrations(dialect)];
      expect(lintSchema(sets, registry)).toEqual([]);
      expect(
        declaredTables(sets).map(({ table, tableClass }) => [
          table,
          tableClass,
        ]),
      ).toEqual([
        [WATCHER_QUEUE_OUTPUTS_TABLE, "D-t"],
        [WATCHER_QUEUE_UNIT_HISTORY_TABLE, "D-t"],
        [WATCHER_QUEUE_CHECKPOINTS_TABLE, "D-t"],
        [WATCHER_DA_ATTESTATIONS_TABLE, "D-t"],
      ]);
    }
  });

  it("refuses the tables when they are not registered as temporal (the lint can fail)", () => {
    expect(
      lintSchema([projection.migrations("sqlite")], createTemporalRegistry([])),
    ).not.toEqual([]);
  });

  it("uses no clock, randomness or network in the projection's sources", async () => {
    const files = await Promise.all(
      PROJECTION_SOURCES.map(async (path) => ({
        path,
        source: await readFile(
          new URL(`../../${path}`, import.meta.url),
          "utf8",
        ),
      })),
    );
    expect(lintDeterminism(files)).toEqual([]);
    expect(
      lintDeterminism([
        { path: "probe.ts", source: "export const now = () => Date.now();" },
      ]),
    ).not.toEqual([]);
  });
});

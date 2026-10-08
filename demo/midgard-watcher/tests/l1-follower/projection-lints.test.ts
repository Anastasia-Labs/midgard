import { fileURLToPath } from "node:url";

import { createTemporalRegistry } from "@al-ft/midgard-l1-follower";
import {
  declaredTables,
  lintDeterminism,
  lintDeterminismModules,
  lintSchema,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

import { WATCHER_FOLLOWER_GENERATION_TABLE } from "../../src/l1-follower/follower-generation.js";
import {
  WATCHER_DA_ATTESTATIONS_TABLE,
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROTOCOL_INIT_FAULTS_TABLE,
  WATCHER_QUEUE_CHECKPOINTS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_CARRIERS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
  watcherProjection,
} from "../../src/l1-follower/projection.js";
import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_PRUNED_HEADERS_TABLE,
  WATCHER_PRUNED_UNITS_TABLE,
  WATCHER_TX_INPUTS_TABLE,
} from "../../src/l1-follower/tables.js";
import { SIM_WATCHER_DEPLOYMENT } from "../support/l1-follower-state-queue-traffic.js";

const projection = watcherProjection(SIM_WATCHER_DEPLOYMENT);

/**
 * Every follower module of the watcher and every module it imports, except
 * the processes and the sources that reach the node (the runtime, the
 * deployment wiring, the fault-proof L1 source and the ledger queries).
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("../..", import.meta.url)),
  include: ["src/l1-follower/*.ts"],
  exclude: [
    "src/l1-follower/deployment-follower.ts",
    "src/l1-follower/fault-proof-l1-source.ts",
    "src/l1-follower/fault-proof-l1-source.signed.ts",
    "src/l1-follower/follower-runtime.ts",
    "src/l1-follower/raw-reads.ledger.ts",
  ],
  allow: [
    {
      path: "src/indexers/authenticated-state-queue-observation.parse-persisted-observation.ts",
      rule: "environment",
      text: "process.env",
      reason:
        "the guard of the test-only replay admission, which no projection calls",
    },
    {
      path: "src/l1-follower/tx-inputs.ts",
      rule: "clock",
      text: "setTimeout",
      reason:
        "the ingest resolver's retry timer; the raw reads call only resolveStoredInputIn",
    },
    // Pre-existing default-locale sorts of the durable store's records.
    // Choosing their locale is a per-site owner call (the eslint baseline's
    // locale-compare-explicit-locale entries), not a lint cleanup.
    ...(
      [
        [
          "src/storage/durable-store.assert-references.ts",
          "left.namespace.localeCompare(right.namespace)",
        ],
        [
          "src/storage/durable-store.assert-references.ts",
          "left.key.localeCompare(right.key)",
        ],
        [
          "src/storage/durable-store.journal-watcher-protocol-utxo-transition.ts",
          "keyOf(left).localeCompare(keyOf(right))",
        ],
      ] as const
    ).map(([path, text]) => ({
      path,
      rule: "implicit_locale" as const,
      text,
      reason:
        "the durable store's record order; its locale is an owner call, untriaged",
    })),
  ],
});

/**
 * The projection's sources and the old decoders it reuses, as the lint once
 * named them by hand: it must still reach each.
 */
const PROJECTION_SOURCES = [
  "src/l1-follower/projection.ts",
  "src/l1-follower/view.ts",
  "src/l1-follower/checkpoints.ts",
  "src/l1-follower/checkpoints.read.ts",
  "src/l1-follower/projection.schema.ts",
  "src/l1-follower/projection.followed-units.ts",
  "src/l1-follower/projection.da-attestations.ts",
  "src/l1-follower/projection.departed-headers.ts",
  "src/l1-follower/projection.protocol-init.ts",
  "src/l1-follower/proof-retention.schema.ts",
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
  it("declares eight class D-t tables, registered as temporal, the proof-retention tables, the pruned-key records and the decision driver's handled generation, on both dialects", () => {
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
        [WATCHER_PROTOCOL_INIT_FAULTS_TABLE, "D-t"],
        [WATCHER_DEPARTED_HEADERS_TABLE, "D-t"],
        [WATCHER_UNIT_CARRIERS_TABLE, "D-t"],
        [WATCHER_UNIT_HISTORY_TABLE, "D-t"],
        [WATCHER_PROOF_PINS_TABLE, "B"],
        [WATCHER_PROOF_PIN_UNITS_TABLE, "B"],
        [WATCHER_TX_INPUTS_TABLE, "C"],
        [WATCHER_PRUNED_UNITS_TABLE, "B"],
        [WATCHER_PRUNED_HEADERS_TABLE, "B"],
        [WATCHER_FOLLOWER_GENERATION_TABLE, "B"],
      ]);
    }
  });

  it("refuses the tables when they are not registered as temporal (the lint can fail)", () => {
    expect(
      lintSchema([projection.migrations("sqlite")], createTemporalRegistry([])),
    ).not.toEqual([]);
  });

  it("uses no clock, randomness, network or host read in the projection's sources and their imports", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(PROJECTION_SOURCES) as unknown,
    );
    expect(
      lintDeterminism([
        { path: "probe.ts", source: "export const now = () => Date.now();" },
      ]),
    ).not.toEqual([]);
  });
});

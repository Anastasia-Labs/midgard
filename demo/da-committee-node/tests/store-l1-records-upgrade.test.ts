import { readFile } from "node:fs/promises";

import { Client } from "pg";
import { describe, expect, it } from "vitest";

import { startupReason } from "../src/startup.js";
import type { CommitteeStoreOpenChecks } from "../src/store/postgres.open-checks.js";
import type { CommitteeRetirementBinding } from "../src/store/retirement-model.js";
import {
  closeTestCommitteeStore,
  healthyL1SourceState,
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";

/**
 * A committee store the BASE build (99392ff72, the C1 merge) wrote, opened
 * by this build. The fixture is a dump of a store that build opened and
 * wrote through its own APIs (source state, signature, outbox effects,
 * capacity evidence, retirement floor), plus two outbox rows a pre-C1 build
 * left under the removed external-provider source mode. Every row a case
 * changes before the open is written into the restored BASE schema, never
 * into this build's.
 */

const FIXTURE = new URL(
  "./fixtures/committee-store-base-99392ff72.sql",
  import.meta.url,
);

/** The slot every point the BASE rows store is at. */
const BASE_POINT_SLOT = 100;

type Row = Readonly<Record<string, unknown>>;

const withClient = async <T>(
  url: string,
  run: (client: Client) => Promise<T>,
): Promise<T> => {
  const client = new Client({ connectionString: url });
  await client.connect();
  try {
    return await run(client);
  } finally {
    await client.end();
  }
};

/** A fresh database holding the BASE store, before this build opens it. */
const baseStore = async () => {
  const database = await testStoreDatabase();
  const dump = await readFile(FIXTURE, "utf8");
  await withClient(database.url, (client) => client.query(dump));
  return database;
};

const records = (url: string, table: string, key: string) =>
  withClient(url, async (client) => {
    const rows = await client.query<{ readonly key: string; record: Row }>(
      `SELECT ${key} AS key, record FROM ${table} ORDER BY ${key}`,
    );
    return Object.fromEntries(rows.rows.map((r) => [r.key, r.record]));
  });

const outbox = (url: string) =>
  records(url, "committee_decision_outbox", "effect_id");

/** The BASE floor's binding under this build's source-authority digest. */
const reboundBinding = async (
  url: string,
  change: Partial<CommitteeRetirementBinding> = {},
): Promise<CommitteeRetirementBinding> => {
  const floor = await records(url, "committee_retirement_metadata", "id");
  return {
    ...(floor["1"]!.binding as CommitteeRetirementBinding),
    sourceAuthoritySha256: "cd".repeat(32),
    ...change,
  };
};

const refusal = (opening: Promise<unknown>) =>
  opening.then(
    () => undefined,
    (error: unknown) => error,
  );

/** The pre-C1 pending and failed rows of the fixture, by status. */
const externalRows = (rows: Readonly<Record<string, Row>>) =>
  Object.fromEntries(
    Object.entries(rows)
      .filter(([, r]) => (r.headerHash as string).startsWith("a"))
      .map(([id, r]) => [r.status as string, { id, record: r }]),
  );

describe("a committee store the BASE build wrote, opened by this build", () => {
  it("rewrites only pending external-provider effects, keeps every other BASE row, and re-binds the retirement floor idempotently", async () => {
    const database = await baseStore();
    const before = await outbox(database.url);
    const signatures = await records(
      database.url,
      "committee_da_signatures",
      "header_hash",
    );
    const evidence = await records(
      database.url,
      "committee_promise_capacity_evidence",
      "evidence_key",
    );
    const sourceState = await records(
      database.url,
      "committee_l1_source_state",
      "id",
    );
    const binding = await reboundBinding(database.url);
    const checks: CommitteeStoreOpenChecks = {
      retirementBinding: binding,
      l1Origin: { slot: BASE_POINT_SLOT },
    };
    const store = await openTestCommitteeStore(database, {
      openChecks: checks,
    });
    const floor = (await store.getRetirementFloor())!;
    expect(floor.binding).toEqual(binding);
    expect(floor.generation).toBe(1);
    await expect(store.getL1SourceState()).resolves.toEqual(sourceState["1"]);
    await closeTestCommitteeStore(store);

    const after = await outbox(database.url);
    const old = externalRows(before),
      upgraded = externalRows(after);
    // The pending pre-C1 effect now runs under the local node; its
    // execution-time checks gate it as they gate any pending effect.
    expect(upgraded.pending!.record).toEqual({
      ...old.pending!.record,
      sourceMode: "local_node",
    });
    // The terminal one keeps its source mode (the schema upgrade strips the
    // quarantine fields every record lost with C1).
    const { quarantineReason, quarantinedAt, ...terminal } = old.failed!.record;
    void quarantineReason;
    void quarantinedAt;
    expect(upgraded.failed!.record).toEqual(terminal);
    expect(upgraded.failed!.record.sourceMode).toBe("external_providers");
    // Every row the BASE build wrote itself is kept as it was.
    for (const [id, record] of Object.entries(before))
      if (!(record.headerHash as string).startsWith("a"))
        expect(after[id]).toEqual(record);
    await expect(
      records(database.url, "committee_da_signatures", "header_hash"),
    ).resolves.toEqual(signatures);
    await expect(
      records(
        database.url,
        "committee_promise_capacity_evidence",
        "evidence_key",
      ),
    ).resolves.toEqual(evidence);

    // A second open changes nothing.
    const rawFloor = await records(
      database.url,
      "committee_retirement_metadata",
      "id",
    );
    const again = await openTestCommitteeStore(database, {
      openChecks: checks,
    });
    await expect(again.getRetirementFloor()).resolves.toEqual(floor);
    await closeTestCommitteeStore(again);
    await expect(
      records(database.url, "committee_retirement_metadata", "id"),
    ).resolves.toEqual(rawFloor);
    await expect(outbox(database.url)).resolves.toEqual(after);
  });

  it("refuses a retirement floor bound to another actor, naming the field, and keeps the floor", async () => {
    const database = await baseStore();
    const stored = await records(
      database.url,
      "committee_retirement_metadata",
      "id",
    );
    const refused = await refusal(
      openTestCommitteeStore(database, {
        openChecks: {
          retirementBinding: await reboundBinding(database.url, {
            actorId: "77".repeat(28),
          }),
        },
      }),
    );
    expect(refused).toBeInstanceOf(Error);
    // The startup retry reports it on /readyz and tries again.
    expect(startupReason(refused)).toBe(
      "starting:committee_retirement_binding_changed: the stored retirement floor is bound to another actorId",
    );
    await expect(
      records(database.url, "committee_retirement_metadata", "id"),
    ).resolves.toEqual(stored);
  });

  it("refuses a stored point older than L1_ORIGIN, naming it for /readyz, and opens at the origin itself", async () => {
    const database = await baseStore();
    const refused = await refusal(
      openTestCommitteeStore(database, {
        openChecks: { l1Origin: { slot: BASE_POINT_SLOT + 1 } },
      }),
    );
    expect(startupReason(refused)).toMatch(
      new RegExp(
        `^starting:committee_store_point_before_l1_origin: the .+ names slot ${BASE_POINT_SLOT.toString()}, before L1_ORIGIN slot ${(BASE_POINT_SLOT + 1).toString()}; L1_ORIGIN must be the deployment's init point$`,
        "u",
      ),
    );
    const store = await openTestCommitteeStore(database, {
      openChecks: { l1Origin: { slot: BASE_POINT_SLOT } },
    });
    await expect(store.getL1SourceState()).resolves.toBeDefined();
  });

  it("refuses the open, changing nothing, for a pending effect that does not parse after the rewrite", async () => {
    const database = await baseStore();
    const before = await outbox(database.url);
    const { pending } = externalRows(before);
    await withClient(database.url, (client) =>
      client.query(
        "UPDATE committee_decision_outbox SET record = jsonb_set(record, '{attemptCount}', '0') WHERE effect_id = $1",
        [pending!.id],
      ),
    );
    const broken = await outbox(database.url);
    const refused = await refusal(openTestCommitteeStore(database));
    expect(startupReason(refused)).toBe(
      `starting:committee_store_record_unreadable: 1 pending decision outbox record(s) do not parse (first ${pending!.id}); resolve or delete them`,
    );
    // The rewrite rolled back with the refusal.
    const { quarantineReason, quarantinedAt, ...stripped } =
      broken[externalRows(broken).failed!.id]!;
    void quarantineReason;
    void quarantinedAt;
    await expect(outbox(database.url)).resolves.toEqual({
      ...broken,
      [externalRows(broken).failed!.id]: stripped,
    });
  });

  it("demotes a terminal header record whose exit is not deeper than k to its landed status, not final, so the tick derives the exit again", async () => {
    const K = 4;
    const database = await baseStore();
    const headers = () =>
      records(database.url, "committee_state_queue_headers", "header_hash");
    const [baseHash, base] = Object.entries(await headers())[0]!;
    // Terminal rows an older build froze at a shallow depth, beside BASE's
    // own row (removed, 5000 deep).
    const terminalAt = (
      byte: string,
      status: string,
      depth: number | undefined,
      changes: Row = {},
    ) => {
      const { depth: _depth, ...point } = base.observedChainPoint as Row;
      void _depth;
      return {
        ...base,
        ...changes,
        headerHash: byte.repeat(28),
        status,
        observedChainPoint: depth === undefined ? point : { ...point, depth },
      };
    };
    const shallow = {
      atK: terminalAt("c1", "merged", K),
      attested: terminalAt("c2", "removed", 1, {
        daAttestation: { Attested: { bitmap: "01" } },
      }),
      conflicted: terminalAt("c3", "merged", 2, {
        validationErrors: ["linked_list_key_mismatch"],
      }),
      noDepth: terminalAt("c4", "removed", undefined),
    };
    const final = terminalAt("c5", "merged", K + 1);
    await withClient(database.url, async (client) => {
      for (const record of [...Object.values(shallow), final])
        await client.query(
          "INSERT INTO committee_state_queue_headers (header_hash, record) VALUES ($1, $2::jsonb)",
          [record.headerHash, JSON.stringify(record)],
        );
    });
    const store = await openTestCommitteeStore(database, {
      openChecks: { securityParameter: K },
    });
    const demoted = (record: Row, status: string) => ({
      ...record,
      status,
      finalized: false,
      observedChainPoint: {
        ...(record.observedChainPoint as Row),
        finalized: false,
      },
    });
    // The tick reads them again: its exits are asked of the follower.
    const unsettled = await store.listUnsettledStateQueueHeaders();
    await closeTestCommitteeStore(store);
    expect(unsettled.map(({ headerHash }) => headerHash)).toEqual(
      Object.values(shallow).map(({ headerHash }) => headerHash),
    );
    const after = await headers();
    expect(Object.values(shallow).map((r) => after[r.headerHash])).toEqual([
      demoted(shallow.atK, "unattested"),
      demoted(shallow.attested, "attested"),
      demoted(shallow.conflicted, "conflicted"),
      demoted(shallow.noDepth, "unattested"),
    ]);
    // Final exits are kept as they were.
    expect(after[baseHash]).toEqual(base);
    expect(after[final.headerHash]).toEqual(final);
    // A second open changes nothing.
    const again = await openTestCommitteeStore(database, {
      openChecks: { securityParameter: K },
    });
    await closeTestCommitteeStore(again);
    await expect(headers()).resolves.toEqual(after);
  });

  it("leaves terminal header records as they are when the open is not given k", async () => {
    const database = await baseStore();
    const before = await records(
      database.url,
      "committee_state_queue_headers",
      "header_hash",
    );
    const store = await openTestCommitteeStore(database);
    await closeTestCommitteeStore(store);
    await expect(
      records(database.url, "committee_state_queue_headers", "header_hash"),
    ).resolves.toEqual(before);
  });

  const OBSERVATION = {
    headerHash: "12".repeat(28),
    stateQueueOutRef: `${"34".repeat(32)}#0`,
    stateQueueStatus: "attested",
    slot: 190,
    blockHash: "56".repeat(32),
    finalized: true,
    hasPersistedDecision: true,
  };
  /** L1 source-state rows a pre-C1 build persisted. */
  const PRE_C1_SOURCE_STATES = {
    "a healthy row carrying the replay anchor": {
      ...healthyL1SourceState,
      observations: [OBSERVATION],
      stateQueueReplayAnchor: {
        deploymentIdentityDigest: "78".repeat(32),
        stateQueuePolicyId: "9a".repeat(28),
        queue: [{ headerHash: null, outRef: `${"bc".repeat(32)}#0` }],
        blockNo: "120",
        transactionIndex: "0",
      },
    },
    "a quarantined row": {
      ...healthyL1SourceState,
      status: "quarantined",
      quarantineReason: "l1_rollback_beyond_finality",
      quarantinedAt: "2026-10-06T00:00:00.000Z",
    },
    "an external-provider row": {
      ...healthyL1SourceState,
      sourceMode: "external_providers",
    },
    "a row with an unknown observation": {
      ...healthyL1SourceState,
      observations: [
        {
          ...OBSERVATION,
          stateQueueStatus: "unknown",
          lastKnownStatus: "attested",
          authenticatedSteps: [
            {
              fromOutRef: OBSERVATION.stateQueueOutRef,
              slot: 190,
              blockHash: OBSERVATION.blockHash,
            },
          ],
        },
      ],
    },
  } as const;

  it.each(Object.entries(PRE_C1_SOURCE_STATES))(
    "deletes %s, so the next healthy tick writes the source state afresh",
    async (_name, sourceState) => {
      const database = await baseStore();
      await withClient(database.url, (client) =>
        client.query(
          "UPDATE committee_l1_source_state SET record = $1::jsonb WHERE id = 1",
          [JSON.stringify(sourceState)],
        ),
      );
      const store = await openTestCommitteeStore(database);
      await expect(store.getL1SourceState()).resolves.toBeUndefined();
      await closeTestCommitteeStore(store);
      const again = await openTestCommitteeStore(database);
      await expect(again.getL1SourceState()).resolves.toBeUndefined();
    },
  );
});

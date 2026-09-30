import { createHash } from "node:crypto";
import { inspect } from "node:util";

import { compareCanonicalJsonKeys } from "@al-ft/midgard-core/canonical-json";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { inspectStateQueueCorrectionRewindObligation } from "../src/services/state-queue-correction-rewind.js";
import {
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";

export const C = Pending.Columns;

export const REWIND_DOMAIN = "midgard-history-correction-rewind-intent-v1";

export const INTEGRITY_FAILURE = "State-queue correction integrity failure";

export type Scenario = Awaited<ReturnType<typeof openCorrectionRewindScenario>>;

/** Every message on a refusal's cause chain. */
export const failureText = async (promise: Promise<unknown>) => {
  const caught = await promise.then(
    () => undefined,
    (error: unknown) => error,
  );
  if (caught === undefined) throw new Error("Expected a refusal");
  const seen = new Set<unknown>();
  const texts: string[] = [inspect(caught, { depth: 40 })];
  const walk = (value: unknown, depth: number) => {
    if (depth > 40 || value === null || typeof value !== "object") return;
    if (seen.has(value)) return;
    seen.add(value);
    if (value instanceof Error) texts.push(value.message);
    for (const key of Reflect.ownKeys(value))
      walk((value as Record<PropertyKey, unknown>)[key], depth + 1);
  };
  walk(caught, 0);
  return texts.join("\n");
};

export const nativeRoot = async (handle: Pick<Scenario["h"], "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The DA terminal outcomes recorded for `headerHash`. */
export const readDaTerminalOutcomes = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<Record<string, unknown>>`
        SELECT * FROM da_payload_terminal_outcomes
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}
        ORDER BY transaction_hash`;
    }),
  );

export const inspectObligation = (scenario: Scenario) =>
  read(inspectStateQueueCorrectionRewindObligation(scenario.authority));

/** Direct journal surgery: the adversarial or broken local state a proof
 * must refuse. Returns the previous values so the test can restore them. */
export const journalUpdate = (
  headerHash: string,
  fields: Readonly<Record<string, unknown>>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const key = Buffer.from(headerHash, "hex");
    const rows = yield* sql<Record<string, unknown>>`
      SELECT * FROM pending_block_finalizations
      WHERE header_hash = ${key}`;
    expect(rows).toHaveLength(1);
    const before = Object.fromEntries(
      Object.keys(fields).map((column) => [column, rows[0]![column]]),
    );
    const updated = yield* sql`UPDATE pending_block_finalizations
      SET ${sql.update(fields as Record<string, never>)}
      WHERE header_hash = ${key} RETURNING header_hash`;
    expect(updated).toHaveLength(1);
    return before;
  });

export const updateJournal = (
  headerHash: string,
  fields: Readonly<Record<string, unknown>>,
) => read(journalUpdate(headerHash, fields));

/**
 * Commits `write` and inspects the obligation it leaves in the same
 * transaction. While a refusal keeps the gate closed, the running owner
 * retries the blocked recovery on its own timer and acts on the first
 * committed state in which the obligation proves. Surgery that moves from one
 * refusal to another is therefore one write, and a restore that makes the
 * obligation prove is inspected exactly as the owner can first see it: the
 * owner may start the rewind as soon as this commits.
 */
export const writeAndInspect = (
  scenario: Scenario,
  write: Effect.Effect<unknown, unknown, SqlClient.SqlClient>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql.withTransaction(
        write.pipe(
          Effect.zipRight(
            inspectStateQueueCorrectionRewindObligation(scenario.authority),
          ),
        ),
      );
    }),
  );

export const readLeaseStatus = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ status: string }>`
        SELECT status FROM state_queue_mutation_leases WHERE token = ${token}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.status;
    }),
  );

/**
 * A commit process killed after handing its block to L1 never releases its
 * state-queue mutation lease: it stays active, and blocks every other
 * state-queue mutation, until it expires.
 */
export const holdLeaseAsCrashed = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'active', released_at = NULL,
          expires_at = NOW() + INTERVAL '10 minutes'
        WHERE token = ${token} RETURNING token`;
      expect(updated).toHaveLength(1);
    }),
  );

/** Retire an injected crash lease the test left active, so a failure never
 * blocks the shard's next state-queue mutation until the lease expires. */
export const retireCrashedLease = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'released', released_at = NOW()
        WHERE token = ${token} AND status = 'active'`;
    }),
  );

/** The canonical key order every observer and transition digest hashes. */
const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};

const canonicalDigest = (value: unknown) =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

type DigestedRecord = Readonly<Record<string, unknown>> & {
  readonly transitionDigest: string;
};

const redigest = (record: Readonly<Record<string, unknown>>) => {
  const { transitionDigest: _prior, ...body } = record;
  return { ...body, transitionDigest: canonicalDigest(body) };
};

/**
 * An internally consistent observer state bound to `deployment`: every
 * transition and the state itself re-digested exactly as their producers do,
 * so only the deployment binding tells it apart from the canonical one.
 */
export const rebindObserverDeployment = (
  state: Readonly<Record<string, unknown>>,
  deployment: string,
) => {
  const rebind = (transition: DigestedRecord) => {
    const nested = transition.correctionTransition as DigestedRecord | null;
    return redigest({
      ...transition,
      deploymentIdentityDigest: deployment,
      correctionTransition:
        nested === null
          ? null
          : redigest({ ...nested, deploymentIdentityDigest: deployment }),
    });
  };
  const { stateDigest: _prior, ...body } = state;
  const rebound = {
    ...body,
    deploymentIdentityDigest: deployment,
    pending: (state.pending as DigestedRecord[]).map(rebind),
    admitted: (state.admitted as DigestedRecord[]).map(rebind),
  };
  return { ...rebound, stateDigest: canonicalDigest(rebound) };
};

export const writeObserverState = (state: Readonly<{ stateDigest: string }>) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_terminal_observer_states
        SET state_digest = ${Buffer.from(state.stateDigest, "hex")},
          state_record = ${JSON.stringify(state)}
        RETURNING deployment_identity_digest`;
      expect(updated).toHaveLength(1);
    }),
  );

type ObserverRecord = Readonly<Record<string, unknown>> & {
  readonly cursorQueue: readonly unknown[];
  readonly admitted: readonly DigestedRecord[];
};

const observerRecord = (raw: unknown) =>
  (typeof raw === "string" ? JSON.parse(raw) : raw) as ObserverRecord;

const redigestState = (state: Readonly<Record<string, unknown>>) => {
  const { stateDigest: _prior, ...body } = state;
  return { ...body, stateDigest: canonicalDigest(body) };
};

/** The persisted observer state with one more node on its cursor queue,
 * re-digested exactly as the observer does. */
export const withCursorQueueNode = (
  raw: unknown,
  node: Readonly<{ headerHash: string; outRef: string }>,
) => {
  const state = observerRecord(raw);
  return redigestState({ ...state, cursorQueue: [...state.cursorQueue, node] });
};

/** The persisted observer state whose admitted removal claims it did not
 * consume `outRef`, every digest recomputed. */
export const withoutConsumedOutRef = (raw: unknown, outRef: string) => {
  const state = observerRecord(raw);
  const strip = (transition: DigestedRecord) => {
    const nested = transition.correctionTransition as DigestedRecord | null;
    const consumed = (transition.consumedQueueOutRefs as string[]).filter(
      (entry) => entry !== outRef,
    );
    return redigest({
      ...transition,
      consumedQueueOutRefs: consumed,
      correctionTransition:
        nested === null
          ? null
          : redigest({ ...nested, consumedQueueOutRefs: consumed }),
    });
  };
  return redigestState({ ...state, admitted: state.admitted.map(strip) });
};

export class RolledBack<A> {
  constructor(readonly value: A) {}
}

/** Inspects the obligation while `headerHash`'s journal records `submitted`,
 * a submission other than its retained intent. The schema refuses to record
 * one, so the refusal is lifted in one transaction that is always rolled back:
 * the proof's own check is exercised and nothing outlives the inspection. */
export const inspectWithForeignSubmission = (
  scenario: Scenario,
  headerHash: string,
  submitted: Buffer,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const outcome = yield* sql
        .withTransaction(
          Effect.gen(function* () {
            yield* sql.unsafe(`ALTER TABLE pending_block_finalizations
              DROP CONSTRAINT pending_signed_ack_matches_intent`);
            const updated = yield* sql`UPDATE pending_block_finalizations
              SET ${sql.update({ [C.SUBMITTED_TX_HASH]: submitted })}
              WHERE header_hash = ${Buffer.from(headerHash, "hex")}
              RETURNING header_hash`;
            expect(updated).toHaveLength(1);
            const obligation =
              yield* inspectStateQueueCorrectionRewindObligation(
                scenario.authority,
              );
            return yield* Effect.fail(new RolledBack(obligation));
          }),
        )
        .pipe(Effect.flip);
      if (!(outcome instanceof RolledBack)) return yield* Effect.fail(outcome);
      return outcome.value;
    }),
  );

/** The durable state a stopped runtime leaves: everything but the native
 * owner's root, which only a running generation can report. */
export const captureStoredState = async (scenario: Scenario) => ({
  sql: (await readSqlLedgerRoot()).root_hex,
  deposits: await readDeposits(),
  journals: await Promise.all(
    scenario.headers.map(async (header) => {
      const journal = await readJournal(header);
      return {
        status: journal[C.STATUS],
        digest: journal[C.CORRECTION_TRANSITION_DIGEST] ?? null,
      };
    }),
  ),
});

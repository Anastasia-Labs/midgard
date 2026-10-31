import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import type { Database } from "midgard-node/services/database";

import type { DbEvidence } from "../e2e/summary.js";
import type { StackJournal } from "../full-stack/journal.js";
import {
  payoutConclusion,
  type SettlementObservation,
} from "../full-stack/payout-body.js";

type SettlementKind = "deposit" | "withdrawal";
export type StackSettlementTarget = {
  readonly cycle: number;
  readonly kind: SettlementKind;
  readonly eventId: string;
};
export type StackDatabaseObservation = {
  /** The row storage.ts writes once per stack database; null when absent. */
  readonly marker: {
    readonly runId: string;
    readonly manifestId: string;
  } | null;
  /** Keyed by `${kind}:${eventId}`. */
  readonly settlements: ReadonlyMap<string, SettlementObservation>;
};

const SOURCE = "postgres";
const settlementKey = (kind: SettlementKind, eventId: string) =>
  `${kind}:${eventId}`;

const stepData = (journal: StackJournal, id: string) => {
  const data = journal.steps[id]?.data;
  return typeof data === "object" && data !== null && !Array.isArray(data)
    ? (data as Record<string, unknown>)
    : {};
};

/** The settlement events the journal's confirmed journey names. */
export function stackSettlementTargets(
  journal: StackJournal | undefined,
  cycles: number,
): StackSettlementTarget[] {
  if (journal === undefined) return [];
  const targets: StackSettlementTarget[] = [];
  for (let cycle = 0; cycle < cycles; cycle++)
    for (const kind of ["deposit", "withdrawal"] as const) {
      const eventId = stepData(journal, `cycle-${cycle}-${kind}`).eventId;
      if (typeof eventId === "string" && /^[0-9a-f]+$/.test(eventId))
        targets.push({ cycle, kind, eventId });
    }
  return targets;
}

/** The stack database's identity row and the settlement rows payout.ts reads. */
export const collectStackDatabase = (
  manifestId: string,
  targets: readonly StackSettlementTarget[],
): Effect.Effect<StackDatabaseObservation, never, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [table] = yield* sql<{ readonly exists: boolean }>`
      SELECT to_regclass('public.full_stack_controller_identity') IS NOT NULL AS exists
    `;
    const [row] = table?.exists
      ? yield* sql<{ readonly runId: string; readonly manifestId: string }>`
          SELECT run_id AS "runId", manifest_id AS "manifestId"
            FROM full_stack_controller_identity
        `
      : [];
    const settlements = new Map<string, SettlementObservation>();
    for (const { kind, eventId } of targets) {
      const jobs = yield* sql<{ readonly phase: string }>`
        SELECT phase FROM settlement_jobs
          WHERE deployment_id = ${manifestId} AND kind = ${kind} AND event_id = ${eventId}
      `;
      const attempts = yield* sql<{
        readonly phase: string;
        readonly status: string;
        readonly txHash: string;
        readonly signedCbor: string;
      }>`
        SELECT phase, status, tx_hash AS "txHash", signed_cbor AS "signedCbor"
          FROM settlement_attempts
          WHERE deployment_id = ${manifestId} AND kind = ${kind} AND event_id = ${eventId}
      `;
      settlements.set(settlementKey(kind, eventId), {
        jobs: jobs.map(({ phase }) => ({ phase })),
        attempts: attempts.map((attempt) => ({ ...attempt })),
      });
    }
    return {
      marker:
        row === undefined
          ? null
          : { runId: row.runId, manifestId: row.manifestId },
      settlements,
    };
  }).pipe(Effect.orDie);

function settlementFailure(
  journal: StackJournal,
  observation: StackDatabaseObservation,
  cycle: number,
): string | undefined {
  const deposit = stepData(journal, `cycle-${cycle}-deposit`).eventId;
  const jobs =
    typeof deposit === "string"
      ? observation.settlements.get(settlementKey("deposit", deposit))?.jobs
      : undefined;
  // journey.ts deposit step: exactly one job for the event, and it is complete.
  if (jobs?.length !== 1 || jobs[0]!.phase !== "complete")
    return "deposit settlement job is not complete";
  const payout = stepData(journal, `cycle-${cycle}-withdrawal`);
  const withdrawal =
    typeof payout.eventId === "string"
      ? observation.settlements.get(settlementKey("withdrawal", payout.eventId))
      : undefined;
  if (withdrawal === undefined) return "withdrawal settlement is missing";
  let conclusion: ReturnType<typeof payoutConclusion>;
  try {
    conclusion = payoutConclusion(withdrawal);
  } catch (error) {
    return error instanceof Error ? error.message : String(error);
  }
  if (conclusion === undefined)
    return "withdrawal settlement has no confirmed payout of a complete job";
  const recorded = payout.observation as
    | { readonly signedTransactionCborHex?: unknown }
    | undefined;
  if (
    conclusion.txHash !== payout.txHash ||
    conclusion.signedCbor !== recorded?.signedTransactionCborHex
  )
    return "confirmed payout differs from the journal's payout";
  return undefined;
}

/**
 * The database the finalizer reads is this run's (storage.ts identity row),
 * and its settlement rows still hold one complete deposit job and exactly one
 * confirmed payout per cycle (payout-body.ts `payoutConclusion`).
 */
export function stackDatabaseGates({
  journal,
  cycles,
  manifestId,
  observation,
}: {
  readonly journal: StackJournal | undefined;
  readonly cycles: number;
  readonly manifestId: string;
  readonly observation: StackDatabaseObservation;
}): readonly DbEvidence[] {
  const marker = observation.marker;
  const identityMatches =
    journal !== undefined &&
    marker?.runId === journal.runId &&
    marker.manifestId === manifestId;
  const outcomes = Array.from({ length: cycles }, (_, cycle) =>
    journal === undefined
      ? "the stack journal did not verify"
      : settlementFailure(journal, observation, cycle),
  );
  return [
    {
      label: "stack_storage_identity",
      status: identityMatches ? "satisfied" : "failed",
      source: SOURCE,
      details: {
        runId: marker?.runId ?? "",
        manifestId: marker?.manifestId ?? "",
        ...(identityMatches
          ? {}
          : {
              reason:
                marker === null
                  ? "the database has no stack identity"
                  : "Storage identity differs from this deployment",
            }),
      },
    },
    {
      label: "stack_settlement",
      status: outcomes.every((failure) => failure === undefined)
        ? "satisfied"
        : "failed",
      source: SOURCE,
      details: Object.fromEntries(
        outcomes.map((failure, cycle) => [
          `cycle-${cycle}`,
          failure ?? "verified",
        ]),
      ),
    },
  ];
}

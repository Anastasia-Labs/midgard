/**
 * One look at everything a defending operator answers (every
 * unmerged landed block the node processed, its own and the ones it built
 * on): the cases on those blocks, our thread of each, and our defense
 * material for each case it is needed for.
 *
 * - The targets are the processed landed blocks after the confirmed
 *   frontier: a case on any other header (merged, foreign to our chain, or
 *   never landed) is not ours to answer and is left out.
 * - Material is built for a case we have not defended yet and for a case
 *   whose thread of ours is live (its reveals need it); a deterministic
 *   outcome is kept in `cache` by case, and an unavailable one is retried
 *   at the next survey.
 */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { recordedValidationTraceDefenders } from "@al-ft/midgard-fault-proofs";
import { postgresDialect } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Data, Effect } from "effect";

import { followerSqlTx } from "../database/follower-schema.js";
import {
  landedChain,
  type LandedLedger,
  landedLedger,
} from "../landed-blocks/ledger.js";
import { type LandedBlockRow, retrieveRows } from "../landed-blocks/store.js";
import {
  type DiscoveredCase,
  type OurThread,
  type OurThreadFacts,
  readDefenseFactsIn,
  type ValidationTraceDefenseChain,
} from "./discovery.js";
import {
  type ValidationTraceDefenseOutcome,
  validationTraceDefenseOutcome,
} from "./material.js";

/** A case on one of our targets, with our standing in it. */
export type DefenseCase = DiscoveredCase &
  Readonly<{
    target: LandedBlockRow;
    /** Our thread of the case. */
    thread: OurThreadFacts;
    /**
     * Whether the case's history names us a defender; undefined when the
     * history is incomplete.
     */
    defended: boolean | undefined;
    /** Every defender the case's history names, when it is complete. */
    priorDefenders: readonly string[] | undefined;
    /** Our material, when the case needs it. */
    outcome: ValidationTraceDefenseOutcome | undefined;
  }>;

export type DefenseSurvey = Readonly<{
  point: Readonly<{ slot: number; hash: string }>;
  cases: readonly DefenseCase[];
  /** Our live threads whose case output is gone. */
  orphanThreads: readonly OurThread[];
}>;

/** The follower or the node database could not be read; nothing is decided. */
export class DefenseSurveyFailed extends Data.TaggedError(
  "DefenseSurveyFailed",
)<{ readonly message: string; readonly cause: unknown }> {}

/** Deterministic material by case unit, kept across surveys. */
export type DefenseMaterialCache = Map<string, ValidationTraceDefenseOutcome>;

const needsMaterial = (
  thread: OurThreadFacts,
  defended: boolean | undefined,
): boolean =>
  thread.kind === "live"
    ? thread.thread.at === "game"
    : thread.kind === "none" && defended !== true;

/** The survey now. */
export const surveyValidationTraceDefense = (
  input: Readonly<{
    chain: ValidationTraceDefenseChain;
    ourVkh: string;
    l1SlotConfig: L1SlotConfig;
    cache: DefenseMaterialCache;
  }>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const facts = yield* sql.withTransaction(
      Effect.flatMap(followerSqlTx, (tx) =>
        Effect.tryPromise(() =>
          readDefenseFactsIn(tx, postgresDialect, {
            chain: input.chain,
            ourVkh: input.ourVkh,
          }),
        ),
      ),
    );
    const rows = yield* retrieveRows;
    const chain = yield* landedChain(rows);
    const targets = new Map(
      (chain?.chain ?? []).map((row) => [row.headerHash, row] as const),
    );
    const threads = new Map(
      facts.threads.map((thread) => [thread.unit, thread] as const),
    );
    let landed: LandedLedger | undefined;
    const cases: DefenseCase[] = [];
    const caseIds = new Set<string>();
    for (const found of facts.cases) {
      caseIds.add(found.caseId);
      const target = targets.get(found.datum.block_header_hash);
      if (target === undefined) continue;
      const thread = [...threads.values()].find(
        ({ caseId }) => caseId === found.caseId,
      )?.facts ?? { kind: "none" as const };
      const priorDefenders =
        found.history === undefined
          ? undefined
          : recordedValidationTraceDefenders({
              transactions: found.history,
              caseUnit: found.caseUnit,
            });
      const defended = priorDefenders?.includes(input.ourVkh);
      let outcome = input.cache.get(found.caseUnit);
      if (outcome === undefined && needsMaterial(thread, defended)) {
        landed ??= yield* landedLedger(rows);
        outcome =
          landed === undefined
            ? {
                kind: "unavailable",
                detail: "the node holds no landed ledger",
              }
            : yield* validationTraceDefenseOutcome({
                target,
                landed,
                caseDatum: found.datum,
                point: facts.point,
                l1SlotConfig: input.l1SlotConfig,
              });
        if (outcome.kind !== "unavailable")
          input.cache.set(found.caseUnit, outcome);
      }
      cases.push({
        ...found,
        target,
        thread,
        defended,
        priorDefenders,
        outcome,
      });
    }
    return {
      point: facts.point,
      cases,
      orphanThreads: facts.threads.filter(
        ({ caseId, facts: standing }) =>
          !caseIds.has(caseId) && standing.kind === "live",
      ),
    } satisfies DefenseSurvey;
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DefenseSurveyFailed({
          message: "the validation-trace defense survey could not be read",
          cause,
        }),
    ),
  );

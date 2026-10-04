import { SqlClient } from "@effect/sql";
import { Context, Effect, Option } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { DatabaseError } from "../database/utils/common.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
  requireCandidateHistory,
  withHistoryWrite,
} from "./event-history-producer.js";
import { HistoryPreparation } from "./event-history-recovery.js";

/** A binding descriptor, not a SQL authority capability. The actual Ready or
 * recovery owner is independently checked on every use. */
export type ForeignVerificationSourceBinding = Readonly<{
  kind: "ready" | "recovery";
  binding: HistoryProducerPermit;
}>;
export const ForeignVerificationSource =
  Context.GenericTag<ForeignVerificationSourceBinding>(
    "midgard/ForeignVerificationSource",
  );
const failure = (cause: unknown) =>
  new DatabaseError({
    table: Authority.tableName,
    message: "Foreign verification source capability changed",
    cause,
  });
const sameToken = (a: Authority.Token, b: Authority.Token) =>
  a.deploymentIdentity === b.deploymentIdentity &&
  a.ownerToken === b.ownerToken &&
  a.generation === b.generation;
const sameCoverage = (a: HistoryOwnerCoverage, b: HistoryOwnerCoverage) =>
  a.bindingDigest === b.bindingDigest &&
  a.checkpointRevision === b.checkpointRevision &&
  a.point.id === b.point.id &&
  a.point.slot === b.point.slot &&
  a.snapshotDigest === b.snapshotDigest &&
  a.includedThroughMs === b.includedThroughMs;

export const assertForeignVerificationSource = (
  source: ForeignVerificationSourceBinding,
) =>
  withHistoryWrite(
    Effect.gen(function* () {
      if (source.kind === "ready") {
        yield* requireCandidateHistory;
        const permit = yield* Effect.serviceOption(HistoryProducer);
        if (
          Option.isNone(permit) ||
          !sameToken(permit.value.token, source.binding.token) ||
          !sameCoverage(permit.value.coverage, source.binding.coverage)
        )
          return yield* Effect.fail(
            failure("Ready permit differs from foreign acceptance"),
          );
        return;
      }
      const preparation = yield* Effect.serviceOption(HistoryPreparation);
      const token = yield* Authority.requireRecoveryTransaction;
      if (
        Option.isNone(preparation) ||
        !sameToken(preparation.value.token, source.binding.token) ||
        !sameToken(token, source.binding.token)
      )
        return yield* Effect.fail(
          failure("Recovery source owner differs from foreign acceptance"),
        );
      yield* preparation.value.assertCurrent;
      const sql = yield* SqlClient.SqlClient;
      const coverage = source.binding.coverage;
      const rows =
        yield* sql`SELECT 1 FROM event_history_cursor WHERE binding_digest = ${Buffer.from(coverage.bindingDigest, "hex")} AND manifest_id = ${Buffer.from(token.deploymentIdentity, "hex")}
    AND revision = ${coverage.checkpointRevision}::bigint AND head_hash = ${Buffer.from(coverage.point.id, "hex")} AND head_slot = ${coverage.point.slot} AND snapshot_digest = ${Buffer.from(coverage.snapshotDigest, "hex")}`;
      if (rows.length !== 1)
        return yield* Effect.fail(
          failure("Recovery source prefix differs from exact checkpoint"),
        );
      yield* preparation.value.assertCurrent;
    }),
  );

/** Recovery coverage is supplied by the existing source owner's preparation
 * callback. Never infer it from an imported DB, a DA body or a stale permit. */
export const currentForeignVerificationSource = (
  recoveryCoverage?: HistoryOwnerCoverage,
) =>
  Effect.gen(function* () {
    const preparation = yield* Effect.serviceOption(HistoryPreparation);
    if (Option.isSome(preparation)) {
      if (recoveryCoverage === undefined)
        return yield* Effect.fail(
          failure("Exact source-owned recovery checkpoint is required"),
        );
      const source: ForeignVerificationSourceBinding = {
        kind: "recovery",
        binding: { token: preparation.value.token, coverage: recoveryCoverage },
      };
      yield* assertForeignVerificationSource(source);
      return source;
    }
    const permit = yield* Effect.serviceOption(HistoryProducer);
    if (Option.isNone(permit))
      return yield* Effect.fail(
        failure("Current Ready or source recovery owner is required"),
      );
    const source: ForeignVerificationSourceBinding = {
      kind: "ready",
      binding: permit.value,
    };
    yield* assertForeignVerificationSource(source);
    return source;
  });

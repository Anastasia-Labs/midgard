import { Effect, Option } from "effect";

import { AdmissionWriter } from "../../services/index.js";

/**
 * The admission writer's part of `/readyz`: its pending-set facts, and the
 * `admission_writer_stalled` reason while Postgres is not taking its writes
 * (then `/submit` refuses until it does). Both are null without a writer.
 */
export const admissionWriterReadiness = Effect.gen(function* () {
  const writer = yield* Effect.serviceOption(AdmissionWriter);
  if (Option.isNone(writer)) return { reason: null, facts: null };
  const health = yield* writer.value.health;
  return {
    reason: health.stalled
      ? `admission_writer_stalled:${health.oldestPendingAgeMs.toString()}:${health.stallMs.toString()}`
      : null,
    facts: {
      pending: health.pending,
      oldestPendingAgeMs: health.oldestPendingAgeMs,
      stallMs: health.stallMs,
      stalled: health.stalled,
    },
  };
});

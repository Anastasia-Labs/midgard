import { Effect, Ref } from "effect";

import * as HistoryAuthority from "../src/database/eventHistoryAuthority.js";
import { beginCommitForeignVerification } from "../src/fibers/block-commitment.prepare-foreign-base.js";
import { applyForeignBaseVerificationOutcome } from "../src/services/foreign-base-verification.js";
import type { Globals } from "../src/services/globals.js";

/**
 * What a serving node holds once a commitment tick has checked its canonical
 * base: a Ready history authority, and foreign-base evidence for exactly that
 * generation. `/readyz` refuses readiness without it
 * (`foreign_base_verification_unobserved`), so a route fixture that models a
 * healthy node seeds it, and each test then isolates the reason it is about.
 *
 * The base here is the empty state queue (no tail header), which the commit
 * worker reports as `not_required`; the evidence goes through the same
 * begin/apply transitions the commitment fiber uses. Call it after the fixture
 * has cleared `event_history_authority`.
 */
export const seedVerifiedForeignBase = (globals: Globals) =>
  Effect.gen(function* () {
    const token = yield* HistoryAuthority.acquire({
      deploymentIdentity: "5a".repeat(32),
      ownerToken: "5a5a5a5a-5a5a-4a5a-8a5a-5a5a5a5a5a5a",
      leaseDurationMs: 600_000,
    });
    yield* HistoryAuthority.publishReady(token, {
      point: { id: "5b".repeat(32), slot: 1 },
      snapshotDigest: "5c".repeat(32),
    });
    const scope = yield* beginCommitForeignVerification(globals, {
      ...token,
      baseHeaderHash: null,
    });
    const state = yield* Ref.updateAndGet(
      globals.FOREIGN_BASE_VERIFICATION,
      (current) =>
        applyForeignBaseVerificationOutcome(current, scope, {
          status: "not_required",
          baseHeaderHash: null,
        }),
    );
    if (state.status !== "verified")
      return yield* Effect.die(
        new Error(
          `readiness fixture could not verify its foreign base: ${state.status}`,
        ),
      );
  });

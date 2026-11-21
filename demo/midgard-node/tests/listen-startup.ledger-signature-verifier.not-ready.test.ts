/**
 * The node's startup wait for the ledger signature verifier is bounded
 * (`listen-startup.await-ledger-signature-verifier.ts`). Here libsodium
 * never initialises: the wait fails by step and reason, exhausted, after
 * its attempts, instead of holding startup forever.
 */
import { Effect, Exit } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  awaitLedgerSignatureVerifierOnStartup,
  LEDGER_SIGNATURE_VERIFIER_STEP,
  LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
} from "../src/commands/listen-startup.await-ledger-signature-verifier.js";
import { findStartupStepFailure } from "../src/services/startup-waiting.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? new Promise<never>(() => undefined)
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

describe("the startup wait for a ledger signature verifier that never initialises", () => {
  it("fails exhausted after its attempts", async () => {
    const exit = await Effect.runPromiseExit(
      awaitLedgerSignatureVerifierOnStartup({
        attemptTimeout: "50 millis",
        maxAttempts: 3,
        retryDelay: "10 millis",
      }),
    );

    expect(Exit.isFailure(exit)).toBe(true);
    expect(
      Exit.isFailure(exit) ? findStartupStepFailure(exit.cause) : undefined,
    ).toMatchObject({
      step: LEDGER_SIGNATURE_VERIFIER_STEP,
      reason: LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
      exhausted: true,
      attempts: 3,
    });
  });
});

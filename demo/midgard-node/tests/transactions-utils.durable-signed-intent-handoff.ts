import "./transactions-utils.sign-submit-wrapper-recovery-options.js";

import { Deferred, Effect, Fiber } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  BeforeSignedTransactionSubmission,
  submitSignedTxWithRecovery,
} from "../src/transactions/utils.js";
import {
  runWithoutFollower,
  TEST_INTENT,
  withoutFollowerJournal,
} from "./helpers/intent-journal.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

describe("durable signed intent handoff", () => {
  it("awaits durable intent before invoking the provider with exact bytes", async () => {
    const entered = await Effect.runPromise(Deferred.make<void>());
    const committed = await Effect.runPromise(Deferred.make<void>());
    const cbor = signedTxCbor({});
    const calls: string[] = [];
    const submitProgram = vi.fn(() =>
      Effect.sync(() => {
        calls.push("provider");
      }),
    );
    const fiber = Effect.runFork(
      withoutFollowerJournal(
        submitSignedTxWithRecovery(
          {} as never,
          { toCBOR: () => cbor, submitProgram } as never,
          "intent-hash",
          TEST_INTENT,
        ).pipe(
          Effect.provideService(BeforeSignedTransactionSubmission, {
            persist: (intent) =>
              Effect.gen(function* () {
                expect(intent).toMatchObject({
                  txHash: "intent-hash",
                  signedTxCbor: cbor,
                });
                calls.push("persist-start");
                yield* Deferred.succeed(entered, undefined);
                yield* Deferred.await(committed);
                calls.push("persist-committed");
              }),
          }),
        ),
      ),
    );
    await Effect.runPromise(Deferred.await(entered));
    expect(submitProgram).not.toHaveBeenCalled();
    await Effect.runPromise(Deferred.succeed(committed, undefined));
    await Effect.runPromise(Fiber.join(fiber));
    expect(calls).toEqual(["persist-start", "persist-committed", "provider"]);
  });
  it("does not invoke the provider when durable intent persistence fails", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    await expect(
      runWithoutFollower(
        submitSignedTxWithRecovery(
          {} as never,
          { toCBOR: () => signedTxCbor({}), submitProgram } as never,
          "intent-hash",
          TEST_INTENT,
        ).pipe(
          Effect.provideService(BeforeSignedTransactionSubmission, {
            persist: () => Effect.fail(new Error("stale owner")),
          }),
        ),
      ),
    ).rejects.toThrow("stale owner");
    expect(submitProgram).not.toHaveBeenCalled();
  });
});

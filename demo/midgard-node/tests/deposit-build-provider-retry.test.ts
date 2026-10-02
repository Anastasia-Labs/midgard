import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Logger } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

const build = vi.hoisted(() => ({
  calls: 0,
  failures: [] as unknown[],
}));

// The builder itself is exercised against the emulator in
// deposit-flow-emulator-submission.test.ts; here only its failures matter.
vi.mock("../src/transactions/submit-deposit.js", async (importOriginal) => {
  const { Effect: EffectModule } = await import("effect");
  return {
    ...(await importOriginal<Record<string, unknown>>()),
    parseBuildDepositRequest: (body: { readonly valid?: boolean }) => {
      if (body.valid !== true) throw new Error("depositAmount is required");
      return { request: "parsed" };
    },
    buildUnsignedDepositTxFromFundingContextProgram: () =>
      EffectModule.suspend(() => {
        build.calls += 1;
        const failure = build.failures.shift();
        return failure === undefined
          ? EffectModule.succeed({ unsignedTxCbor: "84a0" })
          : EffectModule.fail(failure);
      }),
  };
});

vi.mock("../src/transactions/reference-scripts.js", async () => {
  const { Effect: EffectModule } = await import("effect");
  return {
    fetchReferenceScriptUtxosProgram: () => EffectModule.succeed([]),
    referenceScriptByName: () => ({}),
    referenceScriptTargetsByCommand: () => ({ deposit: [] }),
  };
});

import { postDepositBuildHandler } from "../src/commands/listen-router.post-deposit-build-handler.js";
import { Lucid, MidgardContracts } from "../src/services/index.js";
import { SubmitDepositError } from "../src/transactions/submit-deposit.deposit-submission-attempt-from-completed-tx.js";

const post = (body: unknown) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const logs: string[] = [];
      const response = (yield* postDepositBuildHandler.pipe(
        Effect.provideService(
          HttpServerRequest.HttpServerRequest,
          HttpServerRequest.fromWeb(
            new Request("http://midgard.test/deposit/build", {
              method: "POST",
              body: JSON.stringify(body),
              headers: { "content-type": "application/json" },
            }),
          ),
        ),
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ message }) => {
              logs.push(
                (Array.isArray(message) ? message : [message]).join(" "),
              );
            }),
          ),
        ),
      )) as HttpServerResponse.HttpServerResponse;
      const web = HttpServerResponse.toWeb(response);
      return {
        status: web.status,
        body: (yield* Effect.promise(() => web.json())) as Record<
          string,
          unknown
        >,
        logs,
      };
    }).pipe(
      Effect.provideService(Lucid, {
        api: { config: () => ({ network: "Custom" }) },
        referenceScriptsAddress: "addr_test1referencescripts",
      } as unknown as Lucid),
      Effect.provideService(MidgardContracts, {
        referenceScriptAuth: undefined,
      } as unknown as MidgardContracts),
    ),
  );

const transient = () =>
  new SubmitDepositError({
    message: "Failed to build the deposit transaction",
    cause: new Error("fetch failed"),
  });

describe("POST /deposit/build under a transient provider failure", () => {
  beforeEach(() => {
    build.calls = 0;
    build.failures = [];
  });

  it("retries a transient failure and builds exactly once it clears", async () => {
    build.failures = [transient(), transient()];
    const result = await post({ valid: true });
    expect(result.status).toBe(200);
    expect(result.body).toEqual({ unsignedTxCbor: "84a0" });
    expect(build.calls).toBe(3);
    expect(
      result.logs.filter((line) => line.includes("retryable provider error")),
    ).toHaveLength(2);
    expect(
      result.logs.some((line) => line.includes("succeeded after 3 attempt(s)")),
    ).toBe(true);
  });

  it("answers a failure no retry can clear at once", async () => {
    build.failures = [
      new SubmitDepositError({
        message: "Insufficient funding inputs for the deposit",
        cause: new Error("InsufficientFunds"),
      }),
    ];
    const result = await post({ valid: true });
    expect(result.status).toBe(500);
    expect(build.calls).toBe(1);
  });

  it("stops retrying a provider that stays down", async () => {
    build.failures = [transient(), transient(), transient(), transient()];
    const result = await post({ valid: true });
    expect(result.status).toBe(500);
    expect(build.calls).toBe(4);
  });

  it("refuses an invalid request without building", async () => {
    const result = await post({ valid: false });
    expect(result.status).toBe(400);
    expect(result.body).toEqual({ error: "depositAmount is required" });
    expect(build.calls).toBe(0);
  });
});

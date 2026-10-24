import { SqlClient } from "@effect/sql";
import { Effect, Logger, LogLevel } from "effect";
import { describe, expect, it, vi } from "vitest";

// The queue never lists the pending header and is already past its block
// end, so the worker reaches its signed-intent deferral.
vi.mock(
  "../src/workers/utils/confirm-block-commitments.js",
  async (importOriginal) => {
    const { Effect: EffectModule, Option: OptionModule } = await import(
      "effect"
    );
    return {
      ...(await importOriginal<Record<string, unknown>>()),
      fetchSortedCommittedStateQueueBlocks: () =>
        EffectModule.succeed(["tail"]),
      latestCommittedStateQueueBlockFromSorted: () =>
        EffectModule.succeed("tail"),
      resolveStateQueueBlockEndTimeMs: () =>
        EffectModule.succeed(Number.MAX_SAFE_INTEGER),
      findCommittedStateQueueBlockByHeaderHash: () =>
        EffectModule.succeed(OptionModule.none()),
      shouldRunFullStateQueueConfirmationScan: () => true,
    };
  },
);

import { NodeConfig } from "../src/services/config.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { runConfirmBlockCommitmentsWorkerProgram } from "../src/workers/confirm-block-commitments.js";
import type { WorkerInput } from "../src/workers/utils/confirm-block-commitments.js";

const WARNING_AGE_MS = 60_000;

const defer = (ageMs: number) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const logs: { level: string; message: string }[] = [];
      const output = yield* runConfirmBlockCommitmentsWorkerProgram({
        data: {
          firstRun: false,
          pendingBlock: {
            expectedHeaderHash: "aa".repeat(28),
            submittedTxHash: "",
            intendedTxHash: "12".repeat(32),
            blockEndTimeMs: Date.now() - 10 * 60_000,
            updatedAtMs: Date.now() - ageMs,
          },
        },
      } as unknown as WorkerInput).pipe(
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ logLevel, message }) => {
              logs.push({
                level: logLevel.label,
                message: (Array.isArray(message) ? message : [message]).join(
                  " ",
                ),
              });
            }),
          ),
        ),
        Logger.withMinimumLogLevel(LogLevel.Debug),
      );
      return {
        output,
        deferrals: logs.filter((line) =>
          line.message.includes("deferring to the landed-block rebase"),
        ),
      };
    }).pipe(
      Effect.provideService(Lucid, {
        api: { awaitTxConfirmation: () => Promise.reject(new Error("none")) },
      } as unknown as Lucid),
      Effect.provideService(MidgardContracts, {
        stateQueue: {},
      } as unknown as MidgardContracts),
      // The landed-queue reads are mocked above; the database is never read.
      Effect.provideService(
        SqlClient.SqlClient,
        {} as unknown as SqlClient.SqlClient,
      ),
      Effect.provideService(NodeConfig, {
        BLOCK_CONFIRMATION_AWAIT_TIMEOUT_MS: 1_000,
        UNCONFIRMED_BLOCK_MAX_AGE_MS: WARNING_AGE_MS,
      } as unknown as NodeConfig["Type"]),
    ),
  );

describe("confirmation worker over an unresolved signed commit intent", () => {
  it("defers with its age, as info inside the warning age", async () => {
    const result = await defer(1_000);
    expect(result.output).toEqual({ type: "NoTxForConfirmationOutput" });
    expect(result.deferrals).toHaveLength(1);
    expect(result.deferrals[0]!.level).toBe("INFO");
    expect(result.deferrals[0]!.message).toMatch(
      /\(age_ms=\d+, warning_age_ms=60000\)\.$/u,
    );
  });

  it("defers the same way past the warning age, reported as a warning", async () => {
    const result = await defer(2 * WARNING_AGE_MS);
    // Still only a deferral: the journal disposition stays the one decision.
    expect(result.output).toEqual({ type: "NoTxForConfirmationOutput" });
    expect(result.deferrals).toHaveLength(1);
    expect(result.deferrals[0]!.level).toBe("WARN");
    expect(result.deferrals[0]!.message).toContain("(age_ms=1200");
  });
});

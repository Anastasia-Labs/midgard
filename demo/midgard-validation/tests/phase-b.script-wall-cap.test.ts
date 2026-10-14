import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  commitValidationTraceV1,
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
  type PhaseBConfig,
  runPhaseBValidationWithPatch,
  ScriptWallCapExceededError,
  ValidationTraceStopped,
} from "../src/index.js";
import {
  MIDGARD_TX_EX_UNITS_CAP,
  plutusV3SpendInput,
} from "./helpers/plutus-spend-input.js";
import {
  batchPreState,
  countdownProgram,
  plainSpend,
  plutusCountdownSpend,
  type WallCapCandidate,
} from "./phase-b.script-wall-cap.fixture.js";

/**
 * The per-transaction script wall cap is the node's local resource limit:
 * a transaction whose script runs pass it fails Phase B with a
 * `ScriptWallCapExceededError` naming it, and is never rejected with a
 * script verdict. Without a cap the same transactions are accepted.
 */

// Tens of thousands of CEK steps: far longer than a 1 ms cap.
const SLOW_ITERATIONS = 2_000;
// Commit mode re-runs the script observed; a shorter run still passes the
// recorder's wall-clock check interval many times over.
const COMMIT_ITERATIONS = 400;

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
};

const runBatch = (
  entries: readonly WallCapCandidate[],
  overrides: Partial<PhaseBConfig> = {},
) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch(
        entries.map((entry) => entry.candidate),
        batchPreState(entries),
        { ...config, ...overrides },
      ),
    ),
  );

const txIdHex = (entry: WallCapCandidate) =>
  entry.candidate.ledgerTx.txId.toString("hex");

describe("Phase B script wall cap", () => {
  const slow = plutusCountdownSpend(0x61, SLOW_ITERATIONS);
  const plain = plainSpend(0x62);

  it("accepts the batch without a cap", async () => {
    const result = await runBatch([plain, slow]);
    expect(Either.isRight(result)).toBe(true);
    if (Either.isLeft(result)) return;
    expect(result.right.rejected).toStrictEqual([]);
    expect(
      result.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([txIdHex(plain), txIdHex(slow)]);
  });

  it("fails the batch naming the transaction past the cap", async () => {
    const result = await runBatch([plain, slow], { scriptWallCapMs: 1 });
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ScriptWallCapExceededError);
    const breach = result.left as ScriptWallCapExceededError;
    expect(breach.txId.toString("hex")).toBe(txIdHex(slow));
    expect(breach.wallCapMs).toBe(1);
  });

  it("keeps the cap when the script runs on another evaluator", async () => {
    const requests: MidgardCekVerdictRequest[] = [];
    const result = await runBatch([slow], {
      scriptWallCapMs: 1,
      evaluateCekVerdict: (request) =>
        Effect.sync(() => {
          requests.push(request);
          return evaluateMidgardCekVerdict(request);
        }),
    });
    expect(requests).toHaveLength(1);
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ScriptWallCapExceededError);
  });

  it("leaves a generous cap's verdicts unchanged", async () => {
    const capped = await runBatch([plain, slow], { scriptWallCapMs: 600_000 });
    const uncapped = await runBatch([plain, slow]);
    expect(Either.isRight(capped)).toBe(true);
    expect(Either.isRight(uncapped)).toBe(true);
    if (Either.isLeft(capped) || Either.isLeft(uncapped)) return;
    expect(
      capped.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual(
      uncapped.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    );
    expect(capped.right.rejected).toStrictEqual(uncapped.right.rejected);
  });
});

describe("commit-mode trace wall cap", () => {
  it(
    "fails a commit past the cap and commits the same trace without one",
    {
      timeout: 60_000,
    },
    async () => {
      const input = await plutusV3SpendInput(
        countdownProgram(COMMIT_ITERATIONS),
        MIDGARD_TX_EX_UNITS_CAP,
      );
      const capped = await Effect.runPromise(
        Effect.either(commitValidationTraceV1(input, { wallCapMs: 1 })),
      );
      expect(Either.isLeft(capped)).toBe(true);
      if (Either.isRight(capped)) return;
      expect(capped.left).toBeInstanceOf(ValidationTraceStopped);
      const stopped = capped.left as ValidationTraceStopped;
      expect(stopped.reason).toBe("unavailable");
      expect(stopped.cause).toBeInstanceOf(ScriptWallCapExceededError);

      const generous = await Effect.runPromise(
        commitValidationTraceV1(input, { wallCapMs: 600_000 }),
      );
      const uncapped = await Effect.runPromise(commitValidationTraceV1(input));
      expect(uncapped.descriptor.verdict).toBe("accepted");
      expect(generous.descriptor).toStrictEqual(uncapped.descriptor);
    },
  );
});

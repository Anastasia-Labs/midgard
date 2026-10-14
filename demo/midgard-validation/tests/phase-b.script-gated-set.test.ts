import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekVerdict,
  MIDGARD_CEK_GATED_SET_V1,
  type MidgardCekGatedSet,
  type MidgardCekVerdictRequest,
  type PhaseBConfig,
  runPhaseBValidationWithPatch,
  ScriptOutsideGatedSetError,
} from "../src/index.js";
import {
  batchPreState,
  plainSpend,
  plutusCountdownSpend,
  type WallCapCandidate,
} from "./phase-b.script-wall-cap.fixture.js";

/**
 * The gated set is a local screen: a transaction whose script runs reach a
 * builtin or machine arm outside it fails Phase B with a
 * `ScriptOutsideGatedSetError` naming it, and is never rejected with a
 * script verdict. Without a set the same transactions are accepted.
 */

const ITERATIONS = 20;
const ADD_INTEGER = 0;

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

const withoutAddInteger: MidgardCekGatedSet = {
  ...MIDGARD_CEK_GATED_SET_V1,
  builtins: MIDGARD_CEK_GATED_SET_V1.builtins.filter(
    (tag) => tag !== ADD_INTEGER,
  ),
};

const withoutLambdas: MidgardCekGatedSet = {
  ...MIDGARD_CEK_GATED_SET_V1,
  arms: MIDGARD_CEK_GATED_SET_V1.arms.filter((arm) => arm !== "computeLambda"),
};

describe("Phase B gated set", () => {
  const script = plutusCountdownSpend(0x71, ITERATIONS);
  const plain = plainSpend(0x72);

  it("accepts the batch under gated set v1, as without a set", async () => {
    const gated = await runBatch([plain, script], {
      cekGatedSet: MIDGARD_CEK_GATED_SET_V1,
    });
    const ungated = await runBatch([plain, script]);
    expect(Either.isRight(gated)).toBe(true);
    expect(Either.isRight(ungated)).toBe(true);
    if (Either.isLeft(gated) || Either.isLeft(ungated)) return;
    expect(gated.right.rejected).toStrictEqual([]);
    expect(
      gated.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([txIdHex(plain), txIdHex(script)]);
    expect(
      gated.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual(
      ungated.right.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    );
  });

  it.each([
    ["a builtin", withoutAddInteger],
    ["a machine arm", withoutLambdas],
  ])(
    "fails the batch naming the transaction that reaches %s outside the set",
    async (_, cekGatedSet) => {
      const result = await runBatch([plain, script], { cekGatedSet });
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left).toBeInstanceOf(ScriptOutsideGatedSetError);
      const breach = result.left as ScriptOutsideGatedSetError;
      expect(breach.txId.toString("hex")).toBe(txIdHex(script));
    },
  );

  it("accepts a script-free batch under any set", async () => {
    const result = await runBatch([plain], {
      cekGatedSet: { version: 0, builtins: [], arms: [] },
    });
    expect(Either.isRight(result)).toBe(true);
    if (Either.isLeft(result)) return;
    expect(result.right.rejected).toStrictEqual([]);
  });

  it("keeps the set when the script runs on another evaluator", async () => {
    const requests: MidgardCekVerdictRequest[] = [];
    const result = await runBatch([script], {
      cekGatedSet: withoutAddInteger,
      evaluateCekVerdict: (request) =>
        Effect.sync(() => {
          const copy = structuredClone(request);
          requests.push(copy);
          return evaluateMidgardCekVerdict(copy);
        }),
    });
    expect(requests).toHaveLength(1);
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ScriptOutsideGatedSetError);
  });
});

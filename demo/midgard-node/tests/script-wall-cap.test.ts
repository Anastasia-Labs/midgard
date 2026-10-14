import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  batchPreState,
  plainSpend,
  plutusCountdownSpend,
  type WallCapCandidate,
} from "../../midgard-validation/tests/phase-b.script-wall-cap.fixture.js";
import {
  ADMISSION_REJECT_CODE_SCRIPT_WALL_CAP,
  decideAdmissionBatch,
} from "../src/fibers/tx-queue-processor.classify-plutus-evaluation-failure.js";
import {
  evaluateEvictingUncommittable,
  makeCommitStageRejectionBreaker,
} from "../src/mpf/commit-rejection.evict-uncommittable-normal-txs.js";
import { COMMIT_REJECT_CODE_TRACE_UNAVAILABLE } from "../src/mpf/commit-rejection.js";
import { evaluateNormalBlockCandidates } from "../src/mpf/process.evaluate-normal-block-candidates.js";

/**
 * The script wall caps are local limits. At admission, a transaction whose
 * scripts pass the submit cap is refused under the node's own code and the
 * rest of the batch is decided without it. At commit, one past the commit
 * cap is evicted through the commit-stage rejection path, and the block is
 * built from the rest.
 */

// Tens of thousands of CEK steps: far longer than a 1 ms cap.
const SLOW_ITERATIONS = 2_000;

const hex = (entry: WallCapCandidate) =>
  entry.candidate.ledgerTx.txId.toString("hex");

const run = <A, E>(effect: Effect.Effect<A, E>) =>
  Effect.runPromise(
    effect.pipe(Effect.provide(Logger.remove(Logger.defaultLogger))),
  );

describe("submit wall cap", () => {
  const slow = plutusCountdownSpend(0x81, SLOW_ITERATIONS);
  const plain = plainSpend(0x82);
  const decide = (scriptWallCapMs?: number) =>
    run(
      decideAdmissionBatch({
        phaseA: {
          accepted: [slow.candidate, plain.candidate],
          rejected: [],
        },
        pendingWithdrawalOutRefHexes: new Set(),
        ledgerState: batchPreState([slow, plain]),
        phaseBConfig: {
          nowCardanoSlotNo: 100n,
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          ...(scriptWallCapMs === undefined ? {} : { scriptWallCapMs }),
        },
      }),
    );

  it("admits both transactions without a cap", async () => {
    const { phaseB, allRejected } = await decide();
    expect(allRejected).toStrictEqual([]);
    expect(
      phaseB.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")).sort(),
    ).toStrictEqual([hex(slow), hex(plain)].sort());
  });

  it("refuses the transaction past the cap and admits the rest", async () => {
    const { phaseB, allRejected } = await decide(1);
    expect(
      allRejected.map((rejection) => [
        rejection.txId.toString("hex"),
        rejection.code,
      ]),
    ).toStrictEqual([[hex(slow), ADMISSION_REJECT_CODE_SCRIPT_WALL_CAP]]);
    expect(
      phaseB.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([hex(plain)]);
  });
});

describe("commit wall cap", () => {
  const slow = plutusCountdownSpend(0x91, SLOW_ITERATIONS);
  const plain = plainSpend(0x92);
  const build = (scriptWallCapMs?: number) =>
    run(
      evaluateEvictingUncommittable({
        candidates: [plain.candidate, slow.candidate],
        stage: "Phase B",
        breaker: makeCommitStageRejectionBreaker(),
        evaluate: (candidates) =>
          evaluateNormalBlockCandidates({
            candidates,
            state: batchPreState([plain, slow]),
            blockSlot: 100n,
            l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
            bucketConcurrency: 1,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            scriptWallCapMs,
          }),
      }),
    );

  it("commits both transactions without a cap", async () => {
    const built = await build();
    expect(built.rejected).toStrictEqual([]);
    expect(
      built.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([hex(plain), hex(slow)]);
  });

  it("evicts the transaction past the cap and commits the rest", async () => {
    const built = await build(1);
    expect(
      built.rejected.map((rejection) => [
        Buffer.from(rejection.txId).toString("hex"),
        rejection.code,
      ]),
    ).toStrictEqual([[hex(slow), COMMIT_REJECT_CODE_TRACE_UNAVAILABLE]]);
    expect(
      built.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([hex(plain)]);
  });
});

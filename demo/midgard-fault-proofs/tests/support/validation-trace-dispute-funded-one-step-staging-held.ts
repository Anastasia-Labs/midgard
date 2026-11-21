/** The rows of our staging outputs under the reconciliation-only permit once
 * the challenged header has rolled back, over the scene of
 * validation-trace-dispute-funded-one-step-staging.ts with each transaction
 * final only `RELEASE_DEPTH` blocks under the tip. Reconciliation-only
 * authority refuses the thread's next move: while the thread stands, each
 * run holds for a fresh submission, past the case's expiry as before it,
 * leaving each output and the thread where they are. Once the defender
 * closes the case, each output comes back to our wallet before the case's
 * ending, a reclaim that authority admits by its kind; the ending waits on
 * no spend back's finality: once a spend back rolls back, its outputs are
 * live again and the next runs spend them back again. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
  type FraudProofWorkflowRunResult,
} from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { landWith } from "./validation-trace-dispute-defender-moves.js";
import { defenderWonCaseClose } from "./validation-trace-dispute-defender-won-close.js";
import type { fundedStagingScene } from "./validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./validation-trace-dispute-funded-one-step-turns.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  expectSelectedForSweep,
  expectSpentBackAgainAfterRollback,
} from "./validation-trace-dispute-staging-left-for-sweep.js";

/** How many blocks under the tip a transaction is final, once the header
 * has rolled back. */
export const RELEASE_DEPTH = 4;

type HeldScene = ReturnType<typeof fundedStagingScene>;

export const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

/** Whether `result` holds for a fresh submission: the refusal of the
 * thread's next move under reconciliation-only authority. */
export const isFreshSubmissionHold = (result: FraudProofWorkflowRunResult) =>
  result.kind === "pending" &&
  result.reason === FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED;

/** Our staging outputs on the ledger now, by out-ref. */
const liveOf = async ({ stagedOutputs }: HeldScene) =>
  (await stagedOutputs()).map(outRefOf).sort();

/**
 * Runs `held` `turns` times: each run holds for a fresh submission and
 * signs nothing, each output of `staged` stays live, the thread stands at
 * `holder` when one is given, and a proof-materials view selects each
 * output for a sweep.
 */
const expectFreshHolds = async (
  held: HeldScene,
  staged: readonly string[],
  holder: string | undefined,
  turns: number,
) => {
  const { scene } = held.at;
  const signed = scene.l1.signedCbors.size;
  const history: string[] = [];
  for (let turn = 0; turn < turns; turn += 1) {
    const result = await held.turns().runOnce();
    history.push(describeRun(result));
    expect(isFreshSubmissionHold(result), history.join("\n")).toBe(true);
  }
  expect(await liveOf(held), history.join("\n")).toStrictEqual(staged);
  expect(scene.l1.signedCbors.size, history.join("\n")).toBe(signed);
  if (holder !== undefined)
    expect((await held.place())?.holder, history.join("\n")).toBe(holder);
  await expectSelectedForSweep(scene, staged);
};

/**
 * Past the case's expiry, with nobody closing the case: each of three runs
 * holds for a fresh submission, signs nothing, and leaves each output of
 * `staged` and the thread at `holder` where they are.
 */
export const expectFreshHoldPastExpiry = async (
  held: HeldScene,
  staged: readonly string[],
  holder: string,
) => {
  const { at } = held;
  awaitLastSlotAtOrBefore(at.scene, at.caseExpiresAtMs + 20_000);
  await expectFreshHolds(held, staged, holder, 3);
};

/** Whether `result` ends the workflow as a case closed without our close. */
const isClosedWithoutOurClose = (result: FraudProofWorkflowRunResult) =>
  result.kind === "stalled" &&
  result.reason.includes(
    "validation-trace case is closed without this workflow's close",
  );

/**
 * The defender closes the case; then each output of `staged` comes back to
 * our wallet under the reconciliation-only permit before the run ends as a
 * case closed without our close. The ending waits on no spend back's
 * finality: once each spend back rolls back, the outputs are live again, and
 * the next runs spend them back again and end as before.
 */
export const expectSpentBackOnClose = async (
  held: HeldScene,
  staged: readonly string[],
) => {
  const { at, turns } = held;
  const { scene } = at;
  const { lucid } = at.side.defender;
  const outputs: readonly UTxO[] = await scene.h.proverLucid.utxosByOutRef(
    staged.map(parseOutRefLabel),
  );
  expect(outputs).toHaveLength(staged.length);
  await landWith(
    lucid,
    await defenderWonCaseClose({
      scene,
      lucid,
      caseUtxo: await liveCase({ scene, caseUnit: at.caseUnit }),
      bond: at.bond,
    }),
  );
  const history: FraudProofWorkflowRunResult[] = [];
  for (let turn = 0; turn < 30; turn += 1) {
    const result = await turns().runOnce();
    history.push(result);
    if (result.kind !== "pending") break;
  }
  const described = history.map(describeRun).join("\n");
  expect(isClosedWithoutOurClose(history.at(-1)!), described).toBe(true);
  expect(await liveOf(held), described).toStrictEqual([]);
  await expectSpentBackAgainAfterRollback({
    scene,
    outputs,
    runOnce: async () => await turns().runOnce(),
    describe: describeRun,
    ended: isClosedWithoutOurClose,
  });
};

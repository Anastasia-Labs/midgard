/** The rows of our staging outputs held under the reconciliation-only permit
 * once the challenged header has rolled back, over the scene of
 * validation-trace-dispute-funded-one-step-staging.ts with each transaction
 * final only `RELEASE_DEPTH` blocks under the tip: past the case's expiry,
 * each run reports the outputs a thread of ours holds and signs nothing;
 * once the defender closes the case, each output comes back to our wallet,
 * one transaction each, and the case's ending is taken only once each spend
 * back is final at release depth. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  VALIDATION_TRACE_STAGING_HELD_PAST_EXPIRY,
  VALIDATION_TRACE_STAGING_OUTSTANDING,
} from "../../src/validation-trace-dispute/one-step-staging-sweep.js";
import { validationTraceRunState } from "../../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { landWith } from "./validation-trace-dispute-defender-moves.js";
import { defenderWonCaseClose } from "./validation-trace-dispute-defender-won-close.js";
import type { fundedStagingScene } from "./validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./validation-trace-dispute-funded-one-step-turns.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
} from "./validation-trace-dispute-one-step-case.js";
import { landedSpenderOf } from "./validation-trace-dispute-one-step-staged-drive.js";

/** How many blocks under the tip a transaction is final, once the header
 * has rolled back. */
export const RELEASE_DEPTH = 4;

type HeldScene = ReturnType<typeof fundedStagingScene>;

export const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

const isHeldPastExpiry = (result: FraudProofWorkflowRunResult) =>
  result.kind === "pending" &&
  result.reason.startsWith(`${VALIDATION_TRACE_STAGING_HELD_PAST_EXPIRY}: `);

/** Our staging outputs on the ledger now, by out-ref. */
const liveOf = async ({ stagedOutputs }: HeldScene) =>
  (await stagedOutputs()).map(outRefOf).sort();

/**
 * Past the case's expiry, with nobody closing the case: each run reports
 * the outputs `staged` the thread at `holder` holds, their lovelace and the
 * case's expiry, under a named alarm, signs nothing, and leaves each output
 * and the thread where they are.
 */
export const expectHeldPastExpiry = async (
  held: HeldScene,
  staged: readonly string[],
  holder: string,
) => {
  const { at, turns, place, stagedOutputs } = held;
  const { scene } = at;
  awaitLastSlotAtOrBefore(scene, at.caseExpiresAtMs + 20_000);
  const lovelace = (await stagedOutputs()).reduce(
    (sum, { assets }) => sum + (assets.lovelace ?? 0n),
    0n,
  );
  const signed = scene.l1.signedCbors.size;
  const history: string[] = [];
  for (let turn = 0; turn < 3; turn += 1) {
    const result = await turns().runOnce();
    history.push(describeRun(result));
    expect(isHeldPastExpiry(result), history.join("\n")).toBe(true);
    expect(validationTraceRunState(result)).toMatchObject({
      namedState: {
        name: VALIDATION_TRACE_STAGING_HELD_PAST_EXPIRY,
        alarm: true,
      },
    });
    const { reason } = result as { readonly reason: string };
    for (const outRef of staged) expect(reason).toContain(outRef);
    expect(reason).toContain(`${lovelace.toString()} lovelace`);
    expect(reason).toContain(at.caseExpiresAtMs.toString());
  }
  expect(await liveOf(held), history.join("\n")).toStrictEqual(staged);
  expect(scene.l1.signedCbors.size, history.join("\n")).toBe(signed);
  expect((await place())?.holder, history.join("\n")).toBe(holder);
};

/**
 * The defender closes the case; then each output of `staged` comes back to
 * our wallet, one transaction each, final at release depth before the run
 * ends as a case closed without our close, and no run reports the outputs
 * held past the case's expiry any more.
 */
export const expectSpentBackOnClose = async (
  held: HeldScene,
  staged: readonly string[],
) => {
  const { at, turns } = held;
  const { scene } = at;
  const { lucid } = at.side.defender;
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
  let observedAt = 0;
  for (let turn = 0; turn < 30; turn += 1) {
    observedAt = scene.h.emulator.blockHeight;
    const result = await turns().runOnce();
    history.push(result);
    if (result.kind !== "pending") break;
  }
  const described = history.map(describeRun).join("\n");
  const ending = history.at(-1)!;
  expect(ending.kind, described).toBe("stalled");
  expect("reason" in ending && ending.reason).toContain(
    "validation-trace case is closed without this workflow's close",
  );
  expect(history.some(isHeldPastExpiry), described).toBe(false);
  expect(
    history.some(
      (result) =>
        result.kind === "pending" &&
        result.reason.startsWith(VALIDATION_TRACE_STAGING_OUTSTANDING),
    ),
    described,
  ).toBe(true);
  expect(await liveOf(held)).toStrictEqual([]);
  for (const outRef of staged) {
    const reclaim = await landedSpenderOf(
      scene.l1.signedCbors,
      scene.h.emulator,
      outRef,
    );
    expect(reclaim, outRef).toBeDefined();
    const body = CML.Transaction.from_cbor_hex(
      scene.l1.signedCbors.get(reclaim!)!,
    ).body();
    expect(body.inputs().len()).toBe(1);
    expect(body.outputs().len()).toBe(1);
    expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
      scene.h.proverSigner.address,
    );
    const status = await scene.h.emulator.getTransactionStatus(reclaim!);
    expect(status.status).toBe("confirmed");
    if (status.status !== "confirmed") return;
    expect(
      observedAt - status.confirmation.blockHeight!,
      outRef,
    ).toBeGreaterThanOrEqual(RELEASE_DEPTH);
  }
};

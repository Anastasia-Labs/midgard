/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip.
 *
 * Once both threads are past the PrepareSelected move, a runner of ours
 * over our journal as it stood before the staging finds no staging for the
 * step the threads committed: each run holds on the one-step alarm, naming
 * the staging that differs from the commitment, run after run, with no
 * thread moved and nothing staged or sent.
 *
 * With both threads closed and each staging output still live, another
 * prover removes the block against its own record, and no thread of ours
 * stands. While the provider does not show one staging output and a
 * confirmation of ours is inside the provider's grace, our run holds its
 * resolution, run after run, on that output. Once the provider shows it, our
 * run spends each staging output back to our wallet, one transaction each,
 * and ends resolved by that record only once each spend back is final at
 * release depth. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../src/validation-trace-dispute/threads.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
import { threadPlace } from "./support/validation-trace-dispute-one-step-play.js";
import {
  type OurRun,
  RELEASE_DEPTH,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

const other = stagedOtherRecord();
const { drive } = other;

const DIFFERS =
  "validation-trace staged step's staging differs from the evidence its thread committed";

/** The staging outputs, each still live once both threads closed. */
let stagings: readonly string[];
/** The record their removal was made against. */
let theirRecord: string;
/** Ends the provider's blind spot. */
let hidden: (() => void) | undefined;

/** Where both threads stand. */
const places = async () => {
  const { scene, deployment, a, b } = drive.dispute();
  return await Promise.all(
    [a.unit, b.unit].map(
      async (unit) => (await threadPlace(scene, deployment, unit))?.holder,
    ),
  );
};

/** A runner of ours over a copy of our journal as it stood before its
 * first staging. */
const beforeStaging = async () => {
  const { scene, deployment, runner, removal } = drive.dispute();
  const [first] = drive.entries();
  const entries = await runner.journal.load(first!.workflowId);
  const staged = entries.findIndex(
    ({ event }) =>
      "actionId" in event &&
      event.actionId.startsWith("validation-trace-stage:"),
  );
  expect(staged).toBeGreaterThan(0);
  const journal = new MemoryFraudProofWorkflowJournalStore();
  for (const entry of entries.slice(0, staged))
    await journal.append(entry, entry.sequence);
  return makeRunner(scene, {
    deployment,
    journal,
    signedRecovery: emulatorSignedTransactionRecovery(
      scene.h.emulator,
      RELEASE_DEPTH,
    ),
    removal: removal!.removal,
    terminalVerifier: removal!.terminalVerifier(
      scene.failing.evidence.headerHash,
    ),
  });
};

describe("validation-trace dispute runner: a staged step's staging, once another prover removes the block against its own record", () => {
  beforeAll(other.setUp, 1_800_000);

  afterAll(() => {
    hidden?.();
    other.tearDown();
  });

  it("holds each run on the one-step alarm, naming the staging that differs from the commitment, while no staging the threads committed is found", async () => {
    await drive.play(async () => !(await places()).includes("prepare"), 40);
    const at = await places();
    expect(at).toContain("core");
    const staged = await drive.stagedOnL1();
    expect(staged.length).toBeGreaterThan(1);
    const { scene } = drive.dispute();
    const ours = await beforeStaging();
    const sent = scene.l1.signedCbors.size;
    for (let run = 0; run < 3; run += 1) {
      const result = await ours.runOnce(scene.failing);
      expect(result.kind, other.describeRun(result)).toBe("pending");
      expect("reason" in result && result.reason).toMatch(
        new RegExp(`^${VALIDATION_TRACE_ONE_STEP_REQUIRED}: `),
      );
      expect("reason" in result && result.reason).toContain(DIFFERS);
    }
    expect(await places()).toStrictEqual(at);
    expect(await drive.stagedOnL1()).toStrictEqual(staged);
    expect(scene.l1.signedCbors.size).toBe(sent);
  }, 1_200_000);

  it("holds the resolution by their record, run after run, while a staging output the provider does not show may still be live inside the provider's grace", async () => {
    stagings = await other.playToClose();
    theirRecord = await other.removeByTheirs();
    const { scene } = drive.dispute();
    const unseen = stagings.at(-1)!;
    hidden = hideFromProvider(scene.h.proverLucid, unseen);
    const held = `${VALIDATION_TRACE_STAGING_OUTSTANDING}: ${unseen} is not yet visible to the provider`;
    const history: string[] = [];
    const ours = async () => {
      const result = await drive.dispute().runner.runOnce(scene.failing);
      history.push(other.describeRun(result));
      expect(result.kind, history.join("\n")).not.toBe(
        "resolved_by_another_record",
      );
      return "reason" in result ? result.reason : result.kind;
    };
    while ((await ours()) !== held)
      expect(history.length, history.join("\n")).toBeLessThan(20);
    // Each output the provider shows is spent back meanwhile.
    for (const outRef of stagings)
      if (outRef !== unseen) await other.expectSpentBack(outRef);
    for (let run = 0; run < 3; run += 1) expect(await ours()).toBe(held);
    expect(onLedger(scene, parseOwnOutRef(unseen))).toBe(true);
  }, 1_800_000);

  it("spends each staging output back to our wallet and ends resolved by their record only once each spend back is final at release depth", async () => {
    hidden?.();
    hidden = undefined;
    const runs: OurRun[] = [];
    const resolved = await other.runToResolution(stagings, runs);
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
    expect("terminal" in resolved).toBe(false);
    const { observedAt } = runs.at(-1)!;
    const intents = other.spendBackIntents(resolved.entries);
    for (const outRef of stagings) {
      const { hash, block } = await other.expectSpentBack(outRef);
      expect([...(intents.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
      expect(observedAt - block, outRef).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    }
    // Our run waited on the spend backs before it let the resolution go.
    expect(
      runs.some(
        ({ result }) =>
          result.kind === "pending" &&
          result.reason.startsWith(VALIDATION_TRACE_STAGING_OUTSTANDING),
      ),
    ).toBe(true);
  }, 1_800_000);
});

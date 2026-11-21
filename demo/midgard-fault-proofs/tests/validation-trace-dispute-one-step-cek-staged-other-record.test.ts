/** The validation-trace dispute runner at a staged builtin step (arm 31) on
 * the emulator (support/validation-trace-dispute-one-step-staged-other-record.ts),
 * each transaction final only `RELEASE_DEPTH` blocks under the tip.
 *
 * Once both threads are past the PrepareSelected move, a fresh process asks
 * first for what the process that was not stopped asks for
 * (support/validation-trace-dispute-restart-probe.ts).
 *
 * With both threads closed and each staging output still live, another
 * prover removes the block against its own record, and no thread of ours
 * stands. While the provider does not show one staging output, our run
 * spends each output it shows back to our wallet, one transaction each, and
 * ends resolved by that record, waiting on no grace for the one it does not
 * show; once the provider shows it, our next runs take it back. The ending
 * waits on no spend back's finality. Once each spend back has landed and
 * the process has seen it land, it follows none of them: once they roll
 * back, our next runs spend each output back once more, by its first spend
 * back or one new one, and end resolved by their record again. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { threadPlace } from "./support/validation-trace-dispute-one-step-play.js";
import {
  type OurRun,
  stagedOtherRecord,
} from "./support/validation-trace-dispute-one-step-staged-other-record.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";
import {
  expectSameNextAfterRestart,
  recordingAsks,
} from "./support/validation-trace-dispute-restart-probe.js";

const other = stagedOtherRecord();
const { drive } = other;
const seen: string[] = [];

/** The staging outputs, each still live once both threads closed. */
let stagings: readonly string[];
/** The staging output the provider did not show. */
let unseen: string;
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

describe("validation-trace dispute runner: a staged step's staging, once another prover removes the block against its own record", () => {
  beforeAll(async () => {
    await other.setUp();
    drive.restart({
      wrap: (adapter) => other.holding(recordingAsks(seen)(adapter)),
    });
  }, 1_800_000);

  afterAll(() => {
    hidden?.();
    other.tearDown();
  });

  it("asks first, as a fresh process, for what the process that was not stopped asks for, once both threads are past the PrepareSelected move", async () => {
    await drive.play(async () => !(await places()).includes("prepare"), 40);
    expect(await places()).toContain("core");
    expect((await drive.stagedOnL1()).length).toBeGreaterThan(1);
    const { scene } = drive.dispute();
    await expectSameNextAfterRestart({
      processOver: drive.processOver,
      runner: drive.dispute().runner,
      seen,
      block: scene.failing,
      opened: true,
    });
  }, 1_200_000);

  it("ends resolved by their record waiting on no grace while the provider does not show a staging output, spending back each one it shows, and takes the unseen one back once the provider shows it", async () => {
    stagings = await other.playToClose();
    const theirRecord = await other.removeByTheirs();
    const { scene } = drive.dispute();
    unseen = stagings.at(-1)!;
    hidden = hideFromProvider(scene.h.proverLucid, unseen);
    const runs: OurRun[] = [];
    const resolved = await other.runToResolution(stagings, runs);
    expect(resolved.resolution.referencedProofTokenOutRef).toBe(theirRecord);
    expect("terminal" in resolved).toBe(false);
    const described = runs.map(({ result }) => other.describeRun(result));
    expect(
      described.some((run) =>
        run.includes(VALIDATION_TRACE_STAGING_OUTSTANDING),
      ),
      described.join("\n"),
    ).toBe(false);
    for (const outRef of stagings)
      if (outRef !== unseen) await other.expectSpentBack(outRef);
    hidden();
    hidden = undefined;
    const after: string[] = [];
    for (
      let run = 0;
      run < 12 && onLedger(scene, parseOwnOutRef(unseen));
      run += 1
    )
      after.push(
        other.describeRun(await drive.dispute().runner.runOnce(scene.failing)),
      );
    expect(onLedger(scene, parseOwnOutRef(unseen)), after.join("\n")).toBe(
      false,
    );
  }, 1_800_000);

  it("spends each staging output back to our wallet once, and, once those spend backs roll back after the ending, spends each back once again, each by its first spend back or one new one", async () => {
    const sends = other.spendBackSends();
    for (const outRef of stagings)
      if (outRef !== unseen) {
        const { hash } = await other.expectSpentBack(outRef);
        expect([...(sends.get(outRef) ?? [])], outRef).toStrictEqual([hash]);
      }
    // A spend back the process still follows is sent again with its own
    // bytes; one it had seen land final past release depth may be renewed.
    await other.spentBackAgainAfterRollback();
  }, 1_800_000);
});

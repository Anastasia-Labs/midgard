/** The validation-trace dispute runner stopping when a defender wins a thread
 * of our case, on the emulator. The runner resolves the operator's thread in
 * the one-step resolution and closes it into our case while the successor is
 * silent. The successor then reveals, and no process of ours answers before
 * our clock on its thread runs out: the successor moves its thread to the
 * timeout validator and closes it into the case as its win, from its own
 * wallet. The runner then stops under its own named state, raising no alarm,
 * hands the session back at once, and moves nothing more. */
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DEFENDER_WON } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceChallengerDeadlineMs } from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { continuePendingWorkflow } from "../src/workflow/pending-continuation.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  awaitLastSlotAtOrBefore,
  defenderClosesItsWin,
  defenderEntersOurTimeout,
  defendTwoThreads,
  gameThread,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { playThreads } from "./support/validation-trace-dispute-one-step-play.js";
import {
  isTurn,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";
import { DEPLOYMENT_FINGERPRINT } from "./workflow.make-adapter.js";

let dispute: TwoThreadCase;
/** The successor's close of its won thread. */
let won: string;

describe("validation-trace dispute runner: a defender wins a thread of our case", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({ withRemoval: false });
    const { scene, runner, deployment, a, b, caseUnit } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, { ...b, silent: true }],
      until: async () => await threadClosed(dispute, a.unit),
      limit: 60,
    });
    expect(dispute.prepared.map(({ stage }) => stage)).toContain(
      "close_awarded",
    );
    // The successor reveals; no process of ours answers before our clock on
    // its thread runs out.
    await revealAsDefender(
      scene,
      b.defender,
      await gameThread(scene, b.unit),
      b.trace,
    );
    const revealed = await gameThread(scene, b.unit);
    expect(isTurn(threadOf(revealed), "AwaitingChallenger")).toBe(true);
    awaitLastSlotAtOrBefore(
      scene,
      validationTraceChallengerDeadlineMs(threadOf(revealed)) + 1_000,
    );
    const timedOut = await defenderEntersOurTimeout({
      deployment,
      defender: b.defender,
      thread: revealed,
    });
    expect(timedOut.address).toBe(
      deployment.chain.timeout.spendingScriptAddress,
    );
    won = await defenderClosesItsWin({
      deployment,
      defender: b.defender,
      thread: timedOut,
      unit: b.unit,
      caseUtxo: await liveCase({ scene, caseUnit }),
    });
  }, 1_800_000);

  it("leaves the case a defender's win: no thread open, the thread's rest at the defender's key", async () => {
    const { scene, caseUnit, b } = dispute;
    const caseUtxo = await liveCase({ scene, caseUnit });
    expect(caseUtxo.txHash).toBe(won);
    expect(Data.from(caseUtxo.datum!, SDK.Case)).toMatchObject({
      open_threads: 0n,
      defender_won: true,
    });
    const [leftover] = await scene.h.proverLucid.utxosByOutRef([
      { txHash: won, outputIndex: 1 },
    ]);
    expect(leftover!.address).toBe(
      credentialToAddress(network, { type: "Key", hash: b.defender.vkey }),
    );
    expect(await threadClosed(dispute, b.unit)).toBe(true);
  }, 300_000);

  it("stops under its own named state with no alarm, handing the session back at once, and moves nothing more", async () => {
    const { scene, runner, prepared } = dispute;
    const before = prepared.length;
    let runs = 0;
    const result = await continuePendingWorkflow({
      invocation: {
        mode: "run",
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "validationTraceDispute",
        headerHash: scene.failing.evidence.headerHash,
      },
      journal: runner.journal,
      execute: async () => {
        runs += 1;
        return validationTraceRunState(await runner.runOnce(scene.failing));
      },
    });
    expect(runs).toBe(1);
    expect(result).toMatchObject({
      kind: "pending",
      namedState: { name: VALIDATION_TRACE_DEFENDER_WON, alarm: false },
    });
    // A later run stops the same way, with nothing signed.
    const again = await runner.runOnce(scene.failing);
    expect(again.kind).toBe("pending");
    if (!("reason" in again)) throw new Error("no reason");
    expect(again.reason.startsWith(VALIDATION_TRACE_DEFENDER_WON)).toBe(true);
    expect(prepared.slice(before)).toStrictEqual([]);
  }, 300_000);
});

/** The validation-trace dispute runner on a case a defender wins while a
 * thread we won waits at the award, on the emulator. The runner resolves the
 * operator's thread to the award while the successor is silent, and is not
 * run again before the successor reveals and no process of ours answers
 * before our clock on its thread runs out: the successor times us out and
 * closes its thread into the case as its win. The case can no longer
 * convict, yet the runner still closes the thread it won into the case with
 * that defender's lost-defense receipt, and only then stops under its own
 * named state with nothing more to sign. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DEFENDER_WON } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceChallengerDeadlineMs } from "../src/validation-trace-dispute/threads.js";
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
import {
  isPendingFor,
  playThreads,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  isTurn,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

let dispute: TwoThreadCase;
/** What the runner signed before the successor's win. */
let signedBefore: number;

describe("validation-trace dispute runner: a defender wins while a thread we won waits at the award", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({ withRemoval: false });
    const { scene, runner, deployment, a, b, caseUnit } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, { ...b, silent: true }],
      until: async () =>
        (await threadPlace(scene, deployment, a.unit))?.holder === "award",
      limit: 60,
    });
    signedBefore = dispute.prepared.length;
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
    await defenderClosesItsWin({
      deployment,
      defender: b.defender,
      thread: await defenderEntersOurTimeout({
        deployment,
        defender: b.defender,
        thread: revealed,
      }),
      unit: b.unit,
      caseUtxo: await liveCase({ scene, caseUnit }),
    });
  }, 1_800_000);

  it("leaves the case a defender's win with the thread we won still open at the award", async () => {
    const { scene, deployment, caseUnit, a, b } = dispute;
    const caseDatum = Data.from(
      (await liveCase({ scene, caseUnit })).datum!,
      SDK.Case,
    );
    expect(caseDatum).toMatchObject({ open_threads: 1n, defender_won: true });
    expect((await threadPlace(scene, deployment, a.unit))?.holder).toBe(
      "award",
    );
    expect(await threadClosed(dispute, b.unit)).toBe(true);
  }, 300_000);

  it("closes the thread it won into the case with its defender's lost-defense receipt, then stops under defender-won with nothing more to sign", async () => {
    const { scene, runner, prepared, caseUnit, a } = dispute;
    const { result, history } = await playThreads({
      scene,
      runner,
      sides: [],
      until: async (run) => isPendingFor(run, VALIDATION_TRACE_DEFENDER_WON),
      limit: 10,
    });
    expect(result.kind, history.join("\n")).toBe("pending");
    const after = prepared.slice(signedBefore);
    expect(after.map(({ stage }) => stage)).toStrictEqual(["close_awarded"]);
    const close = after[0]!.hash;
    expect(await threadClosed(dispute, a.unit)).toBe(true);
    const { chain, h } = scene;
    const caseUtxo = await liveCase({ scene, caseUnit });
    expect(caseUtxo.txHash).toBe(close);
    expect(Data.from(caseUtxo.datum!, SDK.Case)).toMatchObject({
      open_threads: 0n,
      defender_won: true,
    });
    const [receipt] = await h.proverLucid.utxosAtWithUnit(
      chain.award.spendingScriptAddress,
      chain.award.policyId + a.unit.slice(56),
    );
    expect(receipt?.txHash).toBe(close);
    expect(Data.from(receipt!.datum!, SDK.LostDefenseReceipt)).toMatchObject({
      defender_vkh: a.defender.vkey,
      outcome: "ChallengerWins",
      bounty_claimant: h.proverSigner.paymentKeyHash,
    });
    // A later run stops the same way, with nothing signed.
    const again = await runner.runOnce(scene.failing);
    expect(isPendingFor(again, VALIDATION_TRACE_DEFENDER_WON)).toBe(true);
    expect(prepared.slice(signedBefore + 1)).toStrictEqual([]);
  }, 600_000);
});

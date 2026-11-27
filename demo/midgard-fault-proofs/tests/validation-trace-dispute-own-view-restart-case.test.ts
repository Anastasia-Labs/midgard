/** The validation-trace dispute runner restarted as its case opens, on the
 * emulator: after our Init, after our Open, and after another party's
 * VerifySource over our claim. At each point a fresh process asks first
 * for what the process that was not stopped asks for (support/validation-trace-dispute-restart-probe.ts):
 * our Open after our Init and our VerifySource after our Open, never a
 * second Init; and once another party's VerifySource opened our case, no
 * VerifySource of ours and so no second bond. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { anotherParty } from "./support/validation-trace-dispute-foreign-verify.js";
import { recordedOutput } from "./support/validation-trace-dispute-funded-moves.js";
import {
  capturePrepared,
  type PreparedMove,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  failingMaterial,
  honestPresentation,
  type Scene,
  setUpScene,
  verifySource,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  expectSameNextAfterRestart,
  type ProcessOver,
  recordingAsks,
} from "./support/validation-trace-dispute-restart-probe.js";
import {
  makeRunner,
  type Runner,
} from "./support/validation-trace-dispute-runner.js";

/** A scene, our runner on it recording what it asks for and signs, and
 * fresh processes of the runner. */
const opening = async () => {
  const scene = await setUpScene();
  const seen: string[] = [];
  const prepared: PreparedMove[] = [];
  const runner: Runner = makeRunner(scene, {
    wrap: (adapter) => capturePrepared(prepared)(recordingAsks(seen)(adapter)),
  });
  const processOver: ProcessOver = (wrap) => makeRunner(scene, { wrap });
  /** Runs our runner until it has signed `stage`; that run's block lands
   * it. */
  const runUntilSigned = async (stage: PreparedMove["stage"]) => {
    for (let run = 0; run < 8; run += 1) {
      if (prepared.some((move) => move.stage === stage)) return;
      await runner.runOnce(scene.failing);
    }
    throw new Error(`our runner never signed ${stage}`);
  };
  const restart = async (opened: boolean) =>
    await expectSameNextAfterRestart({
      processOver,
      runner,
      seen,
      block: scene.failing,
      opened,
    });
  return { scene, prepared, runUntilSigned, restart };
};

/** The opener output our Open `move` left: the one carrying our thread's
 * token. */
const openerOf = (scene: Scene, move: PreparedMove) => {
  const policy = scene.h.contracts.computationThread.policyId;
  const outputs = CML.Transaction.from_cbor_hex(move.cbor).body().outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (
      Object.keys(coreToTxOutput(outputs.get(index)).assets).some((unit) =>
        unit.startsWith(policy),
      )
    )
      return recordedOutput(scene, `${move.hash}#${index.toString()}`);
  throw new Error("our Open left no opener output");
};

describe("validation-trace dispute runner restarted as its case opens", () => {
  it("asks for our Open after our Init, and for our VerifySource after our Open, never Initing again", async () => {
    const { runUntilSigned, restart, prepared } = await opening();
    await runUntilSigned("init");
    const afterInit = await restart(false);
    expect(afterInit).toMatch(/^validation-trace-open:/);
    await runUntilSigned("open");
    const afterOpen = await restart(false);
    expect(afterOpen).toMatch(/^validation-trace-verify-source:/);
    expect(
      prepared.filter(({ stage }) => stage === "init").map(({ hash }) => hash),
    ).toHaveLength(1);
  }, 900_000);

  it("asks for no VerifySource of ours once another party's VerifySource opened our case over our claim", async () => {
    const { scene, runUntilSigned, restart, prepared } = await opening();
    await runUntilSigned("open");
    const opener = openerOf(
      scene,
      prepared.find(({ stage }) => stage === "open")!,
    );
    const material = await failingMaterial(scene);
    const lucid = await anotherParty(scene);
    const foreign = verifySource(scene, opener, {
      ...honestPresentation(material),
      lucid,
    });
    await lucid.awaitTx(
      await (
        await (await foreign.build()).sign.withWallet().complete()
      ).submit(),
    );
    await restart(true);
    await restart(true);
    expect(prepared.map(({ stage }) => stage)).not.toContain("verify_source");
  }, 900_000);
});

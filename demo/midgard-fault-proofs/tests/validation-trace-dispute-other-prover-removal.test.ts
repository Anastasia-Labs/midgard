/** Another prover's case on the same failing block convicts first, and that
 * prover removes the block against its own record while our case on the
 * block is still open. Our runner still closes its case, its bond back and
 * its record minted. It then reads the block as removed against another
 * record and, past rollback recovery, ends resolved by that record: never
 * as our conviction, with no removal of ours and no terminal. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk/da";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data, generateEmulatorAccount, Lucid } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { resolveProverSigner } from "../src/runtime.resolve-prover-signer.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init/queue-assertions.js";
import type { ConfirmedValidationTraceStage } from "./support/validation-trace-confirmed-stages.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { recordedRemoval } from "./support/validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";
import { RELEASE_FINALITY_POLICY } from "./workflow.make-adapter.js";

/** The first stage `stage` that `runner`'s process sent and the emulator
 * confirmed. */
const stageOf = async (
  runner: Runner,
  stage: ConfirmedValidationTraceStage["action"]["stage"],
) => (await runner.confirmed()).find(({ action }) => action.stage === stage);

/** The shared remove stage's transactions `runner`'s process sent. */
const removals = (runner: Runner) =>
  runner.process
    .sent()
    .filter(({ actionId }) => actionId.startsWith("remove:"));

const defenseDeadline = async (scene: Scene, outRef: string) => {
  const [live] = await scene.h.proverLucid.utxosByOutRef([
    parseOutRefLabel(outRef),
  ]);
  return validationTraceDefenseDeadlineMs(Data.from(live!.datum!, SDK.Case));
};

describe("validation-trace dispute runner: another prover convicts the block and removes it while our case is open", () => {
  let recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  let scene: Scene;
  let ours: Runner;
  let theirs: Runner;
  let theirKey: string;
  const opened: string[] = [];

  beforeAll(async () => {
    // Before the scene, so the raw L1 view has the whole history.
    recorder = recordCrossBlockRawEmulator();
    scene = await setUpScene();
    const { h } = scene;
    const { headerHash } = scene.failing.evidence;
    // Another prover, funded from ours, with its own wallet and key.
    const signer = resolveProverSigner({
      network,
      walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
    });
    theirKey = signer.paymentKeyHash;
    expect(theirKey).not.toBe(h.proverSigner.paymentKeyHash);
    const funding = await h.proverLucid
      .newTx()
      .pay.ToAddress(signer.address, { lovelace: 15_000_000_000n })
      .pay.ToAddress(signer.address, { lovelace: 5_000_000_000n })
      .complete({ localUPLCEval: true });
    await h.proverLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    const lucid = await Lucid(h.emulator, "Custom");
    signer.selectWallet(lucid);
    const ourRemoval = await recordedRemoval(scene, recorder);
    const theirRemoval = await recordedRemoval(scene, recorder, theirKey);
    ours = makeRunner(scene, {
      removal: ourRemoval.removal,
      terminalVerifier: ourRemoval.terminalVerifier(headerHash),
    });
    theirs = makeRunner(scene, {
      prover: { lucid, signer },
      removal: theirRemoval.removal,
      terminalVerifier: theirRemoval.terminalVerifier(headerHash),
    });
    for (const runner of [ours, theirs]) {
      await runUntil(runner, scene.failing, VALIDATION_TRACE_CASE_OPEN);
      opened.push((await stageOf(runner, "verify_source"))!.outputs[0]!.outRef);
    }
  }, 1_800_000);

  afterAll(() => recorder?.restore());

  it("closes our case, then ends resolved by their record past rollback recovery, never as our conviction", async () => {
    const { h } = scene;
    const lucid = h.proverLucid;
    const deadline = Math.max(
      ...(await Promise.all(
        opened.map(async (outRef) => await defenseDeadline(scene, outRef)),
      )),
    );
    h.emulator.awaitSlot(
      lucid.unixTimeToSlot(deadline) - lucid.currentSlot() + 1,
    );
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    try {
      // They convict their case and remove the block against their record.
      let theirResult: FraudProofWorkflowRunResult | undefined;
      const history: string[] = [];
      for (
        let attempt = 0;
        attempt < 16 && theirResult === undefined;
        attempt += 1
      ) {
        const outcome = await theirs.runOnce(scene.failing);
        history.push(
          "reason" in outcome
            ? `${outcome.kind}: ${outcome.reason}`
            : outcome.kind,
        );
        if (outcome.kind === "terminal_included") theirResult = outcome;
      }
      if (theirResult === undefined || !("terminal" in theirResult))
        throw new Error(`their removal did not land: ${history.join("\n")}`);
      const theirRecord = (await stageOf(theirs, "close"))!.outputs[0]!.outRef;
      expect(theirResult.terminal.correction.referencedProofTokenOutRef).toBe(
        theirRecord,
      );
      await expectStateQueueHeaderOrder({
        lucid,
        contracts: h.contracts,
        expectedHeaderHashes: [scene.failing.evidence.header.prevHeaderHash],
      });

      // Our open case still closes, and our record names our key; the block
      // is then read as removed against their record.
      await runUntil(
        ours,
        scene.failing,
        "the header was removed against another record; the removal awaits release finality",
        16,
      );
      const close = (await stageOf(ours, "close"))!;
      expect(close.action.consumes).toBe(opened[0]);
      const [ourRecord] = await lucid.utxosByOutRef([
        parseOutRefLabel(close.outputs[0]!.outRef),
      ]);
      expect(
        Data.from(ourRecord!.datum!, SDKFraudProof.FraudProofTokenDatum)
          .fraud_prover,
      ).toBe(h.proverSigner.paymentKeyHash);

      h.emulator.awaitBlock(
        RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2,
      );
      const resolved = await ours.runOnce(scene.failing);
      if (resolved.kind !== "resolved_by_another_record")
        throw new Error(`our run did not resolve: ${resolved.kind}`);
      expect(resolved.resolution).toMatchObject({
        removalTxHash: theirResult.terminal.correction.removalTxHash,
        referencedProofTokenOutRef: theirRecord,
      });
      expect("terminal" in resolved).toBe(false);
      expect(removals(ours)).toEqual([]);
    } finally {
      clock.mockRestore();
    }
  }, 900_000);
});

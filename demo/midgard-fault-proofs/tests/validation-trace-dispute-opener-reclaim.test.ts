/** Our thread stands at the dispute opener, Inited but not yet Opened,
 * when another prover convicts its own case on the same failing block and
 * removes the block against its record. Open reads the block, so our run
 * cannot carry the thread on. Past rollback recovery of the removal, our
 * run cancels the thread back to our wallet through the opener's Cancel
 * (the thread's lovelace less the cancel's fee), waits out the cancel's
 * rollback recovery (one block short of it, the run still waits), and only
 * then ends resolved by their record: never as our conviction, with no
 * removal of ours. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { resolveProverSigner } from "../src/index.js";
import {
  type ConfirmedValidationTraceStage,
  confirmedValidationTraceStages,
} from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import {
  expectResolvedWithReclaim,
  walletLovelace,
} from "./support/own-thread-reclaim-expectations.js";
import { network } from "./support/submit-init-emulator-shared.js";
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

const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

/** Our run waits here, its thread at the opener, until the test lets it on. */
const OUR_OPEN_HELD = "our open is held by the test";

const entriesOf = (result: FraudProofWorkflowRunResult) => {
  if (!("entries" in result)) throw new Error("no workflow journal");
  return result.entries;
};

const stageOf = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  stage: ConfirmedValidationTraceStage["action"]["stage"],
) =>
  confirmedValidationTraceStages(entries).find(
    ({ action }) => action.stage === stage,
  );

describe("validation-trace dispute runner: another prover removes the block while our thread stands at the opener", () => {
  let recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
  let scene: Scene;
  let ours: Runner;
  let theirs: Runner;
  let ourThread: string;
  let theirCase: string;
  const hold = { open: true };

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
    const theirRemoval = await recordedRemoval(
      scene,
      recorder,
      signer.paymentKeyHash,
    );
    ours = makeRunner(scene, {
      removal: ourRemoval.removal,
      terminalVerifier: ourRemoval.terminalVerifier(headerHash),
      // Our run Inits, then stops short of its Open while the test holds it.
      wrap: (adapter) => ({
        ...adapter,
        observe: async (context) => {
          const observed = await adapter.observe(context);
          return hold.open &&
            observed.kind === "action_required" &&
            observed.action.input.stage === "open"
            ? { kind: "pending", reason: OUR_OPEN_HELD }
            : observed;
        },
      }),
    });
    theirs = makeRunner(scene, {
      prover: { lucid, signer },
      removal: theirRemoval.removal,
      terminalVerifier: theirRemoval.terminalVerifier(headerHash),
    });
    const held = await runUntil(ours, scene.failing, OUR_OPEN_HELD);
    ourThread = stageOf(entriesOf(held.result), "init")!.outputs[0]!.outRef;
    const opened = await runUntil(
      theirs,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    theirCase = stageOf(entriesOf(opened.result), "verify_source")!.outputs[0]!
      .outRef;
  }, 1_800_000);

  afterAll(() => recorder?.restore());

  it("cancels our opener thread to our wallet past rollback recovery, then resolves by their record", async () => {
    const { h, chain } = scene;
    const lucid = h.proverLucid;
    const [liveCase] = await lucid.utxosByOutRef([parseOwnOutRef(theirCase)]);
    const deadline = validationTraceDefenseDeadlineMs(
      Data.from(liveCase!.datum!, SDK.Case),
    );
    h.emulator.awaitSlot(
      lucid.unixTimeToSlot(deadline) - lucid.currentSlot() + 1,
    );
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const submit = h.emulator.submitTx.bind(h.emulator);
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
      const theirRecord = stageOf(theirResult.entries, "close")!.outputs[0]!
        .outRef;

      // Our thread still stands at the opener, and our run is let on.
      const [thread] = await lucid.utxosByOutRef([parseOwnOutRef(ourThread)]);
      expect(thread?.address).toBe(chain.opener.spendingScriptAddress);
      const threadLovelace = thread!.assets.lovelace!;
      hold.open = false;
      const sent: string[] = [];
      h.emulator.submitTx = async (cbor: string) => {
        sent.push(cbor);
        return await submit(cbor);
      };

      // The removal is still within rollback recovery: nothing is sent.
      expect(await ours.runOnce(scene.failing)).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `header removal awaits release finality before our thread ${ourThread} is reclaimed`,
        ),
      });
      expect(sent).toEqual([]);

      h.emulator.awaitBlock(FINAL_DEPTH);
      const before = await walletLovelace(lucid);
      // Past the removal's rollback recovery: the opener's Cancel is sent.
      expect((await ours.runOnce(scene.failing)).kind).toBe("pending");
      expect(sent).toHaveLength(1);
      const reclaim = CML.Transaction.from_cbor_hex(sent[0]!);
      const reclaimTxHash = CML.hash_transaction(reclaim.body()).to_hex();

      // The cancel one block short of release finality: no resolve yet.
      const depth = async () => {
        const status = await h.emulator.getTransactionStatus(reclaimTxHash);
        return status.status === "confirmed"
          ? (status.confirmation.confirmations ?? 0)
          : 0;
      };
      h.emulator.awaitBlock(FINAL_DEPTH - 1 - (await depth()));
      expect(await depth()).toBe(FINAL_DEPTH - 1);
      expect(await ours.runOnce(scene.failing)).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `reclaim ${reclaimTxHash} of our thread ${ourThread} awaits release finality`,
        ),
      });

      // The block each run ends with puts the cancel past rollback recovery.
      expect(await depth()).toBe(FINAL_DEPTH);
      const result = await ours.runOnce(scene.failing);

      // One transaction of ours: the opener's Cancel of our thread, which
      // paid the thread's lovelace back to our wallet less its fee.
      expect(sent).toHaveLength(1);
      expect(await walletLovelace(lucid)).toBe(
        before + threadLovelace - reclaim.body().fee(),
      );
      expect(await lucid.utxosByOutRef([parseOwnOutRef(ourThread)])).toEqual(
        [],
      );
      expectResolvedWithReclaim(
        result,
        { threadOutRef: ourThread, lovelace: threadLovelace.toString() },
        reclaimTxHash,
      );
      if (result.kind !== "resolved_by_another_record") return;
      expect(result.resolution).toMatchObject({
        removalTxHash: theirResult.terminal.correction.removalTxHash,
        referencedProofTokenOutRef: theirRecord,
      });
    } finally {
      h.emulator.submitTx = submit;
      clock.mockRestore();
    }
  }, 900_000);
});

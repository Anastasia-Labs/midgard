/** A staged builtin step's case of ours (arm 31) on the emulator
 * (validation-trace-dispute-one-step-staged-drive.ts) whose block another
 * record removes. Another prover opens a case of its own on the block beside
 * ours and leaves it undefended. Both threads of our case play through the
 * staged step to their award and close while the test holds our spend backs
 * until our case's close is confirmed, so each staging output is still live;
 * past both cases' deadlines that prover convicts and removes the block
 * against its own record, and the removal goes past rollback recovery before
 * our run is let on. Our case then closes, and our run reads the block as
 * removed against another record.
 *
 * With `sibling`, an Init of ours that an earlier process of ours sent and
 * never followed lands once our case is open (a process of ours that sends
 * that Init and stops short of its Open stands in for it), so a thread on
 * our key stands at the opener when the block is removed: our run, its case
 * open, opens no second case from it. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk/da";
import {
  CML,
  coreToTxOutput,
  Data,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import { resolveProverSigner } from "../../src/runtime.resolve-prover-signer.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../../src/validation-trace-dispute/adapter.js";
import {
  encodeValidationTraceDisputeArtifact,
  parseValidationTraceDisputeArtifact,
} from "../../src/validation-trace-dispute/artifact.js";
import { VALIDATION_TRACE_STAGING_RECLAIM } from "../../src/validation-trace-dispute/one-step-staging-types.js";
import {
  captureValidationTraceInit,
  validationTraceThreadUnit,
} from "../../src/validation-trace-dispute/transactions.js";
import { validationTraceDefenseDeadlineMs } from "../../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import { RELEASE_FINALITY_POLICY } from "../workflow.make-adapter.js";
import { network } from "./emulator/blueprints.js";
import type { ConfirmedValidationTraceStage } from "./validation-trace-confirmed-stages.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
  threadClosed,
} from "./validation-trace-dispute-one-step-case.js";
import { stagedDrive } from "./validation-trace-dispute-one-step-staged-drive.js";
import { onLedger } from "./validation-trace-dispute-one-step-staging-ledger.js";
import { recordedRemoval } from "./validation-trace-dispute-removal-l1.js";
import {
  makeRunner,
  type Runner,
  runUntil,
} from "./validation-trace-dispute-runner.js";
import { expectSpentBackAgainAfterRollback } from "./validation-trace-dispute-staging-left-for-sweep.js";

/** How many blocks under the tip a transaction of ours is final. */
export const RELEASE_DEPTH = 4;

/** How deep a removal, or our cancel of a thread, is past rollback
 * recovery. */
const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

const SPEND_BACKS_HELD = "spend backs held by the test";
/** How many runs past our cancel's landing the chain waits on our spend
 * backs before it is let past the cancel's rollback recovery. */
const AFTER_CANCEL = 3 * RELEASE_DEPTH;
const RECLAIM_PREFIX = "validation-trace-reclaim-staging:";

const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

/** The first output of the confirmed `stage` among `runner`'s sends. */
const stageOutRef = async (
  runner: Runner,
  stage: ConfirmedValidationTraceStage["action"]["stage"],
): Promise<string> => {
  const found = (await runner.confirmed()).find(
    ({ action }) => action.stage === stage,
  );
  if (found === undefined) throw new Error(`no ${stage} is confirmed`);
  return found.outputs[0]!.outRef;
};

/** The out-refs the signed transaction `cbor` spends. */
const inputsOf = (cbor: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** A run of ours from the block's removal on, and the tip it observed. */
export type OurRun = Readonly<{
  result: FraudProofWorkflowRunResult;
  observedAt: number;
}>;

export const stagedOtherRecord = ({
  sibling = false,
}: { readonly sibling?: boolean } = {}) => {
  const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH, withRemoval: true });
  let theirs: Runner;
  let theirCase: string;
  let siblingThread: string | undefined;
  let stagedOutputs: readonly UTxO[] = [];
  let clock: { readonly mockRestore: () => void } | undefined;

  /** Holds each spend back our workflow would take, as a wait, until our
   * case's close is confirmed. */
  const holding = (
    adapter: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter => ({
    ...adapter,
    observe: async (context) => {
      const observed = await adapter.observe(context);
      return observed.kind === "action_required" &&
        observed.action.input.schemaVersion ===
          VALIDATION_TRACE_STAGING_RECLAIM &&
        !(await drive.confirmed()).some(
          ({ action }) => action.stage === "close",
        )
        ? { kind: "pending", reason: SPEND_BACKS_HELD }
        : observed;
    },
  });

  const scene = () => drive.dispute().scene;

  /** A runner of another prover, funded from ours, with its own wallet and
   * key. */
  const anotherProver = async (
    recorder: Parameters<typeof recordedRemoval>[1],
    headerHash: string,
  ): Promise<Runner> => {
    const { h } = scene();
    const signer = resolveProverSigner({
      network,
      walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
    });
    // Our runs leave the shared Lucid on the wallet a run restricts to its
    // plain unheld coins; the funding pays from the whole wallet.
    h.proverSigner.selectWallet(h.proverLucid);
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
    const theirRemoval = await recordedRemoval(
      scene(),
      recorder,
      signer.paymentKeyHash,
    );
    return makeRunner(scene(), {
      prover: { lucid, signer },
      removal: theirRemoval.removal,
      terminalVerifier: theirRemoval.terminalVerifier(headerHash),
    });
  };

  /** Our case set up, our spend backs held; the other prover's case open;
   * with `sibling`, the Init of ours an earlier process sent landed. */
  const setUp = async () => {
    await drive.setUp();
    drive.restart({ wrap: holding });
    const { h, failing } = scene();
    const { deployment, recorder } = drive.dispute();
    const { headerHash } = failing.evidence;
    theirs = await anotherProver(recorder!, headerHash);
    await runUntil(theirs, failing, VALIDATION_TRACE_CASE_OPEN);
    theirCase = await stageOutRef(theirs, "verify_source");
    if (!sibling) return;
    // An earlier process of ours signed and sent an Init and stopped short
    // of its Open: its Init lands once our case is open. This process holds
    // nothing of it.
    const artifact = parseValidationTraceDisputeArtifact(
      encodeValidationTraceDisputeArtifact({
        headerHash,
        material: drive.dispute().material,
      }),
    );
    h.proverSigner.selectWallet(h.proverLucid);
    const { transaction } = await captureValidationTraceInit({
      lucid: h.proverLucid,
      signer: h.proverSigner,
      deployment,
      artifact,
    });
    const initHash = await transaction.signed.submit();
    h.emulator.awaitBlock(1);
    const unit = validationTraceThreadUnit(deployment, headerHash);
    const outputs = transaction.signed.toTransaction().body().outputs();
    const index = Array.from({ length: outputs.len() }, (_, at) => at).find(
      (at) => (coreToTxOutput(outputs.get(at)).assets[unit] ?? 0n) > 0n,
    );
    if (index === undefined) throw new Error("the Init minted no thread");
    siblingThread = `${initHash}#${index.toString()}`;
    expect(onLedger(scene(), parseOutRefLabel(siblingThread))).toBe(true);
  };

  /** Plays both threads of our case to their award and close, our spend
   * backs held; the staging outputs, each still live. */
  const playToClose = async (): Promise<readonly string[]> => {
    const { a, b } = drive.dispute();
    await drive.play(
      async () =>
        (await threadClosed(drive.dispute(), a.unit)) &&
        (await threadClosed(drive.dispute(), b.unit)),
      60,
    );
    const stagings = (await drive.stagedOnL1()).map(({ outRef }) => outRef);
    expect(stagings.length).toBeGreaterThan(1);
    for (const outRef of stagings)
      expect(onLedger(scene(), parseOutRefLabel(outRef)), outRef).toBe(true);
    stagedOutputs = await scene().h.proverLucid.utxosByOutRef(
      stagings.map(parseOutRefLabel),
    );
    expect(stagedOutputs).toHaveLength(stagings.length);
    return stagings;
  };

  /** After our run's ending, each staging output back in our wallet: once
   * each spend back rolls back, our next runs spend each output back again,
   * by its spend back's own bytes or by at most one new spend back; and they
   * end resolved by their record again
   * (validation-trace-dispute-staging-left-for-sweep.ts). */
  const spentBackAgainAfterRollback = async () =>
    await expectSpentBackAgainAfterRollback({
      scene: scene(),
      outputs: stagedOutputs,
      runOnce: async () =>
        await drive.dispute().runner.runOnce(scene().failing),
      describe: describeRun,
      ended: (result) => result.kind === "resolved_by_another_record",
    });
  /** Past both cases' deadlines, the other prover convicts its case and
   * removes the block against its record, and the removal goes past
   * rollback recovery; that record. */
  const removeByTheirs = async (): Promise<string> => {
    const { h, failing } = scene();
    const [theirs_] = await h.proverLucid.utxosByOutRef([
      parseOutRefLabel(theirCase),
    ]);
    const deadline = Math.max(
      ...[(await liveCase(drive.dispute())).datum!, theirs_!.datum!].map(
        (datum) => validationTraceDefenseDeadlineMs(Data.from(datum, SDK.Case)),
      ),
    );
    if (h.emulator.now() <= deadline)
      awaitLastSlotAtOrBefore(scene(), deadline + 1_000);
    // The removal dates its validity window from the application clock,
    // which on the emulator is the emulator's.
    clock ??= vi.spyOn(Date, "now").mockImplementation(() => h.emulator.now());
    const history: string[] = [];
    for (let attempt = 0; attempt < 16; attempt += 1) {
      const outcome = await theirs.runOnce(failing);
      history.push(describeRun(outcome));
      if (outcome.kind === "terminal_included" && "terminal" in outcome) {
        h.emulator.awaitBlock(FINAL_DEPTH);
        return outcome.terminal.correction.referencedProofTokenOutRef;
      }
    }
    throw new Error(`their removal did not land:\n${history.join("\n")}`);
  };

  /**
   * Runs ours until it resolves, each run's outcome in `runs`; `between`
   * runs after each run and may restart the runner. With `sibling`, once the
   * other run's thread is spent, the chain is let past the rollback recovery
   * of our cancel when each of `stagings` is spent, or after `AFTER_CANCEL`
   * runs.
   */
  const runToResolution = async (
    stagings: readonly string[],
    runs: OurRun[],
    between?: () => Promise<void>,
  ) => {
    const { h, failing } = scene();
    let jumped = false;
    let sinceCancel = 0;
    for (let run = 0; run < 60; run += 1) {
      const observedAt = h.emulator.blockHeight;
      const result = await drive.dispute().runner.runOnce(failing);
      runs.push({ result, observedAt });
      if (result.kind === "resolved_by_another_record") return result;
      await between?.();
      if (
        !jumped &&
        siblingThread !== undefined &&
        !onLedger(scene(), parseOutRefLabel(siblingThread)) &&
        ((sinceCancel += 1) > AFTER_CANCEL ||
          stagings.every(
            (outRef) => !onLedger(scene(), parseOutRefLabel(outRef)),
          ))
      ) {
        jumped = true;
        h.emulator.awaitBlock(FINAL_DEPTH);
      }
    }
    throw new Error(
      `our run did not resolve:\n${runs.map(({ result }) => describeRun(result)).join("\n")}`,
    );
  };

  /** The one landed transaction of ours that spent `outRef`, and its
   * block; every signed transaction the ledger took that spends it. */
  const spentBy = async (outRef: string) => {
    const hash = await drive.spenderOf(outRef);
    expect(hash, outRef).toBeDefined();
    return {
      hash: hash!,
      block: await drive.blockOf(hash!),
      signed: [...scene().l1.signedCbors].filter(([, cbor]) =>
        inputsOf(cbor).includes(outRef),
      ),
    };
  };

  /** Expects `outRef` spent back to our wallet alone, by exactly one
   * transaction the ledger took; that spend back and its block. */
  const expectSpentBack = async (outRef: string) => {
    const spent = await spentBy(outRef);
    expect(
      spent.signed.map(([hash]) => hash),
      outRef,
    ).toStrictEqual([spent.hash]);
    const body = CML.Transaction.from_cbor_hex(
      scene().l1.signedCbors.get(spent.hash)!,
    ).body();
    expect(body.inputs().len()).toBe(1);
    expect(body.outputs().len()).toBe(1);
    expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
      scene().h.proverSigner.address,
    );
    return spent;
  };

  /** Our spend backs the dispute's processes sent, by what each spends. */
  const spendBackSends = () => {
    const sends = new Map<string, Set<string>>();
    for (const { actionId, txHash } of drive.sent())
      if (actionId.startsWith(RECLAIM_PREFIX)) {
        const consumes = actionId.slice(RECLAIM_PREFIX.length);
        sends.set(consumes, (sends.get(consumes) ?? new Set()).add(txHash));
      }
    return sends;
  };

  return {
    drive,
    holding,
    setUp,
    playToClose,
    removeByTheirs,
    runToResolution,
    spentBackAgainAfterRollback,
    spentBy,
    expectSpentBack,
    spendBackSends,
    describeRun,
    /** The thread the earlier process's Init left. */
    siblingThread: () => siblingThread!,
    tearDown: () => {
      clock?.mockRestore();
      drive.dispute()?.recorder?.restore();
    },
  };
};

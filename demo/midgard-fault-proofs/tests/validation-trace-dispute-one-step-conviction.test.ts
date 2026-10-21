/** The validation-trace dispute runner convicting a case whose two defenders
 * both lose their threads in the one-step resolution, on the emulator. The
 * operator's thread goes to a CEK compute step and the successor's to a CEK
 * machine step. The runner resolves the first and closes it into our case
 * while the second defender is silent. The second defender reveals once; our
 * reply comes late in our own clock, so its turn runs past the case's defense
 * deadline. Past that deadline, with its thread still open, the runner waits
 * and never closes the case. The defender then plays on to its loss; the
 * runner resolves and closes its thread, closes the case Convicted with our
 * bond back, and, once the L1 view has seen the record, removes the block. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import {
  VALIDATION_TRACE_THREAD_AWAITING,
  validationTraceDefenderDeadlineMs,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  expectRealMoveFee,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  gameThread,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  confirmedOneStepMoves,
  isPendingFor,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import { BOND_LOVELACE } from "./support/validation-trace-dispute-open-scene.js";
import {
  isTurn,
  reportFit,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

/** How late in our clock our first reply on the second thread comes. */
const LATE_REPLY_MS = 30 * 60 * 1_000;

let dispute: TwoThreadCase;
/** The runs past the defense deadline with the second thread open. */
let waited: FraudProofWorkflowRunResult[] = [];
/** The run that closed the case. */
let convicted: FraudProofWorkflowRunResult;

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const signedCbor = (txHash: string) =>
  dispute.scene.l1.signedCbors.get(txHash)!;

/** The address of output `index` of the landed transaction `txHash`. */
const outputAddress = (txHash: string, index: number) =>
  CML.Address.from_hex(
    CML.Transaction.from_cbor_hex(signedCbor(txHash))
      .body()
      .outputs()
      .get(index)
      .address()
      .to_hex(),
  ).to_bech32();

/** The shared remove stage's journaled intents. */
const removalIntents = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionInput.schemaVersion === CURSOR_FAMILY_ACTION
      ? [event]
      : [],
  );

describe("validation-trace dispute runner: conviction after both defenders lose their one-step resolutions", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({ withRemoval: true });
  }, 1_800_000);

  afterAll(() => dispute?.recorder?.restore());

  it("resolves the operator's thread and closes it into our case while the successor is silent", async () => {
    const { scene, runner, a, b, caseUnit } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, { ...b, silent: true }],
      until: async () => await threadClosed(dispute, a.unit),
      limit: 60,
    });
    const caseDatum = Data.from(
      (await liveCase({ scene, caseUnit })).datum!,
      SDK.Case,
    );
    expect(caseDatum.open_threads).toBe(1n);
    expect(caseDatum.defender_won).toBe(false);
    expect(dispute.prepared.map(({ stage }) => stage)).not.toContain("close");
  }, 1_800_000);

  it("waits past the defense deadline while the successor's thread is open on its defender's turn, and never closes the case", async () => {
    const { scene, runner, a, b, caseUnit } = dispute;
    // The defender reveals once; our reply comes late in our own clock.
    await revealAsDefender(
      scene,
      b.defender,
      await gameThread(scene, b.unit),
      b.trace,
    );
    scene.h.emulator.awaitSlot(LATE_REPLY_MS / 1_000);
    await playThreads({
      scene,
      runner,
      sides: [a, { ...b, silent: true }],
      until: async () =>
        isTurn(threadOf(await gameThread(scene, b.unit)), "AwaitingOperator"),
      limit: 5,
    });
    const caseUtxo = await liveCase({ scene, caseUnit });
    const deadline = validationTraceDefenseDeadlineMs(
      Data.from(caseUtxo.datum!, SDK.Case),
    );
    const defenderDeadline = validationTraceDefenderDeadlineMs(
      threadOf(await gameThread(scene, b.unit)),
    );
    expect(defenderDeadline).toBeGreaterThan(deadline + 10 * 60 * 1_000);
    awaitLastSlotAtOrBefore(scene, deadline + 1_000);
    const before = dispute.prepared.length;
    waited = [];
    for (let run = 0; run < 3; run += 1)
      waited.push(await runner.runOnce(scene.failing));
    for (const result of waited)
      expect(isPendingFor(result, VALIDATION_TRACE_THREAD_AWAITING)).toBe(true);
    // Nothing was signed: no close of a case with an open thread.
    expect(dispute.prepared.slice(before)).toStrictEqual([]);
    expect(outRefOf(await liveCase({ scene, caseUnit }))).toBe(
      outRefOf(caseUtxo),
    );
    expect(Data.from(caseUtxo.datum!, SDK.Case).open_threads).toBe(1n);
  }, 900_000);

  it("resolves the successor's thread through the CEK machine arm once it plays on, closes it, and only then closes the case Convicted with our bond back, sending no removal while the L1 view has not seen the record", async () => {
    const { scene, runner, a, b, caseUnit, deployment, removal } = dispute;
    // The L1 view stops before the close.
    removal!.holdView();
    await removal!
      .l1For(scene.failing.evidence.headerHash)
      .observe({ headerHash: scene.failing.evidence.headerHash });
    const caseUtxo = await liveCase({ scene, caseUnit });
    const { result, history } = await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async (run) => isPendingFor(run, VALIDATION_TRACE_CASE_CONVICTED),
      limit: 60,
    });
    convicted = result;
    if (!("entries" in result)) throw new Error("no workflow journal");
    const moves = confirmedOneStepMoves(result.entries);
    expect(
      moves.map(({ stage }) => stage),
      history.join("\n"),
    ).toStrictEqual([
      ...Array.from({ length: 2 }, () => [
        "enter_resolution",
        "resolve",
        "resolve",
        "resolve",
        "resolve",
        "resolve",
        "close_awarded",
      ]).flat(),
    ]);
    // The successor's resolution runs through the machine arm.
    const second = moves.slice(7);
    expect(outputAddress(second[3]!.txHash, 0)).toBe(
      validationTraceOneStepHolder(deployment, "machine").address,
    );
    for (const { txHash } of second)
      expectRealMoveFee(await landedMoveFee(scene, txHash));
    expect(await threadClosed(dispute, b.unit)).toBe(true);
    // The case closes after both threads, from the case the last close left.
    const stages = confirmedValidationTraceStages(result.entries);
    const close = stages.at(-1)!;
    expect(close.action.stage).toBe("close");
    expect(close.action.consumes).toBe(`${second.at(-1)!.txHash}#0`);
    // Nothing was signed after the case's close.
    expect(dispute.prepared.at(-1)!.stage).toBe("close");
    expect(
      dispute.prepared.filter(({ stage }) => stage === "close"),
    ).toHaveLength(1);
    expect(outRefOf(caseUtxo)).not.toBe(close.action.consumes);
    const lucid = scene.h.proverLucid;
    expect(
      await lucid.utxosByOutRef([parseOwnOutRef(dispute.bondOutRef)]),
    ).toStrictEqual([]);
    const [refund] = await lucid.utxosByOutRef([
      { txHash: close.txHash, outputIndex: 0 },
    ]);
    expect(refund!.address).toBe(
      credentialToAddress(network, {
        type: "Key",
        hash: scene.h.proverSigner.paymentKeyHash,
      }),
    );
    expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
    if (!("reason" in result)) throw new Error("no reason");
    expect(result.reason).toContain("not yet seen");
    expect(removalIntents(result.entries)).toEqual([]);
  }, 1_800_000);

  it("removes the convicted block once the L1 view has seen the record", async () => {
    const { scene, runner, removal } = dispute;
    const { h } = scene;
    removal!.releaseView();
    // The removal dates its validity window from the application clock,
    // which on the emulator is the emulator's.
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const result = await (async () => {
      const history: string[] = [];
      for (let run = 0; run < 12; run += 1) {
        const outcome = await runner.runOnce(scene.failing);
        history.push(
          "reason" in outcome
            ? `${outcome.kind}: ${outcome.reason}`
            : outcome.kind,
        );
        if (
          outcome.kind === "terminal_included" ||
          outcome.kind === "completed"
        )
          return outcome;
      }
      throw new Error(`the removal did not complete: ${history.join("\n")}`);
    })().finally(() => clock.mockRestore());
    if (!("terminal" in result) || !("entries" in convicted))
      throw new Error("no terminal");
    const record = confirmedValidationTraceStages(convicted.entries).at(-1)!
      .outputs[0]!.outRef;
    expect(result.terminal.correction.referencedProofTokenOutRef).toBe(record);
    await expectStateQueueHeaderOrder({
      lucid: h.proverLucid,
      contracts: h.contracts,
      expectedHeaderHashes: [scene.failing.evidence.header.prevHeaderHash],
    });
  }, 900_000);

  it("fits the machine arm's move within the transaction limits", () => {
    if (!("entries" in convicted)) throw new Error("no workflow journal");
    const moves = confirmedOneStepMoves(convicted.entries).slice(7);
    reportFit("VerifyArm machine", [signedCbor(moves[4]!.txHash)]);
  });
});

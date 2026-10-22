/**
 * The validation-trace dispute through the watcher's installed application,
 * on real L1 transactions: the admitted runner, given only the deployment
 * the watcher loads, binds every one-step reference and the script-decode
 * deployment from it; a committed block whose script fails is detected by
 * the watcher's classifier, and the runner opens its case, verifies the
 * source, waits out the undefended case, convicts, and removes the block's
 * descendants and then the block itself, slashing its operator's bond under
 * the deployment's finalized manifest; an honest block whose script passes
 * is never attacked.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  bind: undefined as unknown as (input: {
    readonly headerHash: string;
  }) => unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofDisputeDeployment: async (input: {
    readonly headerHash: string;
  }) => hooks.bind(input),
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceFraudProofUnit } from "../src/validation-trace-dispute/close.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { CURSOR_FAMILY_ACTION } from "../src/workflow/cursor-family-state.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { emulatorSignedTransactionRecovery } from "./support/emulator-signed-recovery.js";
import { installedWorkflowEmulatorClock } from "./support/installed-workflow.advance-emulator-observation.js";
import { scratchDirectory } from "./support/scratch-directory.js";
import { scriptDecodeDeploymentOf } from "./support/script-decode-emulator.js";
import { installApplicationClock } from "./support/script-decode-installed-host.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init-emulator-fixtures.js";
import {
  type InstalledApplication,
  installValidationTraceApplication,
} from "./support/validation-trace-dispute-installed.js";
import {
  type InstalledScene,
  scriptHashOf,
  setUpInstalledScene,
} from "./support/validation-trace-dispute-installed-scene.js";

/** A run polls once per emulator block; one turn of the dispute stays well
 * inside this, and a run that never returns stops here instead. */
const MAX_POLLS_PER_TURN = 200;
const MAX_TURNS = 30;

let recorder: ReturnType<typeof recordCrossBlockRawEmulator>;
let installed: InstalledScene;
let app: InstalledApplication;
let workflowClock: ReturnType<typeof installedWorkflowEmulatorClock>;

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** `value` with each reference UTxO inside it replaced by `as` of it. */
const replacedIn = (value: unknown, as: (utxo: UTxO) => string): unknown =>
  typeof value !== "object" || value === null
    ? value
    : "txHash" in value && "outputIndex" in value
      ? as(value as UTxO)
      : Object.fromEntries(
          Object.entries(value).map(([key, inner]) => [
            key,
            replacedIn(inner, as),
          ]),
        );

const submissionIntents = (
  entries: readonly FraudProofWorkflowJournalEntry[],
) =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" ? [event] : [],
  );

const queue = (hashes: readonly string[]) =>
  expectStateQueueHeaderOrder({
    lucid: installed.scene.h.proverLucid,
    contracts: installed.scene.h.contracts,
    expectedHeaderHashes: hashes,
  });

describe("validation-trace dispute through the installed watcher application", () => {
  beforeAll(async () => {
    // Before the scene, so the raw L1 view has the whole history.
    recorder = recordCrossBlockRawEmulator();
    installed = await setUpInstalledScene();
    const { emulator } = installed.scene.h;
    hooks.authority = recorder.authority;
    hooks.recovery = emulatorSignedTransactionRecovery(emulator);
    workflowClock = installedWorkflowEmulatorClock(emulator);
    workflowClock.awaitReleaseDepth();
    app = await installValidationTraceApplication({ installed, recorder });
    hooks.bind = app.bindingFor;
  }, 1_800_000);

  afterAll(async () => {
    await app?.close();
    recorder?.restore();
  });

  it("binds every one-step reference and the script-decode deployment from the deployment the watcher loads", async () => {
    const { workflow } = await app.apply(installed.scene.failing, app.decision);
    const { deployment } = workflow;
    // The scene publishes some scripts twice; each role is bound to a
    // published copy of its own script.
    expect(replacedIn(deployment.references.oneStep, scriptHashOf)).toEqual(
      replacedIn(installed.oneStep, scriptHashOf),
    );
    const scriptDecode = scriptDecodeDeploymentOf(installed.scene.h.emulator);
    expect(deployment.scriptDecode).toBeDefined();
    expect(
      replacedIn(deployment.scriptDecode, outRefOf) as object,
    ).toMatchObject({
      reference: outRefOf(scriptDecode.reference),
      stakerReferences: replacedIn(scriptDecode.stakerReferences, outRefOf),
    });
    expect(deployment.scriptDecode!.validator.spendingScriptHash).toBe(
      scriptDecode.validator.spendingScriptHash,
    );
  }, 300_000);

  it("never attacks an honest committed block whose script passes", async () => {
    const { scene } = installed;
    const decision = await app.classify(scene.passing);
    expect(decision.decision).toBe("healthy");
    const sent = recorder.signedCbors.size;
    const { workflow } = await app.apply(scene.passing, decision);
    const scratch = await scratchDirectory("validation-trace-honest-");
    try {
      const result = (await app.record.execute({
        workflow,
        sources: app.sources,
        journal: new DirectoryFraudProofWorkflowJournalStore(scratch.path),
        mode: "run",
      })) as FraudProofWorkflowRunResult;
      expect(result.kind).toBe("no_fault_detected");
      if ("entries" in result)
        expect(submissionIntents(result.entries)).toEqual([]);
    } finally {
      await scratch.remove();
    }
    expect(recorder.signedCbors.size).toBe(sent);
    await queue([
      scene.failing.evidence.header.prevHeaderHash,
      scene.failing.evidence.headerHash,
      scene.passing.evidence.headerHash,
      scene.forced.evidence.headerHash,
    ]);
  }, 600_000);

  it("convicts a committed block whose script fails, from detection through the undefended case's close to its own removal, slashing its operator's bond", async () => {
    const { scene } = installed;
    const { h } = scene;
    const lucid = h.proverLucid;
    const { deployment } = (await app.apply(scene.failing, app.decision))
      .workflow;
    const clock = installApplicationClock(h.emulator);
    try {
      // The first turn opens the case, verifies the source, and hands the
      // wait on the defender back to the host with the case's deadline.
      clock.state.crashAfterPolls = clock.state.polls + MAX_POLLS_PER_TURN;
      const opened = await app.turn();
      expect(opened).toMatchObject({ kind: "awaiting_counterparty" });
      if (opened.kind !== "awaiting_counterparty")
        throw new Error("no wait on the defender");
      expect(opened.reason).toMatch(
        new RegExp(`^${VALIDATION_TRACE_CASE_OPEN}: `, "u"),
      );
      const [caseUtxo] = await lucid.utxosByOutRef([
        parseOwnOutRef(opened.outRef),
      ]);
      expect(opened.responseDeadline).toBe(
        validationTraceDefenseDeadlineMs(Data.from(caseUtxo!.datum!, SDK.Case)),
      );
      // No defender answers: the deadline passes.
      h.emulator.awaitSlot(
        lucid.unixTimeToSlot(opened.responseDeadline) - lucid.currentSlot() + 1,
      );

      // The run closes the case, peels the descendants, and removes the
      // convicted block, slashing its operator's bond: the deployment's
      // finalized manifest admits that slash's funding.
      const operators = await lucid.utxosAt(
        h.contracts.activeOperators.spendingScriptAddress,
      );
      const history: string[] = [];
      let result = opened as Awaited<ReturnType<InstalledApplication["turn"]>>;
      for (
        let turn = 0;
        result.kind !== "terminal_included" && result.kind !== "completed";
        turn += 1
      ) {
        expect(
          turn,
          `the target's removal within the turn budget: ${history.join("\n")}`,
        ).toBeLessThan(MAX_TURNS);
        await workflowClock.advance(result);
        clock.state.crashAfterPolls = clock.state.polls + MAX_POLLS_PER_TURN;
        result = await app.turn();
        history.push(
          "reason" in result && result.reason !== undefined
            ? `${result.kind}: ${result.reason}`
            : result.kind,
        );
        expect(
          ["pending", "terminal_included", "completed"],
          history.at(-1),
        ).toContain(result.kind);
      }
      if (!("entries" in result)) throw new Error("no journal");
      const stages = confirmedValidationTraceStages(result.entries).map(
        ({ action }) => action.stage,
      );
      expect(stages).toEqual(["init", "open", "verify_source", "close"]);
      const close = confirmedValidationTraceStages(result.entries).at(-1)!;
      const record = close.outputs[0]!.outRef;
      const [recorded] = await lucid.utxosByOutRef([parseOwnOutRef(record)]);
      const proofUnit = validationTraceFraudProofUnit(
        deployment,
        scene.failing.evidence.headerHash,
      );
      expect(recorded!.address).toBe(
        h.contracts.fraudProof.spendingScriptAddress,
      );
      expect(recorded!.assets[proofUnit]).toBe(1n);
      expect(
        Data.from(recorded!.datum!, SDK.FraudProofTokenDatum).fraud_prover,
      ).toBe(h.proverSigner.paymentKeyHash);
      // Each removal peels one block against our record: the two
      // descendants, then the convicted block.
      const intents = submissionIntents(result.entries);
      const removals = intents.filter(
        ({ actionInput }) => actionInput.schemaVersion === CURSOR_FAMILY_ACTION,
      );
      expect(removals).toHaveLength(3);
      for (const removal of removals)
        expect(removal.actionInput).toMatchObject({
          stage: "remove",
          fraudProofOutRef: record,
        });
      await queue([scene.failing.evidence.header.prevHeaderHash]);
      // The block's removal slashed its operator: the bond it spent was an
      // active operator's node, now gone, and our reward is at our key.
      if (!("terminal" in result)) throw new Error("no terminal");
      const { correction, economics } = result.terminal;
      expect(correction.removalTxHash).toBe(removals.at(-1)!.txHash);
      expect(correction.referencedProofTokenOutRef).toBe(record);
      const bond = operators.find(
        (utxo) => outRefOf(utxo) === economics.operatorBondInputOutRef,
      );
      expect(bond, String(economics.operatorBondInputOutRef)).toBeDefined();
      expect(economics.operatorBondInputLovelace).toBe(
        bond!.assets.lovelace!.toString(),
      );
      expect(
        await lucid.utxosByOutRef([parseOwnOutRef(outRefOf(bond!))]),
      ).toStrictEqual([]);
      const [reward] = await lucid.utxosByOutRef([
        parseOwnOutRef(economics.proverRewardOutputOutRef!),
      ]);
      expect(reward!.address).toBe(
        credentialToAddress(installed.scene.h.proverLucid.config().network!, {
          type: "Key",
          hash: h.proverSigner.paymentKeyHash,
        }),
      );

      // Every transaction the dispute sent, as it landed.
      const measured = intents.map(({ txHash, actionInput }) => {
        const cbor = recorder.signedCbors.get(txHash);
        expect(cbor, `${String(actionInput.stage)} was sent`).toBeDefined();
        const { completeSignedBytes, executionMemory, executionSteps } =
          measureCompleteSignedTransaction(cbor!);
        return {
          stage: String(actionInput.stage),
          completeSignedBytes,
          executionMemory: executionMemory.toString(),
          executionSteps: executionSteps.toString(),
        };
      });
      console.info(JSON.stringify(measured));
      for (const { completeSignedBytes } of measured)
        expect(completeSignedBytes).toBeLessThanOrEqual(16_384);
    } finally {
      clock.restore();
    }
  }, 1_800_000);
});

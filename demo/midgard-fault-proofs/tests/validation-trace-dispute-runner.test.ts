/** The validation-trace dispute runner's open step on the emulator: from the
 * complete DA replay's detection of a committed transaction whose script
 * fails, the shared workflow orchestrator Inits a thread, Opens the claim
 * and VerifySources it, and the case and its bond are live at the out-refs
 * the journal claimed from its own signed bodies. A block with no detection
 * starts no workflow. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import {
  BOND_LOVELACE,
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";

let scene: Scene;

describe("validation-trace dispute runner: open step", () => {
  beforeAll(async () => {
    scene = await setUpScene();
  }, 1_800_000);

  it("starts no workflow on a block whose script passes", async () => {
    const runner = makeRunner(scene);
    const result = await runner.runOnce(scene.passing);
    expect(result.kind).toBe("no_fault_detected");
  });

  it("opens a case and its bond on the detected failing script, claimed by out-ref", async () => {
    const { h, chain } = scene;
    const runner = makeRunner(scene);
    const { result } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    if (!("entries" in result)) throw new Error("no workflow journal");
    const stages = confirmedValidationTraceStages(result.entries);
    expect(stages.map(({ action }) => action.stage)).toStrictEqual([
      "init",
      "open",
      "verify_source",
    ]);
    const [init, open, verify] = stages;
    // Each stage consumed the output of ours the previous one claimed.
    expect(open!.action.consumes).toBe(init!.outputs[0]!.outRef);
    expect(verify!.action.consumes).toBe(open!.outputs[0]!.outRef);

    const [caseClaim, bondClaim] = verify!.outputs;
    const [caseOutput, bond] = await h.proverLucid.utxosByOutRef([
      parseOwnOutRef(caseClaim!.outRef),
      parseOwnOutRef(bondClaim!.outRef),
    ]);
    expect(caseOutput?.address).toBe(chain.case.spendingScriptAddress);
    expect(caseOutput!.assets[caseClaim!.unit]).toBe(1n);
    const material = await failingMaterial(scene);
    expect(Data.from(caseOutput!.datum!, SDK.Case)).toMatchObject({
      block_header_hash: scene.failing.evidence.headerHash,
      event_key: scene.failing.eventKey,
      challenger_root: material.challengerRoot,
      step_count: material.stepCount,
      open_threads: 0n,
      defender_won: false,
      bounty_claimant: h.proverSigner.paymentKeyHash,
    });
    expect(bond?.address).toBe(chain.disputeBond.spendingScriptAddress);
    expect(bond!.assets[bondClaim!.unit]).toBe(1n);
    expect(bond!.assets.lovelace).toBe(BOND_LOVELACE);

    // The case is the one the opener's out-ref names.
    expect(caseClaim!.unit).toBe(
      chain.case.policyId +
        SDK.validationCaseId(
          scene.failing.evidence.headerHash,
          scene.failing.eventKey,
          parseOwnOutRef(open!.outputs[0]!.outRef),
        ),
    );

    // Running again submits nothing: the open case is awaited.
    const again = await runner.runOnce(scene.failing);
    expect(again.kind).toBe("pending");
    if (again.kind === "pending")
      expect(again.reason.startsWith(VALIDATION_TRACE_CASE_OPEN)).toBe(true);
    if (!("entries" in again)) throw new Error("no workflow journal");
    expect(
      again.entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(3);
  });
});

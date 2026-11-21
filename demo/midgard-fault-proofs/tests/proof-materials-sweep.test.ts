/**
 * The sweep of proof materials no live workflow needs, at a workflow's end:
 * whatever the process's view says no live workflow needs and no live thread
 * pins, whoever paid it, spent back in pages of at most
 * `PROOF_MATERIALS_SWEEP_PAGE_INPUTS`, each paying one plain coin to the
 * funding address with no collateral; the ending is never held, not on a
 * sweep's finality nor on a page that does not build. An output two workflows
 * need is swept by the last to end; a copy of another shape is never adopted.
 */
import { describe, expect, it, vi } from "vitest";

import { isPlainFundingOutput } from "../src/funding/plain-funding-output.js";
import * as actuationAuthority from "../src/workflow/actuation-permit.js";
import type { FraudProofWorkflowObservation } from "../src/workflow/orchestrator.js";
import {
  assertProofMaterialsSweepOutput,
  PROOF_MATERIALS_SWEEP_PAGE_INPUTS,
} from "../src/workflow/proof-materials-sweep.action.js";
import {
  PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING,
  type ProofMaterialsObjective,
} from "../src/workflow/proof-materials-view.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import {
  actionOf,
  ADA,
  completedAt,
  CONFLICT,
  contextOf,
  datumOf,
  FINAL_DEPTH,
  HEADER_A,
  HEADER_B,
  holderOf,
  label,
  PENDING,
  prover,
  submitSweep,
  sweeping,
  sweptOutRefs,
  viewOf,
} from "./support/proof-materials-sweep-scene.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  createFixtureChain,
  makeAdapter,
  run,
} from "./workflow.make-adapter.js";

describe("the proof-materials sweep", () => {
  it("spends back what no live workflow needs, paying one plain coin to funding, and leaves a needed output live", async () => {
    const scene = await prover();
    const unneeded = await scene.publish(3, "aa");
    const needed = await scene.publish(2, "ab");
    const view = viewOf(scene);
    view.need(holderOf("workflow-b", HEADER_B), needed.datums);
    const adapter = sweeping(scene, view, CONFLICT);

    const action = actionOf(await adapter.observe(contextOf()));
    const txHashes = sweptOutRefs(action).map((outRef) => outRef.split("#")[0]);
    expect(txHashes).toEqual(Array(3).fill(unneeded.txHash));
    const { signed, txHash } = await submitSweep(scene, adapter, action);

    const body = signed.toTransaction().body();
    expect(body.collateral_inputs()).toBeUndefined();
    expect(body.outputs().len()).toBe(1);
    const [paid] = (await scene.at(scene.fundingAddress)).filter(
      (utxo) => utxo.txHash === txHash,
    );
    expect(isPlainFundingOutput(paid!)).toBe(true);
    expect(paid!.datum ?? paid!.datumHash).toBeUndefined();
    expect(paid!.assets.lovelace).toBe(6n * ADA - body.fee());
    const left = await scene.at(scene.materialsAddress);
    expect(left.map((utxo) => utxo.txHash)).toEqual([
      needed.txHash,
      needed.txHash,
    ]);
    // Nothing more to sweep: the ending goes through unchanged.
    expect(await adapter.observe(contextOf())).toEqual(CONFLICT);
  }, 300_000);

  it("pages more materials than one sweep holds across sweeps of at most a page each", async () => {
    const scene = await prover();
    const count = PROOF_MATERIALS_SWEEP_PAGE_INPUTS + 30;
    await scene.publish(count, "bb");
    const adapter = sweeping(scene, viewOf(scene), completedAt(FINAL_DEPTH));
    const pages: number[] = [];

    for (let turn = 0; turn < 3; turn += 1) {
      const observed = await adapter.observe(contextOf());
      if (observed.kind !== "action_required") break;
      pages.push(sweptOutRefs(observed.action).length);
      const { signed } = await submitSweep(scene, adapter, observed.action);
      const transaction = signed.toTransaction();
      expect(transaction.body().outputs().len()).toBe(1);
      // A full page, with its one sig(key) lock and signature, stays within
      // a quarter of maxTxSize.
      expect(transaction.witness_set().native_scripts()?.len()).toBe(1);
      expect(transaction.to_cbor_hex().length / 2).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize / 4,
      );
    }

    expect(pages).toEqual([PROOF_MATERIALS_SWEEP_PAGE_INPUTS, 30]);
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
    expect(await adapter.observe(contextOf())).toEqual(
      completedAt(FINAL_DEPTH),
    );
  }, 600_000);

  it("sweeps nothing before the workflow has ended, or before a completed ending is final", async () => {
    const scene = await prover();
    await scene.publish(1, "cc");
    const view = viewOf(scene);

    expect(await sweeping(scene, view, PENDING).observe(contextOf())).toEqual(
      PENDING,
    );
    expect(
      await sweeping(scene, view, completedAt(FINAL_DEPTH - 1)).observe(
        contextOf(),
      ),
    ).toEqual(completedAt(FINAL_DEPTH - 1));
  }, 300_000);

  it("never holds the ending: not while a sweep waits for its block, and not on a page that does not build", async () => {
    const scene = await prover();
    await scene.publish(1, "dd");
    const view = viewOf(scene);
    const adapter = sweeping(scene, view, CONFLICT);
    const action = actionOf(await adapter.observe(contextOf()));

    // A page that does not build is the run's alarm; the ending goes
    // through, and the material is still there for a later sweep.
    expect(
      await adapter.observe(
        contextOf({ unbuiltActionIds: new Set([action.actionId]) }),
      ),
    ).toEqual(CONFLICT);

    // A sweep in the mempool keeps its page out of every other sweep, and
    // the ending goes through without waiting for its block.
    await submitSweep(scene, adapter, action, { land: false });
    expect(await adapter.observe(contextOf())).toEqual(CONFLICT);
    expect(
      await sweeping(scene, view, CONFLICT).observe(
        contextOf({}, "workflow-b", HEADER_B),
      ),
    ).toEqual(CONFLICT);
    scene.emulator.awaitBlock();
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
  }, 300_000);

  it("sweeps a workflow under reconciliation-only authority too, with no funding coin", async () => {
    const scene = await prover();
    await scene.publish(2, "ee");
    const adapter = sweeping(scene, viewOf(scene), CONFLICT);
    const context = contextOf({ reconciliationOnly: true });

    const action = actionOf(await adapter.observe(context));
    const preflight = await adapter.preflight({ ...context, action } as never);
    const inputs = workflowPreflightTransaction(preflight)!
      .toTransaction()
      .body()
      .inputs();
    const spent: string[] = [];
    for (let index = 0; index < inputs.len(); index += 1) {
      const input = inputs.get(index);
      spent.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
    expect(spent.sort()).toEqual([...sweptOutRefs(action)]);
  }, 300_000);

  it("takes the sweep through the runner under reconciliation-only authority, which admits it by its kind", async () => {
    const scene = await prover();
    await scene.publish(2, "ef");
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const view = viewOf(scene);
    const objectiveWallet = process.objectiveWallet({
      category: "doubleSpend",
      headerHash: evidence.headerHash,
    });
    objectiveWallet.installProofMaterialsView(view);
    objectiveWallet.installSweepWallet({
      lucid: scene.lucid,
      fundingAddress: scene.fundingAddress,
      view,
    });
    // The sweep is the run's own action, sent through its sweep wallet.
    const submitted: string[] = [];
    const submitTx = scene.emulator.submitTx.bind(scene.emulator);
    vi.spyOn(scene.emulator, "submitTx").mockImplementation(async (cbor) => {
      const txHash = await submitTx(cbor);
      submitted.push(txHash);
      return txHash;
    });
    const chain = createFixtureChain();
    // The header rolled back: the family's thread ended in a conflict.
    const adapter = {
      ...makeAdapter({ chain }),
      observe: async () => CONFLICT,
    };
    const readonly = vi
      .spyOn(actuationAuthority, "workflowRunIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      // The sweep goes out and the ending goes through without waiting for
      // its block.
      const ended = {
        kind: "stalled",
        reason: "chain conflict: a rollback replaced the thread",
      };
      expect(await run({ evidence, adapter, process })).toMatchObject(ended);
      expect(submitted).toHaveLength(1);
      const [sweep] = submitted;
      expect(adapter.preflight).not.toHaveBeenCalled();
      scene.emulator.awaitBlock();
      chain.land(sweep!);
      expect(await scene.at(scene.materialsAddress)).toEqual([]);
      expect(
        (await scene.at(scene.fundingAddress)).filter(
          ({ txHash }) => txHash === sweep,
        ),
      ).toHaveLength(1);
      expect(await run({ evidence, adapter, process })).toMatchObject(ended);
      expect(submitted).toHaveLength(1);
    } finally {
      readonly.mockRestore();
    }
  }, 300_000);

  it("leaves a byte-identical chunk two workflows share to the last of them to end", async () => {
    const scene = await prover();
    const shared = await scene.publish(1, "f0");
    const view = viewOf(scene);
    // Both workflows find the one output by its content.
    const [foundByA] = await view.find(
      shared.datums,
      holderOf("workflow-a", HEADER_A),
    );
    const [foundByB] = await view.find(
      shared.datums,
      holderOf("workflow-b", HEADER_B),
    );
    expect(label(foundByA!)).toBe(`${shared.txHash}#0`);
    expect(label(foundByB!)).toBe(label(foundByA!));

    // A ends first: B still needs the chunk, so A sweeps nothing.
    expect(await sweeping(scene, view, CONFLICT).observe(contextOf())).toEqual(
      CONFLICT,
    );
    expect((await scene.at(scene.materialsAddress)).map(label)).toEqual([
      `${shared.txHash}#0`,
    ]);

    // B ends last and sweeps it.
    const contextB = contextOf({}, "workflow-b", HEADER_B);
    const adapterB = sweeping(scene, view, CONFLICT);
    const action = actionOf(await adapterB.observe(contextB));
    expect(sweptOutRefs(action)).toEqual([`${shared.txHash}#0`]);
    await submitSweep(scene, adapterB, action, { context: contextB });
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
  }, 300_000);

  it("never adopts or sweeps a planted copy with a token, a reference script or a datum hash, and the ending is not held on them", async () => {
    const scene = await prover();
    const datum = datumOf("f1", 0);
    const misshapen = await scene.plantMisshapen(datum);
    const view = viewOf(scene);
    expect(await view.find([datum], holderOf("workflow-a", HEADER_A))).toEqual([
      undefined,
    ]);

    // A plain ada-only output a third party sent is swept with ours; the
    // planted copies stay, so the sweep's one output is a plain coin.
    const thirdParty = await scene.publish(1, "f2");
    const adapter = sweeping(scene, view, CONFLICT);
    const action = actionOf(await adapter.observe(contextOf()));
    expect(sweptOutRefs(action)).toEqual([`${thirdParty.txHash}#0`]);
    const { signed } = await submitSweep(scene, adapter, action);
    expect(() =>
      assertProofMaterialsSweepOutput(signed, scene.fundingAddress),
    ).not.toThrow();
    expect(await adapter.observe(contextOf())).toEqual(CONFLICT);
    expect(
      (await scene.at(scene.materialsAddress))
        .map((utxo) => utxo.txHash)
        .sort(),
    ).toEqual([...misshapen].sort());
  }, 300_000);

  it("never selects a staging output a live thread pins", async () => {
    const scene = await prover();
    const staged = await scene.publish(2, "f3");
    const view = viewOf(scene);
    const pinned = `${staged.txHash}#1`;
    view.need(holderOf("dispute", HEADER_B), [], [pinned]);
    const adapter = sweeping(scene, view, CONFLICT);

    const action = actionOf(await adapter.observe(contextOf()));
    expect(sweptOutRefs(action)).toEqual([`${staged.txHash}#0`]);
    await submitSweep(scene, adapter, action);
    expect(await adapter.observe(contextOf())).toEqual(CONFLICT);
    expect((await scene.at(scene.materialsAddress)).map(label)).toEqual([
      pinned,
    ]);
  }, 300_000);

  it("after a restart sweeps nothing until the first pass at the tip is done and every live objective has observed", async () => {
    const scene = await prover();
    await scene.publish(1, "f4");
    let live: readonly ProofMaterialsObjective[] | undefined = undefined;
    const view = viewOf(scene, () => live);
    const objective = (headerHash: string) => ({
      category: "doubleSpend",
      headerHash,
    });
    const adapterA = sweeping(scene, view, CONFLICT);
    let observedB: FraudProofWorkflowObservation = PENDING;
    const adapterB = sweeping(scene, view, () => observedB);

    // No pass at the tip yet: the live objectives are not known.
    expect(await adapterA.observe(contextOf())).toEqual(CONFLICT);
    expect(view.readiness()).toEqual({
      open: false,
      reason: PROOF_MATERIALS_SWEEP_FIRST_PASS_PENDING,
    });

    // The pass is done with two objectives live; B has not observed.
    live = [objective(HEADER_A), objective(HEADER_B)];
    expect(await adapterA.observe(contextOf())).toEqual(CONFLICT);
    expect(view.readiness()).toMatchObject({
      open: false,
      reason: expect.stringContaining(HEADER_B),
    });
    expect((await scene.at(scene.materialsAddress)).length).toBe(1);

    // Once B has observed, the next ending sweeps.
    const contextB = contextOf({}, "workflow-b", HEADER_B);
    expect(await adapterB.observe(contextB)).toEqual(PENDING);
    expect(view.readiness()).toEqual({ open: true });
    observedB = CONFLICT;
    const action = actionOf(await adapterB.observe(contextB));
    await submitSweep(scene, adapterB, action, { context: contextB });
    expect(await scene.at(scene.materialsAddress)).toEqual([]);
  }, 300_000);
});

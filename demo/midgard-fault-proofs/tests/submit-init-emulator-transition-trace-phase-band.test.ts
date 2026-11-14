import { outRefLabel } from "@al-ft/midgard-core";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildEventToStepMismatchFault,
  buildTransitionFaultProof,
  detectTransitionTraceFaults,
  resolveTransitionTraceDeploymentContracts,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import { phaseForStepIndex } from "../src/transition-trace/phase-band.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  makeHarness,
  setupChallenge,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import {
  funderPaymentKeyHash,
  network,
} from "./support/submit-init-emulator-shared.js";
import { buildForcedAndL2Block } from "./support/transition-trace-phase-band.forced-and-l2-block.js";

/**
 * Lands a forced-plus-L2 block through the production state queue (no
 * admission bypass) and opens a transition-trace thread against it.
 */
const landBlock = async (forcedLast: boolean) => {
  const context = await makeHarness();
  const { harness, publications, transitionTraceReferenceScripts } = context;
  const block = await buildForcedAndL2Block({
    forcedLast,
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(await alignedHeaderStart(harness)),
  });
  const lifecycle = await setupChallenge({
    harness,
    publications,
    transitionTraceReferenceScripts,
    header: block.header,
  });
  expect(lifecycle.setup.headerHash).toBe(block.headerHash);
  return { ...context, block, lifecycle };
};

const submit = async (
  landed: Awaited<ReturnType<typeof landBlock>>,
  proof: ReturnType<typeof buildTransitionFaultProof>,
) =>
  await submitTransitionTraceProof({
    lucid: landed.harness.proverLucid,
    blueprint: landed.harness.realBlueprint,
    deploymentInfo: landed.lifecycle.deploymentInfo,
    network,
    signer: landed.harness.proverSigner,
    threadOutRef: outRefLabel(
      await firstThreadUtxo({
        harness: landed.harness,
        init: landed.lifecycle.init,
      }),
    ),
    proof,
    witnessReferenceScripts: landed.harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });

describe("transition-trace phase-band EventToStepMismatch lifecycle", () => {
  it("proves an L2 transaction stepped in the forced band and removes the block", async () => {
    const landed = await landBlock(true);
    const detections = await detectTransitionTraceFaults(
      landed.block.reconstruction,
    );
    const [first] = detections;
    expect(detections.map(({ invariant }) => invariant)).toEqual([
      "event_to_step_phase_band",
      "event_to_step_phase_band",
    ]);
    if (first === undefined || !first.buildable) {
      throw new Error("band detection is not buildable");
    }
    if (!("EventToStepMismatch" in first.fault)) {
      throw new Error("band detection is not an EventToStepMismatch");
    }
    const { trace_proof, event_to_step } = first.fault.EventToStepMismatch;
    // e2s agrees with the trace; only the header band disagrees.
    expect(trace_proof.value.phase).toBe("L2Transaction");
    expect(
      phaseForStepIndex(landed.block.header, trace_proof.value.step_index),
    ).toBe("ForcedTransaction");
    if (!("EventToStepMembership" in event_to_step)) {
      throw new Error("band witness is not an event_to_step membership");
    }
    expect(event_to_step.EventToStepMembership.membership.value).toEqual({
      step_index: trace_proof.value.step_index,
      phase: trace_proof.value.phase,
    });
    expect(transitionTraceFinalIndex(first.proof)).toBe(0);

    const proofResult = await submit(landed, first.proof);
    await removeAndAssertPermanentProof({
      harness: landed.harness,
      setup: landed.lifecycle.setup,
      deploymentInfo: landed.lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("refuses the same membership witness against a band-consistent step", async () => {
    const landed = await landBlock(false);
    const { reconstruction, header } = landed.block;
    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    const fault = await buildEventToStepMismatchFault({
      reconstruction,
      stepIndex: 0n,
    });
    if (!("EventToStepMismatch" in fault)) {
      throw new Error("expected an EventToStepMismatch fault");
    }
    const { trace_proof, event_to_step } = fault.EventToStepMismatch;
    expect("EventToStepMembership" in event_to_step).toBe(true);
    expect(trace_proof.value.phase).toBe("ForcedTransaction");
    expect(phaseForStepIndex(header, trace_proof.value.step_index)).toBe(
      "ForcedTransaction",
    );
    const proof = buildTransitionFaultProof({ reconstruction, fault });
    expect(transitionTraceFinalIndex(proof)).toBe(0);

    const refusal = await submit(landed, proof).then(
      () => undefined,
      (error: unknown) => String(error),
    );
    // The final transaction's only script spend is the routed thread.
    expect(refusal).toMatch(/failed script execution Spend\[\d+\]/u);

    // The router accepted the proof and moved the thread to final 0; the
    // final-0 EventToStepMismatch check refused it, so no fraud-proof token
    // exists and the block stays in the state queue.
    const { harness, lifecycle } = landed;
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.transitionTrace.finals[0]!.spendingScriptAddress,
        lifecycle.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          resolved.contracts.fraudProof.policyId,
          lifecycle.init.computationThreadAssetName,
        ),
      ),
    ).resolves.toHaveLength(0);
    await expect(
      harness.funderLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        lifecycle.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 180_000);
});

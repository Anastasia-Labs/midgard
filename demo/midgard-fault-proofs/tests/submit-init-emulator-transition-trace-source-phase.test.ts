import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSourcePhaseMismatchFault,
  buildTransitionFaultProof,
  detectTransitionTraceFaults,
  resolveTransitionTraceDeploymentContracts,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
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
 * Lands a forced-plus-L2 block whose header pins the validation-trace pair to
 * the empty tree, through the production state queue, and opens a
 * transition-trace thread against it. `l2Phase` is the phase the trace commits
 * for the L2 transaction.
 */
const landBlock = async (l2Phase: SDK.TransitionPhase) => {
  const context = await makeHarness();
  const { harness, publications, transitionTraceReferenceScripts } = context;
  const block = await buildForcedAndL2Block({
    forcedLast: false,
    l2Phase,
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(await alignedHeaderStart(harness)),
  });
  expect(block.header.validationTracesRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  expect(block.header.validationTraceCount).toBe(0n);
  expect(
    block.header.forcedTransactionCount + block.header.l2TransactionCount,
  ).toBe(2n);
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

describe("transition-trace SourcePhaseMismatch lifecycle", () => {
  it("proves an L2 transaction stepped under the forced phase and removes the block", async () => {
    const landed = await landBlock("ForcedTransaction");
    const detections = await detectTransitionTraceFaults(
      landed.block.reconstruction,
    );
    const source = detections.find(
      ({ invariant }) => invariant === "source_phase_matches_trace_phase",
    );
    if (source === undefined || !source.buildable) {
      throw new Error("source phase detection is not buildable");
    }
    if (!("SourceMembershipMismatch" in source.fault)) {
      throw new Error(
        "source phase detection is not a SourceMembershipMismatch",
      );
    }
    const { witness } = source.fault.SourceMembershipMismatch;
    if (!("SourcePhaseMismatch" in witness)) {
      throw new Error("source phase witness is not a SourcePhaseMismatch");
    }
    expect(witness.SourcePhaseMismatch.trace_proof.value.phase).toBe(
      "ForcedTransaction",
    );
    expect(
      "L2KeyOpening" in witness.SourcePhaseMismatch.source_membership,
    ).toBe(true);
    expect(transitionTraceFinalIndex(source.proof)).toBe(1);

    const proofResult = await submit(landed, source.proof);
    await removeAndAssertPermanentProof({
      harness: landed.harness,
      setup: landed.lifecycle.setup,
      deploymentInfo: landed.lifecycle.deploymentInfo,
      proofResult,
    });
  }, 180_000);

  it("refuses the same source-phase witness against an honest step", async () => {
    const landed = await landBlock("L2Transaction");
    const { reconstruction } = landed.block;
    expect(await detectTransitionTraceFaults(reconstruction)).toEqual([]);
    const fault = await buildSourcePhaseMismatchFault({
      reconstruction,
      stepIndex: 1n,
    });
    if (
      !("SourceMembershipMismatch" in fault) ||
      !("SourcePhaseMismatch" in fault.SourceMembershipMismatch.witness)
    ) {
      throw new Error("expected a SourcePhaseMismatch fault");
    }
    const { trace_proof, source_membership } =
      fault.SourceMembershipMismatch.witness.SourcePhaseMismatch;
    expect(trace_proof.value.phase).toBe("L2Transaction");
    expect("L2KeyOpening" in source_membership).toBe(true);
    const proof = buildTransitionFaultProof({ reconstruction, fault });
    expect(transitionTraceFinalIndex(proof)).toBe(1);

    const refusal = await submit(landed, proof).then(
      () => undefined,
      (error: unknown) => String(error),
    );
    // The final transaction's only script spend is the routed thread.
    expect(refusal).toMatch(/failed script execution Spend\[\d+\]/u);

    // The router accepted the proof and moved the thread to final 1; the
    // source-phase check refused it, so no fraud-proof token exists and the
    // block stays in the state queue.
    const { harness, lifecycle } = landed;
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.transitionTrace.finals[1]!.spendingScriptAddress,
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

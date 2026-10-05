import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { deriveValidationProofItemPublication } from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { selectValidationCompleteItemCarriage } from "../src/validation-dispute/submit/validity.js";
const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/validation-dispute/workflow-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/validation-dispute/workflow-binding.js")
  >()),
  bindValidationTraceDisputeWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyLocalKupmiosL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyLocalKupmiosL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});
import { createManifestBoundValidationTraceDisputeWorkflow } from "../src/validation-dispute/workflow-v1.js";
import { canonicalObservationControls } from "./support/canonical-observation-controls.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { buildInstalledCanonicalFixture } from "./support/installed-canonical-fixture.js";
import { stageInstalledValidationTraceDisputeJourney } from "./support/installed-validation-trace-dispute-journey.js";

it.each([
  { outputSizes: [14_332] },
  { outputSizes: [13_522] },
  { outputSizes: [13_518] },
])(
  "completes the cold installed canonical field route (%j)",
  async ({ outputSizes }) => {
    const journey = await stageInstalledValidationTraceDisputeJourney(
      hooks,
      false,
      true,
      false,
      (input) => buildInstalledCanonicalFixture({ ...input, outputSizes }),
    );
    try {
      const auxiliary =
        journey.fixture.challengerTrace.witnesses[
          journey.fixture.disputedLowIndex
        ]!.auxiliary;
      expect(auxiliary?.kind).toBe("transactionFieldItem");
      if (auxiliary?.kind !== "transactionFieldItem")
        throw new Error("missing canonical chunk");
      expect(auxiliary.fieldIndex).toBe(2);
      expect(auxiliary.fieldPreimage.length).toBeGreaterThan(outputSizes[0]!);
      if (outputSizes.length === 2 && outputSizes[1] === 16_377)
        expect(auxiliary.fieldPreimage.length).toBe(32_768);
      const controls =
        outputSizes.length === 1 && outputSizes[0] === 16_384
          ? canonicalObservationControls(journey)
          : undefined;
      let { result } = await journey.runCold();
      const roles = new Set<string>();
      for (let hop = 0; hop < 250 && result.kind !== "completed"; hop++) {
        await journey.advance(result);
        if (result.kind === "awaiting_counterparty") {
          await journey.operatorResponds();
          journey.emulator.awaitBlock();
        }
        const workflow =
          await createManifestBoundValidationTraceDisputeWorkflow(
            journey.config,
          );
        const stage = await workflow.deriveStage(Date.now());

        if (stage.kind === "semantic_in_flight") roles.add(stage.role);
        await controls?.beforeHop(result.workflowId);
        ({ result } = await journey.runCold());
      }
      expect(result.kind).toBe("completed");
      expect([...roles].sort()).toEqual(
        ["source", "observe", "proof", "settlement"].sort(),
      );
      await controls?.assertComplete(result.workflowId);
      const referenceDelivery =
        selectValidationCompleteItemCarriage(auxiliary.fieldPreimage.length) ===
        "reference";
      const pre =
        journey.fixture.challengerTrace.states[
          journey.fixture.disputedLowIndex
        ]!;
      const expectedPublication = deriveValidationProofItemPublication({
        transactionId: pre.transactionId.toString("hex"),
        transactionCommitment: pre.transactionCommitment.toString("hex"),
        fieldPreimage: auxiliary.fieldPreimage.toString("hex"),
      });
      const proofAddress =
        journey.resolvedContracts.contracts.validationTraceDispute.proofItem
          .spendingScriptAddress;
      const published = (
        await journey.config.lucid.utxosAt(proofAddress)
      ).filter(
        (utxo) =>
          utxo.datum === expectedPublication.datumCbor &&
          utxo.scriptRef == null,
      );
      expect(published).toHaveLength(referenceDelivery ? 1 : 0);
      if (referenceDelivery) {
        const proof = published[0]!;
        let observedByReference = false;
        for (const cbor of journey.recorder.signedCbors.values()) {
          const transaction = CML.Transaction.from_cbor_hex(cbor);
          const bodyRefs = transaction.body().reference_inputs();
          // Plutus reference indices use ledger order, not the CBOR set order.
          const refs = Array.from({ length: bodyRefs?.len() ?? 0 }, (_, i) => ({
            txHash: bodyRefs!.get(i).transaction_id().to_hex(),
            outputIndex: Number(bodyRefs!.get(i).index()),
          })).sort(compareOutRefs);
          let index: number | undefined;
          for (let i = 0; i < refs.length; i++) {
            const input = refs[i]!;
            if (
              input.txHash === proof.txHash &&
              input.outputIndex === proof.outputIndex
            )
              index = i;
          }
          if (index === undefined) continue;
          const redeemers = transaction
            .witness_set()
            .redeemers()
            ?.to_flat_format();
          for (let i = 0; i < (redeemers?.len() ?? 0); i++) {
            const data = Data.from(redeemers!.get(i).data().to_cbor_hex());
            if (
              data instanceof Constr &&
              data.index === 1 &&
              data.fields[0] instanceof Constr &&
              data.fields[0].index === 1 &&
              data.fields[0].fields[2] === BigInt(index)
            )
              observedByReference = true;
          }
        }
        expect(observedByReference).toBe(true);
      }
      let maxBytes = 0;
      let maxMemory = 0n;
      let maxSteps = 0n;
      for (const cbor of journey.recorder.signedCbors.values()) {
        const measured = measureCompleteSignedTransaction(cbor);
        expect(measured.completeSignedBytes).toBeLessThanOrEqual(16_384);
        expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
        expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
        maxBytes = Math.max(maxBytes, measured.completeSignedBytes);
        if (measured.executionMemory > maxMemory)
          maxMemory = measured.executionMemory;
        if (measured.executionSteps > maxSteps)
          maxSteps = measured.executionSteps;
      }
      console.log("canonical installed completed", {
        maxBytes,
        maxMemory: maxMemory.toString(),
        maxSteps: maxSteps.toString(),
        fieldBytes: auxiliary.fieldPreimage.length,
        lowIndex: journey.fixture.disputedLowIndex,
        roles: [...roles],
      });
    } finally {
      await journey.cleanup();
    }
  },
  600_000,
);

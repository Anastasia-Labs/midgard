import { join } from "node:path";

import { expect, it, vi } from "vitest";
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
import { CML } from "@lucid-evolution/lucid";

import { type ValidationTraceDisputeRetainedRouteInput } from "../src/validation-dispute/workflow-engine.js";
import { createManifestBoundValidationTraceDisputeWorkflow } from "../src/validation-dispute/workflow-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type JournalJsonObject,
} from "../src/workflow/journal.js";
import { type FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { stageInstalledValidationTraceDisputeJourney } from "./support/installed-validation-trace-dispute-journey.js";
import { testRepeatedRequiredSignerCarriage } from "./validation-dispute-certified-installed.repeated-required-signers.js";

it.each([false, true])(
  "publishes/certifies the actual 318 field and recovers cold (spent prepared publication: %s)",
  async (spendPreparedPublication) => {
    const journey = await stageInstalledValidationTraceDisputeJourney(
      hooks,
      false,
      true,
    );
    const auxiliary =
      journey.fixture.challengerTrace.witnesses[
        journey.fixture.disputedLowIndex
      ]!.auxiliary;
    expect(auxiliary?.kind).toBe("transactionFieldChunk");
    if (auxiliary?.kind !== "transactionFieldChunk")
      throw new Error("missing authentic signature source");
    expect(auxiliary.fieldIndex).toBe(7);
    expect(auxiliary.fieldPreimage.length).toBe(32757);
    const journal = new DirectoryFraudProofWorkflowJournalStore(
      join(journey.directory, "journal"),
    );
    let publicationReceiptChecked = false;
    let preparedBindingChecked = false;
    let frozenBindingChecked = false;
    let spentPublication = false;
    try {
      let { result } = await journey.runCold();
      for (let hop = 0; hop < 250 && result.kind !== "completed"; hop++) {
        journey.emulator.awaitBlock();
        if (result.kind === "awaiting_counterparty") {
          await journey.operatorResponds();
          journey.emulator.awaitBlock();
        }
        const entries = await journal.load(result.workflowId);
        const latest = [...entries]
          .reverse()
          .find(({ event }) => event.kind === "submission_intent")?.event;
        if (
          latest?.kind === "submission_intent" &&
          latest.actionInput.stage === "publish_field_carriage" &&
          !publicationReceiptChecked
        ) {
          const workflow =
            await createManifestBoundValidationTraceDisputeWorkflow(
              journey.config,
            );
          const recovery = structuredClone(
            latest.durableRecovery!.fieldCarriageRecovery,
          ) as JournalJsonObject;
          const payload = recovery.fieldCarriage as JournalJsonObject;
          const [hash, index] = (payload.outRef as string).split("#");
          const forgedRecovery = {
            ...recovery,
            fieldCarriage: {
              ...payload,
              outRef: `${hash}#${Number(index) + 1}`,
            },
          };
          await expect(
            workflow.fieldCarriage.prerequisite.reconcile({
              headerHash: journey.setup.headerHash,
              txHash: latest.txHash,
              action: latest.actionInput
                .fieldCarriageAction as unknown as FraudProofWorkflowAction,
              artifact: {},
              durableRecovery: forgedRecovery,
            }),
          ).rejects.toThrow(
            "authenticated publication output differs from its journaled content identity",
          );
          publicationReceiptChecked = true;
        }
        if (
          latest?.kind === "submission_intent" &&
          latest.actionInput.stage === "prepare_selected" &&
          !frozenBindingChecked
        ) {
          const workflow =
            await createManifestBoundValidationTraceDisputeWorkflow(
              journey.config,
            );
          const stage = await workflow.deriveStage(Date.now());
          if (stage.kind !== "semantic_pending")
            throw new Error(
              "prepared signature route did not reach semantic resolution",
            );
          const route = latest.actionInput
            .durableRouteInput as JournalJsonObject;
          const binding = route.fieldCarriageBinding as JournalJsonObject;
          const forged = {
            ...route,
            fieldCarriageBinding: { ...binding, referenceOutRefs: [] },
          } as unknown as ValidationTraceDisputeRetainedRouteInput;
          await expect(
            workflow.actuator.capture({
              action: {
                stage: "semantic_resolution",
                threadOutRef: stage.threadOutRef,
              },
              material: workflow.material!,
              retained: forged,
            }),
          ).rejects.toThrow("references changed after preparation");
          frozenBindingChecked = true;
          if (spendPreparedPublication) {
            const candidates = await journey.config.lucid.utxosAt(
              journey.config.signer.address,
            );
            const publication = candidates.find(
              (utxo) =>
                (binding.referenceOutRefs as readonly string[]).includes(
                  `${utxo.txHash}#${utxo.outputIndex}`,
                ) &&
                utxo.datum != null &&
                utxo.scriptRef == null,
            );
            if (publication === undefined)
              throw new Error(
                "prepared field publication was not retained at its actual owner",
              );
            const unsigned = await journey.config.lucid
              .newTx()
              .collectFrom([publication])
              .pay.ToAddress(journey.config.signer.address, {
                lovelace: 2_000_000n,
              })
              .complete({ localUPLCEval: true });
            const signed = await unsigned.sign.withWallet().complete();
            await signed.submit();
            journey.emulator.awaitBlock();
            spentPublication = true;
          }
        }
        if (
          latest?.kind === "submission_intent" &&
          latest.actionInput.stage === "semantic_resolution"
        ) {
          const route = latest.actionInput
            .durableRouteInput as JournalJsonObject;
          const binding = route.fieldCarriageBinding as JournalJsonObject;
          expect(binding.fieldIndex).toBe(7);
          expect(binding.referenceOutRefs).toHaveLength(5); // Three source publications, certificate, semantic script.
          const refs = CML.Transaction.from_cbor_hex(
            journey.recorder.signedCbors.get(latest.txHash)!,
          )
            .body()
            .reference_inputs()!;
          const outRefs = Array.from({ length: refs.len() }, (_, index) => {
            const input = refs.get(index);
            return `${input.transaction_id().to_hex()}#${input.index()}`;
          });
          expect(outRefs.sort()).toEqual(binding.referenceOutRefs);
          preparedBindingChecked = true;
        }
        ({ result } = await journey.runCold());
      }
      expect(result.kind).toBe("completed");
      const entries = await journal.load(result.workflowId);
      const intents = entries.filter(
        ({ event }) => event.kind === "submission_intent",
      );
      expect(
        intents.filter(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionInput.stage === "publish_field_carriage",
        ),
      ).toHaveLength(spendPreparedPublication ? 4 : 3);
      expect(
        intents.filter(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionInput.stage === "certify_field_carriage",
        ),
      ).toHaveLength(1);
      expect(publicationReceiptChecked).toBe(true);
      expect(preparedBindingChecked).toBe(true);
      expect(frozenBindingChecked).toBe(true);
      expect(spentPublication).toBe(spendPreparedPublication);
      expect(
        intents.filter(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionInput.stage === "cancel_semantic_route",
        ),
      ).toHaveLength(spendPreparedPublication ? 1 : 0);
    } finally {
      await journey.cleanup();
    }
  },
  600_000,
);

testRepeatedRequiredSignerCarriage(hooks);

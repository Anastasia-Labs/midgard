import { join } from "node:path";

import { expect } from "vitest";

import { createManifestBoundValidationTraceDisputeWorkflow } from "../../src/validation-dispute/workflow-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type JournalJsonObject,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";
import type { stageInstalledValidationTraceDisputeJourney } from "./installed-validation-trace-dispute-journey.js";

type Journey = Awaited<
  ReturnType<typeof stageInstalledValidationTraceDisputeJourney>
>;

/** Source receipts remain strict; Option B observation can reacquire spent carriage. */
export const canonicalObservationControls = (journey: Journey) => {
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    join(journey.directory, "journal"),
  );
  let checkedReceipt = false;
  let spent = false;
  return {
    beforeHop: async (workflowId: string) => {
      const entries = await journal.load(workflowId);
      const latest = [...entries]
        .reverse()
        .find(({ event }) => event.kind === "submission_intent")?.event;
      if (latest?.kind !== "submission_intent") return;
      const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
        journey.config,
      );
      if (
        !checkedReceipt &&
        latest.actionInput.stage === "publish_field_carriage"
      ) {
        const recovery = structuredClone(
          latest.durableRecovery!.fieldCarriageRecovery,
        ) as JournalJsonObject;
        const payload = recovery.fieldCarriage as JournalJsonObject;
        const [hash, index] = (payload.outRef as string).split("#");
        await expect(
          workflow.fieldCarriage.prerequisite.reconcile({
            headerHash: journey.setup.headerHash,
            txHash: latest.txHash,
            action: latest.actionInput
              .fieldCarriageAction as unknown as FraudProofWorkflowAction,
            artifact: {},
            durableRecovery: {
              ...recovery,
              fieldCarriage: {
                ...payload,
                outRef: `${hash}#${Number(index) + 1}`,
              },
            },
          }),
        ).rejects.toThrow(
          "authenticated publication output differs from its journaled content identity",
        );
        checkedReceipt = true;
      }
      if (!spent && latest.actionInput.stage === "certify_field_carriage") {
        const stage = await workflow.deriveStage(Date.now());
        expect(stage).toMatchObject({
          kind: "semantic_in_flight",
          role: "observe",
          canonicalOutput: true,
        });
        const candidates = await journey.config.lucid.utxosAt(
          journey.config.signer.address,
        );
        const publication = candidates.find(
          (utxo) => utxo.datum != null && utxo.scriptRef == null,
        );
        if (publication === undefined)
          throw new Error(
            "canonical publication was not retained at its actual owner",
          );
        journey.config.signer.selectWallet(journey.config.lucid);
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
        spent = true;
      }
    },
    assertComplete: async (workflowId: string) => {
      expect(checkedReceipt).toBe(true);
      expect(spent).toBe(true);
      const entries = await journal.load(workflowId);
      const intents = entries.flatMap(({ event }) =>
        event.kind === "submission_intent" ? [event] : [],
      );
      expect(
        intents.filter(
          (event) => event.actionInput.stage === "publish_field_carriage",
        ),
      ).toHaveLength(3);
      expect(
        intents.filter(
          (event) => event.actionInput.stage === "certify_field_carriage",
        ),
      ).toHaveLength(1);
      expect(
        intents.filter(
          (event) => event.actionInput.stage === "cancel_semantic_route",
        ),
      ).toHaveLength(0);
    },
  };
};

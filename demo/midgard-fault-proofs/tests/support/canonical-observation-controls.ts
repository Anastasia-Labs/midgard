import { join } from "node:path";

import { expect } from "vitest";

import { createManifestBoundValidationTraceDisputeWorkflow } from "../../src/validation-dispute/workflow-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type JournalJsonObject,
} from "../../src/workflow/journal.js";
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
  let spentPublicationOutRef: string | undefined;
  let spentPublicationActionId: string | undefined;
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
          latest.durableRecovery!,
        ) as JournalJsonObject;
        const payload = recovery.fieldCarriage as JournalJsonObject;
        const [hash, index] = (payload.outRef as string).split("#");
        await expect(
          workflow.fieldCarriage.prerequisite.reconcile({
            headerHash: journey.setup.headerHash,
            txHash: latest.txHash,
            action: {
              actionId: latest.actionId,
              input: latest.actionInput,
            },
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
        const publicationOutRef = `${publication.txHash}#${publication.outputIndex}`;
        const publicationIntent = entries.find(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionInput.stage === "publish_field_carriage" &&
            (event.durableRecovery?.fieldCarriage as JournalJsonObject)
              ?.outRef === publicationOutRef,
        );
        if (publicationIntent?.event.kind !== "submission_intent")
          throw new Error("spent publication has no exact journaled intent");
        const publicationTxHash = publicationIntent.event.txHash;
        expect(
          entries.some(
            ({ event }) =>
              event.kind === "confirmed" && event.txHash === publicationTxHash,
          ),
        ).toBe(true);
        spentPublicationOutRef = publicationOutRef;
        spentPublicationActionId = publicationIntent.event.actionId;
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
      const publications = intents.filter(
        (event) => event.actionInput.stage === "publish_field_carriage",
      );
      expect(publications).toHaveLength(3);
      const replacements = intents.filter(
        (event) =>
          event.actionInput.stage === "publish_field_carriage" &&
          event.actionInput.replacementOutRef !== undefined,
      );
      // Every hop reopens the journal: one replacement intent demonstrates
      // that the consumed-output identity stays stable across cold restarts.
      expect(replacements).toHaveLength(1);
      expect(spentPublicationOutRef).toBeDefined();
      expect(spentPublicationActionId).toBeDefined();
      expect(publications[2]!.actionId).toBe(replacements[0]!.actionId);
      expect(replacements[0]!.actionInput.replacementOutRef).toBe(
        spentPublicationOutRef,
      );
      expect(replacements[0]!.actionId).not.toBe(spentPublicationActionId);
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

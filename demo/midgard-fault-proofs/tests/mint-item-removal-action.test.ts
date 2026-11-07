import { midgardFieldCommitment } from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { createMintItemNonCanonicalCentralJournalAdapter } from "../src/mint-item-non-canonical/central-journal.js";
import { prepareMintItemEvidence } from "../src/mint-item-non-canonical/mint-item-non-canonical.js";
import {
  createManifestBoundMintItemNonCanonicalRuntime,
  createMintItemNonCanonicalRawL1StageResolver,
} from "../src/mint-item-non-canonical/workflow.js";
import * as removal from "../src/remove-fraudulent-block.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type { FraudProofPreSubmitBoundary } from "../src/workflow/transaction-boundary.js";
import { mintField, mintItem } from "./support/mint-item-vectors.js";

const target = `${"11".repeat(32)}#0`;
const proof = `${"22".repeat(32)}#0`;
const descendant = `${"88".repeat(32)}#0`;
afterEach(() => vi.restoreAllMocks());

const builtRemoval = (
  spent: string,
  referenced = proof,
): Parameters<FraudProofPreSubmitBoundary>[0] => {
  const input = (outRef: string) =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(outRef.slice(0, 64)),
      BigInt(outRef.slice(65)),
    );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(spent));
  const refs = CML.TransactionInputList.new();
  refs.add(input(referenced));
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    500_000_000n,
  );
  body.set_reference_inputs(refs);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    referenceScripts: [],
    signed: { toTransaction: () => transaction },
  } as unknown as Parameters<FraudProofPreSubmitBoundary>[0];
};

const fixture = (firstRemoval = target) => {
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const store = {
    load: async () => entries,
    append: async (entry: FraudProofWorkflowJournalEntry) => {
      entries.push(entry);
    },
  };
  const openJournal = () =>
    createMintItemNonCanonicalCentralJournalAdapter({
      store,
      deploymentFingerprint: "33".repeat(32),
      headerHash: "44".repeat(28),
      decisionDigest: "55".repeat(32),
      transactionConfirmed: async () => true,
    });
  const config = {
    binding: {
      blueprint: {},
      deploymentInfo: {},
      network: "Custom",
      definition: { headerHash: "44".repeat(28) },
    },
    lucid: {},
    signer: {},
  } as unknown as Parameters<
    typeof createManifestBoundMintItemNonCanonicalRuntime
  >[0]["config"];
  let nextRemovalOutRef = firstRemoval;
  let removed = false;
  const resolveStage = createMintItemNonCanonicalRawL1StageResolver({
    config,
    l1: {
      observe: async () => ({
        stage: {
          kind: "proof_token",
          stateQueueBlockOutRef: target,
          nextRemovalOutRef,
          fraudProofOutRef: proof,
        },
      }),
    } as unknown as Parameters<
      typeof createMintItemNonCanonicalRawL1StageResolver
    >[0]["l1"],
    source: {} as Parameters<
      typeof createMintItemNonCanonicalRawL1StageResolver
    >[0]["source"],
  });
  const prepare = vi.spyOn(
    funding,
    "prepareWorkflowFundingReservationTransaction",
  );
  const begin = vi.spyOn(funding, "beginWorkflowFundingReservationAction");
  const submit = vi
    .spyOn(removal, "submitRemoveFraudulentBlock")
    .mockImplementation(async ({ preSubmitBoundary }) => {
      if (nextRemovalOutRef === descendant) {
        await preSubmitBoundary!(builtRemoval(descendant));
        nextRemovalOutRef = target;
      }
      const built = builtRemoval(target);
      await preSubmitBoundary!(built);
      removed = true;
      return { txHash: built.txHash } as Awaited<
        ReturnType<typeof removal.submitRemoveFraudulentBlock>
      >;
    });
  const fieldPreimage = mintField(mintItem(), mintItem());
  const evidence = prepareMintItemEvidence({
    finding: {
      subject: acceptedVerdictSubject("77".repeat(32)),
      fieldIndex: 5,
      itemIndex: 1,
    },
    fieldPreimage,
    committedFieldHashHex:
      midgardFieldCommitment(fieldPreimage).toString("hex"),
  });
  return {
    run: () => {
      const centralJournal = openJournal();
      return createManifestBoundMintItemNonCanonicalRuntime({
        config,
        journal: centralJournal.familyJournal,
        observe: async () => (removed ? "removed" : "proven"),
        resolveStage,
        centralJournal,
        stateQueueMutationLeaseCoordinator:
          {} as removal.StateQueueMutationLeaseCoordinator,
      }).runOrResume(evidence);
    },
    setNextRemoval: (outRef: string) => {
      nextRemovalOutRef = outRef;
    },
    entries,
    prepare,
    begin,
    submit,
  };
};

it("binds the runner's removal funding action to its authenticated target and proof", async () => {
  const value = fixture();
  await expect(value.run()).resolves.toBe("removed");
  expect(value.prepare).toHaveBeenCalledOnce();
  const action = value.prepare.mock.calls[0]![0].action;
  expect(action.input).toMatchObject({
    actionKind: "remove",
    nextRemovalOutRef: target,
    fraudProofOutRef: proof,
  });
  expect(
    value.begin.mock.calls.every(
      ([input]) => input.action.input.actionKind === "remove",
    ),
  ).toBe(true);
  expect(value.entries.at(-1)!.event).toMatchObject({
    kind: "confirmed",
    txHash: builtRemoval(target).txHash,
  });
});

it("records and confirms each descendant separately before the target removal", async () => {
  const value = fixture(descendant);
  await expect(value.run()).resolves.toBe("removed");
  expect(
    value.prepare.mock.calls.map(([input]) => input.action.input),
  ).toMatchObject([
    {
      actionKind: "remove",
      nextRemovalOutRef: descendant,
      fraudProofOutRef: proof,
      sourceStage: "proven",
      targetStage: "proven",
    },
    {
      actionKind: "remove",
      nextRemovalOutRef: target,
      fraudProofOutRef: proof,
      sourceStage: "proven",
      targetStage: "removed",
    },
  ]);
  const intents = value.entries.filter(
    ({ event }) => event.kind === "submission_intent",
  );
  expect(intents).toHaveLength(2);
  expect(intents[0]!.event).not.toMatchObject({
    actionId: (intents[1]!.event as { actionId: string }).actionId,
  });
  expect(
    value.entries.filter(({ event }) => event.kind === "confirmed"),
  ).toHaveLength(2);
});

it.each(["target", "proof"] as const)(
  "refuses a signed transaction substituting the authenticated %s",
  async (changed) => {
    const value = fixture();
    value.submit.mockImplementationOnce(async ({ preSubmitBoundary }) => {
      await preSubmitBoundary!(
        builtRemoval(
          changed === "target" ? descendant : target,
          changed === "proof" ? descendant : proof,
        ),
      );
      throw new Error("must stop before submission");
    });
    await expect(value.run()).rejects.toThrow(
      "removal changed its authenticated inputs",
    );
    expect(value.prepare).not.toHaveBeenCalled();
    expect(
      value.entries.some(({ event }) => event.kind === "submission_intent"),
    ).toBe(false);
  },
);

it("resumes after an included descendant without resubmitting it", async () => {
  const value = fixture(descendant);
  value.submit.mockImplementationOnce(async ({ preSubmitBoundary }) => {
    await preSubmitBoundary!(builtRemoval(descendant));
    value.setNextRemoval(target);
    throw new Error("process stopped after descendant inclusion");
  });
  await expect(value.run()).rejects.toThrow(
    "process stopped after descendant inclusion",
  );
  expect(
    value.entries.filter(({ event }) => event.kind === "submission_intent"),
  ).toHaveLength(1);
  expect(value.entries.some(({ event }) => event.kind === "confirmed")).toBe(
    false,
  );
  await expect(value.run()).resolves.toBe("removed");
  expect(
    value.prepare.mock.calls.map(
      ([input]) => input.action.input.nextRemovalOutRef,
    ),
  ).toEqual([descendant, target]);
  expect(
    value.entries.filter(({ event }) => event.kind === "confirmed"),
  ).toHaveLength(2);
});

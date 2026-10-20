/**
 * Lands a field's carriage the way a workflow does: through the production
 * field-carriage prerequisite, each publication and certificate a journaled
 * action whose signed body names the out-refs the workflow reads back. The
 * journal the actions are confirmed into is the one a test's later captures
 * read their carriage from.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type {
  Emulator,
  LucidEvolution,
  Network,
  UTxO,
} from "@lucid-evolution/lucid";

import { resolveFaultProofFieldPreimageCertificate } from "../../src/field-opening.certify-fault-proof-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "../../src/field-opening.plan-fault-proof-field-opening.js";
import { resolveFaultProofFieldCarriagePublications } from "../../src/field-opening.publish-fault-proof-field-carriage.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
} from "../../src/workflow/field-carriage-prerequisite.js";
import { workflowFieldCarriageOwnOutputs } from "../../src/workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "../../src/workflow/raw-l1-publication-observation.js";
import { submitCapturedTransaction } from "../../src/workflow/transaction-boundary.js";
import type { createConfirmedActionJournal } from "./confirmed-action-journal.js";

/** Bounds the prerequisite's turns: one per publication batch and certificate. */
const MAX_TURNS = 256;

/** Reads one expected output by its out-ref from the emulator ledger. */
export const emulatorPublicationObserver = (
  lucid: Pick<LucidEvolution, "utxosByOutRef">,
): FraudProofAuthenticatedPublicationObserver => ({
  observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  observeExact: async (input) => {
    const [txHash, index] = input.expectedOutRef.split("#");
    const [found] = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(index) },
    ]);
    return found === undefined ||
      found.address !== input.address ||
      found.datum !== input.expectedDatumCbor ||
      (input.expectedUnit !== undefined &&
        found.assets[input.expectedUnit] !== 1n)
      ? { kind: "not_found" }
      : { kind: "confirmed", outRef: input.expectedOutRef };
  },
});

export const installFieldCarriage = async ({
  category,
  headerHash,
  lucid,
  emulator,
  network,
  signer,
  requirement,
  journal,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly lucid: LucidEvolution;
  readonly emulator: Pick<Emulator, "awaitBlock">;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly requirement: FieldCarriageRequirement & {
    readonly planned: FaultProofFieldOpeningPlan;
  };
  readonly journal: ReturnType<typeof createConfirmedActionJournal>;
}): Promise<{
  readonly chunks: readonly UTxO[];
  readonly certificate: UTxO | undefined;
}> => {
  const port = createAuthenticatedFieldCarriagePrerequisitePort({
    category,
    lucid,
    network,
    signer,
    publications: emulatorPublicationObserver(lucid),
    requirementForAction: () => requirement,
    transactionConfirmed: async () => false,
  });
  const baseAction = {
    actionId: `${category}:field-${requirement.planned.plan.fieldIndex.toString()}`,
    input: { stage: "field_carriage" },
  };
  for (let turn = 0; ; turn += 1) {
    if (turn === MAX_TURNS)
      throw new Error("the field-carriage prerequisite never settled");
    const inspected = await port.inspect({
      headerHash,
      baseAction,
      artifact: {},
      entries: journal.entries(),
    });
    if (inspected.kind === "satisfied" || inspected.kind === "not_required")
      break;
    if (inspected.kind !== "required")
      throw new Error(`field prerequisite is ${inspected.kind}`);
    const { transaction, durableRecovery } = await port.capture({
      headerHash,
      action: inspected.action,
      artifact: {},
      entries: journal.entries(),
    });
    await submitCapturedTransaction(transaction);
    emulator.awaitBlock(1);
    const reconciled = await port.reconcile({
      headerHash,
      action: inspected.action,
      artifact: {},
      txHash: transaction.txHash,
      durableRecovery,
    });
    if (reconciled.kind !== "confirmed")
      throw new Error(`field prerequisite action is ${reconciled.kind}`);
    journal.confirm({
      action: inspected.action,
      txHash: transaction.txHash,
      durableRecovery,
    });
  }
  const ownOutputs = workflowFieldCarriageOwnOutputs({
    category,
    entries: journal.entries(),
  });
  const chunks = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
    lucid,
    planned: requirement.planned,
    publisherAddress: signer.address,
  });
  if (chunks === undefined)
    throw new Error("the installed field carriage is not readable");
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    ownOutputs,
    lucid,
    network,
    planned: requirement.planned,
    certificatePolicyId: requirement.certificate.policyId,
  });
  if (
    requirement.planned.plan.tier === "Certified" &&
    certificate === undefined
  )
    throw new Error("the installed field certificate is not readable");
  return { chunks, certificate };
};

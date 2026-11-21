/**
 * Lands a field's carriage the way a workflow does: through the production
 * field-carriage prerequisite, each publication and certificate an action
 * landed on the emulator before the next inspection. A test's later captures
 * find the carriage at the proof-materials address by content.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
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
import { createAuthenticatedFieldCarriagePrerequisitePort } from "../../src/workflow/field-carriage-prerequisite.create-authenticated-field-carriage-prerequisite-port.js";
import type { FieldCarriageRequirement } from "../../src/workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import { workflowFieldCarriageOwnOutputs } from "../../src/workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
  type FraudProofAuthenticatedPublicationObserver,
} from "../../src/workflow/raw-l1-publication-observation.js";
import { submitCapturedTransaction } from "../../src/workflow/transaction-boundary.js";
import {
  TEST_PROOF_MATERIALS_HOLDER,
  testProofMaterials,
} from "./proof-materials-holder.js";

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
  });
  const baseAction = {
    actionId: `${category}:field-${requirement.planned.plan.fieldIndex.toString()}`,
    input: { stage: "field_carriage" },
  };
  for (let turn = 0; ; turn += 1) {
    if (turn === MAX_TURNS)
      throw new Error("the field-carriage prerequisite never settled");
    const inspected = await port.inspect({
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      baseAction,
      artifact: {},
    });
    if (inspected.kind === "satisfied" || inspected.kind === "not_required")
      break;
    if (inspected.kind !== "required")
      throw new Error(`field prerequisite is ${inspected.kind}`);
    const { transaction } = await port.capture({
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      action: inspected.action,
      artifact: {},
    });
    await submitCapturedTransaction(transaction);
    emulator.awaitBlock(1);
  }
  const ownOutputs = workflowFieldCarriageOwnOutputs({
    category,
    materials: testProofMaterials(signer, lucid),
  });
  const chunks = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
    lucid,
    planned: requirement.planned,
    publisherAddress: signer.proofMaterialsAddress,
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

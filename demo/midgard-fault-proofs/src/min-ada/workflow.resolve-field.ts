import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import type { OwnOutputJournal } from "../workflow/own-outputs.js";
import type { MinAdaContracts } from "./contracts.js";
import { admitMinAdaWorkflowArtifact as admitMinAdaArtifact } from "./workflow-artifact.js";

type AdmittedMinAdaArtifact = Awaited<ReturnType<typeof admitMinAdaArtifact>>;

export type MinAdaWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  yields: Readonly<{ tx: UTxO }>;
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"minAda">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MinAdaContracts;
  references: MinAdaWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export const isForced = (
  admitted: AdmittedMinAdaArtifact,
): admitted is Extract<AdmittedMinAdaArtifact, { forcedSource: unknown }> =>
  "forcedSource" in admitted;

const witnessSet = (admitted: AdmittedMinAdaArtifact) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    (isForced(admitted)
      ? decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence
      : decodeMidgardNativeTxFullFromCanonicalCbor)(
      Buffer.from(admitted.prepared.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

export const txFieldPlan = (admitted: AdmittedMinAdaArtifact, owner: string) =>
  planFaultProofFieldOpening({
    anchorSourceKind: isForced(admitted) ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: admitted.prepared.outputItemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    label: "min-ada transaction field 2",
  });

export const resolveField = async ({
  config,
  admitted,
  ownOutputs,
}: {
  readonly config: BoundConfig;
  readonly admitted: AdmittedMinAdaArtifact;
  /** Our carriage claims, which the carriage is read by. */
  readonly ownOutputs: OwnOutputJournal;
}) => {
  const planned = txFieldPlan(admitted, config.signer.paymentKeyHash);
  const publications = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("min-ada field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    ownOutputs,
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("min-ada field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
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
import type { NativeScriptInvalidContracts } from "./contracts.js";
import { nativeScriptInvalidUsesDirectRoute } from "./evidence-machine.js";
import { admitNativeScriptInvalidWorkflowArtifact } from "./workflow-artifact.js";

export type NativeScriptInvalidWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: NativeScriptInvalidContracts;
  references: NativeScriptInvalidWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export const buffers = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

export const witnessSet = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    (admitted.forced !== undefined
      ? decodeMidgardForcedTxFullFromCanonicalCbor
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

export const scriptFieldPlan = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: admitted.forced !== undefined ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.scriptWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash: admitted.witnessSetHash,
    label: "native-script-invalid field 6",
  });

export const signerFieldPlan = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: admitted.forced !== undefined ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.addrWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash: admitted.witnessSetHash,
    label: "native-script-invalid field 7",
  });

export const isDirect = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
): boolean =>
  nativeScriptInvalidUsesDirectRoute({
    signerCount: admitted.prepared.addrWitnessItemCbors.length,
    scriptBytes: decodeMidgardVersionedScript(
      Buffer.from(admitted.prepared.scriptItemCbor, "hex"),
    ).scriptBytes.length,
  });

export const resolveField = async ({
  config,
  plan,
}: {
  readonly config: BoundConfig;
  readonly plan: ReturnType<typeof scriptFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("native-script-invalid field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned: plan,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("native-script-invalid field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

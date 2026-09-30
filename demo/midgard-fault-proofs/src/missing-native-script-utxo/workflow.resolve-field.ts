import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { admitMissingNativeScriptUtxoArtifact } from "./artifact.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts.js";

export type MissingNativeScriptUtxoWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptUtxo">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MissingNativeScriptUtxoContracts;
  references: MissingNativeScriptUtxoWorkflowReferenceScripts;
  historicalCorpus(): HistoricalNativeScriptCorpus;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Admitted = ReturnType<typeof admitMissingNativeScriptUtxoArtifact>;

export const bytes = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

export const witnessSet = (admitted: Admitted) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
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

export const spendInputs = (admitted: Admitted): readonly MidgardTxInput[] =>
  admitted.artifact.spendInputs.map((input) => ({
    tx_id: input.tx_id,
    output_index: BigInt(input.output_index),
  }));

export const spendFieldPlan = (admitted: Admitted, owner: string) =>
  planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: bytes(admitted.prepared.spendInputItemCbors),
    owner,
    publish: true,
    label: "missing-native-script-utxo field 0",
  });

export const scriptFieldPlan = (admitted: Admitted, owner: string) =>
  planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: bytes(admitted.prepared.scriptWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash:
      admitted.prepared.txInclusion.nativeTx.witness_set_hash,
    label: "missing-native-script-utxo field 6",
  });

export const direct = (admitted: Admitted): boolean =>
  admitted.prepared.scriptWitnessItemCbors.length <=
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT;

export const resolveField = async ({
  config,
  planned,
}: {
  readonly config: BoundConfig;
  readonly planned: ReturnType<typeof spendFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error(
      "missing-native-script-utxo field publications disappeared",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("missing-native-script-utxo certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

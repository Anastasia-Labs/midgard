import {
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { MintAuthorizationClaimEvidence } from "@al-ft/midgard-sdk";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import { createStructuredDataPreimageRequirement } from "../workflow/raw-datum-preimage.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import { admitMintAuthorizationWorkflowArtifact } from "./artifact.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import { mintAuthorizationEvaluationPreimage } from "./evaluate.js";
import { buildMintAuthorizationStep02Evidence } from "./evidence.js";

export type MintAuthorizationWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"mintAuthorization">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MintAuthorizationContracts;
  references: MintAuthorizationWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Prepared = Awaited<
  ReturnType<typeof admitMintAuthorizationWorkflowArtifact>
>;

export const witnessSet = (admitted: Prepared) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

export const planMintAuthorizationWorkflowField = (
  admitted: Prepared,
  owner: string,
  stage: unknown,
) => {
  const fieldIndex =
    stage === "step_02"
      ? MIDGARD_FIELD_INDEX.mint
      : stage === "step_03"
        ? admitted.finding.direction === 0n
          ? MIDGARD_FIELD_INDEX.scriptWitnesses
          : MIDGARD_FIELD_INDEX.addressWitnesses
        : stage === "step_04"
          ? MIDGARD_FIELD_INDEX.referenceInputs
          : null;
  if (fieldIndex === null) return null;
  const items =
    fieldIndex === MIDGARD_FIELD_INDEX.mint
      ? admitted.mintItemCbors
      : fieldIndex === MIDGARD_FIELD_INDEX.scriptWitnesses
        ? admitted.scriptWitnessItemCbors
        : fieldIndex === MIDGARD_FIELD_INDEX.addressWitnesses
          ? admitted.addrWitnessItemCbors
          : admitted.referenceInputItemCbors;
  return planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex,
    anchorTxId: admitted.txInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.nativeTxCompactCbor,
    itemCbors: items.map((item) => Buffer.from(item, "hex")),
    owner,
    ...(fieldIndex >= MIDGARD_FIELD_INDEX.scriptWitnesses
      ? {
          witnessSet: witnessSet(admitted),
          anchorWitnessSetHash: admitted.witnessSet,
        }
      : {}),
    label: `mint authorization field ${fieldIndex}`,
  });
};

export const mintAuthorizationWorkflowFieldRequirement = (
  admitted: Prepared,
  owner: string,
  stage: unknown,
  certificate: FieldCarriageRequirement["certificate"],
): FieldCarriageRequirement | null => {
  const planned = planMintAuthorizationWorkflowField(admitted, owner, stage);
  if (planned === null) return null;
  return {
    planned,
    compactCbor: admitted.nativeTxCompactCbor,
    witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
      ),
    ).witnessSetCompactCbor.toString("hex"),
    certificate,
  };
};

export const mintAuthorizationWorkflowRawRequirement = async (
  admitted: Prepared,
  stage: unknown,
) => {
  if (stage === "step_02") {
    const evidence = await buildMintAuthorizationStep02Evidence({
      reconstruction: admitted.current,
      eventKey: {
        L2TransactionEventKey: { tx_id: admitted.txInclusion.nativeTxId },
      },
    });
    const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(
        {
          header: admitted.current.header,
          event_to_step_membership: evidence.eventToStepMembership,
          transition_step_membership: evidence.transitionStepMembership,
          policy_index: admitted.finding.policyIndex,
          direction: admitted.finding.direction,
        },
        MintAuthorizationClaimEvidence,
      ),
    );
    return preimageHex.length <= 6000 * 2
      ? null
      : createStructuredDataPreimageRequirement({ preimageHex });
  }
  if (stage !== "step_03" && stage !== "step_06" && stage !== "step_07")
    return null;
  const preimage =
    admitted.finding.direction === 0n
      ? encodeMidgardFieldPreimage(
          admitted.scriptWitnessItemCbors.map((item) =>
            Buffer.from(item, "hex"),
          ),
        )
      : mintAuthorizationEvaluationPreimage(
          Buffer.from(admitted.finding.scriptBytesHex!, "hex"),
          encodeMidgardFieldPreimage(
            admitted.addrWitnessItemCbors.map((item) =>
              Buffer.from(item, "hex"),
            ),
          ),
        );
  if (
    preimage.length === 0 ||
    preimage.length > (admitted.finding.direction === 0n ? 32_768 : 65_536)
  )
    throw new Error(
      "mint authorization preimage exceeds the canonical 32768-byte domain",
    );
  return createRawDatumPreimageRequirement({ preimage });
};

export const resolveField = async ({
  config,
  plan,
}: {
  readonly config: BoundConfig;
  readonly plan: NonNullable<
    ReturnType<typeof planMintAuthorizationWorkflowField>
  >;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("mint-authorization field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned: plan,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("mint-authorization field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

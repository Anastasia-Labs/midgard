import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type FieldOpening } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import type { ObserversForbiddenContracts } from "../../src/observers-forbidden-on-untagged-network/contracts.js";
import { type ObserversForbiddenEvidence } from "../../src/observers-forbidden-on-untagged-network/family.js";
import {
  ObserversForbiddenStep02DatumSchema,
  ObserversForbiddenStep02RedeemerSchema,
} from "../../src/observers-forbidden-on-untagged-network/schemas.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { FAMILY } from "./observers-forbidden-on-untagged-network-raw.submit-observers-forbidden-step01-forced-raw.js";

/**
 * Step 02 without the off-chain `observersForbiddenEvidenceCloses` and bound
 * network-scalar guards, and with the opening exposed: an honest verdict
 * reaches `terminal_contradiction_v1`, and `mutateOpening` rewrites the
 * redeemer's opening after every off-chain check has passed. Carriage must
 * already be published (and certified when the tier requires it) exactly as
 * the production builder expects.
 */
export const submitObserversForbiddenStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
  mutateOpening = (opening) => opening,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserversForbiddenEvidence;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<{ subject: unknown; network_id: bigint }>({
    threadUtxo,
    signer,
    schema: ObserversForbiddenStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.observerFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: `${FAMILY} raw field 3`,
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error(`${FAMILY} raw: field carriage is not published`);
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error(`${FAMILY} raw: field certificate is not published`);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...(witnessReferenceScripts.computationThreadMint === undefined
      ? []
      : [witnessReferenceScripts.computationThreadMint]),
    ...(witnessReferenceScripts.fraudProofMint === undefined
      ? []
      : [witnessReferenceScripts.fraudProofMint]),
  ];
  const opening = mutateOpening(
    faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw field 3`,
    }),
    referenceInputs,
  );
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserversForbiddenStep02RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      observer_opening: opening,
    }),
    referenceScriptUtxo,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/** Patch a certified carriage's reference-input coordinates. */
export const mutateCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  }) => {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  },
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("Certified" in carriage))
    throw new Error("certified carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        Certified: patch({
          cert_ref_input_index: carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/**
 * Point a published (RawUtxo) carriage at a different reference input. A
 * published plan promotes the inline tier to RawUtxo, so a small field's
 * bytes are always read from the named reference input; naming another one
 * substitutes the bytes the door commits.
 */
export const mutateRawUtxoCarriage = (
  opening: FieldOpening,
  offset: bigint,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  const carriage = opening.BodyFieldOpening.carriage;
  if (!("RawUtxo" in carriage)) throw new Error("raw-utxo carriage expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      carriage: {
        RawUtxo: {
          ref_input_index: carriage.RawUtxo.ref_input_index + offset,
        },
      },
    },
  };
};

/** Replace the compact transaction bytes the opening is anchored to. */
export const mutateCompactSource = (
  opening: FieldOpening,
  nativeTxCompactCbor: string,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      native_tx_compact_cbor: nativeTxCompactCbor,
    },
  };
};

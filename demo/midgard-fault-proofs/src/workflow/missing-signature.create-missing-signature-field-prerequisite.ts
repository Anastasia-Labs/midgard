import { encodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  planMissingSignatureAddressWitnessesOpening,
  planMissingSignatureRequiredSignersOpening,
} from "../missing-signature/evidence.js";
import {
  admitMissingSignatureForcedArtifact,
  MISSING_SIGNATURE_FORCED_ARTIFACT,
} from "../missing-signature/forced-artifact.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { createAuthenticatedFieldCarriagePrerequisitePort } from "./field-carriage-prerequisite.js";
import { admitMissingSignatureArtifact } from "./missing-signature.admit-missing-signature-artifact.js";
import {
  type AdmittedMissingSignatureArtifact,
  record,
} from "./missing-signature.parse-artifact.js";

export const createMissingSignatureFieldPrerequisite = ({
  lucid,
  network,
  signer,
  publications,
  certificate,
  certificateReference,
  transactionConfirmed,
}: {
  readonly lucid: LucidEvolution;
  readonly network: import("@lucid-evolution/lucid").Network;
  readonly signer: ResolvedProverSigner;
  readonly publications: import("./raw-l1-publication-observation.js").FraudProofAuthenticatedPublicationObserver;
  readonly certificate: {
    readonly policyId: string;
    readonly mintingScript: import("@lucid-evolution/lucid").MintingPolicy;
  };
  readonly certificateReference: UTxO | undefined;
  readonly transactionConfirmed: (input: {
    headerHash: string;
    txHash: string;
  }) => Promise<boolean>;
}) => {
  const certificateOf = () => {
    if (certificateReference === undefined)
      throw new Error(
        "missing-signature installed workflow omitted certificate mint reference",
      );
    return {
      policyId: certificate.policyId,
      mintingScript: certificate.mintingScript,
      referenceScriptUtxo: certificateReference,
    };
  };
  /**
   * The accepted path's step-02 and step-04 fields, planned exactly as their
   * builders plan them from the thread state step 01 binds: the field
   * carriage either needs is published and journaled before the step.
   */
  const acceptedRequirement = (
    stage: "step_02" | "step_04",
    admitted: AdmittedMissingSignatureArtifact,
  ) => {
    const anchorTxId = admitted.txInclusion.nativeTxId;
    const planned =
      stage === "step_02"
        ? planMissingSignatureRequiredSignersOpening({
            anchorSourceKind: 0n,
            anchorTxId,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            requiredSignerHashes: admitted.requiredSignerHashes,
            owner: signer.paymentKeyHash,
          })
        : planMissingSignatureAddressWitnessesOpening({
            anchorSourceKind: 0n,
            anchorTxId,
            nativeTxCompactCbor: admitted.nativeTxCompactCbor,
            addrTxWits: admitted.addrTxWits,
            witnessSet: admitted.witnessSetCompact,
            anchorWitnessSetHash:
              admitted.txInclusion.nativeTx.witness_set_hash,
            owner: signer.paymentKeyHash,
          });
    // Inline carriage has nothing to publish or certify.
    if (
      planned.plan.publications.length === 0 &&
      planned.plan.tier !== "Certified"
    )
      return null;
    return {
      planned,
      compactCbor: admitted.nativeTxCompactCbor,
      certificate: certificateOf(),
    };
  };
  return createAuthenticatedFieldCarriagePrerequisitePort({
    category: "missingSignature",
    lucid: lucid,
    network: network,
    signer: signer,
    publications: publications,
    requirementForAction: async ({ action, artifact }) => {
      const input = record(
        action.input,
        "missing-signature prerequisite action",
      );
      if (artifact.schemaVersion !== MISSING_SIGNATURE_FORCED_ARTIFACT) {
        if (input.stage !== "step_02" && input.stage !== "step_04") return null;
        return acceptedRequirement(
          input.stage,
          await admitMissingSignatureArtifact(artifact),
        );
      }
      if (input.stage !== "step_06" && input.stage !== "step_07") return null;
      const prepared = await admitMissingSignatureForcedArtifact(artifact);
      if (input.stage === "step_07" && prepared.witnessIndex === -1n)
        return null;
      const planned =
        input.stage === "step_06"
          ? planMissingSignatureRequiredSignersOpening({
              anchorSourceKind: 1n,
              anchorTxId: prepared.transactionId,
              nativeTxCompactCbor: prepared.nativeTxCompactCbor,
              requiredSignerHashes: prepared.evidence.requiredSignerHashes,
              owner: signer.paymentKeyHash,
            })
          : planMissingSignatureAddressWitnessesOpening({
              anchorSourceKind: 1n,
              anchorTxId: prepared.transactionId,
              nativeTxCompactCbor: prepared.nativeTxCompactCbor,
              addrTxWits: prepared.evidence.addrTxWits,
              witnessSet: prepared.witnessSetCompact,
              anchorWitnessSetHash: prepared.verifiedWitnessSetHash,
              owner: signer.paymentKeyHash,
            });
      return {
        planned,
        compactCbor: prepared.nativeTxCompactCbor,
        witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact({
          addrTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.addr_tx_wits_hash,
            "hex",
          ),
          scriptTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.script_tx_wits_hash,
            "hex",
          ),
          redeemerTxWitsHash: Buffer.from(
            prepared.witnessSetCompact.redeemer_tx_wits_hash,
            "hex",
          ),
        }).toString("hex"),
        certificate: certificateOf(),
      };
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await transactionConfirmed({ headerHash, txHash }),
  });
};

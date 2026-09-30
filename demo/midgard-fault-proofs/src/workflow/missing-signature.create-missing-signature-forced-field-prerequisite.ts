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
import { record } from "./missing-signature.parse-artifact.js";

export const createMissingSignatureForcedFieldPrerequisite = ({
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
      if (
        artifact.schemaVersion !== MISSING_SIGNATURE_FORCED_ARTIFACT ||
        (input.stage !== "step_06" && input.stage !== "step_07")
      )
        return null;
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
      const referenceScriptUtxo = certificateReference;
      if (referenceScriptUtxo === undefined)
        throw new Error(
          "missing-signature installed forced path omitted certificate mint reference",
        );
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
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo,
        },
      };
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await transactionConfirmed({ headerHash, txHash }),
  });
};

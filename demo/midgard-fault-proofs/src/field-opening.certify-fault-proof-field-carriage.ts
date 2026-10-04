import {
  layOutMidgardFieldCarriage,
  type MidgardFieldCarriagePlan,
} from "@al-ft/midgard-core";
import {
  buildUnsignedFieldPreimageCertificationProgram,
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
} from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  credentialToAddress,
  type LucidEvolution,
  type MintingPolicy,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

export const fieldPreimageCertificateAddress = ({
  network,
  certificatePolicyId,
}: {
  readonly network: Network;
  readonly certificatePolicyId: string;
}): string =>
  credentialToAddress(network, {
    type: "Script",
    hash: certificatePolicyId,
  });

/** Finds the exact mint-welded tier-3 manifest, never a token-only match. */
export const resolveFaultProofFieldPreimageCertificate = async ({
  lucid,
  network,
  planned,
  certificatePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId: string;
}): Promise<UTxO | undefined> => {
  if (planned.plan.tier !== "Certified") return undefined;
  const certification = deriveFieldPreimageCertification(planned.plan);
  const unit = `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  const candidates = await lucid.utxosAt(
    fieldPreimageCertificateAddress({ network, certificatePolicyId }),
  );
  return candidates
    .filter(
      (candidate) =>
        candidate.datum === certification.datumCbor &&
        candidate.datumHash == null &&
        candidate.scriptRef == null &&
        candidate.assets[unit] === 1n,
    )
    .sort((left, right) =>
      left.txHash === right.txHash
        ? left.outputIndex - right.outputIndex
        : left.txHash < right.txHash
          ? -1
          : 1,
    )[0];
};

export type CertifiedFaultProofFieldCarriage = {
  readonly txHash: string;
  readonly certificateUtxo: UTxO;
};

/**
 * Strict tier-3 certification transaction. The certificate policy is carried
 * only by its hash-checked published reference script; inline attachment is
 * unavailable on this production path.
 */
export const certifyFaultProofFieldCarriage = async ({
  lucid,
  network,
  signer,
  planned,
  certificatePolicyId,
  certificateMintingScript,
  certificateReferenceScriptUtxo,
  chunkUtxos,
  compactCbor,
  witnessSetCompactCbor = "",
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly planned: Readonly<{
    readonly plan: MidgardFieldCarriagePlan;
    readonly sourceKind: 0n | 1n;
  }>;
  readonly certificatePolicyId: string;
  readonly certificateMintingScript: MintingPolicy;
  readonly certificateReferenceScriptUtxo: UTxO;
  readonly chunkUtxos: readonly UTxO[];
  readonly compactCbor: string;
  readonly witnessSetCompactCbor?: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<CertifiedFaultProofFieldCarriage> => {
  if (planned.plan.tier !== "Certified") {
    throw new Error("field-preimage certification requires a tier-3 plan");
  }
  const certificateAddress = fieldPreimageCertificateAddress({
    network,
    certificatePolicyId,
  });
  const certification = deriveFieldPreimageCertification(planned.plan);
  signer.selectWallet(lucid);
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimageCertificationProgram(lucid, {
      sourceKind: planned.sourceKind,
      plan: planned.plan,
      certificatePolicyId,
      certificateAddress,
      certificateWitness: {
        kind: "reference_script",
        referenceUtxo: certificateReferenceScriptUtxo,
      },
      chunkUtxos,
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "field-preimage-certificate-mint",
        utxo: certificateReferenceScriptUtxo,
        expectedScript: certificateMintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  const output = signed.toTransaction().body().outputs().get(0);
  const produced = coreToTxOutput(output);
  const unit = `${certificatePolicyId}${certification.assetNameHex}`;
  if (
    produced.address !== certificateAddress ||
    produced.datum !== certification.datumCbor ||
    produced.assets[unit] !== 1n
  ) {
    throw new Error(
      "field-preimage certification transaction output 0 does not carry the planned manifest/token",
    );
  }
  const certificateUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: { txHash, outputIndex: 0 },
    label: "field-preimage certificate",
  });
  return { txHash, certificateUtxo };
};

/**
 * The reference inputs a step transaction must read for one planned opening, in
 * §8.4 order.
 *
 * Under tier 3 the certificate comes first and the chunks follow, which is the
 * order `layOutMidgardFieldCarriage` produces and the order a certificate's
 * digest vector is written in. Deriving the ordering from the layout rather than
 * from a hand-written list is what keeps the two from drifting.
 */
export const faultProofFieldCarriageReferenceOrder = (
  planned: FaultProofFieldOpeningPlan,
): readonly number[] =>
  layOutMidgardFieldCarriage({ plan: planned.plan }).referenceInputIndices;

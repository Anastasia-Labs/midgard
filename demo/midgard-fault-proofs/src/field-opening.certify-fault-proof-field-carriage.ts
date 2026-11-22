import type { MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk/core";
import {
  buildUnsignedFieldPreimageCertificationProgram,
  deriveFieldPreimageCertification,
} from "@al-ft/midgard-sdk/state-queue";
import {
  credentialToAddress,
  type LucidEvolution,
  type MintingPolicy,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { PrerequisiteTurn } from "./fault-proof-turn.js";
import { findFieldCertificate } from "./field-opening.own-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import { fieldCarriagePlanTransactions } from "./field-opening.publish-fault-proof-field-carriage.js";
import type { ResolvedProverSigner } from "./runtime.js";
import { finishStep } from "./step-completion.js";
import { workflowReferenceScript } from "./workflow/transaction-boundary.js";

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

/**
 * The live tier-3 certificate for this opening, found by content
 * (`findFieldCertificate`), whoever minted it. Undefined under tiers 1-2 or
 * when there is none.
 */
export const resolveFaultProofFieldPreimageCertificate = async ({
  lucid,
  network,
  planned,
  certificatePolicyId,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAtWithUnit">;
  readonly network: Network;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId: string;
}): Promise<UTxO | undefined> =>
  planned.plan.tier !== "Certified"
    ? undefined
    : await findFieldCertificate({
        lucid,
        address: fieldPreimageCertificateAddress({
          network,
          certificatePolicyId,
        }),
        datumCbor: deriveFieldPreimageCertification(planned.plan).datumCbor,
        unit: `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
      });

/**
 * Strict tier-3 certification transaction, built and signed but not
 * submitted. The certificate policy is carried only by its hash-checked
 * published reference script; inline attachment is unavailable on this
 * production path.
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
}): Promise<PrerequisiteTurn> => {
  if (planned.plan.tier !== "Certified") {
    throw new Error("field-preimage certification requires a tier-3 plan");
  }
  const certificateAddress = fieldPreimageCertificateAddress({
    network,
    certificatePolicyId,
  });
  signer.selectWallet(lucid);
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimageCertificationProgram(lucid, {
      sourceKind: planned.sourceKind,
      plan: planned.plan,
      certificatePolicyId,
      certificateAddress,
      certificatePolicyReferenceUtxo: certificateReferenceScriptUtxo,
      chunkUtxos,
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  return {
    kind: "certificate",
    transaction: finishStep(signed, {
      referenceScripts: [
        workflowReferenceScript({
          role: "field-preimage-certificate-mint",
          utxo: certificateReferenceScriptUtxo,
          expectedScript: certificateMintingScript,
        }),
      ],
    }),
    label: "field-preimage certification",
    plan: fieldCarriagePlanTransactions(planned.plan),
  };
};

/**
 * The certificate a step's opening references: the live one
 * `resolveFaultProofFieldPreimageCertificate` finds for this exact opening,
 * and otherwise the transaction that mints one.
 */
export const reuseOrCertifyFaultProofFieldCarriage = async ({
  certificateReferenceScriptUtxo,
  missingReferenceScript,
  ...input
}: Omit<
  Parameters<typeof certifyFaultProofFieldCarriage>[0],
  "certificateReferenceScriptUtxo"
> & {
  /** Read only when a certificate has to be minted. */
  readonly certificateReferenceScriptUtxo: UTxO | undefined;
  /** The refusal when minting is needed and the reference script is absent. */
  readonly missingReferenceScript: string;
}): Promise<UTxO | PrerequisiteTurn> => {
  const onChain = await resolveFaultProofFieldPreimageCertificate(input);
  if (onChain !== undefined) return onChain;
  if (certificateReferenceScriptUtxo === undefined)
    throw new Error(missingReferenceScript);
  return await certifyFaultProofFieldCarriage({
    ...input,
    certificateReferenceScriptUtxo,
  });
};

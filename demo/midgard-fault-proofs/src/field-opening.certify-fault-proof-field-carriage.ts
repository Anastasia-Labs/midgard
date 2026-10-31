import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { buildUnsignedFieldPreimageCertificationProgram } from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  type MintingPolicy,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  fieldCertificateSlotOf,
  lucidOwnOutputReader,
} from "./field-opening.own-field-carriage.js";
import { type FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  claimOwnOutputs,
  type OwnOutputJournal,
  resolveOwnOutputs,
} from "./workflow/own-outputs.js";
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

/**
 * Our live tier-3 certificate for this opening, read by the out-ref claimed
 * for it in `ownOutputs`; undefined when there is none.
 */
export const resolveFaultProofFieldPreimageCertificate = async ({
  lucid,
  network,
  planned,
  certificatePolicyId,
  ownOutputs,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosByOutRef">;
  readonly network: Network;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId: string;
  readonly ownOutputs: OwnOutputJournal;
}): Promise<UTxO | undefined> => {
  if (planned.plan.tier !== "Certified") return undefined;
  const { key, slot } = fieldCertificateSlotOf({
    plan: planned.plan,
    certificateAddress: fieldPreimageCertificateAddress({
      network,
      certificatePolicyId,
    }),
    certificatePolicyId,
  });
  const resolution = await resolveOwnOutputs({
    slots: [{ slot, claims: ownOutputs.claims(key) }],
    read: lucidOwnOutputReader(lucid),
  });
  return resolution.kind === "ready" ? resolution.utxos[0] : undefined;
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
  ownOutputs,
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
  /** Where the certificate's out-ref is claimed before submission. */
  readonly ownOutputs?: OwnOutputJournal;
}): Promise<CertifiedFaultProofFieldCarriage> => {
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
  const { key, slot } = fieldCertificateSlotOf({
    plan: planned.plan,
    certificateAddress,
    certificatePolicyId,
  });
  const [claim] = claimOwnOutputs({ signed, slots: [slot] });
  if (!claim!.outRef.endsWith("#0")) {
    throw new Error(
      "field-preimage certification transaction output 0 does not carry the planned manifest/token",
    );
  }
  ownOutputs?.claim(key, claim!.outRef);
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
  const certificateUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: { txHash, outputIndex: 0 },
    label: "field-preimage certificate",
  });
  return { txHash, certificateUtxo };
};

/**
 * The certificate a step's opening references: ours, already live for this
 * exact opening (claimed in `ownOutputs` by an earlier certify), and a newly
 * minted one, claimed there, only when none is.
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
  /** Our certificate claims; a newly minted certificate is claimed here. */
  readonly ownOutputs: OwnOutputJournal;
}): Promise<UTxO> => {
  const onChain = await resolveFaultProofFieldPreimageCertificate(input);
  if (onChain !== undefined) return onChain;
  if (certificateReferenceScriptUtxo === undefined)
    throw new Error(missingReferenceScript);
  return (
    await certifyFaultProofFieldCarriage({
      ...input,
      certificateReferenceScriptUtxo,
    })
  ).certificateUtxo;
};

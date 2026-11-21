import type { MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk/core";
import { buildUnsignedFieldPreimageCertificationProgram } from "@al-ft/midgard-sdk/state-queue";
import {
  credentialToAddress,
  type LucidEvolution,
  type MintingPolicy,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { PrerequisiteTurn } from "./fault-proof-turn.js";
import {
  fieldCertificateSlotOf,
  lucidOwnOutputReader,
} from "./field-opening.own-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import { fieldCarriagePlanTransactions } from "./field-opening.publish-fault-proof-field-carriage.js";
import type { ResolvedProverSigner } from "./runtime.js";
import { finishStep } from "./step-completion.js";
import { walletOnlyValidTo } from "./wallet-only-validity.js";
import {
  claimOwnOutputs,
  type OwnOutputJournal,
  resolveOwnOutputs,
} from "./workflow/own-outputs.js";
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
 * The live tier-3 certificate for this opening: the one read by the out-ref
 * claimed for it in `ownOutputs`, and otherwise, for a workflow (whose
 * `ownOutputs` carries its proof-materials access and keeps no claims
 * across a restart), the first output by out-ref at the certificate address
 * holding exactly this opening's datum and token, whoever minted it: the
 * certificate policy mints the token only beside that datum checked.
 * Undefined when there is none.
 */
export const resolveFaultProofFieldPreimageCertificate = async ({
  lucid,
  network,
  planned,
  certificatePolicyId,
  ownOutputs,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosByOutRef" | "utxosAtWithUnit">;
  readonly network: Network;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId: string;
  readonly ownOutputs: OwnOutputJournal;
}): Promise<UTxO | undefined> => {
  if (planned.plan.tier !== "Certified") return undefined;
  const certificateAddress = fieldPreimageCertificateAddress({
    network,
    certificatePolicyId,
  });
  const { key, slot } = fieldCertificateSlotOf({
    plan: planned.plan,
    certificateAddress,
    certificatePolicyId,
  });
  const resolution = await resolveOwnOutputs({
    slots: [{ slot, claims: ownOutputs.claims(key) }],
    read: lucidOwnOutputReader(lucid),
  });
  if (resolution.kind === "ready") return resolution.utxos[0];
  if (ownOutputs.materials === undefined) return undefined;
  const [first] = (
    await lucid.utxosAtWithUnit(
      certificateAddress,
      `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
    )
  )
    .filter((candidate) => slot.matches(candidate))
    .sort((left, right) =>
      left.txHash === right.txHash
        ? left.outputIndex - right.outputIndex
        : left.txHash < right.txHash
          ? -1
          : 1,
    );
  return first;
};

/**
 * Strict tier-3 certification transaction, built and signed but not
 * submitted; its certificate output is claimed in `ownOutputs` before it is
 * returned. It spends only wallet coins and carries the wallet-only validity
 * window (`walletOnlyValidTo`). The certificate policy is carried only by its hash-checked
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
  /** Where the certificate's out-ref is claimed. */
  readonly ownOutputs?: OwnOutputJournal;
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
      validTo: walletOnlyValidTo(lucid),
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
    output: { outRef: claim!.outRef, label: "field-preimage certificate" },
    plan: fieldCarriagePlanTransactions(planned.plan),
  };
};

/**
 * The certificate a step's opening references: the live one
 * `resolveFaultProofFieldPreimageCertificate` finds for this exact opening,
 * and otherwise the transaction that mints one, claimed in `ownOutputs`.
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

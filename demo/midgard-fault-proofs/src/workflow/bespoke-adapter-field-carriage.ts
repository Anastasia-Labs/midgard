/**
 * Field carriage for the double-spend and network-id adapters, found by
 * content. A planned publication at the proof-materials address is the k-th
 * plain output holding its datum for the k-th slot holding it, found through
 * the workflow's view, whoever paid it, since only the prover can spend there
 * and the step checks content alone; at another address it is the first
 * plainly shaped output holding exactly its datum (inline datum, no datum
 * hash, no reference script, ada only). The certificate is the first output
 * (by out-ref) at the certificate address holding exactly its datum and
 * token, whoever paid it: the certificate policy mints the token only beside
 * that datum checked.
 */
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk/state-queue";
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import { isAdaOnlyInlineDatumOutput } from "../ada-only-carriage-output.js";
import { fieldPreimageCertificateAddress } from "../field-opening.certify-fault-proof-field-carriage.js";
import {
  fieldCertificateSlotOf,
  fieldPublicationSlots,
} from "../field-opening.own-field-carriage.js";
import { type FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  type OwnOutputClaimed,
  type OwnOutputJournal,
  ownOutRefLabel,
  parseOwnOutRef,
} from "./own-outputs.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";

/**
 * The carriage of `planned` as claims: its publications and its certificate
 * found by content.
 */
export const addressFieldCarriageOwnOutputs = async ({
  lucid,
  network,
  publisherAddress,
  planned,
  certificatePolicyId,
  materials,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
  readonly publisherAddress: string;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId?: string;
  /** The workflow's view of its proof-materials address. */
  readonly materials: ProofMaterialsAccess;
}): Promise<OwnOutputJournal> => {
  const kept = new Map<string, OwnOutputClaimed[]>();
  const slots = fieldPublicationSlots({ plan: planned.plan, publisherAddress });
  if (slots.length > 0 && publisherAddress === materials.view.address) {
    const found = await materials.view.find(
      slots.map(({ datumCbor }) => datumCbor),
      materials.holder,
    );
    for (const [index, { key }] of slots.entries()) {
      const utxo = found[index];
      if (utxo !== undefined)
        kept.set(key, [Object.freeze({ outRef: ownOutRefLabel(utxo) })]);
    }
  } else if (slots.length > 0) {
    const candidates = await lucid.utxosAt(publisherAddress);
    const used = new Set<string>();
    for (const { key, datumCbor } of slots) {
      const match = candidates.find(
        (candidate) =>
          !used.has(ownOutRefLabel(candidate)) &&
          candidate.datum === datumCbor &&
          isAdaOnlyInlineDatumOutput(candidate),
      );
      if (match === undefined) continue;
      used.add(ownOutRefLabel(match));
      kept.set(key, [Object.freeze({ outRef: ownOutRefLabel(match) })]);
    }
  }
  if (planned.plan.tier === "Certified" && certificatePolicyId !== undefined) {
    const certificateAddress = fieldPreimageCertificateAddress({
      network,
      certificatePolicyId,
    });
    const { key, slot } = fieldCertificateSlotOf({
      plan: planned.plan,
      certificateAddress,
      certificatePolicyId,
    });
    const [first] = (await lucid.utxosAt(certificateAddress))
      .filter((candidate) => slot.matches(candidate))
      .sort((left, right) =>
        left.txHash === right.txHash
          ? left.outputIndex - right.outputIndex
          : left.txHash < right.txHash
            ? -1
            : 1,
      );
    if (first !== undefined) {
      kept.set(key, [Object.freeze({ outRef: ownOutRefLabel(first) })]);
    }
  }
  return Object.freeze({
    claims: (key: string) => Object.freeze([...(kept.get(key) ?? [])]),
    claim: (key: string, outRef: string) => {
      parseOwnOutRef(outRef);
      kept.set(key, [...(kept.get(key) ?? []), Object.freeze({ outRef })]);
    },
    materials,
  });
};

export type MissingFieldPublication = {
  readonly digest: string;
  readonly datumCbor: string;
  readonly chunkIndex: number;
};

/** The first plan-ordered publication `ownOutputs` holds no claim on. */
export const firstUnclaimedFieldPublication = ({
  planned,
  publisherAddress,
  ownOutputs,
}: {
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly publisherAddress: string;
  readonly ownOutputs: OwnOutputJournal;
}): MissingFieldPublication | undefined => {
  const slots = fieldPublicationSlots({ plan: planned.plan, publisherAddress });
  for (const [index, publication] of planned.plan.publications.entries()) {
    if (ownOutputs.claims(slots[index]!.key).length > 0) continue;
    return {
      digest: publication.digest.toString("hex"),
      datumCbor: fieldPreimagePublicationDatumCbor(publication.bytes),
      chunkIndex: publication.chunkIndex,
    };
  }
  return undefined;
};

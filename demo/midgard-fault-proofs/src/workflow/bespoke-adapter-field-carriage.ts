/**
 * Field carriage for the double-spend and network-id adapters, which do not
 * yet journal the out-refs of their own publish and certify transactions.
 * Until they do, their claims are read off the publisher and certificate
 * addresses by content, as before: each planned publication takes the first
 * unused ada-only output holding its datum, and the certificate the first
 * output (by out-ref) holding exactly its datum and token.
 *
 * Every other caller claims carriage from its own signed transactions
 * (`field-opening.own-field-carriage.ts`); this module goes when these two
 * adapters do the same.
 */
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";
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

/**
 * The carriage of `planned` found at its addresses, as claims; a publication
 * made through the journal is claimed in memory.
 */
export const addressFieldCarriageOwnOutputs = async ({
  lucid,
  network,
  publisherAddress,
  planned,
  certificatePolicyId,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
  readonly publisherAddress: string;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly certificatePolicyId?: string;
}): Promise<OwnOutputJournal> => {
  const kept = new Map<string, OwnOutputClaimed[]>();
  const slots = fieldPublicationSlots({ plan: planned.plan, publisherAddress });
  if (slots.length > 0) {
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

/**
 * Field carriage found by identity: each planned publication and each
 * certificate is a slot, and a slot's outputs are the out-refs claimed for it
 * from our own signed transactions (`workflow/own-outputs.ts`). Nothing here
 * reads an address, so an unrelated output with the same content is never
 * taken for ours.
 *
 * Publication slots are keyed by their position in the plan and their datum,
 * so two identical chunks are two slots with two outputs. A certificate slot
 * is keyed by its datum, which names the opening and its owner.
 */
import { createHash } from "node:crypto";

import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, TxOutput } from "@lucid-evolution/lucid";

import {
  inlineDatumCarriageSlot,
  type OwnOutputJournal,
  type OwnOutputReader,
  type OwnOutputSlot,
  type OwnOutputSlotClaims,
} from "./workflow/own-outputs.js";

const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

/** The claim key of publication `index` of a plan, holding `datumCbor`. */
export const fieldPublicationClaimKey = (
  index: number,
  datumCbor: string,
): string => `field-publication:${index.toString()}:${sha256(datumCbor)}`;

/** The claim key of the certificate holding `datumCbor`. */
export const fieldCertificateClaimKey = (datumCbor: string): string =>
  `field-certificate:${sha256(datumCbor)}`;

/**
 * A certificate slot: `datumCbor` inline at `address`, exactly one `unit`,
 * and nothing else but ada (the steps reference this output, so any other
 * asset would ride into their script context).
 */
export const fieldCertificateSlot = (
  address: string,
  datumCbor: string,
  unit: string,
): OwnOutputSlot =>
  Object.freeze({
    address,
    matches: (output: TxOutput) =>
      output.address === address &&
      output.datum === datumCbor &&
      output.datumHash == null &&
      output.scriptRef == null &&
      output.assets[unit] === 1n &&
      Object.entries(output.assets).every(
        ([asset, quantity]) =>
          asset === "lovelace" || asset === unit || quantity === 0n,
      ),
  });

/** Reads our outputs through the provider, by out-ref alone. */
export const lucidOwnOutputReader =
  (lucid: Pick<LucidEvolution, "utxosByOutRef">): OwnOutputReader =>
  async (outRefs) =>
    await lucid.utxosByOutRef([...outRefs]);

/** One planned publication: its claim key, its slot and the datum it holds. */
export type FieldPublicationSlot = Readonly<{
  key: string;
  slot: OwnOutputSlot;
  datumCbor: string;
}>;

/** The publication slots of `plan` at `publisherAddress`, in plan order. */
export const fieldPublicationSlots = ({
  plan,
  publisherAddress,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly publisherAddress: string;
}): readonly FieldPublicationSlot[] =>
  plan.publications.map((publication, index) => {
    const datumCbor = fieldPreimagePublicationDatumCbor(publication.bytes);
    return Object.freeze({
      key: fieldPublicationClaimKey(index, datumCbor),
      slot: inlineDatumCarriageSlot(publisherAddress, datumCbor),
      datumCbor,
    });
  });

/** The publication slots of `plan` with every claim `ownOutputs` holds on each. */
export const fieldPublicationSlotClaims = ({
  plan,
  publisherAddress,
  ownOutputs,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly publisherAddress: string;
  readonly ownOutputs: OwnOutputJournal;
}): readonly OwnOutputSlotClaims[] =>
  fieldPublicationSlots({ plan, publisherAddress }).map(({ key, slot }) =>
    Object.freeze({ slot, claims: ownOutputs.claims(key) }),
  );

/** The certificate slot of a tier-3 `plan`, and its claim key. */
export const fieldCertificateSlotOf = ({
  plan,
  certificateAddress,
  certificatePolicyId,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly certificateAddress: string;
  readonly certificatePolicyId: string;
}): Readonly<{ key: string; slot: OwnOutputSlot }> => {
  const { datumCbor } = deriveFieldPreimageCertification(plan);
  return Object.freeze({
    key: fieldCertificateClaimKey(datumCbor),
    slot: fieldCertificateSlot(
      certificateAddress,
      datumCbor,
      `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`,
    ),
  });
};

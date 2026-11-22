/**
 * Field carriage found by content. Each planned publication is the plain
 * output holding its datum at the publisher address (`workflow/own-outputs.ts`),
 * so two identical chunks take two outputs. A tier-3 certificate is the
 * output at the certificate address holding exactly its datum and one
 * certificate token, whoever minted it: the certificate policy mints the
 * token only beside that datum checked, and the datum names the owner, who
 * alone retires it.
 */
import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk/state-queue";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

/** The datum of each publication of `plan`, in plan order. */
export const fieldPublicationDatums = (
  plan: MidgardFieldCarriagePlan,
): readonly string[] =>
  plan.publications.map((publication) =>
    fieldPreimagePublicationDatumCbor(publication.bytes),
  );

/**
 * The first output by out-ref at `address` holding `datumCbor` inline, one
 * `unit` and nothing else but ada (the steps reference this output, so any
 * other asset would ride into their script context); undefined when none.
 */
export const findFieldCertificate = async ({
  lucid,
  address,
  datumCbor,
  unit,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAtWithUnit">;
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string;
}): Promise<UTxO | undefined> =>
  (await lucid.utxosAtWithUnit(address, unit))
    .filter(
      (utxo) =>
        utxo.address === address &&
        utxo.datum === datumCbor &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        utxo.assets[unit] === 1n &&
        Object.entries(utxo.assets).every(
          ([asset, quantity]) =>
            asset === "lovelace" || asset === unit || quantity === 0n,
        ),
    )
    .sort(compareOutRefs)[0];

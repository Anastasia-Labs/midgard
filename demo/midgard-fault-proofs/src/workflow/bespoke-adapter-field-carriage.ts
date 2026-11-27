/**
 * The next field publication the double-spend and network-id adapters ask
 * for: the first planned publication whose content `findCarriage` does not
 * find at the publisher address.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import { fieldPublicationDatums } from "../field-opening.own-field-carriage.js";
import { type FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import { findCarriage } from "./own-outputs.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";

export type MissingFieldPublication = {
  readonly digest: string;
  readonly datumCbor: string;
  readonly chunkIndex: number;
};

/** The first plan-ordered publication not found at `publisherAddress`. */
export const firstMissingFieldPublication = async ({
  lucid,
  planned,
  publisherAddress,
  materials,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly publisherAddress: string;
  /** The workflow's view of its proof-materials address. */
  readonly materials: ProofMaterialsAccess;
}): Promise<MissingFieldPublication | undefined> => {
  const datums = fieldPublicationDatums(planned.plan);
  const found = await findCarriage({
    lucid,
    address: publisherAddress,
    datums,
    materials,
  });
  const index = found.findIndex((utxo) => utxo === undefined);
  const publication = planned.plan.publications[index];
  return publication === undefined
    ? undefined
    : {
        digest: publication.digest.toString("hex"),
        datumCbor: datums[index]!,
        chunkIndex: publication.chunkIndex,
      };
};

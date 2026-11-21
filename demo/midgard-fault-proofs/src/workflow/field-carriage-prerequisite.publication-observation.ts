import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  fieldCertificateSlot,
  lucidOwnOutputReader,
} from "../field-opening.own-field-carriage.js";
import {
  inlineDatumCarriageSlot,
  type OwnOutputClaimed,
  readOwnOutputs,
} from "./own-outputs.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

export type OwnCarriageObservation =
  | Readonly<{ kind: "confirmed"; utxo: UTxO }>
  | Readonly<{ kind: "pending" | "absent" }>;

/**
 * Where one carriage slot stands, from our own claims on it alone, the newest
 * first: confirmed when a claim is both authenticated on the current chain
 * and visible to the provider the step is built with; pending when it is only
 * one of the two (one source lags the other); absent when no claim is either.
 */
export const observeOwnCarriage = async ({
  publications,
  lucid,
  headerHash,
  kind,
  address,
  datumCbor,
  unit,
  claims,
}: {
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly lucid: Pick<LucidEvolution, "utxosByOutRef">;
  readonly headerHash: string;
  readonly kind: "field_publication" | "field_certificate";
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string | null;
  readonly claims: readonly OwnOutputClaimed[];
}): Promise<OwnCarriageObservation> => {
  const slot =
    unit === null
      ? inlineDatumCarriageSlot(address, datumCbor)
      : fieldCertificateSlot(address, datumCbor, unit);
  const live = await readOwnOutputs(
    lucidOwnOutputReader(lucid),
    claims.map(({ outRef }) => outRef),
  );
  for (const { outRef } of [...claims].reverse()) {
    const utxo = live.get(outRef);
    const visible = utxo !== undefined && slot.matches(utxo);
    const observed = await publications.observeExact({
      headerHash,
      kind,
      address,
      expectedOutRef: outRef,
      expectedDatumCbor: datumCbor,
      ...(unit === null ? {} : { expectedUnit: unit }),
    });
    if (visible && observed.kind === "confirmed")
      return Object.freeze({ kind: "confirmed", utxo: utxo! });
    if (visible || observed.kind === "confirmed")
      return Object.freeze({ kind: "pending" });
  }
  return Object.freeze({ kind: "absent" });
};

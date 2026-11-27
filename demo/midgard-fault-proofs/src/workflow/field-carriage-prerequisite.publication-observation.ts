import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import type { UTxO } from "@lucid-evolution/lucid";

import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";

export type OwnCarriageObservation =
  | Readonly<{ kind: "confirmed"; utxo: UTxO }>
  | Readonly<{ kind: "pending" | "absent" }>;

/**
 * Where one carriage slot stands from the output its content finds: absent
 * when none; confirmed when that output is authenticated on the current
 * chain; pending when the provider shows it but the chain does not yet.
 */
export const observeOwnCarriage = async ({
  publications,
  headerHash,
  kind,
  address,
  datumCbor,
  unit,
  utxo,
}: {
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly headerHash: string;
  readonly kind: "field_publication" | "field_certificate";
  readonly address: string;
  readonly datumCbor: string;
  readonly unit: string | null;
  readonly utxo: UTxO | undefined;
}): Promise<OwnCarriageObservation> => {
  if (utxo === undefined) return Object.freeze({ kind: "absent" });
  const observed = await publications.observeExact({
    headerHash,
    kind,
    address,
    expectedOutRef: outRefLabel(utxo),
    expectedDatumCbor: datumCbor,
    ...(unit === null ? {} : { expectedUnit: unit }),
  });
  return observed.kind === "confirmed"
    ? Object.freeze({ kind: "confirmed", utxo })
    : Object.freeze({ kind: "pending" });
};

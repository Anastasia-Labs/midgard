import { computeHash32 } from "@al-ft/midgard-core";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";

import { structuredDataPublicationPlan } from "./structured-data-preimage.js";

export type RawDatumPreimageRequirement = Readonly<{
  kind: "raw_datum_preimage" | "structured_data_preimage";
  preimageHex: string;
  publicationDatums: readonly string[];
  publicationDigests: readonly string[];
}>;

const publicationsFor = (preimage: Buffer) => {
  if (preimage.length === 0 || preimage.length > 65_536)
    throw new Error("raw datum preimage must contain 1..65536 bytes");
  const publications = [];
  for (let offset = 0; offset < preimage.length; offset += 15_000) {
    const bytes = preimage.subarray(offset, offset + 15_000);
    publications.push(
      Object.freeze({
        chunkIndex: publications.length,
        bytes,
        digest: computeHash32(bytes),
      }),
    );
  }
  return Object.freeze(publications);
};

/** Immutable bytes and ordered content identities; the consumer authenticates their meaning. */
export const createRawDatumPreimageRequirement = ({
  preimage,
}: {
  readonly preimage: Uint8Array;
}): RawDatumPreimageRequirement => {
  const bytes = Buffer.from(preimage);
  const publications = publicationsFor(bytes);
  return Object.freeze({
    kind: "raw_datum_preimage",
    preimageHex: bytes.toString("hex"),
    publicationDatums: Object.freeze(
      publications.map(({ bytes }) => fieldPreimagePublicationDatumCbor(bytes)),
    ),
    publicationDigests: Object.freeze(
      publications.map(({ digest }) => digest.toString("hex")),
    ),
  });
};

export const rawDatumPreimagePublicationPlan = (
  requirement: RawDatumPreimageRequirement,
) => {
  if (
    Object.keys(requirement).sort().join(",") !==
      "kind,preimageHex,publicationDatums,publicationDigests" ||
    !["raw_datum_preimage", "structured_data_preimage"].includes(
      requirement.kind,
    ) ||
    !/^(?:[0-9a-f]{2})+$/u.test(requirement.preimageHex)
  )
    throw new Error("raw datum preimage requirement is not canonical");
  if (requirement.kind === "structured_data_preimage") {
    const planned = structuredDataPublicationPlan(requirement.preimageHex);
    if (
      JSON.stringify(requirement.publicationDatums) !==
        JSON.stringify(planned.publicationDatums) ||
      JSON.stringify(requirement.publicationDigests) !==
        JSON.stringify(planned.publicationDigests)
    )
      throw new Error("structured evidence publication identities changed");
    return planned;
  }
  const preimage = Buffer.from(requirement.preimageHex, "hex");
  const expected = createRawDatumPreimageRequirement({ preimage });
  if (
    JSON.stringify(requirement.publicationDatums) !==
      JSON.stringify(expected.publicationDatums) ||
    JSON.stringify(requirement.publicationDigests) !==
      JSON.stringify(expected.publicationDigests)
  )
    throw new Error("raw datum preimage publication identities changed");
  return Object.freeze({
    plan: Object.freeze({
      tier: "RawDatums" as const,
      publications: publicationsFor(preimage),
    }),
    publicationDatums: expected.publicationDatums,
    publicationDigests: expected.publicationDigests,
  });
};

export const createStructuredDataPreimageRequirement = ({
  preimageHex,
}: {
  preimageHex: string;
}): RawDatumPreimageRequirement => {
  const planned = structuredDataPublicationPlan(preimageHex);
  return Object.freeze({
    kind: "structured_data_preimage",
    preimageHex,
    publicationDatums: Object.freeze(planned.publicationDatums),
    publicationDigests: Object.freeze(planned.publicationDigests),
  });
};

import { computeHash32 } from "@al-ft/midgard-core";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";

import { structuredDataPublicationPlan } from "./structured-data-preimage.js";

export type RawDatumPreimageRequirement = Readonly<{
  kind:
    | "raw_datum_preimage"
    | "structured_data_preimage"
    | "chunked_raw_datum_preimage";
  preimageHex: string;
  publicationDatums: readonly string[];
  publicationDigests: readonly string[];
}>;

const publicationsFor = (preimage: Buffer, chunkBytes = 15_000) => {
  if (preimage.length === 0 || preimage.length > 65_536)
    throw new Error("raw datum preimage must contain 1..65536 bytes");
  const publications = [];
  for (let offset = 0; offset < preimage.length; offset += chunkBytes) {
    const bytes = preimage.subarray(offset, offset + chunkBytes);
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
const createRawRequirement = ({
  preimage,
  chunkBytes,
}: {
  readonly preimage: Uint8Array;
  readonly chunkBytes: 4096 | 15000;
}): RawDatumPreimageRequirement => {
  const bytes = Buffer.from(preimage);
  const publications = publicationsFor(bytes, chunkBytes);
  return Object.freeze({
    kind:
      chunkBytes === 4096 ? "chunked_raw_datum_preimage" : "raw_datum_preimage",
    preimageHex: bytes.toString("hex"),
    publicationDatums: Object.freeze(
      publications.map(({ bytes }) => fieldPreimagePublicationDatumCbor(bytes)),
    ),
    publicationDigests: Object.freeze(
      publications.map(({ digest }) => digest.toString("hex")),
    ),
  });
};

export const createRawDatumPreimageRequirement = (input: {
  readonly preimage: Uint8Array;
}) => createRawRequirement({ ...input, chunkBytes: 15000 });

/** Fixed native proof carriage chunks, authenticated by the same journal port. */
export const createChunkedRawDatumPreimageRequirement = (input: {
  readonly preimage: Uint8Array;
}) => createRawRequirement({ ...input, chunkBytes: 4096 });

export const rawDatumPreimagePublicationPlan = (
  requirement: RawDatumPreimageRequirement,
) => {
  if (
    Object.keys(requirement).sort().join(",") !==
      "kind,preimageHex,publicationDatums,publicationDigests" ||
    ![
      "raw_datum_preimage",
      "structured_data_preimage",
      "chunked_raw_datum_preimage",
    ].includes(requirement.kind) ||
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
  const chunkBytes =
    requirement.kind === "chunked_raw_datum_preimage" ? 4096 : 15000;
  const expected = createRawRequirement({ preimage, chunkBytes });
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
      publications: publicationsFor(preimage, chunkBytes),
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

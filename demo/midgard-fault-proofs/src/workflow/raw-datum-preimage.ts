import { computeHash32 } from "@al-ft/midgard-core";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";

export type RawDatumPreimageRequirement = Readonly<{
  kind: "raw_datum_preimage";
  preimageHex: string;
  publicationDatums: readonly string[];
  publicationDigests: readonly string[];
}>;

const publicationsFor = (preimage: Buffer) => {
  if (preimage.length === 0 || preimage.length > 32_768)
    throw new Error("raw datum preimage must contain 1..32768 bytes");
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
    requirement.kind !== "raw_datum_preimage" ||
    !/^(?:[0-9a-f]{2})+$/u.test(requirement.preimageHex)
  )
    throw new Error("raw datum preimage requirement is not canonical");
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

import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";

import { sha256 } from "../sha256.js";
import {
  type RawEntry,
  type ReservedAdmissionRequest,
  type SubmitSource,
} from "./txAdmissions.verify-claimed-payload-rows.js";

type ReservedAdmissionVariant = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly txFullHashV1: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly programMaterialSidecarSha256: Buffer;
  readonly submitSource: Exclude<SubmitSource, "backfill">;
  readonly requestIndices: readonly number[];
  readonly variantOrdinal: number;
  readonly firstVariantForTxId: boolean;
};

export type BatchResolvedRawEntry = RawEntry & {
  readonly variant_ordinal: number;
  readonly result_kind: "new" | "duplicate";
};

export const groupReservedAdmissionVariants = (
  requests: readonly ReservedAdmissionRequest[],
): readonly ReservedAdmissionVariant[] => {
  const byTxId = new Map<string, ReservedAdmissionVariant[]>();
  for (let index = 0; index < requests.length; index += 1) {
    const request = requests[index]!;
    const txIdHex = request.txId.toString("hex");
    const variants = byTxId.get(txIdHex) ?? [];
    const matching = variants.find(
      (variant) =>
        variant.txCanonicalCbor.equals(request.txCanonicalCbor) &&
        variant.programMaterialSidecarCbor.equals(
          request.programMaterialSidecarCbor,
        ) &&
        variant.submitSource === request.submitSource,
    );
    if (matching === undefined) {
      variants.push({
        ...request,
        txFullHashV1: computeMidgardNativeTxFullHashFromCanonicalCbor(
          request.txCanonicalCbor,
        ),
        programMaterialSidecarSha256: sha256(
          request.programMaterialSidecarCbor,
        ),
        requestIndices: [index],
        variantOrdinal: -1,
        firstVariantForTxId: variants.length === 0,
      });
      byTxId.set(txIdHex, variants);
    } else {
      (matching.requestIndices as number[]).push(index);
    }
  }
  return [...byTxId.values()]
    .flat()
    .sort((left, right) => Buffer.compare(left.txId, right.txId))
    .map((variant, variantOrdinal) => ({ ...variant, variantOrdinal }));
};

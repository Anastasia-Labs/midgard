import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";

import {
  type RawEntry,
  type ReservedAdmissionRequest,
  type SubmitSource,
} from "./txAdmissions.verify-claimed-payload-rows.js";

type ReservedAdmissionVariant = {
  readonly txId: Buffer;
  readonly txCanonicalCbor: Buffer;
  readonly txFullHashV1: Buffer;
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
        variant.submitSource === request.submitSource,
    );
    if (matching === undefined) {
      variants.push({
        ...request,
        txFullHashV1: computeMidgardNativeTxFullHashFromCanonicalCbor(
          request.txCanonicalCbor,
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

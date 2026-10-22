/**
 * The refusal of an external Order whose retention output is not in the
 * facts (`retention_output_unseen`). Admission references that output on
 * chain, and the facts hold every retention output created at or after the
 * store's origin until it is spent, so the facts lack it because it was
 * created before the origin. The refusal names the referenced outputs the
 * facts do not hold (`txHash#index`), one of which is that output; its
 * `detail` is exactly that list, written and read back here only.
 */
import { compareOutRefs } from "../codec.js";
import type { SqlTx } from "../sql/backend.js";
import type { OutRef } from "../types.js";
import { REFUSALS_TABLE } from "./schema.js";

export const RETENTION_OUTPUT_UNSEEN = "retention_output_unseen";

const OUT_REF_TEXT = /^([0-9a-f]{64})#(0|[1-9][0-9]{0,4})$/;

/** `txHash#index`, the tx hash in lowercase hex. */
const formatOutRef = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/** The detail of an unseen refusal: `absent`, in ledger order, space-separated. */
export const retentionUnseenDetail = (absent: readonly OutRef[]): string => {
  if (absent.length === 0)
    throw new Error("an unseen refusal names at least one absent output");
  return [...absent].sort(compareOutRefs).map(formatOutRef).join(" ");
};

/** The absent outputs an unseen refusal's detail names; throws on any other text. */
const parseRetentionUnseenDetail = (detail: string): OutRef[] =>
  detail.split(" ").map((text) => {
    const match = OUT_REF_TEXT.exec(text);
    const index = Number(match?.[2]);
    if (match === null || index > 0xffff)
      throw new Error(`unseen refusal detail is not an outref list: ${detail}`);
    return { txHash: Buffer.from(match[1]!, "hex"), index };
  });

/**
 * The orders refused as `RETENTION_OUTPUT_UNSEEN` at or before `atSlot`,
 * keyed `${kind}:${event key hex}`, each with the referenced outputs the
 * facts did not hold. A refusal stays in the facts until the prune passes
 * its slot. Throws when a row's detail is not such a list.
 */
export const retentionUnseenRefusalsIn = async (
  tx: SqlTx,
  atSlot: number,
): Promise<ReadonlyMap<string, readonly OutRef[]>> =>
  new Map(
    (
      await tx.query(
        `SELECT kind, event_key, detail FROM ${REFUSALS_TABLE} WHERE reason = ? AND slot <= ?`,
        [RETENTION_OUTPUT_UNSEEN, atSlot],
      )
    ).map((row) => [
      `${String(row.kind)}:${Buffer.from(row.event_key as Uint8Array).toString("hex")}`,
      parseRetentionUnseenDetail(String(row.detail)),
    ]),
  );

import {
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
} from "@al-ft/midgard-core";
import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { UnusedRedeemerEvidenceV1 } from "./family-v1.js";
import {
  UnusedRedeemerAuthenticatedV1Schema,
  UnusedRedeemerReverseScanV1Schema,
} from "./schemas-v1.js";

export type UnusedRedeemerAuthenticatedDatumV1 = Data.Static<
  typeof UnusedRedeemerAuthenticatedV1Schema
>;
export type UnusedRedeemerReverseScanDatumV1 = Data.Static<
  typeof UnusedRedeemerReverseScanV1Schema
>;
const hash = (value: Buffer): string =>
  Effect.runSync(hashHexWithBlake2b(value.toString("hex"), 32));
const integer = (value: bigint): Buffer =>
  Buffer.from(Data.to(value as never, Data.Integer()), "hex");
const bool = (value: boolean): Buffer =>
  Buffer.from(Data.to(value as never, Data.Boolean()), "hex");

export const initialUnusedRedeemerReverseScanV1 = (
  authenticated: UnusedRedeemerAuthenticatedDatumV1,
): UnusedRedeemerReverseScanDatumV1 => ({
  authenticated,
  cursor: 0n,
  used: false,
  checkpoint_hash: hash(
    Buffer.concat([
      Buffer.from("MidgardUnusedRedeemerScanV1", "ascii"),
      Buffer.from(authenticated.bound.subject.transaction_id, "hex"),
      integer(authenticated.bound.redeemer_index),
      Buffer.from(authenticated.redeemer_leaf, "hex"),
      integer(authenticated.purpose_count),
    ]),
  ),
});

export const advanceUnusedRedeemerSelectionsV1 = ({
  state,
  evidence,
  itemBudget = 16,
}: {
  state: UnusedRedeemerReverseScanDatumV1;
  evidence: UnusedRedeemerEvidenceV1;
  itemBudget?: number;
}): UnusedRedeemerReverseScanDatumV1 => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget < 1 || itemBudget > 16)
    throw new Error("unusedRedeemer item budget changed");
  let next = state;
  for (const opening of evidence.selections.slice(
    Number(state.cursor),
    Number(state.cursor) + itemBudget,
  )) {
    if (next.used || opening.frontierIndex !== Number(next.cursor)) break;
    const purposeLeaf = hashMidgardScriptPurposeLeafV1({
      purposeKind: opening.purposeKind,
      purposeIndex: BigInt(opening.purposeIndex),
      scriptHash: Buffer.from(opening.scriptHashHex, "hex"),
      subject: Buffer.from(opening.purposeSubjectHex, "hex"),
    });
    const executionLeaf = hashMidgardScriptExecutionLeafV1({
      languageTag: opening.languageTag,
      purposeLeaf,
      sourceLeaf: Buffer.from(opening.sourceLeafHex, "hex"),
      redeemerLeaf: Buffer.from(opening.redeemerLeafHex, "hex"),
    });
    const cursor = next.cursor + 1n;
    const tag = ([0, 1, 3, 6] as const)[opening.purposeKind];
    const used =
      BigInt(tag) === next.authenticated.purpose_tag &&
      BigInt(opening.purposeIndex) === next.authenticated.pointer_index &&
      opening.redeemerLeafHex === next.authenticated.redeemer_leaf;
    next = {
      ...next,
      cursor,
      used,
      checkpoint_hash: hash(
        Buffer.concat([
          Buffer.from(next.checkpoint_hash, "hex"),
          integer(cursor),
          purposeLeaf,
          executionLeaf,
          bool(used),
        ]),
      ),
    };
  }
  return next;
};

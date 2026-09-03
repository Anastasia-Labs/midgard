import {
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { UnusedRedeemerEvidence } from "./family.js";
import {
  UnusedRedeemerAuthenticatedSchema,
  UnusedRedeemerReverseScanSchema,
} from "./schemas.js";

export type UnusedRedeemerAuthenticatedDatum = Data.Static<
  typeof UnusedRedeemerAuthenticatedSchema
>;
export type UnusedRedeemerReverseScanDatum = Data.Static<
  typeof UnusedRedeemerReverseScanSchema
>;
const hash = (value: Buffer): string =>
  Effect.runSync(hashHexWithBlake2b(value.toString("hex"), 32));
const integer = (value: bigint): Buffer =>
  Buffer.from(Data.to(value as never, Data.Integer()), "hex");
const bool = (value: boolean): Buffer =>
  Buffer.from(Data.to(value as never, Data.Boolean()), "hex");

export const initialUnusedRedeemerReverseScan = (
  authenticated: UnusedRedeemerAuthenticatedDatum,
): UnusedRedeemerReverseScanDatum => ({
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

export const advanceUnusedRedeemerSelections = ({
  state,
  evidence,
  itemBudget = 16,
}: {
  state: UnusedRedeemerReverseScanDatum;
  evidence: UnusedRedeemerEvidence;
  itemBudget?: number;
}): UnusedRedeemerReverseScanDatum => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget < 1 || itemBudget > 16)
    throw new Error("unusedRedeemer item budget changed");
  let next = state;
  for (const opening of evidence.selections.slice(
    Number(state.cursor),
    Number(state.cursor) + itemBudget,
  )) {
    if (next.used || opening.frontierIndex !== Number(next.cursor)) break;
    const purposeLeaf = hashMidgardScriptPurposeLeaf({
      purposeKind: opening.purposeKind,
      purposeIndex: BigInt(opening.purposeIndex),
      scriptHash: Buffer.from(opening.scriptHashHex, "hex"),
      subject: Buffer.from(opening.purposeSubjectHex, "hex"),
    });
    const executionLeaf = hashMidgardScriptExecutionLeaf({
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

import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { UnusedScriptWitnessEvidenceV1 } from "./family-v1.js";
import {
  UnusedScriptAuthenticatedWitnessV1Schema,
  UnusedScriptReverseScanV1Schema,
} from "./schemas-v1.js";

export type UnusedScriptAuthenticatedWitnessDatumV1 = Data.Static<
  typeof UnusedScriptAuthenticatedWitnessV1Schema
>;
export type UnusedScriptReverseScanDatumV1 = Data.Static<
  typeof UnusedScriptReverseScanV1Schema
>;

const hash = (bytes: Buffer): string =>
  Effect.runSync(hashHexWithBlake2b(bytes.toString("hex"), 32));
const integer = (value: bigint): Buffer =>
  Buffer.from(Data.to(value as never, Data.Integer()), "hex");
const domain = Buffer.from("MidgardUnusedScriptWitnessScanV1", "ascii");

export const initialUnusedScriptWitnessReverseScanV1 = (
  witness: UnusedScriptAuthenticatedWitnessDatumV1,
): UnusedScriptReverseScanDatumV1 => ({
  witness,
  alternate_cursor: 0n,
  purpose_cursor: 0n,
  shadowed: false,
  used: false,
  checkpoint_hash: hash(
    Buffer.concat([
      domain,
      Buffer.from(witness.bound.subject.transaction_id, "hex"),
      integer(witness.bound.script_index),
      Buffer.from(witness.script_hash, "hex"),
      integer(witness.purpose_count),
    ]),
  ),
});

export const advanceUnusedScriptWitnessSourcesV1 = ({
  state,
  evidence,
}: {
  state: UnusedScriptReverseScanDatumV1;
  evidence: UnusedScriptWitnessEvidenceV1;
}): UnusedScriptReverseScanDatumV1 => {
  let next = state;
  while (next.alternate_cursor < next.witness.bound.script_index) {
    const source = evidence.sources[Number(next.alternate_cursor)];
    if (
      source === undefined ||
      source.originKind !== 0 ||
      source.sourceIndex !== Number(next.alternate_cursor)
    )
      throw new Error("unusedScriptWitness earlier source frontier changed");
    const cursor = next.alternate_cursor + 1n;
    next = {
      ...next,
      alternate_cursor: cursor,
      shadowed:
        next.shadowed || source.scriptHashHex === next.witness.script_hash,
      checkpoint_hash: hash(
        Buffer.concat([
          Buffer.from(next.checkpoint_hash, "hex"),
          integer(cursor),
          Buffer.from(source.scriptHashHex, "hex"),
        ]),
      ),
    };
  }
  return next;
};

export const advanceUnusedScriptWitnessPurposesV1 = ({
  state,
  evidence,
  itemBudget = 24,
}: {
  state: UnusedScriptReverseScanDatumV1;
  evidence: UnusedScriptWitnessEvidenceV1;
  itemBudget?: number;
}): UnusedScriptReverseScanDatumV1 => {
  if (!Number.isSafeInteger(itemBudget) || itemBudget < 1 || itemBudget > 24)
    throw new Error("unusedScriptWitness item budget changed");
  let next = state;
  let remaining = itemBudget;
  while (
    remaining > 0 &&
    !next.used &&
    next.purpose_cursor < next.witness.purpose_count
  ) {
    const purpose = evidence.purposes[Number(next.purpose_cursor)];
    if (
      purpose === undefined ||
      purpose.frontierIndex !== Number(next.purpose_cursor)
    )
      throw new Error("unusedScriptWitness purpose frontier changed");
    const cursor = next.purpose_cursor + 1n;
    const matched =
      !next.shadowed && purpose.scriptHashHex === next.witness.script_hash;
    next = {
      ...next,
      purpose_cursor: cursor,
      used: matched,
      checkpoint_hash: hash(
        Buffer.concat([
          Buffer.from(next.checkpoint_hash, "hex"),
          integer(BigInt(purpose.purposeKind)),
          integer(BigInt(purpose.purposeIndex)),
          Buffer.from(purpose.scriptHashHex, "hex"),
        ]),
      ),
    };
    remaining -= 1;
  }
  return next;
};

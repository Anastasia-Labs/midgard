import {
  type Data,
  DataB,
  DataI,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekMintItem,
  initialMidgardCekContextControl,
  type MidgardCekContextControl,
  type MidgardCekMintAsset,
  runMidgardCekMintFold,
  summarizeMidgardCekData,
} from "../src/cek-context.js";

// The vector of `context_mint_walk_over_two_policies_ends_in_the_mint_map`
// (`onchain/aiken/validators/fraud-proofs/validation-trace/cek-split-v1.test.ak`):
// the same frontier gives the same opened leaves and the same mint summary.
const policyA = Buffer.alloc(28, 0xaa);
const policyB = Buffer.alloc(28, 0xbb);
const asset = (
  policyId: Buffer,
  name: string,
  quantity: bigint,
): MidgardCekMintAsset => ({
  policyId,
  assetName: Buffer.from(name, "hex"),
  quantity,
});
const frontier: readonly MidgardCekMintAsset[] = [
  asset(policyA, "00", 7n),
  asset(policyA, "ff", 1n),
  asset(policyB, "01", 2n),
];
const MINT_WALK_TWO_POLICIES_ROOT =
  "eb30636005ea5bbb5a821dad0fb2af9df94c1cb5520da94cadeb15bbf72e3ebf";

const pristine = (): MidgardCekContextControl => ({
  ...initialMidgardCekContextControl({
    languageTag: 3,
    programTermRoot: Buffer.alloc(32, 0xaa),
    programEnvelopeHash: Buffer.alloc(32, 0xdd),
    purposeKind: 1,
    purposeIndex: 0n,
    scriptHash: policyA,
    subject: policyA,
    redeemerLeaf: Buffer.alloc(32, 0xcc),
  }),
  stage: 8,
});

const pair = (key: Buffer, value: Data) => new DataPair(new DataB(key), value);

describe("CEK stage-8 mint fold (TS twin)", () => {
  it("opens leaves 2, 1, 0 and ends in the ascending mint map", () => {
    const opened: number[] = [];
    const done = runMidgardCekMintFold(pristine(), frontier, (_pre, index) =>
      opened.push(index),
    );
    const mintMap = new DataMap([
      pair(
        policyA,
        new DataMap([
          pair(Buffer.from("00", "hex"), new DataI(7n)),
          pair(Buffer.from("ff", "hex"), new DataI(1n)),
        ]),
      ),
      pair(
        policyB,
        new DataMap([pair(Buffer.from("01", "hex"), new DataI(2n))]),
      ),
    ]);
    expect(opened).toStrictEqual([2, 1, 0]);
    expect(done.stage).toBe(9);
    expect(done.mintCursor).toBe(3);
    expect(done.mintSummary).toStrictEqual(summarizeMidgardCekData(mintMap));
    expect(Buffer.from(done.mintSummary.root).toString("hex")).toBe(
      MINT_WALK_TWO_POLICIES_ROOT,
    );
  });

  it("differs when the walk opens the lower policy first", () => {
    // The machine refuses this step (`context_mint_walk_refuses_the_lower_
    // policy_first`); folding it anyway publishes a different map.
    const honest = runMidgardCekMintFold(pristine(), frontier);
    let control = pristine();
    for (const index of [1, 0, 2])
      control = advanceMidgardCekMintItem(control, frontier[index]!);
    const reordered = runMidgardCekMintFold(control, frontier);
    expect(reordered.mintSummary).not.toStrictEqual(honest.mintSummary);
  });
});

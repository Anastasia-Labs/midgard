import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import { phaseBRejection } from "./reject-subject.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeOutput,
  makePhaseBCandidate,
  nativeScriptWitness,
  outRefFromByte,
} from "./validation-fixtures.js";

/**
 * Phase B applies the ValueAndMint rules in the validation machine's order:
 * the whole input fold over the input-resolution schedule, then each output's
 * min-ADA check followed by that output's assets, then mint, then value
 * preservation. The bound counts asset entries: every asset of every spend
 * input's value, every asset of every output and every mint entry is one
 * entry, whether or not the same unit was folded before. The crossing is the
 * entry that would take the count past the bound, and it names the
 * coordinate `bind_coordinate_v1` binds.
 *
 * Sixteen spend inputs carry 1,024 assets each, so the input fold ends with
 * exactly the bound counted. Units are numbered so canonical value order is
 * numeric order: unit n is name `n mod 1024` (two bytes) under policy
 * `n div 1024`.
 */

const LIMIT = MIDGARD_CONSENSUS_LIMITS.maxDistinctAssetCount;
const PER_INPUT = 1_024;
const INPUTS = LIMIT / PER_INPUT;

const policyHex = (policy: number): string =>
  Buffer.concat([Buffer.alloc(27, 0xc0), Buffer.from([policy])]).toString(
    "hex",
  );
const nameHex = (name: number): string =>
  Buffer.from([name >> 8, name & 0xff]).toString("hex");

/** Units `[start, start + count)`, each with quantity 1. */
const units = (
  start: number,
  count: number,
): Map<string, Map<string, bigint>> => {
  const assets = new Map<string, Map<string, bigint>>();
  for (let unit = start; unit < start + count; unit += 1) {
    const policy = policyHex(Math.floor(unit / PER_INPUT));
    const names = assets.get(policy) ?? new Map<string, bigint>();
    names.set(nameHex(unit % PER_INPUT), 1n);
    assets.set(policy, names);
  }
  return assets;
};

const holding = (
  assets: Map<string, Map<string, bigint>>,
  lovelace = FUNDED_OUTPUT_LOVELACE,
): Buffer => makeOutput(lovelace, undefined, assets);

/** A reference input sorting before every spend; its assets never fold. */
const reference = outRefFromByte(0x10);
const spends = Array.from({ length: INPUTS }, (_, index) =>
  outRefFromByte(0x20 + index),
);
/** Spend input `index` holds units `[index * 1,024, (index + 1) * 1,024)`. */
const saturatingState = (): (readonly [Buffer, Buffer])[] => [
  // Units the spends never carry: folding them would cross before any spend.
  [reference, holding(units(LIMIT + 100, 8))],
  ...spends.map(
    (outRef, index) =>
      [outRef, holding(units(index * PER_INPUT, PER_INPUT))] as const,
  ),
];

const passing = nativeScriptWitness({ type: "all", scripts: [] });
const PASSING_POLICY = hashScriptWitness(passing);

const underMinAda = makeOutput(1n);
const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);

describe("phase B value-and-mint order", () => {
  it("names the input fold crossing by its schedule position", async () => {
    // A seventeenth spend sorts after the others and carries one unit seen
    // nowhere else. The reference input takes schedule position 0, so the
    // crossing input is schedule position 17 but spend ordinal 16. Value is
    // not preserved and an output is under its floor: both rules come later
    // in the machine, so the crossing is what Phase B names.
    const crossing = outRefFromByte(0x40);
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [...spends, crossing],
        referenceInputs: [reference],
        outputs: [underMinAda],
      }),
      [...saturatingState(), [crossing, holding(units(LIMIT, 1))]],
      RejectCodes.AssetCount,
    );
    expect(rejection.consensusPhase).toBe("valueAndMint");
    expect(rejection.subject).toStrictEqual({
      arm: "InputAssetAccumulationLimit",
      index: 17n,
      assetIndex: 0n,
    });
  });

  it("counts an output entry of a unit the inputs already carry", async () => {
    // The inputs fold exactly the bound; output 0 pays back one unit spend 0
    // carries. Its entry is the bound's successor, so it crosses.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        referenceInputs: [reference],
        outputs: [holding(units(0, 1))],
      }),
      saturatingState(),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 0n,
      assetIndex: 0n,
    });
  });

  it("counts repeated input entries and crosses on a repeated one", async () => {
    // Every spend carries the same 1,024 units: 1,024 distinct units, 16,384
    // entries. A seventeenth spend repeats unit 0 and crosses at its first
    // asset, schedule position 17 behind the reference input.
    const repeated = spends.map(
      (outRef) => [outRef, holding(units(0, PER_INPUT))] as const,
    );
    const crossing = outRefFromByte(0x40);
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: [...spends, crossing],
        referenceInputs: [reference],
        outputs: [holding(units(0, PER_INPUT), 100_000_000n)],
      }),
      [saturatingState()[0]!, ...repeated, [crossing, holding(units(0, 1))]],
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "InputAssetAccumulationLimit",
      index: 17n,
      assetIndex: 0n,
    });
  });

  it("names the output asset that crosses, after earlier entries", async () => {
    // Fifteen spends fold 15,360 entries. Output 0 pays back 1,025 units the
    // spends carry: its asset 1,023 is entry 16,384 and its asset 1,024
    // crosses.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends.slice(0, INPUTS - 1),
        outputs: [holding(units(0, PER_INPUT + 1), 100_000_000n)],
      }),
      saturatingState().slice(1, INPUTS),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 0n,
      assetIndex: BigInt(PER_INPUT),
    });
  });

  it("names the output crossing on the script-executing path too", async () => {
    // Minting runs the full Phase B path; the output crossing still comes
    // before the mint fold.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        outputs: [funded, holding(units(LIMIT + 1, 1))],
        scriptWitnesses: [passing],
        mintPreimageCbor: makeMintPreimageCbor(
          new Map([
            [
              Buffer.from(PASSING_POLICY, "hex"),
              new Map([[Buffer.from("aa", "hex"), 1n]]),
            ],
          ]),
        ),
      }),
      saturatingState().slice(1),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 1n,
      assetIndex: 0n,
    });
  });

  it("names the mint entry that crosses, on a unit the inputs carry", async () => {
    // The last spend carries 1,021 numbered units and the passing policy's
    // units `aa` and `ab`, so the input fold ends one entry under the bound.
    // Outputs carry no assets. Mint entry 0 burns one `aa` and takes the
    // count to the bound; entry 1 burns one `ab` and crosses.
    const lastAssets = units((INPUTS - 1) * PER_INPUT, PER_INPUT - 3);
    lastAssets.set(
      PASSING_POLICY,
      new Map([
        ["aa", 2n],
        ["ab", 2n],
      ]),
    );
    const state = saturatingState().slice(1);
    state[INPUTS - 1] = [spends[INPUTS - 1]!, holding(lastAssets)];
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        outputs: [funded],
        scriptWitnesses: [passing],
        mintPreimageCbor: makeMintPreimageCbor(
          new Map([
            [
              Buffer.from(PASSING_POLICY, "hex"),
              new Map([
                [Buffer.from("aa", "hex"), -1n],
                [Buffer.from("ab", "hex"), -1n],
              ]),
            ],
          ]),
        ),
      }),
      state,
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "MintAssetAccumulationLimit",
      index: 1n,
    });
  });

  it("names an earlier output under its floor before a later crossing", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        outputs: [underMinAda, holding(units(LIMIT + 1, 1))],
      }),
      saturatingState().slice(1),
      RejectCodes.MinAda,
    );
    expect(rejection.consensusPhase).toBe("valueAndMint");
    expect(rejection.subject).toStrictEqual({
      arm: "OutputBelowMinAda",
      index: 0n,
    });
  });

  it("names an earlier crossing before a later output under its floor", async () => {
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        outputs: [holding(units(LIMIT + 1, 1)), underMinAda],
      }),
      saturatingState().slice(1),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 0n,
      assetIndex: 0n,
    });
  });

  it("accepts exactly the bound and names value preservation after it", async () => {
    // The fold reaches the bound without crossing, so the walk ends at value
    // preservation: the spends' assets are not paid out.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({ spent: spends, outputs: [funded] }),
      saturatingState().slice(1),
      RejectCodes.ValueNotPreserved,
    );
    expect(rejection.consensusPhase).toBe("valueAndMint");
    expect(rejection.subject).toBeUndefined();
  });
});

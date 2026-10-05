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
 * preservation. A distinct-asset crossing is the first asset that inserts a
 * new unit once the accumulator has seen the bound, and it names the
 * coordinate `bind_coordinate_v1` binds.
 *
 * Sixteen spend inputs carry 1,024 distinct assets each, so the input fold
 * ends with exactly the bound seen. Units are numbered so canonical value
 * order is numeric order: unit n is name `n mod 1024` (two bytes) under
 * policy `n div 1024`.
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

  it("counts a unit whose running delta returned to zero as seen", async () => {
    // Output 0 pays back every unit of spend 0, so their deltas return to
    // zero; they still count. Output 1's first asset is new and crosses.
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        referenceInputs: [reference],
        outputs: [
          holding(units(0, PER_INPUT), 100_000_000n),
          holding(units(LIMIT + 1, 2)),
        ],
      }),
      saturatingState(),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 1n,
      assetIndex: 0n,
    });
  });

  it("names the output asset that crosses, after earlier seen assets", async () => {
    // Output 0 carries three already-seen units, then two new ones: the
    // first new one (asset 3) crosses.
    const assets = units(0, 3);
    for (const [policy, names] of units(LIMIT + 1, 2))
      assets.set(policy, names);
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        referenceInputs: [reference],
        outputs: [holding(assets)],
      }),
      saturatingState(),
      RejectCodes.AssetCount,
    );
    expect(rejection.subject).toStrictEqual({
      arm: "OutputAssetAccumulationLimit",
      index: 0n,
      assetIndex: 3n,
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

  it("names the mint entry that crosses", async () => {
    // Two always-true native policies. The last spend trades one of its
    // numbered units for the lower policy's unit, so the fold still ends at
    // the bound and that unit is seen. Outputs carry no assets; mint entry 0
    // re-mints the seen unit and entry 1, under the higher policy, is new.
    const [low, high] = [
      passing,
      nativeScriptWitness({
        type: "any",
        scripts: [{ type: "all", scripts: [] }],
      }),
    ].sort((left, right) =>
      // Lowercase hex: code-unit order is the policy's byte order.
      hashScriptWitness(left) < hashScriptWitness(right) ? -1 : 1,
    );
    const lowPolicy = hashScriptWitness(low!);
    const highPolicy = hashScriptWitness(high!);
    const lastAssets = units((INPUTS - 1) * PER_INPUT, PER_INPUT - 1);
    lastAssets.set(lowPolicy, new Map([["aa", 1n]]));
    const state = saturatingState().slice(1);
    state[INPUTS - 1] = [spends[INPUTS - 1]!, holding(lastAssets)];
    const rejection = await phaseBRejection(
      makePhaseBCandidate({
        spent: spends,
        outputs: [funded],
        scriptWitnesses: [low!, high!],
        mintPreimageCbor: makeMintPreimageCbor(
          new Map([
            [
              Buffer.from(lowPolicy, "hex"),
              new Map([[Buffer.from("aa", "hex"), 1n]]),
            ],
            [
              Buffer.from(highPolicy, "hex"),
              new Map([[Buffer.from("aa", "hex"), 1n]]),
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

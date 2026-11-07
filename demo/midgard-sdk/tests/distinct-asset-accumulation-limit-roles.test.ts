import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkDistinctAssetAccumulationLimitRoleOrder,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/distinct-asset-accumulation-limit.js";
import {
  distinctAssetAccumulationLimitConvicts,
  distinctAssetCrossing,
  type DistinctAssetUnits,
} from "../src/fraud-proof/distinct-asset-accumulation-limit.js";
import type { ValueFaultPosition } from "../src/fraud-proof/first-fault-later-phases.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("distinctAssetAccumulationLimit"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("distinct-asset-accumulation-limit role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.length,
    );
    expect(() =>
      checkDistinctAssetAccumulationLimitRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the spend take and resolve steps", () => {
    const take = DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.indexOf("inputs");
    const resolve =
      DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.indexOf("inputsResolve");
    expect(resolve).toBe(take + 1);
    expect(() =>
      checkDistinctAssetAccumulationLimitRoleOrder(swapped(take, resolve)),
    ).toThrow(
      `distinct-asset-accumulation-limit role inputs is fraudProofDistinctAssetAccumulationLimitInputs, but manifest step ${take} is fraudProofDistinctAssetAccumulationLimitInputsResolve`,
    );
  });

  it("refuses a manifest that swaps two of the appended steps", () => {
    const grammar =
      DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.indexOf("mintGrammar");
    const mint = DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.indexOf("mint");
    expect(() =>
      checkDistinctAssetAccumulationLimitRoleOrder(swapped(grammar, mint)),
    ).toThrow(
      `distinct-asset-accumulation-limit role mintGrammar is fraudProofDistinctAssetAccumulationLimitMintGrammar, but manifest step ${grammar} is fraudProofDistinctAssetAccumulationLimitMint`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() =>
        checkDistinctAssetAccumulationLimitRoleOrder(changed),
      ).toThrow(
        `distinct-asset-accumulation-limit has ${roles} roles; the manifest has ${changed.length} steps`,
      );
  });
});

// The fold rows below are pinned on-chain in
// distinct-asset-accumulation-limit/fold.test.ak; change both together.

const spends = (...entries: bigint[]): DistinctAssetUnits => ({
  inputs: entries.map((count, index) => ({
    scheduleIndex: BigInt(index),
    entries: count,
  })),
  outputs: [],
  mint: [],
});

const input = (inputIndex: bigint, assetIndex: bigint): ValueFaultPosition => ({
  kind: "input",
  inputIndex,
  assetIndex,
});
const output = (
  outputIndex: bigint,
  assetIndex: bigint,
): ValueFaultPosition => ({ kind: "output", outputIndex, assetIndex });
const mint = (mintIndex: bigint): ValueFaultPosition => ({
  kind: "mint",
  mintIndex,
});

describe("distinct-asset-accumulation-limit fold twin", () => {
  it("refuses the entry after 16,384 in the spend field", () => {
    expect(distinctAssetCrossing(spends(16_383n))).toBeNull();
    expect(distinctAssetCrossing(spends(16_384n))).toBeNull();
    expect(distinctAssetCrossing(spends(16_385n))).toEqual(input(0n, 16_384n));
    expect(distinctAssetCrossing(spends(16_000n, 384n))).toBeNull();
    expect(distinctAssetCrossing(spends(16_000n, 385n))).toEqual(
      input(1n, 384n),
    );
  });

  it("names a crossing input by its schedule position", () => {
    expect(
      distinctAssetCrossing({
        inputs: [
          { scheduleIndex: 1n, entries: 16_000n },
          { scheduleIndex: 3n, entries: 385n },
        ],
        outputs: [],
        mint: [],
      }),
    ).toEqual(input(3n, 384n));
  });

  it("indexes output assets across policies and carries the count", () => {
    const outputs = (inputs: bigint, values: bigint[][]) =>
      distinctAssetCrossing({ ...spends(inputs), outputs: values });
    expect(outputs(16_382n, [[1n]])).toBeNull();
    expect(outputs(16_382n, [[2n]])).toBeNull();
    expect(outputs(16_382n, [[3n]])).toEqual(output(0n, 2n));
    expect(outputs(16_382n, [[1n, 2n]])).toEqual(output(0n, 2n));
    expect(outputs(16_382n, [[1n], [2n]])).toEqual(output(1n, 1n));
    expect(outputs(16_384n, [[], [1n]])).toEqual(output(1n, 0n));
    expect(outputs(16_380n, [[], [2n, 3n], [1n]])).toEqual(output(1n, 4n));
  });

  it("indexes mint assets flat across policies", () => {
    const minted = (
      inputs: bigint,
      counts: bigint[],
      values: bigint[][] = [],
    ) =>
      distinctAssetCrossing({
        ...spends(inputs),
        outputs: values,
        mint: counts,
      });
    expect(minted(16_382n, [1n])).toBeNull();
    expect(minted(16_382n, [2n])).toBeNull();
    expect(minted(16_382n, [3n])).toEqual(mint(2n));
    expect(minted(16_381n, [2n, 1n])).toBeNull();
    expect(minted(16_381n, [2n, 2n])).toEqual(mint(3n));
    expect(minted(16_380n, [2n], [[1n], [2n]])).toEqual(mint(1n));
    expect(minted(16_383n, [], [[1n]])).toBeNull();
  });
});

const TX = "ab".repeat(32);

const accepted = acceptedVerdictSubject(TX);

const forced = (reason: RejectionReason | null): VerdictSubject =>
  forcedVerdictSubject({
    transactionId: TX,
    sourceKey: { transactionId: "cd".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });

const context = { body_network_mismatch: false };

const inputLimit = (
  input_index: bigint,
  asset_index: bigint,
): RejectionReason => ({
  InputAssetAccumulationLimit: { input_index, asset_index },
});
const outputLimit = (
  output_index: bigint,
  asset_index: bigint,
): RejectionReason => ({
  OutputAssetAccumulationLimit: { output_index, asset_index },
});
const mintLimit = (mint_index: bigint): RejectionReason => ({
  MintAssetAccumulationLimit: { mint_index },
});

describe("distinct-asset-accumulation-limit conviction twin", () => {
  // The same cases are pinned on-chain in
  // distinct-asset-accumulation-limit/terminal.test.ak; change both together.
  it("convicts each direction exactly when its verdict contradicts the fold", () => {
    const rows: readonly [
      VerdictSubject,
      ValueFaultPosition | null,
      boolean,
    ][] = [
      // Wrongful acceptance, either source.
      [accepted, input(1n, 384n), true],
      [accepted, output(1n, 1n), true],
      [accepted, mint(3n), true],
      [accepted, null, false],
      [forced(null), mint(3n), true],
      [forced(null), null, false],
      // A recorded crossing is justified only at the derived one.
      [forced(inputLimit(1n, 384n)), input(1n, 384n), false],
      [forced(outputLimit(1n, 1n)), output(1n, 1n), false],
      [forced(mintLimit(3n)), mint(3n), false],
      [forced(inputLimit(0n, 16_384n)), null, true],
      [forced(outputLimit(0n, 0n)), null, true],
      [forced(mintLimit(0n)), null, true],
      [forced(inputLimit(1n, 383n)), input(1n, 384n), true],
      [forced(inputLimit(0n, 384n)), input(1n, 384n), true],
      [forced(outputLimit(1n, 0n)), output(1n, 1n), true],
      [forced(mintLimit(2n)), mint(3n), true],
      [forced(mintLimit(4n)), mint(3n), true],
      [forced(outputLimit(0n, 0n)), mint(3n), true],
      [forced(inputLimit(-1n, 384n)), input(1n, 384n), true],
      // Another reason is justified at or before the crossing.
      [forced("ValueNotPreserved"), input(1n, 384n), true],
      [forced("ValueNotPreserved"), mint(3n), true],
      [
        forced({ OutputBelowMinAda: { output_index: 2n } }),
        output(1n, 1n),
        true,
      ],
      [forced("ValueNotPreserved"), null, false],
      [
        forced({ OutputBelowMinAda: { output_index: 1n } }),
        output(1n, 1n),
        false,
      ],
      [
        forced({ OutputBelowMinAda: { output_index: 0n } }),
        output(1n, 1n),
        false,
      ],
      [forced({ OutputBelowMinAda: { output_index: 0n } }), mint(3n), false],
      [
        forced({ InputNotFound: { source_kind: 1n, input_index: 0n } }),
        input(1n, 384n),
        false,
      ],
    ];
    for (const [subject, crossing, convicts] of rows)
      expect(
        distinctAssetAccumulationLimitConvicts(subject, context, crossing),
      ).toBe(convicts);
  });
});

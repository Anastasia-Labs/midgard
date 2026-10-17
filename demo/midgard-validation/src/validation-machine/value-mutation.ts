/**
 * The value accumulator and the value mutation steps derived from Midgard values.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeCbor, type MidgardLedgerOutputAsset } from "@al-ft/midgard-core";
import { type MidgardValue } from "@al-ft/midgard-core/codec";

import {
  exactTrieRoot,
  type ValidationMachineValueMutationStep,
} from "./ledger-mutation.js";

type ValidationValueAccumulator = {
  lovelaceDelta: bigint;
  assetRoot: Buffer;
  seenAssetCount: number;
  nonzeroAssetCount: number;
};

export const emptyValidationValueAccumulator =
  (): ValidationValueAccumulator => ({
    lovelaceDelta: 0n,
    assetRoot: Buffer.alloc(32),
    seenAssetCount: 0,
    nonzeroAssetCount: 0,
  });

export const encodeValidationValueAccumulator = (
  accumulator: ValidationValueAccumulator,
): Buffer =>
  encodeCbor([
    accumulator.lovelaceDelta,
    accumulator.assetRoot,
    BigInt(accumulator.seenAssetCount),
    BigInt(accumulator.nonzeroAssetCount),
  ]);

export type ValidationValueContribution = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
};

export const midgardValueAssets = (
  value: MidgardValue,
): readonly MidgardLedgerOutputAsset[] =>
  [...value.assets.entries()].flatMap(([policyId, policyAssets]) =>
    [...policyAssets.entries()].map(([assetName, quantity]) => ({
      policyId: Buffer.from(policyId, "hex"),
      assetName: Buffer.from(assetName, "hex"),
      quantity,
    })),
  );

export const midgardValueContributions = (
  value: MidgardValue,
  multiplier: 1n | -1n,
): readonly ValidationValueContribution[] =>
  midgardValueAssets(value).map(({ policyId, assetName, quantity }) => ({
    unit: Buffer.concat([policyId, assetName]),
    quantityDelta: quantity * multiplier,
  }));

export const buildValidationValueMutationSteps = async (
  contributions: readonly ValidationValueContribution[],
): Promise<readonly ValidationMachineValueMutationStep[]> => {
  const assetStore = new Store(undefined);
  await assetStore.ready();
  const assetTrie = new Trie(assetStore);
  const deltas = new Map<string, bigint>();
  const steps: ValidationMachineValueMutationStep[] = [];

  for (const contribution of contributions) {
    if (contribution.quantityDelta === 0n) {
      throw new Error("value mutation quantity delta must be non-zero");
    }
    const unit = Buffer.from(contribution.unit);
    const unitHex = unit.toString("hex");
    const oldDelta = deltas.get(unitHex) ?? null;
    const preAssetRoot = exactTrieRoot(assetTrie);
    const proofCbor = Buffer.from(
      (await assetTrie.prove(unit, oldDelta === null)).toCBOR(),
    );
    const nextDelta = (oldDelta ?? 0n) + contribution.quantityDelta;

    if (oldDelta !== null) {
      await assetTrie.delete(unit);
    }
    await assetTrie.insert(unit, encodeCbor(nextDelta));
    deltas.set(unitHex, nextDelta);

    steps.push({
      unit,
      quantityDelta: contribution.quantityDelta,
      oldDelta,
      preAssetRoot,
      postAssetRoot: exactTrieRoot(assetTrie),
      proofCbor,
      postSeenAssetCount: deltas.size,
      postNonzeroAssetCount: [...deltas.values()].filter(
        (quantity) => quantity !== 0n,
      ).length,
    });
  }
  return steps;
};

export const applyValidationValueMutationStep = (
  accumulator: ValidationValueAccumulator,
  step: ValidationMachineValueMutationStep,
): void => {
  if (!accumulator.assetRoot.equals(step.preAssetRoot)) {
    throw new Error(
      "value mutation step does not continue the authenticated root",
    );
  }
  accumulator.assetRoot = Buffer.from(step.postAssetRoot);
  accumulator.seenAssetCount = step.postSeenAssetCount;
  accumulator.nonzeroAssetCount = step.postNonzeroAssetCount;
};

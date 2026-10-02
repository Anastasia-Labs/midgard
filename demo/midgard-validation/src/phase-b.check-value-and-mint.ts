import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";

import { LedgerColumns } from "./ledger.js";
import type { RejectSubject } from "./reject-subject.js";
import { PhaseAValidatedTx, RejectCode, RejectCodes } from "./types.js";
import { orderMidgardInputResolutionSchedule } from "./validation-machine/input-resolution.js";
import { midgardValueAssets } from "./validation-machine/value-mutation.js";
import {
  describeValueDelta,
  isZeroValueDelta,
  outputCborMeetsMinAda,
  outputCborMinAdaLovelace,
  sumMidgardValues,
  valuePreservationDelta,
} from "./value-accounting.js";

/** The first ValueAndMint rule a transaction fails. */
export type ValueAndMintRejection = {
  readonly code: RejectCode;
  readonly detail: string;
  readonly subject?: RejectSubject;
};

/**
 * The ValueAndMint phase, applied in the validation machine's order so that the
 * rule it names first, and the coordinate it names, are the ones the machine
 * reaches (`onchain/aiken/lib/midgard/validation-machine/value-and-mint.ak`,
 * mirrored by `buildDeterministicValidationMachineTrace` in
 * `./validation-machine/trace-builder-complete.ts`):
 *
 * 1. Stage 2, the input fold: every input-resolution schedule node in order
 *    (`value_and_mint_replay_input` / `value_and_mint_replay_asset`). A spend
 *    node folds each asset of its resolved value; a reference node takes a
 *    schedule position and folds nothing.
 * 2. Stage 3, for each output in field order: the min-ADA check on its
 *    descriptor (`value_and_mint_output_descriptor`, `output_meets_min_ada_v1`)
 *    and then each of its assets (`value_and_mint_output_asset`).
 * 3. Stage 4, each mint-field asset in order (`value_and_mint_mint_asset`).
 * 4. Stage 5, value preservation (`value_and_mint_stage_five`).
 *
 * Every asset step in 1-3 goes through `apply_value_asset_mutation`, which
 * rejects the first step that inserts a unit the accumulator has not seen once
 * `seen_asset_count` has reached `max_distinct_asset_count`. A unit counts as
 * seen from its first insertion, including after its running delta has
 * returned to zero. The coordinate is the one
 * `distinct-asset-accumulation-limit/rule.ak` `bind_coordinate_v1` binds: the
 * schedule position and asset position for the input fold (the machine's
 * `replay_cursor` and `replay_asset_cursor - 1`), the output index and asset
 * position for outputs, and the mint index for mint.
 *
 * The phase runs last in Phase B, after every check of the earlier machine
 * phases (`resolveInputs` through `cek`), and its rejections carry the
 * `valueAndMint` consensus phase: the rules are stateless, but reaching them
 * earlier would name a rejection the machine never reaches.
 *
 * `spentValues` holds the resolved value of every spend input by its out-ref
 * key; the schedule is taken from `orderMidgardInputResolutionSchedule`, the
 * same function the trace builder orders its schedule with.
 */
export const checkValueAndMint = (input: {
  readonly candidate: PhaseAValidatedTx;
  readonly spentOutRefs: Iterable<string>;
  readonly referenceOutRefs: Iterable<string>;
  readonly spentValues: ReadonlyMap<string, MidgardValue>;
}): ValueAndMintRejection | null => {
  const { candidate, spentValues } = input;
  const limit = MIDGARD_CONSENSUS_LIMITS.maxDistinctAssetCount;
  const seen = new Set<string>();
  /** True when folding `unit` would cross the distinct-asset bound. */
  const crosses = (unit: string): boolean => {
    if (seen.has(unit)) return false;
    if (seen.size >= limit) return true;
    seen.add(unit);
    return false;
  };
  const crossingDetail = (where: string): string =>
    `${where} inserts a distinct asset after ${limit.toString()} have been seen`;

  const schedule = orderMidgardInputResolutionSchedule({
    spend: [...input.spentOutRefs],
    reference: [...input.referenceOutRefs],
    keyOf: (outRefHex) => Buffer.from(outRefHex, "hex"),
  });
  for (let index = 0; index < schedule.length; index += 1) {
    const node = schedule[index]!;
    if (node.sourceKind !== "spend") continue;
    const value = spentValues.get(node.item);
    if (value === undefined) {
      throw new Error(
        `phase B value walk: spend input ${node.item} has no resolved value`,
      );
    }
    const assets = midgardValueAssets(value);
    for (let assetIndex = 0; assetIndex < assets.length; assetIndex += 1) {
      const asset = assets[assetIndex]!;
      if (
        crosses(
          asset.policyId.toString("hex") + asset.assetName.toString("hex"),
        )
      ) {
        return {
          code: RejectCodes.AssetCount,
          detail: crossingDetail(
            `schedule input[${index.toString()}] asset[${assetIndex.toString()}]`,
          ),
          subject: {
            arm: "InputAssetAccumulationLimit",
            index: BigInt(index),
            assetIndex: BigInt(assetIndex),
          },
        };
      }
    }
  }

  // The bytes are `graph.produced[i][LedgerColumns.OUTPUT]`, the canonical
  // output encoding the on-chain descriptor's `total_length` binds; both the
  // min-ADA price and the asset order are read from them.
  const produced = candidate.graph.produced;
  for (let index = 0; index < produced.length; index += 1) {
    const outputCbor = produced[index]![LedgerColumns.OUTPUT];
    const value = decodeMidgardTxOutput(outputCbor).value;
    if (!outputCborMeetsMinAda(outputCbor, value.lovelace)) {
      return {
        code: RejectCodes.MinAda,
        detail: `output[${index.toString()}] ${value.lovelace.toString()} < ${outputCborMinAdaLovelace(
          outputCbor,
        ).toString()} for ${outputCbor.length.toString()} serialized bytes`,
        subject: { arm: "OutputBelowMinAda", index: BigInt(index) },
      };
    }
    const assets = midgardValueAssets(value);
    for (let assetIndex = 0; assetIndex < assets.length; assetIndex += 1) {
      const asset = assets[assetIndex]!;
      if (
        crosses(
          asset.policyId.toString("hex") + asset.assetName.toString("hex"),
        )
      ) {
        return {
          code: RejectCodes.AssetCount,
          detail: crossingDetail(
            `output[${index.toString()}] asset[${assetIndex.toString()}]`,
          ),
          subject: {
            arm: "OutputAssetAccumulationLimit",
            index: BigInt(index),
            assetIndex: BigInt(assetIndex),
          },
        };
      }
    }
  }

  const mintAssets = candidate.ledgerTx.mint.assets;
  for (let index = 0; index < mintAssets.length; index += 1) {
    const asset = mintAssets[index]!;
    if (
      crosses(
        Buffer.from(asset.policyId).toString("hex") +
          Buffer.from(asset.assetName).toString("hex"),
      )
    ) {
      return {
        code: RejectCodes.AssetCount,
        detail: crossingDetail(`mint[${index.toString()}]`),
        subject: { arm: "MintAssetAccumulationLimit", index: BigInt(index) },
      };
    }
  }

  const delta = valuePreservationDelta(
    sumMidgardValues([...spentValues.values()]),
    candidate.ledgerTx.fee,
    candidate.derived.mintDelta,
    candidate.derived.outputSum,
  );
  if (!isZeroValueDelta(delta)) {
    return {
      code: RejectCodes.ValueNotPreserved,
      detail: `equation mismatch: inputs - fee + mint - outputs = ${describeValueDelta(delta)}`,
    };
  }
  return null;
};

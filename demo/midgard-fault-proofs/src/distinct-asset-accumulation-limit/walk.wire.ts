/** The thread datums and redeemers of the `distinctAssetAccumulationLimit`
 * walk, each built exactly as its validator requires. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { stepDatum } from "../direct-purpose-family/custody.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import type {
  DistinctAssetAccumulationLimitFieldPlan,
  DistinctAssetAccumulationLimitRedeemerArgs,
} from "./custody.js";
import type { FoldPosition } from "./walk.steps.js";

type Open = Pick<
  DistinctAssetAccumulationLimitRedeemerArgs,
  "indices" | "open"
>;

export const distinctAssetWire = ({
  material,
  prover,
  plan,
}: {
  readonly material: DirectScriptMaterial;
  readonly prover: string;
  readonly plan: (field: number) => DistinctAssetAccumulationLimitFieldPlan;
}) => {
  const count = (field: number) => plan(field).items.length;
  const walkState = (
    field: number,
    position: FoldPosition,
  ): SDK.DistinctAssetWalk => ({
    cursor: plan(field).position("Walking", position.index).cursor,
    complete: position.index === count(field),
    entry_count: position.entryCount,
    resume: position.resume,
    mint_index: position.mintIndex,
  });
  return {
    count,
    walkState,
    /** A walk over `field` with `position.index` items behind the cursor. */
    walk: (field: number, position: FoldPosition) =>
      stepDatum(prover, walkState(field, position), SDK.DistinctAssetWalkDatum),
    /** A variable field certified up to `at`, its walk not yet begun. */
    certifying: (field: number, at: number, position: FoldPosition) =>
      stepDatum(
        prover,
        {
          ...walkState(field, position),
          cursor: plan(field).position("Certifying", at).cursor,
          complete: false,
        },
        SDK.DistinctAssetWalkDatum,
      ),
    /** A spend take of `[start, end)`, awaiting its descriptors. */
    pending: (resumeScriptHash: string, position: FoldPosition, end: number) =>
      stepDatum(
        prover,
        {
          resume_script_hash: resumeScriptHash,
          state: walkState(0, { ...position, index: end }),
          first_ordinal: BigInt(position.index),
          pending: plan(0)
            .items.slice(position.index, end)
            .map((item) => item.toString("hex")),
        },
        SDK.DistinctAssetPendingDatum,
      ),
    rank: (ordinal: number, assetIndex: bigint) =>
      stepDatum(
        prover,
        {
          bound: material.bound,
          input_ordinal: BigInt(ordinal),
          input_item: plan(0).items[ordinal]!.toString("hex"),
          asset_index: assetIndex,
        },
        SDK.DistinctAssetRankDatum,
      ),
    terminal: (crossing: SDK.ValueFaultPosition | null) =>
      stepDatum(
        prover,
        {
          bound: material.bound,
          crossing: SDK.distinctAssetCrossingData(crossing),
        },
        SDK.DistinctAssetTerminalDatum,
      ),
    entryRedeemer: ({ indices, open }: Open) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(0) }] },
        SDK.DistinctAssetEntryRedeemer,
      ),
    /** A take (an item count), an output step (a work budget) or a mint
     * step (a policy count) from the walk checkpoint at `index`. */
    fieldRedeemer:
      (field: number, index: number, budget: bigint) =>
      ({ indices, open }: Open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(field),
                checkpoint_bytes: plan(field).position("Walking", index)
                  .checkpointBytes,
                budget,
              },
            ],
          },
          SDK.DistinctAssetFieldRedeemer,
        ),
    resolveRedeemer:
      (witnesses: readonly SDK.DistinctAssetInputWitness[]) =>
      ({ indices }: Open) =>
        Data.to(
          { Continue: [{ ...indices, witnesses: [...witnesses] }] },
          SDK.DistinctAssetResolveRedeemer,
        ),
    rankRedeemer:
      (rank: bigint) =>
      ({ indices, open }: Open) =>
        Data.to(
          { Continue: [{ ...indices, opening: open(1), rank }] },
          SDK.DistinctAssetRankRedeemer,
        ),
    openRedeemer:
      (field: number) =>
      ({ indices, open }: Open) =>
        Data.to(
          { Continue: [{ Open: { ...indices, opening: open(field) } }] },
          SDK.DirectAllPurposesGrammarRedeemer,
        ),
    certifyRedeemer:
      (field: number, start: number, budget: number) =>
      ({ indices, open }: Open) =>
        Data.to(
          {
            Continue: [
              {
                Certify: {
                  ...indices,
                  opening: open(field),
                  checkpoint_bytes: plan(field).position("Certifying", start)
                    .checkpointBytes,
                  budget: BigInt(budget),
                },
              },
            ],
          },
          SDK.DirectAllPurposesGrammarRedeemer,
        ),
  };
};

export type DistinctAssetWire = ReturnType<typeof distinctAssetWire>;

import {
  asArray,
  asBigInt,
  asBytes,
  decodeMidgardLedgerOutputCommitment,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  buildMidgardMpfProofFoldTrace,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core/mpf-proof-fold";
import {
  AssetFoldClaim,
  ValidationAuxiliaryWitness,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

/** Recompute the claim from the authenticated pre-state and mutation proof. */
export const buildValidationAssetFoldClaim = (
  transitionData: Data,
  auxiliaryData: Data,
): AssetFoldClaim => {
  const transition = Data.from(
    Data.to(transitionData),
    ValidationOneStepWitness,
  );
  const auxiliary = Data.from(
    Data.to(auxiliaryData),
    ValidationAuxiliaryWitness,
  );
  if (typeof auxiliary === "string")
    throw new Error("Asset fold requires an asset witness");
  const witness =
    "ValueInputAssetWitness" in auxiliary
      ? auxiliary.ValueInputAssetWitness
      : "ValueOutputAssetWitness" in auxiliary
        ? auxiliary.ValueOutputAssetWitness
        : "ValueMintAssetWitness" in auxiliary
          ? auxiliary.ValueMintAssetWitness
          : undefined;
  if (witness === undefined)
    throw new Error("Asset fold requires an asset witness");
  const control = asArray(
    decodeSingleCbor(Buffer.from(transition.work_witness_cbor, "hex")),
    "value control",
  );
  if (control.length !== 12)
    throw new Error("Asset fold requires the complete value control");
  const acc = asArray(
    decodeSingleCbor(asBytes(control[11], "value accumulator")),
    "value accumulator",
  );
  if (acc.length !== 4) throw new Error("Invalid value accumulator");
  const pre = {
    lovelace_delta: asBigInt(acc[0], "lovelace delta"),
    asset_root: Buffer.from(asBytes(acc[1], "asset root")).toString("hex"),
    seen_asset_count: asBigInt(acc[2], "seen asset count"),
    nonzero_asset_count: asBigInt(acc[3], "nonzero asset count"),
  };
  const mutation = witness.mutation;
  if (mutation.delta_proof.length > 16)
    throw new Error("Asset mutation exceeds its proof bound");
  const steps = parseMidgardMpfProofJson(
    mutation.delta_proof.map((step) => {
      if ("Branch" in step)
        return {
          type: "branch",
          skip: Number(step.Branch.skip),
          neighbors: step.Branch.neighbors,
        };
      if ("Fork" in step)
        return {
          type: "fork",
          skip: Number(step.Fork.skip),
          neighbor: {
            ...step.Fork.neighbor,
            nibble: Number(step.Fork.neighbor.nibble),
          },
        };
      return {
        type: "leaf",
        skip: Number(step.Leaf.skip),
        neighbor: { key: step.Leaf.key, value: step.Leaf.value },
      };
    }),
  );
  const key = Buffer.from(witness.policy_id + witness.asset_name, "hex");
  const quantity =
    witness.quantity * ("ValueOutputAssetWitness" in auxiliary ? -1n : 1n);
  if (quantity === 0n || key.length < 28 || key.length > 60)
    throw new Error("Invalid asset fold quantity or unit");
  if (!mutation.delta_was_present && mutation.old_delta !== 0n)
    throw new Error("Absent asset has a nonzero old delta");
  const old = mutation.delta_was_present ? mutation.old_delta : 0n;
  const next = old + quantity;
  const fold = (value: bigint) =>
    buildMidgardMpfProofFoldTrace({ key, value: encodeCbor(value), steps })
      .terminal;
  const oldRoot = mutation.delta_was_present
    ? fold(old).includingRoot
    : fold(next).excludingRoot;
  if (oldRoot.toString("hex") !== pre.asset_root)
    throw new Error("Asset mutation does not authenticate the pre-state root");
  const nonzero =
    pre.nonzero_asset_count +
    (old === 0n && next !== 0n ? 1n : old !== 0n && next === 0n ? -1n : 0n);
  const seen = pre.seen_asset_count + (mutation.delta_was_present ? 0n : 1n);
  const outcome: AssetFoldClaim["outcome"] =
    !mutation.delta_was_present && pre.seen_asset_count >= 16_384n
      ? "ValueAccumulatorAssetLimitExceeded"
      : nonzero < 0n || nonzero > seen
        ? "ValueAccumulatorMutationInvalid"
        : {
            ValueAccumulatorUpdated: [
              {
                ...pre,
                asset_root: fold(next).includingRoot.toString("hex"),
                seen_asset_count: seen,
                nonzero_asset_count: nonzero,
              },
            ],
          };
  return {
    policy_id: witness.policy_id,
    asset_name: witness.asset_name,
    quantity: witness.quantity,
    mutation,
    pre_value_accumulator: pre,
    outcome,
    descriptor:
      "descriptor_cbor" in witness
        ? {
            descriptor_cbor: witness.descriptor_cbor,
            asset_index: witness.asset_index,
            asset_peaks: witness.asset_peaks,
            asset_siblings: witness.asset_siblings,
            asset_count: BigInt(
              decodeMidgardLedgerOutputCommitment(
                Buffer.from(witness.descriptor_cbor, "hex"),
              ).assetCount,
            ),
          }
        : null,
  };
};

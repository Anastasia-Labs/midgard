import { Proof } from "@aiken-lang/merkle-patricia-forestry";
import {
  asArray,
  asBigInt,
  asBytes,
  buildMidgardValidationMerkleMembership,
  buildMidgardValidationTraceTree,
  commitMidgardValidationMerkleFrontier,
  computeHash32,
  decodeMidgardLedgerOutputCommitment,
  decodeSingleCbor,
  encodeCbor,
  encodeMidgardLedgerOutputCommitment,
  hashMidgardLedgerOutputAssetLeaf,
  hashMidgardMintAssetLeaf,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  buildMidgardMpfProofFoldTrace,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core/mpf-proof-fold";
import type { DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";

/**
 * Adversarial committed control: a real 14-sibling asset membership and widest
 * 16-step mutation proof. Only the selected one-step relation is asserted;
 * this does not claim the operator's earlier accumulator history was honest.
 */
export const withMaximumValueAssetProof = (
  trace: DeterministicValidationMachineTrace,
  index: number,
): DeterministicValidationMachineTrace => {
  const witness = trace.witnesses[index]!;
  const auxiliary = witness.auxiliary;
  if (
    auxiliary == null ||
    (auxiliary.kind !== "valueInputAsset" &&
      auxiliary.kind !== "valueOutputAsset" &&
      auxiliary.kind !== "valueMintAsset")
  )
    throw new Error("Maximum asset proof requires an asset fold");
  const control = [...asArray(decodeSingleCbor(witness.cbor), "value control")];
  const accumulator = [
    ...asArray(
      decodeSingleCbor(asBytes(control[11], "accumulator")),
      "accumulator",
    ),
  ];
  const mutation = auxiliary.mutationStep;
  const proofJson = Array.from({ length: 16 }, (_, i) => ({
    type: "branch",
    skip: 0,
    neighbors: Buffer.concat(
      Array.from({ length: 4 }, (_, j) => computeHash32(Buffer.from([i, j]))),
    ).toString("hex"),
  }));
  const steps = parseMidgardMpfProofJson(proofJson);
  const old = mutation.oldDelta ?? 0n;
  const next = old + mutation.quantityDelta;
  const roots = (value: bigint) =>
    buildMidgardMpfProofFoldTrace({
      key: mutation.unit,
      value: encodeCbor(value),
      steps,
    }).terminal;
  const preRoot =
    mutation.oldDelta === null
      ? roots(next).excludingRoot
      : roots(old).includingRoot;
  const postRoot = roots(next).includingRoot;
  accumulator[1] = preRoot;
  accumulator[2] = 16_383n;
  accumulator[3] = 8_000n;
  control[11] = encodeCbor(accumulator);
  const postSeen = 16_383 + (mutation.oldDelta === null ? 1 : 0);
  const postNonzero =
    8_000 +
    (old === 0n && next !== 0n ? 1 : old !== 0n && next === 0n ? -1 : 0);
  const mutationStep = {
    ...mutation,
    preAssetRoot: preRoot,
    postAssetRoot: postRoot,
    proofCbor: Buffer.from(
      Proof.fromJSON(mutation.unit, encodeCbor(old), proofJson).toCBOR(),
    ),
    postSeenAssetCount: postSeen,
    postNonzeroAssetCount: postNonzero,
  };
  const leaf =
    auxiliary.kind === "valueMintAsset"
      ? hashMidgardMintAssetLeaf(auxiliary)
      : hashMidgardLedgerOutputAssetLeaf(auxiliary);
  const leaves = Array.from({ length: 16_384 }, (_, i) =>
    i === 0 ? leaf : computeHash32(encodeCbor(BigInt(i))),
  );
  const membership = buildMidgardValidationMerkleMembership(leaves, 0);
  let nextAuxiliary = { ...auxiliary, mutationStep };
  if (auxiliary.kind === "valueMintAsset") {
    const native = [
      ...asArray(
        decodeSingleCbor(asBytes(control[0], "native control")),
        "native control",
      ),
    ];
    native[19] = 16_384n;
    native[20] = membership.frontier.peaks.map((p) => [
      BigInt(p.height),
      p.hash,
    ]);
    control[0] = encodeCbor(native);
    nextAuxiliary = {
      ...auxiliary,
      mutationStep,
      siblings: membership.siblings,
    };
  } else {
    const descriptorCbor = encodeMidgardLedgerOutputCommitment({
      ...decodeMidgardLedgerOutputCommitment(auxiliary.descriptorCbor),
      assetCount: 16_384,
      cardanoValueSize: 5_000,
      totalLength: 16_384,
      assetFrontierCommitment: commitMidgardValidationMerkleFrontier(
        membership.frontier,
      ),
    });
    control[5] = computeHash32(descriptorCbor);
    nextAuxiliary = {
      ...auxiliary,
      mutationStep,
      descriptorCbor,
      assetFrontier: membership.frontier,
      assetSiblings: membership.siblings,
    };
  }
  const postControl = [...control];
  const cursor =
    auxiliary.kind === "valueInputAsset"
      ? 4
      : auxiliary.kind === "valueOutputAsset"
        ? 9
        : 10;
  postControl[cursor] = asBigInt(postControl[cursor], "asset cursor") + 1n;
  postControl[11] = encodeCbor([
    accumulator[0],
    postRoot,
    BigInt(postSeen),
    BigInt(postNonzero),
  ]);
  const preCbor = encodeCbor(control);
  const postCbor = encodeCbor(postControl);
  const states = trace.states.map((s, i) =>
    i === index || i === index + 1
      ? {
          ...s,
          workRoot: Buffer.from(
            hashMidgardValidationWorkWitness({
              phase: "valueAndMint",
              programCounter: s.programCounter,
              witnessCbor: i === index ? preCbor : postCbor,
            }),
          ),
        }
      : s,
  );
  const witnesses = trace.witnesses.map((w, i) =>
    i === index
      ? { ...w, cbor: preCbor, auxiliary: nextAuxiliary }
      : i === index + 1
        ? { ...w, cbor: postCbor }
        : w,
  );
  return {
    ...trace,
    states,
    witnesses,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
};

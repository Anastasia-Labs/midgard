/**
 * HarmonicLabs' builtin evaluator (`BnCEK`), which evaluated Midgard's
 * builtins before the pinned evaluator's `eval_builtin_raw`, kept for the
 * recursive BLS expression oracle (`bls-expression.legacy.ts`) only. Midgard
 * holds a BLS point as its compressed bytes; the evaluator holds the point.
 */
import {
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH,
} from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import {
  BnCEK,
  CEKConst,
  CEKError,
  costModelV3ToBuiltinCosts,
  ExBudget,
  PartialBuiltin,
} from "@harmoniclabs/plutus-machine";

import {
  harmonicBuiltinTag,
  MIDGARD_CEK_CARDANO_BLS_TAGS,
} from "../../src/cek-builtin.cardano-tags.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "../../src/cek-constant.js";
import { DataB } from "../../src/uplc-data.js";

if (
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length !==
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH
) {
  throw new Error("V1 pinned Plutus V3 cost model must have 350 entries");
}

/** HarmonicLabs' Plutus V3 costing over the pinned cost model; unread. */
const REFERENCE_BUILTIN_COSTS = costModelV3ToBuiltinCosts(
  toCostModelV3([
    ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ] as unknown as Parameters<typeof toCostModelV3>[0]),
);

/** Runs Cardano's builtin `tag` (flat numbering) in HarmonicLabs' evaluator. */
export const runHarmonicReferenceBuiltin = (
  tag: number,
  arguments_: readonly CEKConst[],
): CEKConst | CEKError => {
  const builtinTag = harmonicBuiltinTag(tag);
  if (PartialBuiltin.getNRequiredArgsFor(builtinTag) !== arguments_.length) {
    throw new Error("V1 builtin argument count is incomplete");
  }
  const result = new BnCEK(
    REFERENCE_BUILTIN_COSTS,
    new ExBudget({ cpu: 0, mem: 0 }),
    [],
  ).eval(builtinTag, arguments_);
  if (!(result instanceof CEKConst) && !(result instanceof CEKError)) {
    throw new Error("V1 builtin did not saturate to a constant");
  }
  return result;
};

/** HarmonicLabs' point for a BLS constant witness, uncompressed by it. */
export const harmonicBlsPoint = (
  witness: MidgardCekConstantWitness,
): CEKConst => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  if (decoded.type.kind !== "blsG1" && decoded.type.kind !== "blsG2") {
    throw new Error("V1 constant is not a BLS point");
  }
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("V1 BLS payload is not bytes");
  }
  const uncompressed = runHarmonicReferenceBuiltin(
    decoded.type.kind === "blsG1"
      ? MIDGARD_CEK_CARDANO_BLS_TAGS.g1Uncompress
      : MIDGARD_CEK_CARDANO_BLS_TAGS.g2Uncompress,
    [CEKConst.byteString(Uint8Array.from(decoded.payload.bytes))],
  );
  if (uncompressed instanceof CEKError) {
    throw new Error(
      `V1 BLS constant has an invalid encoding: ${uncompressed.msg ?? "unknown reference error"}`,
    );
  }
  return uncompressed;
};

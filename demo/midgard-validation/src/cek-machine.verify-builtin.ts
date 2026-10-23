import {
  hashMidgardCekMachineState,
  hashMidgardCekTermNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import { constrIndexIsOutOfRange } from "./cek-builtin.constr-data-index.js";
import {
  evaluateMidgardCekBlsFinal,
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
  verifyMidgardCekBlsFinal,
  verifyMidgardCekBuiltinTypeFailure,
  verifyMidgardCekDirectBuiltin,
  verifyMidgardCekDirectBuiltinFailure,
} from "./cek-builtin.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import {
  canonicalIntegerLeaf,
  exactTopMaterial,
  verifySemanticPair,
} from "./cek-machine.data-node-topology-matches.js";
import {
  errorSuccessor,
  exactState,
  midgardCekBuiltinArgumentCount,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  type MidgardCekSemanticBuiltinWitness,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import {
  builtinRootMatches,
  constantParts,
  verifyCaseApply,
  verifyCaseSelect,
} from "./cek-machine.verify-case-select.js";
import { verifyCompute } from "./cek-machine.verify-compute.js";
import { verifyLookup } from "./cek-machine.verify-lookup.js";
import { verifyMapConversionStart } from "./cek-machine.verify-map-conversion-start.js";
import { verifyReturn } from "./cek-machine.verify-return.js";
import { verifySemanticBuiltinControl } from "./cek-machine.verify-semantic-builtin-control.js";
import { verifySemanticData } from "./cek-machine.verify-semantic-data.js";
import {
  semanticListSource,
  verifySemanticChooseData,
  verifySemanticList,
} from "./cek-machine.verify-semantic-list.js";
import { midgardCekStagedBuiltinFailureBudget } from "./cek-staged-builtin.failure.js";
import type { MidgardCekStagedBuiltinMaterial } from "./cek-staged-builtin.material.js";
import {
  midgardCekPlaceholderStaging,
  midgardCekStagedReferenceInputs,
  stageMidgardCekStagedBuiltinMaterial,
} from "./cek-staged-builtin.material.js";
import { verifyMidgardCekStagedBuiltin } from "./cek-staged-builtin.verify.js";

const verifySemanticBuiltin = (
  tag: bigint,
  pre: MidgardCekMachineState,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (!builtinRootMatches(pre, tag, arguments_)) return false;
  if (tag === 29n || tag === 30n) {
    return verifySemanticPair(tag, arguments_, result, material);
  }
  if (tag >= 31n && tag <= 35n) {
    return verifySemanticList(tag, arguments_, result, material);
  }
  if (tag === 36n) {
    return verifySemanticChooseData(arguments_, result, material);
  }
  return (
    tag >= 37n &&
    tag <= 51n &&
    tag !== 38n &&
    tag !== 43n &&
    verifySemanticData(tag, arguments_, result, material)
  );
};

/**
 * A staged step read against the staging outputs its material names, each
 * holding its run of the witness's staged operands. A step not yet staged
 * is read as staged on placeholder outputs.
 */
const stagedMaterial = (witness: {
  readonly material: MidgardCekStagedBuiltinMaterial;
  readonly stagedOperands: readonly Uint8Array[];
}) => {
  const staged =
    witness.material.runs.operands.length === witness.stagedOperands.length
      ? witness.material
      : stageMidgardCekStagedBuiltinMaterial(
          witness.material,
          witness.stagedOperands,
          midgardCekPlaceholderStaging(witness.stagedOperands),
        );
  return {
    staged,
    referenceInputs: midgardCekStagedReferenceInputs(
      staged,
      witness.stagedOperands,
    ),
  };
};

const verifyStagedBuiltin = (
  witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "executeBuiltinStaged" }
  >,
): boolean => {
  const { staged, referenceInputs } = stagedMaterial(witness);
  return verifyMidgardCekStagedBuiltin(
    witness.tag,
    witness.arguments,
    witness.result,
    staged,
    referenceInputs,
  );
};

const verifyStagedBuiltinFailure = (
  witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "executeBuiltinStagedFailure" }
  >,
): MidgardCekBuiltinBudget | null => {
  const { staged, referenceInputs } = stagedMaterial(witness);
  return midgardCekStagedBuiltinFailureBudget(
    witness.tag,
    witness.arguments,
    staged,
    referenceInputs,
  );
};

/**
 * constrData fails on an index outside the unsigned 64-bit range. The index
 * is read from its revealed leaf or, with no leaf, from its node memory: an
 * integer of two or more words has magnitude at least 2^64.
 */
const constrDataIndexFails = (
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (
    arguments_.length !== 2 ||
    material.dataNodes.length !== 1 ||
    material.listNodes.length !== 0 ||
    material.pairNodes.length !== 0
  ) {
    return false;
  }
  const index = constantParts(arguments_[0]!);
  const node = material.dataNodes[0]!;
  if (index === null || index.type.kind !== "integer") return false;
  if (material.scalarPreimages.length === 0) {
    return (
      node.kind === "integer" &&
      exactTopMaterial(index.payload, node, [], []) &&
      index.payload.memory >= 6n
    );
  }
  if (material.scalarPreimages.length !== 1) return false;
  const value = canonicalIntegerLeaf(
    index.payload,
    node,
    material.scalarPreimages[0]!,
  );
  return value !== null && constrIndexIsOutOfRange(value);
};

const verifySemanticBuiltinFailure = (
  tag: bigint,
  pre: MidgardCekMachineState,
  arguments_: readonly MidgardCekDirectValueWitness[],
  material: MidgardCekSemanticBuiltinWitness,
): boolean => {
  if (tag === 37n) {
    return (
      builtinRootMatches(pre, tag, arguments_) &&
      constrDataIndexFails(arguments_, material)
    );
  }
  if (
    !builtinRootMatches(pre, tag, arguments_) ||
    arguments_.length !== 1 ||
    material.dataNodes.length !== 1 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const source = constantParts(arguments_[0]!);
  const node = material.dataNodes[0]!;
  if (source === null) return false;
  if (tag === 33n || tag === 34n) {
    const list = semanticListSource(arguments_[0]!, node, material.listNodes);
    return material.pairNodes.length === 0 && list?.sequence.length === 0n;
  }
  if (
    tag < 42n ||
    tag > 46n ||
    source.type.kind !== "data" ||
    source.memory !== source.payload.memory ||
    !exactTopMaterial(
      source.payload,
      node,
      material.listNodes,
      material.pairNodes,
    )
  ) {
    return false;
  }
  if (tag === 42n) {
    return node.kind !== "constrSmall" && node.kind !== "constrLarge";
  }
  if (tag === 43n) return node.kind !== "map";
  if (tag === 44n) return node.kind !== "list";
  if (tag === 45n) return node.kind !== "integer";
  return node.kind !== "bytes";
};

const verifyBuiltin = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  if (witness.kind === "startBuiltinMapConversion") {
    return verifyMapConversionStart(pre, post, witness);
  }
  if (witness.kind === "executeBuiltinSemantic") {
    if (
      !verifySemanticBuiltin(
        witness.tag,
        pre,
        witness.arguments,
        witness.result,
        witness.material,
      )
    ) {
      return false;
    }
    const budget = midgardCekDirectBuiltinBudget(
      witness.tag,
      witness.arguments,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitness(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinStaged") {
    if (
      !builtinRootMatches(pre, witness.tag, witness.arguments) ||
      !verifyStagedBuiltin(witness)
    ) {
      return false;
    }
    const budget = midgardCekDirectBuiltinBudget(
      witness.tag,
      witness.arguments,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitness(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinSemanticFailure") {
    return (
      verifySemanticBuiltinFailure(
        witness.tag,
        pre,
        witness.arguments,
        witness.material,
      ) &&
      sameState(
        post,
        exactState(pre, {
          mode: "haltError",
          focusRoot: hashMidgardCekTermNode({ kind: "error" }),
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
          auxiliary: MidgardCekErrorCodes.BuiltinFailure,
        }),
      )
    );
  }
  if (witness.kind === "executeBuiltinStagedFailure") {
    // The staged failure hop charges a division by zero of a dividend of
    // 4,096 words in the range, and nothing for its other failures.
    if (
      BigInt(witness.arguments.length) !==
        midgardCekBuiltinArgumentCount(witness.tag) ||
      !builtinRootMatches(pre, witness.tag, witness.arguments)
    ) {
      return false;
    }
    const budget = verifyStagedBuiltinFailure(witness);
    return (
      budget !== null &&
      sameState(
        post,
        errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure, budget),
      )
    );
  }
  if (witness.kind === "executeBuiltinTypeFailure") {
    return (
      verifyMidgardCekBuiltinTypeFailure(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
      ) &&
      sameState(post, errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure))
    );
  }
  if (witness.kind === "executeBuiltinBlsFinal") {
    if (
      !verifyMidgardCekBlsFinal(
        pre.focusRoot,
        witness.leftRoot,
        witness.rightRoot,
        witness.leftExpression,
        witness.rightExpression,
        witness.result,
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekBlsFinal(
      witness.leftRoot,
      witness.rightRoot,
      witness.leftExpression,
      witness.rightExpression,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitness(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinDirect") {
    if (
      !verifyMidgardCekDirectBuiltin(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
        witness.result,
      )
    ) {
      return false;
    }
    const budget = midgardCekDirectBuiltinBudget(
      witness.tag,
      witness.arguments,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitness(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinFailure") {
    if (
      !verifyMidgardCekDirectBuiltinFailure(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekDirectBuiltin(
      witness.tag,
      witness.arguments,
    );
    if (evaluated.kind !== "failure") return false;
    return sameState(
      post,
      exactState(pre, {
        mode: "haltError",
        focusRoot: hashMidgardCekTermNode({ kind: "error" }),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
        auxiliary: MidgardCekErrorCodes.BuiltinFailure,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }
  return false;
};

/**
 * Mirrors the Aiken structural CEK one-step verifier. The authenticated
 * zero-cost runtime-type failures and direct semantic success/failure rules
 * are active. BLS final verification retains its dedicated expression
 * witness path and remains separate from the direct evaluator.
 */
export const verifyMidgardCekCoreStep = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  try {
    // Hashing validates every state field's exact width and numeric range.
    hashMidgardCekMachineState(pre);
    hashMidgardCekMachineState(post);
    if (pre.executionIndex !== post.executionIndex) return false;
    switch (pre.mode) {
      case "compute":
        return verifyCompute(pre, post, witness);
      case "lookup":
        return verifyLookup(pre, post, witness);
      case "return":
        return verifyReturn(pre, post, witness);
      case "caseSelect":
        return verifyCaseSelect(pre, post, witness);
      case "caseApply":
        return verifyCaseApply(pre, post, witness);
      case "builtin":
        return verifyBuiltin(pre, post, witness);
      case "semanticBuiltin":
        return verifySemanticBuiltinControl(pre, post, witness);
      case "haltSuccess":
      case "haltError":
        return false;
    }
  } catch {
    return false;
  }
};

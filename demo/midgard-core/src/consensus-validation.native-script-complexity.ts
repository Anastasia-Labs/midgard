import type { MidgardNativeScript } from "./codec/native-script.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import {
  type MidgardConsensusViolation,
  type NativeScriptComplexity,
  violation,
} from "./consensus-validation.reconstruct-midgard-transaction.js";

const nativeScriptComplexity = (
  script: MidgardNativeScript,
): NativeScriptComplexity => {
  let depth = 0;
  let nodeCount = 0;
  const pending: {
    readonly script: MidgardNativeScript;
    readonly depth: number;
  }[] = [{ script, depth: 1 }];

  while (pending.length > 0) {
    const current = pending.pop()!;
    depth = Math.max(depth, current.depth);
    nodeCount += 1;
    if (
      depth > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth ||
      nodeCount > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount
    ) {
      return { depth, nodeCount };
    }
    // Leaf native-script variants have no children to enqueue.
    // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
    switch (current.script.type) {
      case "all":
      case "any":
      case "atLeast":
        for (
          let index = current.script.scripts.length - 1;
          index >= 0;
          index -= 1
        ) {
          pending.push({
            script: current.script.scripts[index]!,
            depth: current.depth + 1,
          });
        }
        break;
    }
  }

  return { depth, nodeCount };
};

export const nativeScriptBoundViolation = (
  script: MidgardNativeScript,
  featureId: string,
): MidgardConsensusViolation | null => {
  const complexity = nativeScriptComplexity(script);
  if (complexity.depth > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth) {
    return violation(
      "E_NATIVE_SCRIPT_DEPTH",
      featureId,
      `${complexity.depth.toString()} > ${MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth.toString()}`,
    );
  }
  if (
    complexity.nodeCount > MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount
  ) {
    return violation(
      "E_NATIVE_SCRIPT_NODE_COUNT",
      featureId,
      `${complexity.nodeCount.toString()} > ${MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount.toString()}`,
    );
  }
  return null;
};

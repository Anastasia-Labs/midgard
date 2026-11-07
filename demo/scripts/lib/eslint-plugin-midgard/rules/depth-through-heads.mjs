// F7: depth decisions go through the heads module.
//
// Plan §9 (docs/exec-plans/l1-architecture-plan.md): one module per process
// knows k and cd and how depth is counted. Before it existed, three
// incompatible depth formulas compared against `confirmationDepth` (head −
// block + 1, head − block, and head − cd) and each disagreed with the others
// by one block. A role takes its levels from
// `@al-ft/midgard-l1-follower` (`depth`, `heightAtDepth`, `levelAtDepth`,
// `isSafe`, `isFinal`, `createHeads`) instead.
//
// Heuristic: the rule reports an ordering comparison (`<`, `<=`, `>`, `>=`)
// that reads cd or k on either side and also reads anything else that is not
// a constant (`depth < cd`, `head - cd >= h`). An expression "reads" a
// parameter when it names one (`confirmationDepth`,
// `minimumConfirmationDepth`, `automaticRecoveryMaxDepth`,
// `securityParameter`, `securityParam` and their snake_case spellings) as an
// identifier or a property, directly or through a local constant,
// arithmetic, a call argument, `??`/`||` or a conditional. Shape validation
// passes: a comparison that reads nothing but depth parameters, constants
// (`Number.MAX_SAFE_INTEGER` included) and `typeof` (`cd < 1`, `k < cd`).
// Equality is not checked, so configuration agreement checks
// (`manifest.confirmationDepth !== config.depth`) pass. A value returned by a
// function imported from `@al-ft/midgard-l1-follower` (`heightAtDepth(tip,
// cd + 1)`) already went through the heads module and reads no parameter.
// The heads module itself is exempt.

import { defineRule } from "../baseline.mjs";
import {
  constantInitializer,
  constantNumber,
  staticName,
  unwrap,
} from "../ast.mjs";

export const HEADS_MODULE = "midgard-l1-follower/src/heads.ts";

const DEPTH_PARAMETER =
  /^(?:confirmationDepth|minimumConfirmationDepth|automaticRecoveryMaxDepth|securityParameter|securityParam|confirmation_depth|minimum_confirmation_depth|automatic_recovery_max_depth|security_parameter)$/u;

const COMPARISON = new Set(["<", "<=", ">", ">="]);

const MAX_HOPS = 5;

const HEADS_PACKAGE = /^@al-ft\/midgard-l1-follower(?:\/|$)/u;

// Whether `identifier` is a binding imported from the follower package.
const importedFromHeads = (sourceCode, identifier) => {
  let scope = sourceCode.getScope(identifier);
  while (scope !== null) {
    const variable = scope.set.get(identifier.name);
    if (variable !== undefined) {
      const [definition] = variable.defs;
      return (
        variable.defs.length === 1 &&
        definition.type === "ImportBinding" &&
        HEADS_PACKAGE.test(definition.parent.source.value)
      );
    }
    scope = scope.upper;
  }
  return false;
};

// Whether `node` reads a depth parameter, and whether it reads anything else
// that is not a constant.
const classify = (sourceCode, node, hops = 0) => {
  const expression = unwrap(node);
  const none = { depth: false, other: false };
  if (expression === undefined || expression === null) return none;
  if (hops > MAX_HOPS) return { depth: false, other: true };
  const recurse = (child) => classify(sourceCode, child, hops + 1);
  const merge = (...parts) =>
    parts.map(recurse).reduce(
      (left, right) => ({
        depth: left.depth || right.depth,
        other: left.other || right.other,
      }),
      none,
    );
  if (constantNumber(sourceCode, expression) !== undefined) return none;
  switch (expression.type) {
    case "Literal":
    case "TemplateLiteral":
      return none;
    case "UnaryExpression":
      return expression.operator === "typeof"
        ? none
        : recurse(expression.argument);
    case "Identifier": {
      if (expression.name === "undefined") return none;
      if (DEPTH_PARAMETER.test(expression.name)) {
        return { depth: true, other: false };
      }
      const init = constantInitializer(sourceCode, expression);
      return init === undefined ? { depth: false, other: true } : recurse(init);
    }
    case "MemberExpression": {
      const name = staticName(expression.property, expression.computed);
      if (name !== undefined && DEPTH_PARAMETER.test(name)) {
        return { depth: true, other: false };
      }
      const object = unwrap(expression.object);
      return object.type === "Identifier" && object.name === "Number"
        ? none
        : { depth: false, other: true };
    }
    case "BinaryExpression":
    case "LogicalExpression":
      return merge(expression.left, expression.right);
    case "ConditionalExpression":
      return merge(expression.consequent, expression.alternate);
    case "CallExpression":
      if (
        expression.callee.type === "Identifier" &&
        importedFromHeads(sourceCode, expression.callee)
      ) {
        return { depth: false, other: true };
      }
      return expression.arguments.length === 0
        ? { depth: false, other: true }
        : merge(...expression.arguments);
    case "AwaitExpression":
      return recurse(expression.argument);
    default:
      return { depth: false, other: true };
  }
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Forbid comparisons against confirmationDepth or k outside the heads module.",
    },
    schema: [],
    messages: {
      directDepthComparison:
        "This compares against cd (`confirmationDepth`) or k (`automaticRecoveryMaxDepth`) directly. Depth is counted one way only, in the heads module (plan §9): the tip is depth 1, `safe` is depth ≥ cd (liveness only) and `final` is depth > k. Fix: compute the depth with `depth(tipHeight, pointHeight)` from `@al-ft/midgard-l1-follower` and decide with `isSafe`, `isFinal`, `levelAtDepth` or `heightAtDepth` (or a `Heads` instance), never with your own formula (docs/agents/lint-rules.md).",
    },
  },
  create: (context, report, file) =>
    file === HEADS_MODULE
      ? {}
      : {
          BinaryExpression(node) {
            if (!COMPARISON.has(node.operator)) return;
            const left = classify(context.sourceCode, node.left);
            const right = classify(context.sourceCode, node.right);
            if ((left.depth || right.depth) && (left.other || right.other)) {
              report({ node, messageId: "directDepthComparison" });
            }
          },
        },
});

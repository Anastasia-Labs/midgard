// W4.6: a pinned fee needs a transaction with no change output.
//
// Validators that read the fee as a protocol payment check `fee == penalty`.
// `setMinFee` only sets a floor: when Lucid still has to add a change output,
// `add_change_if_needed` charges that output's own marginal fee on top, and
// the transaction pays about 3k lovelace more than the penalty, which the
// validator refuses. The construction that lands exactly is coin selection
// off plus an explicit remainder output
// (midgard-sdk/src/operator-lifecycle/exact-fee.ts: `planExactFeeBalance`,
// `exactFeeCompleteOptions`, and `exactFeeViolation` to check the result).
//
// Heuristic: a `setMinFee(...)` call passes when a completion in the same
// function, or in one enclosing or enclosed by it, visibly turns coin
// selection off: `coinSelection: false` in its options (directly, through a
// spread, inside a helper call's arguments, or through a local constant), or
// options built by `exactFeeCompleteOptions`. It does not prove that no change
// output is added; `exactFeeViolation` is the check that does.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  calleeNameNode,
  constantInitializer,
  enclosingFunction,
  functionChain,
  staticName,
  unwrap,
} from "../ast.mjs";

const COMPLETIONS = new Set(["complete", "completeSafe", "completeProgram"]);
const EXACT_FEE_OPTION_BUILDERS = new Set(["exactFeeCompleteOptions"]);

const disablesCoinSelection = (sourceCode, node, depth = 0) => {
  const expression = unwrap(node);
  if (expression === undefined || depth > 4) return false;
  switch (expression.type) {
    case "ObjectExpression":
      return expression.properties.some((property) =>
        property.type === "SpreadElement"
          ? disablesCoinSelection(sourceCode, property.argument, depth + 1)
          : staticName(property.key, property.computed) === "coinSelection" &&
            unwrap(property.value).type === "Literal" &&
            unwrap(property.value).value === false,
      );
    case "CallExpression":
      return (
        EXACT_FEE_OPTION_BUILDERS.has(calleeName(expression)) ||
        expression.arguments.some((argument) =>
          disablesCoinSelection(sourceCode, argument, depth + 1),
        )
      );
    case "Identifier": {
      const init = constantInitializer(sourceCode, expression);
      return (
        init !== undefined && disablesCoinSelection(sourceCode, init, depth + 1)
      );
    }
    case "ConditionalExpression":
      return (
        disablesCoinSelection(sourceCode, expression.consequent, depth + 1) &&
        disablesCoinSelection(sourceCode, expression.alternate, depth + 1)
      );
    default:
      return false;
  }
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require a pinned fee (`setMinFee`) to be completed with coin selection off and no change output.",
    },
    schema: [],
    messages: {
      changeOutput:
        "`setMinFee` is only a floor: while Lucid still adds a change output it charges that output's fee on top (about 3k lovelace), and every on-chain `fee == penalty` check refuses the transaction. Fix: balance it yourself with `planExactFeeBalance` from midgard-sdk/src/operator-lifecycle/exact-fee.ts (an explicit remainder output), complete in this function with `exactFeeCompleteOptions(plan)` or `{ coinSelection: false, ... }`, and assert `exactFeeViolation(tx, plan) === null`. This rule only sees a completion in the same function (or one nested with it) that visibly turns coin selection off.",
    },
  },
  create(context, report, file) {
    if (file.startsWith("lucid-midgard/")) return {};
    const { sourceCode } = context;
    const pinned = [];
    const completions = [];
    return {
      CallExpression(node) {
        const name = calleeName(node);
        if (unwrap(node.callee).type !== "MemberExpression") return;
        if (name === "setMinFee") pinned.push(node);
        if (COMPLETIONS.has(name)) completions.push(node);
      },
      "Program:exit"() {
        const exact = completions
          .filter((call) =>
            disablesCoinSelection(sourceCode, call.arguments[0]),
          )
          .map((call) => ({
            inner: enclosingFunction(sourceCode, call),
            chain: functionChain(sourceCode, call),
          }));
        for (const node of pinned) {
          const inner = enclosingFunction(sourceCode, node);
          const chain = functionChain(sourceCode, node);
          const covered = exact.some(
            (completion) =>
              completion.chain.includes(inner) ||
              chain.includes(completion.inner),
          );
          if (!covered) {
            report({ node: calleeNameNode(node), messageId: "changeOutput" });
          }
        }
      },
    };
  },
});

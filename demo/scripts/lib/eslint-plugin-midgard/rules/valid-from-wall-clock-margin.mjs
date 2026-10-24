// W4.8: a validity lower bound taken from the wall clock keeps a margin.
//
// Cardano node submit validation can lag the local wall clock, so a
// transaction whose `validFrom` is "now" races it and fails with
// OutsideValidityInterval even though every local check passed
// (docs/agents/transaction-finalization.md#validity-windows). The lower bound
// must sit at least 30 seconds before the current time, or be
// `Math.max(protocolLowerBound, now - backoff)` when a protocol rule sets a
// later one.
//
// Heuristic: the rule follows the value handed to `.validFrom(...)` or to a
// `validFrom*` property through local constants and through call arguments
// (`BigInt`, `Math.max`, slot conversions), looking for `Date.now()` or
// `new Date().getTime()`, a parameter that defaults to one, or a parameter
// named like the current time (`now`, `nowMs`, `currentTime`, ...). A
// wall-clock term passes once a constant of at least 30,000 (ms) is
// subtracted from it. A subtrahend it cannot evaluate is given the benefit of
// the doubt.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  constantInitializer,
  constantNumber,
  isParameter,
  parameterDefault,
  staticName,
  unwrap,
} from "../ast.mjs";

export const MINIMUM_MARGIN_MS = 30_000;

// A parameter with one of these names is taken to carry the current time.
const NOW_NAME =
  /^(?:now|nowMs|nowMillis|nowUnixTime|nowUnixMs|currentTime|currentTimeMs|wallClockMs)$/u;

const isWallClock = (expression) => {
  if (expression.type !== "CallExpression") return false;
  const callee = unwrap(expression.callee);
  if (callee.type !== "MemberExpression") return false;
  const method = staticName(callee.property, callee.computed);
  const object = unwrap(callee.object);
  if (method === "now") {
    return object.type === "Identifier" && object.name === "Date";
  }
  return (
    (method === "getTime" || method === "valueOf") &&
    object.type === "NewExpression" &&
    object.callee.type === "Identifier" &&
    object.callee.name === "Date" &&
    object.arguments.length === 0
  );
};

const unmarginedWallClock = (sourceCode, node, depth = 0) => {
  const expression = unwrap(node);
  if (expression === undefined || expression === null || depth > 6) {
    return false;
  }
  if (isWallClock(expression)) return true;
  const recurse = (child) => unmarginedWallClock(sourceCode, child, depth + 1);
  switch (expression.type) {
    case "Identifier": {
      const source =
        constantInitializer(sourceCode, expression) ??
        parameterDefault(sourceCode, expression);
      if (source !== undefined) return recurse(source);
      return (
        NOW_NAME.test(expression.name) && isParameter(sourceCode, expression)
      );
    }
    case "BinaryExpression": {
      if (expression.operator === "-") {
        const margin = constantNumber(sourceCode, expression.right);
        if (margin === undefined) return false;
        return margin < MINIMUM_MARGIN_MS && recurse(expression.left);
      }
      return recurse(expression.left) || recurse(expression.right);
    }
    case "CallExpression":
      return expression.arguments.some(recurse);
    case "ConditionalExpression":
      return recurse(expression.consequent) || recurse(expression.alternate);
    case "LogicalExpression":
      return recurse(expression.left) || recurse(expression.right);
    case "AwaitExpression":
      return recurse(expression.argument);
    default:
      return false;
  }
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require validity lower bounds derived from the wall clock to sit at least 30 seconds in the past.",
    },
    schema: [],
    messages: {
      noMargin:
        "This validity lower bound comes from the wall clock without the 30-second margin: node submit validation can lag the local clock, and the transaction fails with OutsideValidityInterval. Fix: subtract at least 30 seconds (prefer `Date.now() - 60_000` on production and e2e paths); when a protocol rule sets a later bound, use `Math.max(protocolLowerBound, Date.now() - backoff)` and never go below that bound; then recompute `validTo` from the result (docs/agents/transaction-finalization.md#validity-windows). In an emulator test, take times from `emulator.now()` instead.",
    },
  },
  create(context, report) {
    const { sourceCode } = context;
    const check = (value) => {
      if (unmarginedWallClock(sourceCode, value)) {
        report({ node: value, messageId: "noMargin" });
      }
    };
    return {
      CallExpression(node) {
        if (
          unwrap(node.callee).type === "MemberExpression" &&
          calleeName(node) === "validFrom" &&
          node.arguments.length > 0
        ) {
          check(node.arguments[0]);
        }
      },
      Property(node) {
        if (node.parent.type !== "ObjectExpression") return;
        const key = staticName(node.key, node.computed);
        if (key !== undefined && /validFrom/iu.test(key)) check(node.value);
      },
    };
  },
});

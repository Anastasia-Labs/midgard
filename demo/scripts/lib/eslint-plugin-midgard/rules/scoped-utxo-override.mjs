// W4.7: a pinned Lucid wallet view is released where it was pinned.
//
// `lucid.overrideUTxOs(utxos)` makes the wallet read that list from then on,
// for every later build on the same instance: the provider is never asked
// again. A pinned coin that something else spent fails as "Could not spend
// UTxO", and a live coin the pin lacks gets no vkey witness, because the seed
// wallet discovers its own keys from the pinned view ("Missing vkey witness"
// for your own key). Builds that may follow someone else's pin clear it first
// (midgard-node/src/transactions/operators/funding-preflight.ts).
//
// The rule sees pins, not builds, so it enforces the half it can: an
// `overrideUTxOs` call must be followed, in the same function or one nested
// with it, by `clearUTxOOverride()` on the same receiver (matched by source
// text), typically in a `finally`. A pin or release invoked through
// `Function.prototype.call` or `.apply` (`overrideUTxOs.call(lucid, utxos)`,
// `lucid.overrideUTxOs.apply(lucid, [utxos])`) counts the same, with the
// first argument as its receiver.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  calleeNameNode,
  enclosingFunction,
  functionChain,
  receiverOf,
  staticName,
  unwrap,
} from "../ast.mjs";

const PIN = "overrideUTxOs";
const RELEASES = new Set(["clearUTxOOverride", "clearUTxOOverrides"]);
const REBINDS = new Set(["call", "apply"]);

/**
 * The method a call invokes on a wallet, its receiver, and the node naming
 * it: `receiver.name(...)` directly, or `name.call(receiver, ...)`,
 * `x.name.call(receiver, ...)` and the `.apply` forms through
 * `Function.prototype`.
 */
const walletCall = (node) => {
  const name = calleeName(node);
  if (REBINDS.has(name)) {
    const method = unwrap(receiverOf(node));
    const receiver = node.arguments[0];
    if (method === undefined || receiver === undefined) return undefined;
    if (receiver.type === "SpreadElement") return undefined;
    if (method.type === "Identifier") {
      return { name: method.name, receiver, nameNode: method };
    }
    if (method.type === "MemberExpression") {
      return {
        name: staticName(method.property, method.computed),
        receiver,
        nameNode: method.property,
      };
    }
    return undefined;
  }
  const receiver = receiverOf(node);
  if (receiver === undefined) return undefined;
  return { name, receiver, nameNode: calleeNameNode(node) };
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require every `overrideUTxOs` pin to be released with `clearUTxOOverride()` in the function that set it.",
    },
    schema: [],
    messages: {
      unreleasedPin:
        "This pins the wallet's UTxO view for every later build on `{{receiver}}`: a pinned coin spent elsewhere fails as 'Could not spend UTxO', and a live coin missing from the pin gets no vkey witness ('Missing vkey witness' for your own key). Fix: release it after the build that needs it, in this function (`try { {{receiver}}.overrideUTxOs(...); ... } finally { {{receiver}}.clearUTxOOverride(); }`); code that builds from an instance something else may have pinned calls `clearUTxOOverride()` before building, as midgard-node/src/transactions/operators/funding-preflight.ts does.",
    },
  },
  create(context, report, file) {
    if (file.startsWith("lucid-midgard/")) return {};
    const { sourceCode } = context;
    const pins = [];
    const releases = [];
    return {
      CallExpression(node) {
        const call = walletCall(node);
        if (call === undefined) return;
        if (call.name === PIN) pins.push({ node, ...call });
        if (RELEASES.has(call.name)) releases.push({ node, ...call });
      },
      "Program:exit"() {
        const released = releases.map(({ node, receiver }) => ({
          text: sourceCode.getText(receiver),
          start: node.range[0],
          inner: enclosingFunction(sourceCode, node),
          chain: functionChain(sourceCode, node),
        }));
        for (const { node, receiver, nameNode } of pins) {
          const text = sourceCode.getText(receiver);
          const inner = enclosingFunction(sourceCode, node);
          const chain = functionChain(sourceCode, node);
          const covered = released.some(
            (release) =>
              release.text === text &&
              release.start > node.range[0] &&
              (release.chain.includes(inner) || chain.includes(release.inner)),
          );
          if (!covered) {
            report({
              node: nameNode,
              messageId: "unreleasedPin",
              data: { receiver: text },
            });
          }
        }
      },
    };
  },
});

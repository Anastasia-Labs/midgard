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
// text), typically in a `finally`.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  calleeNameNode,
  enclosingFunction,
  functionChain,
  receiverOf,
} from "../ast.mjs";

const RELEASES = new Set(["clearUTxOOverride", "clearUTxOOverrides"]);

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
        const receiver = receiverOf(node);
        if (receiver === undefined) return;
        const name = calleeName(node);
        if (name === "overrideUTxOs") pins.push({ node, receiver });
        if (RELEASES.has(name)) releases.push({ node, receiver });
      },
      "Program:exit"() {
        const released = releases.map(({ node, receiver }) => ({
          text: sourceCode.getText(receiver),
          start: node.range[0],
          inner: enclosingFunction(sourceCode, node),
          chain: functionChain(sourceCode, node),
        }));
        for (const { node, receiver } of pins) {
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
              node: calleeNameNode(node),
              messageId: "unreleasedPin",
              data: { receiver: text },
            });
          }
        }
      },
    };
  },
});

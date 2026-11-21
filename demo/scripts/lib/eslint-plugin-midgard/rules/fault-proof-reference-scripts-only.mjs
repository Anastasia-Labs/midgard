// W4.4: fault-proof scripts are never attached inline.
//
// Owner ruling (2026-08-26): fault-proof step validators and the scripts that
// support them deploy as published reference scripts and are referenced from
// the transaction, whatever their compiled size. That keeps the whole L1
// envelope for redeemers. Submitters take the hash-checked carriage in
// midgard-fault-proofs/src/witness-reference-scripts.ts, which has no inline
// fallback, and the workflow boundary refuses a signed transaction that
// embeds a script (`requireReferenceOnlyScriptWitnesses`).
//
// The rule cannot tell which script a call attaches, so it bans Lucid's
// inline attach API (`tx.attach.<Kind>(script)`) in the source trees that
// build fault-proof transactions.

import { defineRule } from "../baseline.mjs";
import { staticName, unwrap } from "../ast.mjs";

export const FAULT_PROOF_SOURCE_TREES = [
  "midgard-fault-proofs/src/",
  "midgard-sdk/src/fraud-proof/",
  "midgard-watcher/src/fault-proofs/",
];

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Ban inline script attachment in the source trees that build fault-proof transactions.",
    },
    schema: [],
    messages: {
      inlineAttach:
        "Fault-proof scripts deploy as published reference scripts and are never attached inline (owner ruling 2026-08-26). Fix: publish the script to a reference-script UTxO and reach it through `readFrom` with the hash-checked carriage in midgard-fault-proofs/src/witness-reference-scripts.ts (`witnessSpendingValidatorCarriage` and its siblings). An inline witness that only an emulator test needs belongs in that test's harness, not in this source tree.",
    },
  },
  create: (_context, report, file) =>
    FAULT_PROOF_SOURCE_TREES.some((tree) => file.startsWith(tree))
      ? {
          CallExpression(node) {
            const callee = unwrap(node.callee);
            if (callee.type !== "MemberExpression") return;
            const namespace = unwrap(callee.object);
            if (
              namespace.type === "MemberExpression" &&
              staticName(namespace.property, namespace.computed) === "attach"
            ) {
              report({ node: callee.property, messageId: "inlineAttach" });
            }
          },
        }
      : {},
});

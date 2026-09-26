// W4.3: L1 completion evaluates scripts locally, everywhere.
//
// With `localUPLCEval: false`, Lucid hands evaluation to the provider, and the
// lucid-evolution emulator's `evaluateTx` echoes the budgets already in the
// transaction without running any script: the validator never executes and
// a test built that way proves nothing about it. Production must evaluate
// locally too (docs/agents/transaction-finalization.md).
//
// This generalizes the source scan that used to live in
// midgard-fault-proofs/src/proof-fit/limit-escape-scan.ts and covered that
// package only. It inspects option writes, not text: a property, a shorthand
// property, a destructured binding (a default there does not constrain what a
// caller passes) and an assignment all count, and only the literal `true`
// passes. Writes between `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN` and
// `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END` comments are retained diagnostics
// and are allowed; a marker without its partner is reported.

import { defineRule } from "../baseline.mjs";
import { staticName, unwrap } from "../ast.mjs";

const OPTION = "localUPLCEval";
const MARKER = /MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_(BEGIN|END)/gu;

const isLiteralTrue = (node) => {
  const value = unwrap(node);
  return value?.type === "Literal" && value.value === true;
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require `localUPLCEval: true` for every Lucid Evolution completion outside marked diagnostics.",
    },
    schema: [],
    messages: {
      notLocal:
        "`localUPLCEval` must be the literal `true`: with anything else Lucid hands evaluation to the provider, and the emulator's `evaluateTx` runs no script at all, so the validator never executes. Fix: complete with `{ localUPLCEval: true }` (docs/agents/transaction-finalization.md); pass it literally rather than through a variable, and do not destructure it. A retained unpublishable diagnostic goes between `// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN` and `// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END` comments.",
      malformedMarker:
        "Unbalanced diagnostic marker: every `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN` comment needs a later `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END`, and an END needs an open BEGIN. Fix: add the missing partner comment around the diagnostic, or delete the stray marker.",
    },
  },
  create(context, report) {
    const writes = [];
    const record = (node) => writes.push(node);
    return {
      Property(node) {
        if (staticName(node.key, node.computed) !== OPTION) return;
        if (node.parent.type === "ObjectPattern") {
          record(node);
        } else if (node.shorthand || !isLiteralTrue(node.value)) {
          record(node);
        }
      },
      AssignmentExpression(node) {
        if (node.operator !== "=") return;
        const target = unwrap(node.left);
        if (
          target.type === "MemberExpression" &&
          staticName(target.property, target.computed) === OPTION &&
          !isLiteralTrue(node.right)
        ) {
          record(node);
        }
      },
      "Program:exit"() {
        const markers = context.sourceCode.getAllComments().flatMap((comment) =>
          [...comment.value.matchAll(MARKER)].map((match) => ({
            position: comment.range[0],
            comment,
            begin: match[1] === "BEGIN",
          })),
        );
        const events = [
          ...writes.map((node) => ({ position: node.range[0], node })),
          ...markers,
        ].sort((a, b) => a.position - b.position);
        const open = [];
        for (const event of events) {
          if (event.node !== undefined) {
            if (open.length === 0) {
              report({ node: event.node, messageId: "notLocal" });
            }
          } else if (event.begin) {
            open.push(event.comment);
          } else if (open.pop() === undefined) {
            report({ loc: event.comment.loc, messageId: "malformedMarker" });
          }
        }
        for (const comment of open) {
          report({ loc: comment.loc, messageId: "malformedMarker" });
        }
      },
    };
  },
});

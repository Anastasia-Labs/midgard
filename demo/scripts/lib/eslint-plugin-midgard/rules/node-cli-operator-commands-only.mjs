// W4.8: the operator binary registers operator commands only.
//
// demo/midgard-node/AGENTS.md: keep `midgard-node` focused on operating the
// node; test, demo, benchmark and acceptance commands belong to
// midgard-node-tools, which has its own binary. The rule reads the command
// names registered in midgard-node/src/index.ts and flags a name that says it
// is one of those.

import { defineRule } from "../baseline.mjs";
import { calleeName, unwrap } from "../ast.mjs";

export const NODE_CLI_ENTRYPOINT = "midgard-node/src/index.ts";

export const NON_OPERATOR_WORD =
  /(?:^|[-_:.])(?:tests?|testing|demos?|bench|benchmarks?|e2e|acceptance|smoke|soak|stress|fixtures?|throughput|load-test|loadtest)(?=$|[-_:.])/iu;

// The command's name: the first word of the specification string, with the
// dynamic parts of a template literal replaced by `*`.
const commandName = (node) => {
  const argument = unwrap(node);
  if (argument === undefined) return undefined;
  if (argument.type === "Literal" && typeof argument.value === "string") {
    return argument.value.trim().split(/\s+/u)[0];
  }
  if (argument.type === "TemplateLiteral") {
    return argument.quasis
      .map((quasi) => quasi.value.cooked)
      .join("*")
      .trim()
      .split(/\s+/u)[0];
  }
  return undefined;
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Keep test, demo and benchmark commands out of the operator binary's command registry.",
    },
    schema: [],
    messages: {
      nonOperatorCommand:
        "`{{name}}` reads as a test, demo or benchmark command, and the operator binary registers only commands that operate the node. Fix: register it in demo/midgard-node-tools/src/index.ts (the tooling binary) and import what it needs from the node's declared `midgard-node/<subpath>` exports (demo/midgard-node/AGENTS.md).",
    },
  },
  create: (_context, report, file) =>
    file === NODE_CLI_ENTRYPOINT
      ? {
          "CallExpression, NewExpression"(node) {
            const isRegistration =
              node.type === "NewExpression"
                ? unwrap(node.callee).type === "Identifier" &&
                  unwrap(node.callee).name === "Command"
                : unwrap(node.callee).type === "MemberExpression" &&
                  calleeName(node) === "command";
            if (!isRegistration) return;
            const name = commandName(node.arguments[0]);
            if (name !== undefined && NON_OPERATOR_WORD.test(name)) {
              report({
                node: node.arguments[0],
                messageId: "nonOperatorCommand",
                data: { name },
              });
            }
          },
        }
      : {},
});

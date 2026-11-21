// A ledger index in redeemer data comes from the builder's final transaction.
//
// The ledger orders transaction inputs and reference inputs canonically, by
// transaction id and then output index, and only the builder knows the final
// set once coin selection, fee inputs, collateral and reference scripts are
// in. An index derived by hand (a literal, a position in a list the caller
// sorted itself, or a resolver handed a list the caller assembled) is right
// only while that list happens to equal the final one, and breaks the first
// time an input sorts differently. Lucid hands a redeemer callback the final
// transaction: `(ctx) => ...` (`BuildTxWithRedeemer`), with
// `requireInputIndex(ctx, utxo, label)`, `requireReferenceInputIndex`,
// `requireUniqueOutputIndex(ctx.outputs, ...)` and the `require*RedeemerIndex`
// helpers in midgard-sdk/src/tx-context-redeemer.ts, and
// `ctx.referenceInputs` for the carriage resolvers.
//
// What it checks (syntactically, in one file):
//
//   1. A ledger-index property in redeemer data: an object inside the first
//      argument of `Data.to(value, <...Redeemer>)`, or inside the redeemer
//      argument of `collectFrom`, `mintAssets` or `withdraw` (including the
//      arguments of a `...Redeemer(...)` constructor call there). A property
//      is a ledger index when its key, read as snake_case, ends in
//      `input_index`, `output_index` or `redeemer_index` (or `_indices`),
//      unless the object is an output reference (it also has a
//      `transaction_id`, `tx_id` or `txHash` key) or the key names an output
//      reference's own `..._input_output_index`. The value is reported when
//      it is
//        - a numeric literal (or a list of them) outside a builder callback;
//        - computed from a sort, or a search (`findIndex`, `indexOf`,
//          `findOutRefIndex`) over a list, that does not come from the
//          builder context.
//      Values are followed through local constants, `BigInt`/`Number` and
//      spreads.
//   2. A call to a carriage index resolver (`faultProofFieldOpening`,
//      `faultProofFieldCarriage`, `faultProofRawFieldCarriage`,
//      `resolveChunkReferenceIndices`, `resolveCertificateReferenceIndex`,
//      `txOrderMaterialCarriageVector`) whose `referenceInputs` is a list the
//      caller assembled rather than the builder context's (or a parameter
//      passed through unchanged).
//
// A builder callback is a function passed as a redeemer to one of the
// builder methods above, one typed `BuildTxWithRedeemer` (annotation, `as` or
// `satisfies`), a `RedeemerBuilder`'s `makeRedeemer`, or one whose first
// parameter is named `ctx` or typed `RedeemerContext`. A value comes from the
// builder context when it mentions a builder callback's parameter, directly
// or through a local constant.
//
// Scope: lucid-midgard/src (the builder itself) is skipped. In test files only
// code inside a function that builds a transaction (`newTx`, `collectFrom`,
// `mintAssets`, `withdraw`, `readFrom`) is checked, so wire vectors that
// encode a redeemer without building are not reported.
//
// Blind spots (precision over recall):
//   - a literal inside a builder callback or `makeRedeemer` is trusted, since
//     the callback has the final transaction to check it against;
//   - an index computed in another function, including an imported helper
//     that sorts or searches (`index(list, utxo)` from another module), and
//     passed in as a value or parameter is trusted;
//   - `new Constr(...)` fields carry no key and are not checked;
//   - a resolver handed a parameter is trusted to receive the builder's list.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  constantInitializer,
  enclosingFunction,
  isFunction,
  staticName,
  unwrap,
} from "../ast.mjs";
import {
  REDEEMER_ARGUMENT,
  RESOLVERS,
  BUILDER_CALLS,
  isDataTo,
  SEARCHES,
  SORTS,
  OUT_REF_KEYS,
  MAX_DEPTH,
  isLedgerIndexKey,
  typeName,
  isRedeemerSchema,
} from "../redeemer-index-shapes.mjs";

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Require ledger input, output and redeemer indices in redeemer data to come from the builder's final transaction.",
    },
    schema: [],
    messages: {
      literal:
        "`{{key}}` is a ledger index fixed by hand: the ledger orders inputs by (tx id, output index) and only the builder knows the final transaction, so a literal is right only while nothing else is added. Fix: build this redeemer in a `(ctx) => ...` callback (`BuildTxWithRedeemer`) and take the index from it: `requireInputIndex(ctx, utxo, label)`, `requireReferenceInputIndex(ctx, utxo, label)`, `requireUniqueOutputIndex(ctx.outputs, predicate, label)` or a `require*RedeemerIndex` helper (midgard-sdk/src/tx-context-redeemer.ts).",
      handOrdered:
        "`{{key}}` is a position in a list this code sorted or searched itself, not in the builder's final transaction: coin selection, fee inputs and reference scripts change the ledger's order, so the index is right only by coincidence. Fix: derive it in the `(ctx) => ...` redeemer callback from `ctx` (`requireInputIndex(ctx, utxo, label)`, `requireReferenceInputIndex(ctx, utxo, label)`, `requireUniqueOutputIndex(ctx.outputs, predicate, label)`).",
      resolver:
        "`{{callee}}` resolves reference-input positions against the list it is given, and this list was assembled by hand: a reference input the builder adds (a reference script, a certificate) shifts every position after it. Fix: call it inside the `(ctx) => ...` redeemer callback with `referenceInputs: ctx.referenceInputs`.",
    },
  },
  create(context, report, file) {
    if (file.startsWith("lucid-midgard/src/")) return {};
    const testFile = /(?:^|\/)tests?\/|\.test\.[cm]?[jt]s$/u.test(file);
    const { sourceCode } = context;
    const callbacks = new Set();
    const checkedObjects = new Set();

    const markCallback = (node) => {
      const expression = unwrap(node);
      if (expression !== undefined && isFunction(expression)) {
        callbacks.add(expression);
      }
    };

    const isCallbackByShape = (fn) => {
      if (callbacks.has(fn)) return true;
      const [first] = fn.params;
      if (first !== undefined) {
        const parameter =
          first.type === "AssignmentPattern" ? first.left : first;
        if (parameter.type === "Identifier") {
          if (parameter.name === "ctx") return true;
          if (typeName(parameter.typeAnnotation) === "RedeemerContext") {
            return true;
          }
        }
      }
      const parent = fn.parent;
      // Lucid's `RedeemerBuilder`: `{ kind: "selected", makeRedeemer(indices) }`.
      if (
        parent?.type === "Property" &&
        staticName(parent.key, parent.computed) === "makeRedeemer"
      ) {
        return true;
      }
      if (
        (parent?.type === "TSSatisfiesExpression" ||
          parent?.type === "TSAsExpression") &&
        typeName(parent.typeAnnotation) === "BuildTxWithRedeemer"
      ) {
        return true;
      }
      if (
        parent?.type === "VariableDeclarator" &&
        typeName(parent.id.typeAnnotation) === "BuildTxWithRedeemer"
      ) {
        return true;
      }
      return false;
    };

    const enclosingCallbacks = (node) =>
      sourceCode
        .getAncestors(node)
        .filter(
          (ancestor) => isFunction(ancestor) && isCallbackByShape(ancestor),
        );

    const insideCallback = (node) => enclosingCallbacks(node).length > 0;

    // Whether `node` mentions a builder callback's parameter, directly or
    // through local constants.
    const fromBuilder = (node, depth = 0) => {
      if (node === null || node === undefined || depth > MAX_DEPTH) {
        return false;
      }
      let found = false;
      const visit = (current, level) => {
        if (found || current === null || typeof current !== "object") return;
        if (Array.isArray(current)) {
          for (const item of current) visit(item, level);
          return;
        }
        if (typeof current.type !== "string") return;
        if (current.type === "Identifier") {
          const variable = findVariable(current);
          if (variable === undefined) return;
          const [definition] = variable.defs;
          if (
            definition?.type === "Parameter" &&
            (current.name === "ctx" ||
              (isFunction(definition.node) &&
                isCallbackByShape(definition.node)))
          ) {
            found = true;
            return;
          }
          if (level < MAX_DEPTH) {
            const init = constantInitializer(sourceCode, current);
            if (init !== undefined) visit(init, level + 1);
          }
          return;
        }
        for (const [key, value] of Object.entries(current)) {
          if (key === "parent" || key === "loc" || key === "range") continue;
          if (key === "typeAnnotation" || key === "returnType") continue;
          if (
            current.type === "MemberExpression" &&
            key === "property" &&
            !current.computed
          ) {
            continue;
          }
          if (
            current.type === "Property" &&
            key === "key" &&
            !current.computed
          ) {
            continue;
          }
          visit(value, level);
        }
      };
      visit(node, depth);
      return found;
    };

    const findVariable = (identifier) => {
      let scope = sourceCode.getScope(identifier);
      while (scope !== null) {
        const variable = scope.set.get(identifier.name);
        if (variable !== undefined) return variable;
        scope = scope.upper;
      }
      return undefined;
    };

    const isPassThroughParameter = (node) => {
      const expression = unwrap(node);
      if (expression?.type !== "Identifier") return false;
      const variable = findVariable(expression);
      return variable?.defs[0]?.type === "Parameter";
    };

    const isNumericLiteral = (node, depth = 0) => {
      const expression = unwrap(node);
      if (expression === undefined || depth > MAX_DEPTH) return false;
      switch (expression.type) {
        case "Literal":
          return (
            typeof expression.value === "number" ||
            typeof expression.value === "bigint" ||
            expression.bigint !== undefined
          );
        case "ArrayExpression":
          return (
            expression.elements.length > 0 &&
            expression.elements.every(
              (element) =>
                element !== null && isNumericLiteral(element, depth + 1),
            )
          );
        case "CallExpression": {
          const name = calleeName(expression);
          return (
            (name === "BigInt" || name === "Number") &&
            expression.arguments.length === 1 &&
            isNumericLiteral(expression.arguments[0], depth + 1)
          );
        }
        case "Identifier": {
          const init = constantInitializer(sourceCode, expression);
          return init !== undefined && isNumericLiteral(init, depth + 1);
        }
        default:
          return false;
      }
    };

    // Whether the value is a position this code sorted or searched for in a
    // list that does not come from the builder context.
    const isHandOrdered = (node, depth = 0) => {
      const expression = unwrap(node);
      if (expression === undefined || depth > MAX_DEPTH) return false;
      switch (expression.type) {
        case "CallExpression": {
          const name = calleeName(expression);
          if (name === "BigInt" || name === "Number") {
            return expression.arguments.some((argument) =>
              isHandOrdered(argument, depth + 1),
            );
          }
          if (name === "map" && expression.arguments.length === 1) {
            const callee = unwrap(expression.callee);
            return (
              callee.type === "MemberExpression" &&
              isHandOrdered(callee.object, depth + 1)
            );
          }
          if (SEARCHES.has(name)) {
            const callee = unwrap(expression.callee);
            const list =
              name === "findOutRefIndex"
                ? expression.arguments[0]
                : callee.type === "MemberExpression"
                  ? callee.object
                  : undefined;
            return list !== undefined && !fromBuilder(list);
          }
          if (SORTS.has(name)) return !fromBuilder(expression);
          return false;
        }
        case "Identifier": {
          const init = constantInitializer(sourceCode, expression);
          return init !== undefined && isHandOrdered(init, depth + 1);
        }
        case "ConditionalExpression":
          return (
            isHandOrdered(expression.consequent, depth + 1) ||
            isHandOrdered(expression.alternate, depth + 1)
          );
        default:
          return false;
      }
    };

    const checkProperty = (property, attached) => {
      const key = staticName(property.key, property.computed);
      if (!isLedgerIndexKey(key)) return;
      const value =
        property.value.type === "AssignmentPattern"
          ? property.value.right
          : property.value;
      if (
        isNumericLiteral(value) &&
        !insideCallback(property) &&
        (attached || buildsTransaction(property))
      ) {
        report({ node: property.key, messageId: "literal", data: { key } });
        return;
      }
      if (isHandOrdered(value)) {
        report({ node: property.key, messageId: "handOrdered", data: { key } });
      }
    };

    // Walk redeemer data: objects, arrays, spreads, constructor-call
    // arguments and local constants.
    const walkRedeemer = (node, attached, depth = 0) => {
      const expression = unwrap(node);
      if (expression === undefined || depth > MAX_DEPTH) return;
      switch (expression.type) {
        case "ObjectExpression": {
          if (checkedObjects.has(expression)) return;
          checkedObjects.add(expression);
          const outRef = expression.properties.some(
            (property) =>
              property.type === "Property" &&
              OUT_REF_KEYS.has(staticName(property.key, property.computed)),
          );
          for (const property of expression.properties) {
            if (property.type === "SpreadElement") {
              walkRedeemer(property.argument, attached, depth + 1);
              continue;
            }
            if (!outRef) checkProperty(property, attached);
            walkRedeemer(property.value, attached, depth + 1);
          }
          return;
        }
        case "ArrayExpression":
          for (const element of expression.elements) {
            if (element !== null) walkRedeemer(element, attached, depth + 1);
          }
          return;
        case "SpreadElement":
          walkRedeemer(expression.argument, attached, depth + 1);
          return;
        case "ConditionalExpression":
          walkRedeemer(expression.consequent, attached, depth + 1);
          walkRedeemer(expression.alternate, attached, depth + 1);
          return;
        case "CallExpression": {
          const name = calleeName(expression);
          if (isDataTo(expression)) {
            walkRedeemer(expression.arguments[0], attached, depth + 1);
            return;
          }
          if (name !== undefined && /Redeemer$/u.test(name)) {
            for (const argument of expression.arguments) {
              walkRedeemer(argument, attached, depth + 1);
            }
          }
          return;
        }
        case "Identifier": {
          const init = constantInitializer(sourceCode, expression);
          if (init !== undefined) walkRedeemer(init, attached, depth + 1);
          return;
        }
        default:
          return;
      }
    };

    const builderCalls = [];
    let building;
    // Whether the innermost function around `node` builds a transaction
    // itself. Test files encode redeemers as wire vectors without building
    // anything; a literal there is not a ledger index of any transaction.
    const buildsTransaction = (node) => {
      if (!testFile) return true;
      building ??= new Set(
        builderCalls.map((call) => enclosingFunction(sourceCode, call)),
      );
      const inner = enclosingFunction(sourceCode, node);
      return (
        inner !== sourceCode.ast &&
        [...building].some(
          (fn) => fn === inner || sourceCode.getAncestors(fn).includes(inner),
        )
      );
    };
    const dataTo = [];
    const redeemerArguments = [];
    const resolverCalls = [];

    return {
      CallExpression(node) {
        const name = calleeName(node);
        const callee = unwrap(node.callee);
        if (isDataTo(node) && isRedeemerSchema(node.arguments[1])) {
          dataTo.push(node.arguments[0]);
        }
        if (BUILDER_CALLS.has(name) && callee.type === "MemberExpression") {
          builderCalls.push(node);
        }
        if (REDEEMER_ARGUMENT.has(name) && callee.type === "MemberExpression") {
          const argument = node.arguments[REDEEMER_ARGUMENT.get(name)];
          if (argument !== undefined) {
            markCallback(argument);
            redeemerArguments.push(argument);
          }
        }
        // Versioned names (`faultProofFieldOpeningV1`) are the same resolver.
        if (RESOLVERS.has(name?.replace(/V\d+$/u, "")))
          resolverCalls.push(node);
      },
      "Program:exit"() {
        for (const argument of redeemerArguments) walkRedeemer(argument, true);
        for (const value of dataTo) walkRedeemer(value, false);
        for (const call of resolverCalls) {
          const [options] = call.arguments;
          const object = unwrap(options);
          if (object?.type !== "ObjectExpression") continue;
          const property = object.properties.find(
            (candidate) =>
              candidate.type === "Property" &&
              staticName(candidate.key, candidate.computed) ===
                "referenceInputs",
          );
          if (property === undefined) continue;
          if (!buildsTransaction(call)) continue;
          if (fromBuilder(property.value)) continue;
          if (isPassThroughParameter(property.value)) continue;
          report({
            node: property.key,
            messageId: "resolver",
            data: { callee: calleeName(call) },
          });
        }
      },
    };
  },
});

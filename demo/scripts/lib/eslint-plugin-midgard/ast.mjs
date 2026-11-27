// Small ESTree helpers shared by the Midgard rules. The rules are syntactic:
// none of them needs type information, so they run under espree for .mjs
// files and under the typescript-eslint parser for .ts files alike.

const TYPE_WRAPPERS = new Set([
  "TSAsExpression",
  "TSSatisfiesExpression",
  "TSTypeAssertion",
  "TSNonNullExpression",
  "ChainExpression",
]);

/** The expression under any type assertion, `!` or optional-chain wrapper. */
export const unwrap = (node) =>
  node !== null && node !== undefined && TYPE_WRAPPERS.has(node.type)
    ? unwrap(node.expression)
    : node;

/** The static name of a property key or member property, if it has one. */
export const staticName = (node, computed) => {
  if (node === null || node === undefined) return undefined;
  if (!computed && node.type === "Identifier") return node.name;
  if (node.type === "Literal" && typeof node.value === "string") {
    return node.value;
  }
  if (
    node.type === "TemplateLiteral" &&
    node.expressions.length === 0 &&
    node.quasis.length === 1
  ) {
    return node.quasis[0].value.cooked;
  }
  return undefined;
};

/** The name of the method a call invokes: `x.name(...)` or `name(...)`. */
export const calleeName = (call) => {
  const callee = unwrap(call.callee);
  if (callee.type === "Identifier") return callee.name;
  if (callee.type === "MemberExpression") {
    return staticName(callee.property, callee.computed);
  }
  return undefined;
};

/** The node naming the called function: the property of `x.name(...)`. */
export const calleeNameNode = (call) => {
  const callee = unwrap(call.callee);
  return callee.type === "MemberExpression" ? callee.property : callee;
};

/** The receiver of a method call `receiver.name(...)`, or undefined. */
export const receiverOf = (call) => {
  const callee = unwrap(call.callee);
  return callee.type === "MemberExpression" ? callee.object : undefined;
};

export const isFunction = (node) =>
  node.type === "FunctionDeclaration" ||
  node.type === "FunctionExpression" ||
  node.type === "ArrowFunctionExpression";

/** The innermost function containing `node`, or the Program. */
export const enclosingFunction = (sourceCode, node) =>
  sourceCode.getAncestors(node).findLast(isFunction) ?? sourceCode.ast;

/** Every function containing `node`, innermost last, Program first. */
export const functionChain = (sourceCode, node) => [
  sourceCode.ast,
  ...sourceCode.getAncestors(node).filter(isFunction),
];

const resolveVariable = (sourceCode, identifier) => {
  let scope = sourceCode.getScope(identifier);
  while (scope !== null) {
    const variable = scope.set.get(identifier.name);
    if (variable !== undefined) return variable;
    scope = scope.upper;
  }
  return undefined;
};

const isReassigned = (variable) =>
  variable.references.some(
    (reference) => reference.isWrite() && !reference.init,
  );

/**
 * The initializer of the single, non-reassigned variable `identifier` refers
 * to, or undefined when there is none or it cannot be pinned down.
 */
export const constantInitializer = (sourceCode, identifier) => {
  const variable = resolveVariable(sourceCode, identifier);
  if (variable === undefined || variable.defs.length !== 1) return undefined;
  const [definition] = variable.defs;
  if (
    definition.type !== "Variable" ||
    definition.node.id.type !== "Identifier" ||
    definition.node.init === null ||
    isReassigned(variable)
  ) {
    return undefined;
  }
  return definition.node.init;
};

/**
 * The default value of the non-reassigned parameter `identifier` refers to
 * (`(nowMs = Date.now()) => ...`, also inside a destructured parameter), or
 * undefined.
 */
export const parameterDefault = (sourceCode, identifier) => {
  const variable = resolveVariable(sourceCode, identifier);
  if (variable === undefined || variable.defs.length !== 1) return undefined;
  const [definition] = variable.defs;
  if (definition.type !== "Parameter" || isReassigned(variable)) {
    return undefined;
  }
  const pattern = definition.name.parent;
  return pattern?.type === "AssignmentPattern" &&
    pattern.left === definition.name
    ? pattern.right
    : undefined;
};

/** Whether `identifier` names a function parameter (not a local). */
export const isParameter = (sourceCode, identifier) =>
  resolveVariable(sourceCode, identifier)?.defs[0]?.type === "Parameter";

/** A numeric value an expression certainly evaluates to, or undefined. */
export const constantNumber = (sourceCode, node, depth = 0) => {
  const expression = unwrap(node);
  if (expression === undefined || depth > 4) return undefined;
  switch (expression.type) {
    case "Literal":
      if (typeof expression.value === "number") return expression.value;
      if (typeof expression.value === "bigint") {
        return Number(expression.value);
      }
      if (expression.bigint !== undefined) return Number(expression.bigint);
      return undefined;
    case "Identifier": {
      const init = constantInitializer(sourceCode, expression);
      return init === undefined
        ? undefined
        : constantNumber(sourceCode, init, depth + 1);
    }
    case "BinaryExpression": {
      const left = constantNumber(sourceCode, expression.left, depth + 1);
      const right = constantNumber(sourceCode, expression.right, depth + 1);
      if (left === undefined || right === undefined) return undefined;
      if (expression.operator === "*") return left * right;
      if (expression.operator === "+") return left + right;
      if (expression.operator === "-") return left - right;
      return undefined;
    }
    case "CallExpression": {
      const name = calleeName(expression);
      return (name === "BigInt" || name === "Number") &&
        expression.arguments.length === 1
        ? constantNumber(sourceCode, expression.arguments[0], depth + 1)
        : undefined;
    }
    default:
      return undefined;
  }
};

// The syntactic shapes the redeemer-index-from-builder rule recognises:
// which builder methods take a redeemer, which keys are ledger indices, and
// which calls sort, search or resolve reference-input positions.

import { staticName, unwrap } from "./ast.mjs";

export const REDEEMER_ARGUMENT = new Map([
  ["collectFrom", 1],
  ["mintAssets", 1],
  ["withdraw", 2],
]);

export const RESOLVERS = new Set([
  "faultProofFieldOpening",
  "faultProofFieldCarriage",
  "faultProofRawFieldCarriage",
  "resolveChunkReferenceIndices",
  "resolveCertificateReferenceIndex",
  "txOrderMaterialCarriageVector",
]);

export const BUILDER_CALLS = new Set([
  "newTx",
  "collectFrom",
  "mintAssets",
  "withdraw",
  "readFrom",
]);

export const isDataTo = (node) => {
  const callee = unwrap(node.callee);
  return (
    callee.type === "MemberExpression" &&
    staticName(callee.property, callee.computed) === "to" &&
    unwrap(callee.object).type === "Identifier" &&
    unwrap(callee.object).name === "Data"
  );
};

export const SEARCHES = new Set([
  "findIndex",
  "findLastIndex",
  "indexOf",
  "lastIndexOf",
  "findOutRefIndex",
]);
export const SORTS = new Set(["sort", "toSorted", "dedupeAndSortUtxos"]);
export const OUT_REF_KEYS = new Set([
  "transaction_id",
  "tx_id",
  "txHash",
  "txId",
]);
export const MAX_DEPTH = 6;

const snakeCase = (name) =>
  name.replace(/([a-z0-9])([A-Z])/gu, "$1_$2").toLowerCase();

export const isLedgerIndexKey = (name) => {
  if (name === undefined) return false;
  const key = snakeCase(name);
  return (
    /(?:^|_)(?:input|output|redeemer)_ind(?:ex|ices)$/u.test(key) &&
    !/_input_output_ind(?:ex|ices)$/u.test(key)
  );
};

export const typeName = (annotation) => {
  const type = annotation?.typeAnnotation ?? annotation;
  if (type?.type !== "TSTypeReference") return undefined;
  const name = type.typeName;
  if (name.type === "Identifier") return name.name;
  if (name.type === "TSQualifiedName") return name.right.name;
  return undefined;
};

export const isRedeemerSchema = (node) => {
  const expression = unwrap(node);
  if (expression === undefined) return false;
  const name =
    expression.type === "Identifier"
      ? expression.name
      : expression.type === "MemberExpression"
        ? staticName(expression.property, expression.computed)
        : undefined;
  return name !== undefined && /Redeemer(?:V\d+)?$/u.test(name);
};

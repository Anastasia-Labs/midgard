import ts from "typescript";

export const UNPUBLISHABLE_DIAGNOSTIC_BEGIN =
  "MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN";
export const UNPUBLISHABLE_DIAGNOSTIC_END =
  "MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END";

export type FaultProofLimitEscape = {
  readonly path: string;
  readonly line: number;
  readonly kind:
    | "oversized_publication"
    | "raised_tx_bytes"
    | "raised_tx_memory"
    | "raised_tx_cpu"
    | "disabled_local_evaluation"
    | "malformed_diagnostic_marker";
  readonly diagnosticOnly: boolean;
};

const propertyName = (node: ts.Node): string | undefined => {
  if (ts.isIdentifier(node) || ts.isStringLiteral(node)) return node.text;
  if (ts.isComputedPropertyName(node)) return propertyName(node.expression);
  return undefined;
};

const unwrap = (node: ts.Expression): ts.Expression => {
  if (
    ts.isParenthesizedExpression(node) ||
    ts.isAsExpression(node) ||
    ts.isSatisfiesExpression(node) ||
    ts.isTypeAssertionExpression(node)
  )
    return unwrap(node.expression);
  return node;
};

const escapeKind = (
  name: string | undefined,
  initializer: ts.Expression | undefined,
): FaultProofLimitEscape["kind"] | undefined => {
  const value = initializer === undefined ? undefined : unwrap(initializer);
  if (name === "oversized") {
    return value?.kind === ts.SyntaxKind.FalseKeyword
      ? undefined
      : "oversized_publication";
  }
  if (name === "localUPLCEval") {
    return value?.kind === ts.SyntaxKind.TrueKeyword
      ? undefined
      : "disabled_local_evaluation";
  }
  const limit =
    name === "maxTxSize"
      ? { maximum: 16_384n, kind: "raised_tx_bytes" as const }
      : name === "maxTxExMem"
        ? { maximum: 16_500_000n, kind: "raised_tx_memory" as const }
        : name === "maxTxExSteps"
          ? { maximum: 10_000_000_000n, kind: "raised_tx_cpu" as const }
          : undefined;
  if (
    limit === undefined ||
    value === undefined ||
    (!ts.isNumericLiteral(value) && !ts.isBigIntLiteral(value))
  )
    return undefined;
  const numeric = ts.isBigIntLiteral(value)
    ? BigInt(value.text.replace(/_/gu, "").replace(/n$/u, ""))
    : Number(value.text);
  return numeric > limit.maximum ? limit.kind : undefined;
};

/**
 * Inspect executable option writes rather than lines, so formatting, duplicate
 * fields, shorthand options and property assignments cannot hide a switch.
 * Numeric checks cover literal overrides; this is not a data-flow analysis.
 * Only comments may mark retained unpublishable diagnostics.
 */
export const scanFaultProofLimitEscapes = ({
  path,
  source,
}: {
  readonly path: string;
  readonly source: string;
}): readonly FaultProofLimitEscape[] => {
  const file = ts.createSourceFile(path, source, ts.ScriptTarget.Latest, true);
  const findings: FaultProofLimitEscape[] = [];
  const comments = new Map<number, ts.CommentRange>();
  const writes: { position: number; kind: FaultProofLimitEscape["kind"] }[] =
    [];
  const inspect = (node: ts.Node): void => {
    for (const comment of [
      ...(ts.getLeadingCommentRanges(source, node.pos) ?? []),
      ...(ts.getTrailingCommentRanges(source, node.end) ?? []),
    ])
      comments.set(comment.pos, comment);
    let name: string | undefined;
    let value: ts.Expression | undefined;
    if (ts.isPropertyAssignment(node)) {
      name = propertyName(node.name);
      value = node.initializer;
    } else if (ts.isShorthandPropertyAssignment(node)) {
      name = node.name.text;
    } else if (
      ts.isBindingElement(node) &&
      ts.isObjectBindingPattern(node.parent)
    ) {
      // A default does not constrain values supplied by the caller.
      name = propertyName(node.propertyName ?? node.name);
    } else if (
      ts.isBinaryExpression(node) &&
      node.operatorToken.kind === ts.SyntaxKind.EqualsToken
    ) {
      if (ts.isPropertyAccessExpression(node.left)) name = node.left.name.text;
      if (ts.isElementAccessExpression(node.left))
        name = propertyName(node.left.argumentExpression);
      value = node.right;
    }
    const kind = escapeKind(name, value);
    if (kind !== undefined)
      writes.push({ position: node.getStart(file), kind });
    ts.forEachChild(node, inspect);
  };
  inspect(file);
  const markers = [...comments.values()].flatMap((comment) => {
    const text = source.slice(comment.pos, comment.end);
    return [
      ...text.matchAll(/MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_(BEGIN|END)/gu),
    ].map((match) => ({
      position: comment.pos + match.index,
      marker: match[1],
    }));
  });
  const events = [...writes, ...markers].sort(
    (a, b) => a.position - b.position,
  );
  let diagnosticDepth = 0;
  for (const event of events) {
    const line = file.getLineAndCharacterOfPosition(event.position).line + 1;
    if ("marker" in event) {
      if (event.marker === "BEGIN") diagnosticDepth += 1;
      else if (diagnosticDepth > 0) diagnosticDepth -= 1;
      else
        findings.push({
          path,
          line,
          kind: "malformed_diagnostic_marker",
          diagnosticOnly: false,
        });
    } else {
      findings.push({
        path,
        line,
        kind: event.kind,
        diagnosticOnly: diagnosticDepth > 0,
      });
    }
  }
  if (diagnosticDepth !== 0)
    findings.push({
      path,
      line: file.getLineAndCharacterOfPosition(source.length).line + 1,
      kind: "malformed_diagnostic_marker",
      diagnosticOnly: false,
    });
  return Object.freeze(findings);
};

export const assertNoPositiveFaultProofLimitEscapes = (
  findings: readonly FaultProofLimitEscape[],
): void => {
  const positive = findings.filter((finding) => !finding.diagnosticOnly);
  if (positive.length > 0) {
    throw new Error(
      `positive fault-proof limit escapes:\n${positive
        .map(
          (finding) =>
            `${finding.path}:${finding.line.toString()} ${finding.kind}`,
        )
        .join("\n")}`,
    );
  }
};

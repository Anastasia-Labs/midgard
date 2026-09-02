export const UNPUBLISHABLE_DIAGNOSTIC_BEGIN_V1 =
  "MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN";
export const UNPUBLISHABLE_DIAGNOSTIC_END_V1 =
  "MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END";

export type FaultProofLimitEscapeV1 = {
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

const numericEscape = (
  text: string,
  field: string,
  maximum: bigint,
): boolean => {
  const match = new RegExp(`\\b${field}\\s*:\\s*([0-9][0-9_]*)n?\\b`, "u").exec(
    text,
  );
  return match?.[1] !== undefined
    ? BigInt(match[1].replace(/_/gu, "")) > maximum
    : false;
};

const escapeKinds = (
  text: string,
): readonly FaultProofLimitEscapeV1["kind"][] => [
  ...(/\boversized\s*:\s*true\b/u.test(text)
    ? (["oversized_publication"] as const)
    : []),
  ...(numericEscape(text, "maxTxSize", 16_384n)
    ? (["raised_tx_bytes"] as const)
    : []),
  ...(numericEscape(text, "maxTxExMem", 16_500_000n)
    ? (["raised_tx_memory"] as const)
    : []),
  ...(numericEscape(text, "maxTxExSteps", 10_000_000_000n)
    ? (["raised_tx_cpu"] as const)
    : []),
  ...(/\blocalUPLCEval\s*:\s*false\b/u.test(text)
    ? (["disabled_local_evaluation"] as const)
    : []),
];

/**
 * Fail-closed source classifier for proof-fit escape hatches. A retained
 * negative diagnostic must be enclosed by explicit begin/end markers; merely
 * naming a test "diagnostic" does not exempt it.
 */
export const scanFaultProofLimitEscapesV1 = ({
  path,
  source,
}: {
  readonly path: string;
  readonly source: string;
}): readonly FaultProofLimitEscapeV1[] => {
  const findings: FaultProofLimitEscapeV1[] = [];
  let diagnosticDepth = 0;
  source.split(/\r?\n/u).forEach((text, index) => {
    const line = index + 1;
    if (text.includes(UNPUBLISHABLE_DIAGNOSTIC_BEGIN_V1)) {
      diagnosticDepth += 1;
    }
    for (const kind of escapeKinds(text)) {
      findings.push({ path, line, kind, diagnosticOnly: diagnosticDepth > 0 });
    }
    if (text.includes(UNPUBLISHABLE_DIAGNOSTIC_END_V1)) {
      if (diagnosticDepth === 0) {
        findings.push({
          path,
          line,
          kind: "malformed_diagnostic_marker",
          diagnosticOnly: false,
        });
      } else {
        diagnosticDepth -= 1;
      }
    }
  });
  if (diagnosticDepth !== 0) {
    findings.push({
      path,
      line: source.split(/\r?\n/u).length,
      kind: "malformed_diagnostic_marker",
      diagnosticOnly: false,
    });
  }
  return Object.freeze(findings);
};

export const assertNoPositiveFaultProofLimitEscapesV1 = (
  findings: readonly FaultProofLimitEscapeV1[],
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

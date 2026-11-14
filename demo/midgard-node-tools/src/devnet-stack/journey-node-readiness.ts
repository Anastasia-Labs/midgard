import type { Json } from "./journey-values.js";

/** The node's /readyz body, as a long journey wait reports it. */
export type NodeReadiness = Readonly<{
  /** "node ready", or "node not ready: " and each reason, a raised liveness
   * reason with its age, escalation and detail. */
  summary: string;
  /** Each escalated liveness reason, described as in `summary`: raised past
   * the bound its source gives, which the node reports as a stall that needs
   * an operator rather than a passing one. */
  escalated: readonly string[];
}>;

const describeReason = (reason: string, entry: Json | undefined) => {
  if (entry === undefined) return reason;
  let text = reason;
  if (typeof entry.ageMs === "number")
    text += ` for ${Math.round(entry.ageMs / 1000).toString()} s`;
  if (entry.escalated === true) text += " (escalated)";
  if (typeof entry.detail === "string") text += `: ${entry.detail}`;
  return text;
};

export const describeNodeReadiness = (body: Json): NodeReadiness => {
  if (body.ready === true) return { summary: "node ready", escalated: [] };
  const liveness = new Map(
    (Array.isArray(body.livenessReasons) ? body.livenessReasons : [])
      .filter(
        (entry): entry is Json => typeof entry === "object" && entry !== null,
      )
      .map((entry) => [String(entry.reason), entry]),
  );
  const escalated: string[] = [];
  const reasons = (Array.isArray(body.reasons) ? body.reasons : []).map(
    (value) => {
      const reason = String(value);
      const entry = liveness.get(reason);
      const text = describeReason(reason, entry);
      if (entry?.escalated === true) escalated.push(text);
      return text;
    },
  );
  return {
    summary:
      reasons.length === 0
        ? "node not ready"
        : `node not ready: ${reasons.join("; ")}`,
    escalated,
  };
};

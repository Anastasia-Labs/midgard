import { createHash } from "node:crypto";

import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";

export const PROTECTED_OUTPUT_SIGNER_STAGES = [
  "none",
  "step01",
  "step02",
  "scanning",
  "terminal",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ProtectedOutputSignerStage =
  (typeof PROTECTED_OUTPUT_SIGNER_STAGES)[number];

export const protectedOutputSignerEvidenceIdentity = (
  evidence: ProtectedOutputSignerMissingEvidence,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        {
          category: "protectedOutputSignerMissing",
          subject: evidence.subject,
          outputIndex: evidence.outputIndex,
          route: evidence.route,
          tx: evidence.canonicalTransactionCborHex,
          output: evidence.outputCborHex ?? null,
          credential: evidence.paymentCredentialHex ?? null,
          witnesses: evidence.addressWitnessFieldPreimageHex,
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    )
    .digest("hex");

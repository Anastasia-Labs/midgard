import { createHash } from "node:crypto";

import type { SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";

export const SPEND_INPUT_SIGNER_STAGES = [
  "none",
  "step01",
  "step02",
  "scanning",
  "terminal",
  "proven",
  "removed",
  "cancelled",
] as const;
export type SpendInputSignerStage = (typeof SPEND_INPUT_SIGNER_STAGES)[number];

export const spendInputSignerWorkflowEvidenceIdentity = (
  evidence: SpendInputSignerMissingEvidence,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        {
          category: "spendInputSignerMissing",
          subject: evidence.subject,
          inputIndex: evidence.inputIndex,
          route: evidence.route,
          transaction: evidence.canonicalTransactionCborHex,
          priorRoot: evidence.priorRoot,
          outRef:
            evidence.resolved === undefined
              ? null
              : [
                  evidence.resolved.transactionId,
                  evidence.resolved.outputIndex,
                ],
          descriptor: evidence.resolved?.descriptorCborHex ?? null,
          credential: evidence.paymentCredentialHex ?? null,
          witnesses: evidence.addressWitnessFieldPreimageHex,
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    )
    .digest("hex");

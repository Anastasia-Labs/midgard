import { type MidgardValidationPhaseName } from "@al-ft/midgard-core";

import { type RejectSubject } from "../reject-subject.js";
import { RejectedTx } from "../types.js";

export const reject = (
  txId: Buffer,
  code: RejectedTx["code"],
  detail: string | null = null,
  consensusPhase: MidgardValidationPhaseName = "resolveInputs",
  subject?: RejectSubject,
): RejectedTx => ({
  txId,
  code,
  detail,
  consensusPhase,
  ...(subject === undefined ? {} : { subject }),
});

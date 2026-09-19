import {
  encodeCbor,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";

import { encodeValidationFrontierPeaks } from "./control-encoding.js";

export type ValidationTerminalWitness =
  | {
      readonly verdict: "accepted";
      readonly postLedgerRoot: Buffer;
      readonly ledgerDeltaFrontier: MidgardValidationMerkleFrontier;
    }
  | {
      readonly verdict: "rejected";
      readonly rejectionCode: string;
      readonly priorLedgerRoot: Buffer;
    };

/**
 * Canonical V1 terminal work witness. Acceptance carries the applied delta's
 * frontier; rejection preserves the prior ledger root and carries an empty
 * operation list. Replay validates the verdict and ledger delta before encoding.
 */
export const encodeValidationTerminalWitnessCbor = (
  witness: ValidationTerminalWitness,
): Buffer =>
  witness.verdict === "accepted"
    ? encodeCbor([
        1n,
        Buffer.alloc(0),
        witness.postLedgerRoot,
        encodeCbor([
          BigInt(witness.ledgerDeltaFrontier.count),
          encodeValidationFrontierPeaks(witness.ledgerDeltaFrontier),
        ]),
      ])
    : encodeCbor([
        2n,
        Buffer.from(witness.rejectionCode, "ascii"),
        witness.priorLedgerRoot,
        Buffer.from("80", "hex"),
      ]);

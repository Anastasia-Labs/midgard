import {
  encodeCbor,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import {
  encodeMidgardRejectionReasonV1Cbor,
  type MidgardRejectionReasonV1,
} from "@al-ft/midgard-core/validation-trace";

import { encodeValidationFrontierPeaks } from "./control-encoding.js";

export type ValidationTerminalWitness =
  | {
      readonly verdict: "accepted";
      readonly postLedgerRoot: Buffer;
      readonly ledgerDeltaFrontier: MidgardValidationMerkleFrontier;
    }
  | {
      readonly verdict: "rejected";
      readonly reason: MidgardRejectionReasonV1;
      readonly priorLedgerRoot: Buffer;
    };

/**
 * Canonical V1 terminal work witness. Acceptance carries the applied delta's
 * frontier; rejection embeds the reason's Plutus Data CBOR in place, preserves
 * the prior ledger root and carries an empty operation list. Replay validates
 * the verdict and ledger delta before encoding.
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
    : encodeCborArrayRaw([
        encodeCborInteger(2n),
        encodeMidgardRejectionReasonV1Cbor(witness.reason),
        encodeCborBytes(witness.priorLedgerRoot),
        encodeCborBytes(Buffer.from("80", "hex")),
      ]);

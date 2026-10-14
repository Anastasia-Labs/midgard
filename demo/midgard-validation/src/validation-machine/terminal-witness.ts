import { encodeCbor } from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import {
  encodeMidgardRejectionReasonV1Cbor,
  type MidgardRejectionReasonV1,
} from "@al-ft/midgard-core/validation-trace";

export type ValidationTerminalWitness =
  | { readonly verdict: "accepted" }
  | {
      readonly verdict: "rejected";
      readonly reason: MidgardRejectionReasonV1;
      readonly priorLedgerRoot: Buffer;
    };

/**
 * Canonical V1 terminal work witness. Acceptance is the fixed `[1, h'']`;
 * rejection embeds the reason's Plutus Data CBOR in place, preserves the prior
 * ledger root and carries an empty operation list.
 */
export const encodeValidationTerminalWitnessCbor = (
  witness: ValidationTerminalWitness,
): Buffer =>
  witness.verdict === "accepted"
    ? encodeCbor([1n, Buffer.alloc(0)])
    : encodeCborArrayRaw([
        encodeCborInteger(2n),
        encodeMidgardRejectionReasonV1Cbor(witness.reason),
        encodeCborBytes(witness.priorLedgerRoot),
        encodeCborBytes(Buffer.from("80", "hex")),
      ]);

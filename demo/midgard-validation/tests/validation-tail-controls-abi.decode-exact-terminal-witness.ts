import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import { decodeMidgardRejectionReasonV1Cbor } from "@al-ft/midgard-core/validation-trace";
import { fixtureBytes } from "@al-ft/midgard-test-support/hex";

import { encodeValidationTerminalWitnessCbor } from "../src/index.js";
import { bytes } from "./validation-tail-controls-abi.tail-auxiliary-vectors.js";

export const terminalRejectionCbor = encodeValidationTerminalWitnessCbor({
  verdict: "rejected",
  reason: "ValueNotPreserved",
  priorLedgerRoot: fixtureBytes(0x73, 32),
});

/**
 * Independent decoder for the V1 terminal witness. Acceptance is exactly
 * `[1, h'']`; rejection is `[2, reason, prior_ledger_root, h'80']`, where
 * element 1 is the Plutus Data `RejectionReasonV1` and so is read
 * structurally rather than as a byte string.
 */
export const decodeExactTerminalWitness = (
  input: Uint8Array,
):
  | { readonly outcome: "accepted" }
  | { readonly outcome: "rejected"; readonly ledgerRoot: Buffer } => {
  const encoded = Buffer.from(input);
  const header = readCborArrayHeader(encoded, 0, "terminal witness");
  const outcome = readCborInteger(encoded, header.nextOffset, "outcome");
  if (outcome.value === 1n) {
    if (header.length !== 2) {
      throw new Error("accepted terminal witness must contain two fields");
    }
    const empty = readCborBytes(encoded, outcome.nextOffset, "acceptance slot");
    if (empty.value.length !== 0) {
      throw new Error("accepted terminal witness cannot carry a payload");
    }
    if (empty.nextOffset !== encoded.length) {
      throw new Error("terminal witness has trailing bytes");
    }
    if (!bytes("820140").equals(encoded)) {
      throw new Error("terminal witness CBOR is not canonical");
    }
    return { outcome: "accepted" };
  }
  if (outcome.value !== 2n) {
    throw new Error("terminal witness outcome is not canonical V1");
  }
  if (header.length !== 4) {
    throw new Error("rejected terminal witness must contain four fields");
  }
  const elementOneOffset = outcome.nextOffset;
  const reason = decodeMidgardRejectionReasonV1Cbor(encoded, elementOneOffset);
  const reasonItem = encoded.subarray(elementOneOffset, reason.nextOffset);
  const ledgerRoot = readCborBytes(encoded, reason.nextOffset, "ledger root");
  if (ledgerRoot.value.length !== 32) {
    throw new Error("terminal witness ledger root must contain 32 bytes");
  }
  const evidence = readCborBytes(encoded, ledgerRoot.nextOffset, "evidence");
  if (evidence.nextOffset !== encoded.length) {
    throw new Error("terminal witness has trailing bytes");
  }
  const exactEvidence = Buffer.from(evidence.value);
  if (!exactEvidence.equals(bytes("80"))) {
    throw new Error("rejected terminal witness is misclassified");
  }
  const exactRoot = Buffer.from(ledgerRoot.value);
  const canonical = encodeCborArrayRaw([
    encodeCborInteger(outcome.value),
    reasonItem,
    encodeCborBytes(exactRoot),
    encodeCborBytes(exactEvidence),
  ]);
  if (!canonical.equals(encoded)) {
    throw new Error("terminal witness CBOR is not canonical");
  }
  return { outcome: "rejected", ledgerRoot: exactRoot };
};

export const EXPECTED = {
  auxiliaryCorpusHash:
    "4a3d01e5065a094a90d4a177fb691302dbc7858ce95aad746224b321a8b4efbc",
  valueAccumulatorCbor:
    "8407582054545454545454545454545454545454545454545454545454545454545454540201",
  valueAndMintControlHash:
    "d30dfeaa4f1f3323bf2824a1051ef943fee27a31779e171678abe4c05ba2b2e0",
  // Independent Aiken vectors: validation-tail-controls-v1-abi.test.ak,
  // terminal_acceptance_and_rejection_v1_typescript_vectors_are_exact.
  terminalAcceptanceCbor: "820140",
  terminalAcceptanceHash:
    "a4dff8274189cdb6a72ea715f28b42d5170fe475698db83ad9d84e82545ce981",
  terminalRejectionCbor:
    "8402d9052780582073737373737373737373737373737373737373737373737373737373737373734180",
  terminalRejectionHash:
    "f7392de043bf5fa859d1162c1c7cd3e8ef24dc252a48970e743b1b8c3b9b10c1",
} as const;

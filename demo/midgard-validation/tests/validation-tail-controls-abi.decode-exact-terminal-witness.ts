import {
  decodeSingleCbor,
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
 * Independent decoder for the V1 terminal witness
 * `[outcome, reason_or_empty, ledger_root, delta_evidence]`: element 1 is
 * the empty byte string on acceptance and the Plutus Data `RejectionReasonV1`
 * on rejection, so it is read structurally rather than as a byte string.
 */
export const decodeExactTerminalWitness = (
  input: Uint8Array,
): {
  readonly outcome: "accepted" | "rejected";
  readonly ledgerRoot: Buffer;
} => {
  const encoded = Buffer.from(input);
  const header = readCborArrayHeader(encoded, 0, "terminal witness");
  if (header.length !== 4) {
    throw new Error("terminal witness must contain exactly four fields");
  }
  const outcome = readCborInteger(encoded, header.nextOffset, "outcome");
  const elementOneOffset = outcome.nextOffset;
  let rootOffset: number;
  let reasonItem: Buffer;
  if (outcome.value === 1n) {
    const empty = readCborBytes(encoded, elementOneOffset, "rejection slot");
    if (empty.value.length !== 0) {
      throw new Error("accepted terminal witness cannot carry a rejection");
    }
    rootOffset = empty.nextOffset;
    reasonItem = encodeCborBytes(Buffer.alloc(0));
  } else if (outcome.value === 2n) {
    const reason = decodeMidgardRejectionReasonV1Cbor(
      encoded,
      elementOneOffset,
    );
    rootOffset = reason.nextOffset;
    reasonItem = encoded.subarray(elementOneOffset, reason.nextOffset);
  } else {
    throw new Error("terminal witness outcome is not canonical V1");
  }
  const ledgerRoot = readCborBytes(encoded, rootOffset, "ledger root");
  if (ledgerRoot.value.length !== 32) {
    throw new Error("terminal witness ledger root must contain 32 bytes");
  }
  const evidence = readCborBytes(encoded, ledgerRoot.nextOffset, "evidence");
  if (evidence.nextOffset !== encoded.length) {
    throw new Error("terminal witness has trailing bytes");
  }
  const exactEvidence = Buffer.from(evidence.value);
  if (outcome.value === 1n) {
    const frontier = decodeSingleCbor(exactEvidence);
    if (
      !Array.isArray(frontier) ||
      frontier.length !== 2 ||
      (typeof frontier[0] !== "bigint" && typeof frontier[0] !== "number") ||
      !Array.isArray(frontier[1])
    ) {
      throw new Error("accepted terminal witness frontier is malformed");
    }
  } else if (!exactEvidence.equals(bytes("80"))) {
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
  return {
    outcome: outcome.value === 1n ? "accepted" : "rejected",
    ledgerRoot: exactRoot,
  };
};

export const EXPECTED = {
  auxiliaryCorpusHash:
    "544436205849eb463fb2c0c4d4fcc461ff9d38fd44ad1c55d4107ff8f616f761",
  valueAccumulatorCbor:
    "8407582054545454545454545454545454545454545454545454545454545454545454540201",
  valueAndMintControlHash:
    "d30dfeaa4f1f3323bf2824a1051ef943fee27a31779e171678abe4c05ba2b2e0",
  pendingMutationCbor: "8a01000142010242030445840100008020404000",
  ledgerDeltaControlHash:
    "92e07c0c935ac73750a521ed638aed060414828d766774495885b56a04f5481b",
  // Independent Aiken vectors: validation-tail-controls-v1-abi.test.ak,
  // terminal_acceptance_and_rejection_v1_typescript_vectors_are_exact.
  terminalAcceptanceCbor:
    "840140582072727272727272727272727272727272727272727272727272727272727272725827820281820158207171717171717171717171717171717171717171717171717171717171717171",
  terminalAcceptanceHash:
    "0b3defd802c8cc6ee1112724ef19532be5b8f61817ab0282a56db645a2b20948",
  terminalRejectionCbor:
    "8402d9052780582073737373737373737373737373737373737373737373737373737373737373734180",
  terminalRejectionHash:
    "02d93f94e5c0247ed5c22e07812312ea6537ee4dd080a639361b1818d3e746d8",
} as const;

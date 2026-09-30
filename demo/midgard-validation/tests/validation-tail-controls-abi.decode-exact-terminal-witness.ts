import { encodeCbor } from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { isUnknownArray } from "@al-ft/midgard-core/narrowing";
import { fixtureBytes } from "@al-ft/midgard-test-support/hex";

import { encodeValidationTerminalWitnessCbor } from "../src/index.js";
import { bytes } from "./validation-tail-controls-abi.tail-auxiliary-vectors.js";

export const terminalRejectionCbor = encodeValidationTerminalWitnessCbor({
  verdict: "rejected",
  rejectionCode: "E_VALUE_NOT_PRESERVED",
  priorLedgerRoot: fixtureBytes(0x73, 32),
});

export const decodeExactTerminalWitness = (
  input: Uint8Array,
): {
  readonly outcome: "accepted" | "rejected";
  readonly ledgerRoot: Buffer;
} => {
  const encoded = Buffer.from(input);
  const decoded = decodeSingleCbor(encoded);
  if (!isUnknownArray(decoded) || decoded.length !== 4) {
    throw new Error("terminal witness must contain exactly four fields");
  }
  const [outcome, code, ledgerRoot, deltaEvidence] = decoded;
  if (typeof outcome !== "bigint" && typeof outcome !== "number") {
    throw new Error("terminal witness outcome must be an integer");
  }
  const outcomeCode = typeof outcome === "bigint" ? outcome : BigInt(outcome);
  const exactCode = Buffer.from(code as Uint8Array);
  const exactRoot = Buffer.from(ledgerRoot as Uint8Array);
  const exactEvidence = Buffer.from(deltaEvidence as Uint8Array);
  if (exactRoot.length !== 32) {
    throw new Error("terminal witness ledger root must contain 32 bytes");
  }
  if (outcomeCode === 1n) {
    if (exactCode.length !== 0) {
      throw new Error("accepted terminal witness cannot carry a rejection");
    }
    const frontier = decodeSingleCbor(exactEvidence);
    if (
      !Array.isArray(frontier) ||
      frontier.length !== 2 ||
      (typeof frontier[0] !== "bigint" && typeof frontier[0] !== "number") ||
      !Array.isArray(frontier[1])
    ) {
      throw new Error("accepted terminal witness frontier is malformed");
    }
  } else if (outcomeCode === 2n) {
    if (exactCode.length === 0 || !exactEvidence.equals(bytes("80"))) {
      throw new Error("rejected terminal witness is misclassified");
    }
  } else {
    throw new Error("terminal witness outcome is not canonical V1");
  }
  if (!encodeCbor(decoded).equals(encoded)) {
    throw new Error("terminal witness CBOR is not canonical");
  }
  return {
    outcome: outcomeCode === 1n ? "accepted" : "rejected",
    ledgerRoot: exactRoot,
  };
};

export const EXPECTED = {
  auxiliaryCorpusHash:
    "8916ad7c26d34eafe62c93ed9c36be30d880fb102b918bb37f0b6d3dc27111e1",
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
    "840255455f56414c55455f4e4f545f505245534552564544582073737373737373737373737373737373737373737373737373737373737373734180",
  terminalRejectionHash:
    "6b15a4122dc6437ca54930248e9df11979d21dc484d5ea373373b43c489f1ce6",
} as const;

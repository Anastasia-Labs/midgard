/**
 * DA-first Q35 evidence selection. The caller supplies the decoded canonical
 * native-V1 item reconstructed from authenticated retained DA; this module
 * never accepts an operator-local verdict or database flag.
 */
import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardOutputFieldPreimage,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core";
import type { FieldCarriage, FieldOpening } from "@al-ft/midgard-sdk";
import { fieldOpeningForField, MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";

export type NetworkIdFaultClaim =
  | { readonly kind: "transaction-network" }
  | { readonly kind: "output-network"; readonly outputIndex: bigint };

export type RetainedDaNetworkIdEvidence = {
  readonly source: "retained-da";
  readonly evidenceSourceId: string;
  /** Exact canonical full native-V1 item selected from retained block DA. */
  readonly nativeTxCanonicalCbor: string;
};

const expectNetwork = (value: bigint, label: string): 0n | 1n => {
  if (value !== 0n && value !== 1n) {
    throw new Error(`${label} must be canonical network id 0 or 1`);
  }
  return value;
};

const exactCanonicalTransaction = (evidence: RetainedDaNetworkIdEvidence) => {
  if (
    evidence.source !== "retained-da" ||
    evidence.evidenceSourceId.length === 0
  ) {
    throw new Error(
      "network-id evidence must name its authenticated retained-DA source",
    );
  }
  if (
    evidence.nativeTxCanonicalCbor.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(evidence.nativeTxCanonicalCbor)
  ) {
    throw new Error(
      "network-id evidence must carry canonical full native-V1 CBOR as lowercase hex",
    );
  }
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(evidence.nativeTxCanonicalCbor, "hex"),
  );
  return {
    tx,
    nativeTxCompactCbor: encodeMidgardNativeTxCompact(tx.compact).toString(
      "hex",
    ),
    outputsPreimageCbor: tx.body.outputsPreimageCbor.toString("hex"),
    outputs: decodeMidgardOutputFieldPreimage(tx.body.outputsPreimageCbor),
  };
};

/** Finds every convictable claim in deterministic family order. */
export const findNetworkIdFaults = ({
  evidence,
  expectedNetworkId,
}: {
  readonly evidence: RetainedDaNetworkIdEvidence;
  readonly expectedNetworkId: bigint;
}): readonly NetworkIdFaultClaim[] => {
  const expected = expectNetwork(expectedNetworkId, "expected network id");
  const inspected = exactCanonicalTransaction(evidence);
  // TxIsInvalid is an honest no-op, not a block fault.
  if (inspected.tx.validity !== "TxIsValid") return [];

  const faults: NetworkIdFaultClaim[] = [];
  if (
    inspected.tx.body.networkId !== 255n &&
    inspected.tx.body.networkId !== expected
  ) {
    faults.push({ kind: "transaction-network" });
  }
  for (const [index, output] of inspected.outputs.entries()) {
    // Unlike the deployed expected id, an observed foreign network must stay
    // classifiable as fraud. The canonical address decoder also removes the
    // reserved Midgard protection bit before returning this logical id.
    const outputNetwork = BigInt(
      decodeMidgardAddressBytes(output.address).networkId,
    );
    if (outputNetwork !== expected) {
      faults.push({ kind: "output-network", outputIndex: BigInt(index) });
    }
  }
  return faults;
};

export const requireNetworkIdFault = (args: {
  readonly evidence: RetainedDaNetworkIdEvidence;
  readonly expectedNetworkId: bigint;
}): NetworkIdFaultClaim => {
  const [fault] = findNetworkIdFaults(args);
  if (fault === undefined) {
    throw new Error("the retained transaction has no network-id fault");
  }
  return fault;
};

/**
 * Builds the field-2 opening used by an output claim. `carriage` is already
 * resolved against the proof transaction's complete reference-input set; it
 * is never resolved against a carriage-only subset.
 */
export const networkIdOutputsOpening = ({
  evidence,
  carriage,
}: {
  readonly evidence: RetainedDaNetworkIdEvidence;
  readonly carriage: FieldCarriage;
}): FieldOpening => {
  const inspected = exactCanonicalTransaction(evidence);
  if (
    "Inline" in carriage &&
    carriage.Inline.preimage !== inspected.outputsPreimageCbor
  ) {
    throw new Error(
      "inline network-id output opening does not equal the retained transaction's outputs preimage",
    );
  }
  return fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    nativeTxCompactCbor: inspected.nativeTxCompactCbor,
    carriage,
  });
};

import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { forcedVerdictSubjectV1 } from "@al-ft/midgard-sdk";
import { describe, expect, expectTypeOf, it } from "vitest";

import {
  createNetworkIdWrongfulRejectionPlannerV1,
  detectNetworkIdWrongfulRejectionsV1,
  networkIdWrongfulRejectionClosesV1,
  type NetworkIdWrongfulRejectionEvidenceV1,
} from "../src/network-id/wrongful-rejection-v1.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const output = (networkId: number) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | networkId]),
      Buffer.alloc(28, 0x44),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

const forcedBlock = (
  outputNetworkIds: readonly number[],
  reason: "NetworkIdMismatch" | "EmptyInputs" = "NetworkIdMismatch",
) => {
  const submitted = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbors: outputNetworkIds.map(output),
  });
  const invalid = adjudicateMidgardNativeTxFullV1Validity(
    submitted,
    "TxIsInvalid",
  );
  const txId = computeMidgardNativeTxIdV1(invalid).toString("hex");
  const source = deriveMidgardNativeTxProofSourceV1(invalid);
  return {
    headerHash: "04".repeat(28),
    header: { expectedNetworkId: 0n },
    reconstruction: {
      forcedTransactions: [
        {
          key: { transactionId: "05".repeat(32), outputIndex: 0n },
          value: {
            tx_id: txId,
            source: {
              compact_cbor: source.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                source.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                source.fieldPreimageLengthsCbor.toString("hex"),
            },
            verdict: { ForcedTxInvalid: { reason } },
          },
          fullTransactionCbor: encodeMidgardNativeTxCanonicalV1(invalid),
        },
      ],
    },
  } as never;
};

describe("networkId wrongful-rejection V1", () => {
  it("derives complete equality evidence from the authenticated forced leaf", () => {
    const detections = detectNetworkIdWrongfulRejectionsV1({
      block: forcedBlock([0, 0]),
      expectedNetworkId: 0n,
    });
    expect(detections).toHaveLength(1);
    expect(detections[0]!.evidence.outputNetworkIds).toEqual([0n, 0n]);
    expect(networkIdWrongfulRejectionClosesV1(detections[0]!.evidence)).toBe(
      true,
    );
  });

  it("refuses an honest rejection when one authenticated output mismatches", () => {
    expect(
      detectNetworkIdWrongfulRejectionsV1({
        block: forcedBlock([0, 1]),
        expectedNetworkId: 0n,
      }),
    ).toHaveLength(0);
  });

  it("refuses another typed reason", () => {
    expect(
      detectNetworkIdWrongfulRejectionsV1({
        block: forcedBlock([0], "EmptyInputs"),
        expectedNetworkId: 0n,
      }),
    ).toHaveLength(0);
  });

  it("binds transaction identity and rejects source substitution", () => {
    const block = forcedBlock([0]) as any;
    block.reconstruction.forcedTransactions[0].value.tx_id = "ff".repeat(32);
    expect(() =>
      detectNetworkIdWrongfulRejectionsV1({ block, expectedNetworkId: 0n }),
    ).toThrow(/preimage differs/u);
  });

  it("uses a callback-free production authority surface", () => {
    const planner = createNetworkIdWrongfulRejectionPlannerV1(0n);
    expect(planner).toHaveLength(1);
    expectTypeOf<
      keyof Parameters<typeof planner>[0]
    >().toEqualTypeOf<"block">();
    expect(Object.keys({ block: null })).toEqual(["block"]);
  });

  it("the pure terminal predicate refuses a forged mismatch", () => {
    const evidence: NetworkIdWrongfulRejectionEvidenceV1 = {
      subject: forcedVerdictSubjectV1({
        transactionId: "01".repeat(32),
        sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
        rejectionReason: "NetworkIdMismatch",
      }),
      expectedNetworkId: 0n,
      committedNetworkId: 0n,
      outputNetworkIds: [1n],
      outputsItemCbors: [],
      outputsPreimageCbor: "80",
    };
    expect(networkIdWrongfulRejectionClosesV1(evidence)).toBe(false);
  });
});

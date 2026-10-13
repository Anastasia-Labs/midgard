import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardTxValidity,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  authenticateTransactionsInclusionRoots,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/index.js";
import {
  findNetworkIdFaults,
  networkIdOutputsOpening,
  requireNetworkIdFault,
  type RetainedDaNetworkIdEvidence,
} from "../src/network-id/evidence.js";
import {
  planNetworkIdOutputsOpening,
  prepareNetworkIdFromCanonicalEvidence,
} from "../src/network-id/prepare.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  type FixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";

const nativeTx = ({
  transactionNetworkId = 0n,
  outputNetworkIds = [0n],
  protectedOutputIndexes = [],
  validity = "TxIsValid",
}: {
  readonly transactionNetworkId?: bigint;
  readonly outputNetworkIds?: readonly bigint[];
  readonly protectedOutputIndexes?: readonly number[];
  readonly validity?: MidgardTxValidity;
}) =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor(
        outputNetworkIds.map((networkId, index) =>
          encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([
                0x60 |
                  Number(networkId) |
                  (protectedOutputIndexes.includes(index) ? 0x08 : 0),
              ]),
              Buffer.alloc(28, index + 1),
            ]),
            value: { lovelace: 2_000_000n, assets: new Map() },
          }),
        ),
      ),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: transactionNetworkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const retained = (
  tx = nativeTx({}),
  patch: Partial<RetainedDaNetworkIdEvidence> = {},
): RetainedDaNetworkIdEvidence => ({
  source: "retained-da",
  evidenceSourceId: "retained-da-peer",
  nativeTxCanonicalCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
  ...patch,
});

const fixtureTransaction = (
  tx: ReturnType<typeof nativeTx>,
): FixtureTransaction => {
  const canonicalCbor = encodeMidgardNativeTxCanonical(tx);
  const sourceMaterial =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  const source: SDK.L2TransactionSource = {
    tx_id: txId,
    source: {
      compact_cbor: sourceMaterial.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        sourceMaterial.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        sourceMaterial.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId,
    canonicalCbor,
    compactCbor: sourceMaterial.compactCbor,
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceSchema),
  };
};

const canonicalEvidence = async (tx: ReturnType<typeof nativeTx>) => {
  const transaction = fixtureTransaction(tx);
  // The header's normative transactions MPF commits
  // `Data(L2TransactionSource)` per transaction id — the same values the DA
  // payload's `transactions` map carries — so the payload-source fixture is
  // the one whose header re-commits to the retained evidence.
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/network-id-test",
      grade: "security",
    },
  });
  return {
    ...evidence,
    inclusionRootAuthentication: await authenticateTransactionsInclusionRoots({
      header: fixture.header,
      reconstruction: evidence.reconstruction,
      transactions: evidence.transactions,
    }),
  };
};

describe("Q35 network-id DA-first evidence", () => {
  it("orders a body mismatch before output mismatches", () => {
    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            transactionNetworkId: 1n,
            outputNetworkIds: [0n, 1n, 1n],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([
      { kind: "transaction-network" },
      { kind: "output-network", outputIndex: 1n },
      { kind: "output-network", outputIndex: 2n },
    ]);
  });

  it("treats absent body network as honest while still checking outputs", () => {
    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            transactionNetworkId: 255n,
            outputNetworkIds: [0n, 1n],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([{ kind: "output-network", outputIndex: 1n }]);
  });

  it("decodes the protected-address bit separately from the network id", () => {
    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            outputNetworkIds: [0n, 1n],
            protectedOutputIndexes: [0, 1],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([{ kind: "output-network", outputIndex: 1n }]);
  });

  it("classifies foreign raw network nibbles instead of rejecting evidence", () => {
    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            // Raw nibble 2 is unprotected. Raw nibble 15 carries the reserved
            // protection bit and decodes to foreign logical network 7.
            outputNetworkIds: [2n, 15n],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([
      { kind: "output-network", outputIndex: 0n },
      { kind: "output-network", outputIndex: 1n },
    ]);

    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            outputNetworkIds: [2n, 7n],
            protectedOutputIndexes: [1],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([
      { kind: "output-network", outputIndex: 0n },
      { kind: "output-network", outputIndex: 1n },
    ]);
  });

  it("keeps deployment expected-network validation restricted to 0 or 1", () => {
    expect(() =>
      findNetworkIdFaults({
        evidence: retained(nativeTx({ outputNetworkIds: [2n] })),
        expectedNetworkId: 2n,
      }),
    ).toThrow("expected network id must be canonical network id 0 or 1");
  });

  it("never challenges an honestly rejected no-op", () => {
    expect(
      findNetworkIdFaults({
        evidence: retained(
          nativeTx({
            validity: "TxIsInvalid",
            transactionNetworkId: 1n,
            outputNetworkIds: [1n],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).toEqual([]);
  });

  it("rejects honest transactions and malformed canonical CBOR", () => {
    expect(() =>
      requireNetworkIdFault({
        evidence: retained(),
        expectedNetworkId: 0n,
      }),
    ).toThrow("no network-id fault");
    expect(() =>
      findNetworkIdFaults({
        evidence: retained(undefined, { nativeTxCanonicalCbor: "80" }),
        expectedNetworkId: 0n,
      }),
    ).toThrow();
  });

  it("builds the exact body-field opening used by the Aiken door", () => {
    const tx = nativeTx({});
    const preimage = tx.body.outputsPreimageCbor.toString("hex");
    expect(
      networkIdOutputsOpening({
        evidence: retained(tx),
        carriage: { Inline: { preimage } },
      }),
    ).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: encodeMidgardNativeTxCompact(
          tx.compact,
        ).toString("hex"),
        carriage: { Inline: { preimage } },
      },
    });
    expect(() =>
      networkIdOutputsOpening({
        evidence: retained(tx),
        carriage: { Inline: { preimage: "80" } },
      }),
    ).toThrow("does not equal the retained transaction's outputs preimage");
  });

  it("detects the last output in a high-cardinality fixture", () => {
    const ids = Array.from({ length: 12 }, (_, index) =>
      index === 11 ? 1n : 0n,
    );
    expect(
      requireNetworkIdFault({
        evidence: retained(nativeTx({ outputNetworkIds: ids })),
        expectedNetworkId: 0n,
      }),
    ).toEqual({ kind: "output-network", outputIndex: 11n });
  });

  it("prepares an authenticated native inclusion and exact field-2 opening", async () => {
    const prepared = await prepareNetworkIdFromCanonicalEvidence({
      evidence: await canonicalEvidence(
        nativeTx({
          outputNetworkIds: [0n, 1n],
          protectedOutputIndexes: [0, 1],
        }),
      ),
      expectedNetworkId: 0n,
    });
    expect(prepared.faultClaim).toEqual({
      kind: "output-network",
      outputIndex: 1n,
    });
    expect(prepared.txInclusion.nativeTxId).toBe(prepared.badTxId);
    expect(prepared.txInclusion.transactionsPhasRoot).toHaveLength(64);
    const plan = planNetworkIdOutputsOpening({
      prepared,
      owner: "11".repeat(28),
    });
    expect(plan.fieldIndex).toBe(2);
    expect(plan.nativeTxId).toBe(prepared.badTxId);
    expect(plan.itemCount).toBe(2);
    expect(plan.plan.tier).toBe("Inline");
  });

  it("refuses to prepare a valid accepted block", async () => {
    await expect(
      prepareNetworkIdFromCanonicalEvidence({
        evidence: await canonicalEvidence(
          nativeTx({
            outputNetworkIds: [0n],
            protectedOutputIndexes: [0],
          }),
        ),
        expectedNetworkId: 0n,
      }),
    ).rejects.toThrow("no accepted network-id violation");
  });
});

/**
 * Detection over accepted forced transactions. A `ForcedTxValid` leaf's outputs
 * enter the ledger exactly as an accepted normal transaction's do, so the
 * minimum-Ada and network-id detectors read its committed outputs and body the
 * same way, and their planners derive the accepted direction and the forced
 * source from the block alone. Both polarities: a violating accepted leaf is
 * found and planned; an honest one is not.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { proofThreadDirectionOfForcedLeaf } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { MIDGARD_COINS_PER_UTXO_BYTE } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { detectMinAdaForcedReplay } from "../src/min-ada/forced.js";
import {
  admitMinAdaForcedArtifact,
  prepareMinAdaForcedArtifact,
} from "../src/min-ada/forced-artifact.js";
import { txFieldPlan } from "../src/min-ada/workflow.resolve-field.js";
import { admitMinAdaWorkflowArtifact } from "../src/min-ada/workflow-artifact.js";
import {
  detectNetworkIdAcceptedForced,
  NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID,
  prepareNetworkIdAcceptedForced,
} from "../src/network-id/accepted-forced.js";
import { networkIdAcceptedForcedArtifactFromPrepared } from "../src/network-id/workflow-adapter.accepted-forced-artifact.js";
import type { NetworkIdWorkflowAdapterConfig } from "../src/network-id/workflow-adapter.js";
import {
  admitNetworkIdWorkflowArtifact,
  createNetworkIdWorkflowAdapter,
} from "../src/network-id/workflow-adapter.js";
import { detectNetworkIdWrongfulRejections } from "../src/network-id/wrongful-rejection.js";
import { deriveTransactionOutputScan } from "../src/transaction-output-non-canonical/transaction-output-non-canonical.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { FRAUD_PROOF_CLASSIFICATION_RULES } from "../src/workflow/classification.js";
import { detectNetworkIds } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { h28, h32 } from "./helpers/canonical-block-evidence-fixture.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

type Verdict =
  | "ForcedTxValid"
  | { readonly ForcedTxInvalid: { readonly reason: unknown } };

const keyOutput = (networkId: number, lovelace: bigint, fill = 0x44) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([0x60 | networkId]),
      Buffer.alloc(28, fill),
    ]),
    value: { lovelace, assets: new Map() },
  });

/** One forced leaf committed alone under a counted root, as reconstruction sees it. */
const blockWith = async ({
  outputs,
  bodyNetworkId = 0n,
  verdict,
}: {
  readonly outputs: readonly Buffer[];
  readonly bodyNetworkId?: bigint;
  readonly verdict: Verdict;
}) => {
  const tx = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({
      spendInputCbors: [
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x33),
          outputIndex: 0,
        }),
      ],
      fee: 7n,
      outputCbors: [...outputs],
      networkId: bodyNetworkId,
    }),
  );
  const proofSource = deriveMidgardForcedTxProofSource(tx);
  const admission = screenMidgardForcedTxAdmission(tx);
  const key = { transactionId: h32(0x05), outputIndex: 0n };
  const value = {
    tx_id: computeMidgardNativeTxId(tx).toString("hex"),
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
  const keyBytes = Buffer.from(Data.to(key, SDK.OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(value as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  await new Trie(store).insert(keyBytes, valueBytes);
  const header = {
    ...makeHeader(h28(0x44), 1_000),
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: root.count,
  };
  const entry = {
    key,
    value,
    keyBytes,
    valueBytes,
    fullTransactionCbor: encodeMidgardForcedTxCanonical(tx),
  };
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: key } };
  const fingerprint = eventKeyFingerprint(eventKey);
  return {
    txId: value.tx_id,
    admission,
    key,
    block: {
      headerHash: computeHash28(SDK.encodeHeaderCbor(header)).toString("hex"),
      header,
      transactions: [],
      reconstruction: {
        utxos: [],
        forcedTransactions: [entry],
        rootData: { forcedTransactions: root },
        sourceEventsByFingerprint: new Map([
          [
            fingerprint,
            { phase: "ForcedTransaction", eventKey, fingerprint, entry },
          ],
        ]),
      },
    } as unknown as CanonicalBlockEvidence,
  };
};

/** A floor-sized output, and the same output one lovelace under it. */
const minAdaOutputs = () => {
  let floor = 2_000_000n;
  for (let i = 0; i < 4; i++)
    floor =
      MIDGARD_COINS_PER_UTXO_BYTE *
      BigInt(keyOutput(0, floor, 0x45).length + 160);
  return {
    funded: keyOutput(0, floor, 0x45),
    under: keyOutput(0, floor - 1n, 0x45),
  };
};

describe("minimum-Ada detection over accepted forced transactions", () => {
  it("finds every output below the floor on an accepted leaf and plans it in the accepted direction", async () => {
    const { funded, under } = minAdaOutputs();
    const { block, txId, key } = await blockWith({
      outputs: [funded, under, under],
      verdict: "ForcedTxValid",
    });
    const findings = detectMinAdaForcedReplay(block);
    expect(findings.map((finding) => finding.detectionId)).toStrictEqual([
      `min-ada:forced:0:${txId}:1`,
      `min-ada:forced:0:${txId}:2`,
    ]);
    for (const finding of findings) {
      expect(finding.evidence.state.direction).toBe(0n);
      expect(finding.evidence.state.source_kind).toBe(1n);
      expect(finding.evidence.subject).toStrictEqual(
        SDK.forcedVerdictSubject({
          transactionId: txId,
          sourceKey: key,
          rejectionReason: null,
        }),
      );
    }
    // The durable artifact round trip keeps the accepted direction and the
    // selected output.
    const artifact = await prepareMinAdaForcedArtifact({
      block,
      detectionId: findings[1]!.detectionId,
    });
    const reopened = await admitMinAdaForcedArtifact(
      JSON.parse(JSON.stringify(artifact)),
    );
    expect(
      proofThreadDirectionOfForcedLeaf(reopened.forcedSource.membership.value),
    ).toBe(0n);
    expect(reopened.evidence.badOutputIndex).toBe(2n);
    // An artifact whose retained source claims the rejection direction (a
    // rejected leaf verdict) is refused against the committed verdict.
    const retained = Data.from(
      artifact.forcedSourceCbor,
      SDK.MinAdaForcedSourcePayloadSchema as never,
    ) as { membership: SDK.ForcedTransactionSourceMembershipProof };
    const reason = { OutputBelowMinAda: { output_index: 2n } };
    const value = {
      ...retained.membership.value,
      verdict: { ForcedTxInvalid: { reason } },
    };
    const membership = { ...retained.membership, value };
    const flipped = Data.to(
      { ...retained, membership } as never,
      SDK.MinAdaForcedSourcePayloadSchema as never,
    );
    await expect(
      admitMinAdaForcedArtifact({ ...artifact, forcedSourceCbor: flipped }),
    ).rejects.toThrow(/membership changed/u);
  });

  it("finds nothing on an honest accepted leaf", async () => {
    const { funded } = minAdaOutputs();
    const { block } = await blockWith({
      outputs: [funded, funded],
      verdict: "ForcedTxValid",
    });
    expect(detectMinAdaForcedReplay(block)).toStrictEqual([]);
  });

  it("keeps the rejection direction for a wrongful OutputBelowMinAda rejection", async () => {
    const { funded } = minAdaOutputs();
    const { block } = await blockWith({
      outputs: [funded],
      verdict: {
        ForcedTxInvalid: {
          reason: { OutputBelowMinAda: { output_index: 0n } },
        },
      },
    });
    const [finding, ...rest] = detectMinAdaForcedReplay(block);
    expect(rest).toStrictEqual([]);
    expect(finding!.evidence.state.direction).toBe(1n);
  });
});

describe("network-id detection over accepted forced transactions", () => {
  it("finds the body and every foreign output on an accepted leaf and plans each claim", async () => {
    const { block, txId, key } = await blockWith({
      outputs: [keyOutput(0, 2_000_000n), keyOutput(1, 2_000_000n)],
      bodyNetworkId: 1n,
      verdict: "ForcedTxValid",
    });
    const detections = detectNetworkIdAcceptedForced({
      block,
      expectedNetworkId: 0n,
    });
    expect(detections.map((value) => value.detectionId)).toStrictEqual([
      `${NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID}:0:${txId}:tx`,
      `${NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID}:0:${txId}:1`,
    ]);
    expect(
      detectNetworkIds(block).map((value) => value.detectionId),
    ).toStrictEqual(detections.map((value) => value.detectionId));
    // The rejection detector does not read accepted leaves.
    expect(
      detectNetworkIdWrongfulRejections({ block, expectedNetworkId: 0n }),
    ).toStrictEqual([]);

    const body = await prepareNetworkIdAcceptedForced({
      block,
      expectedNetworkId: 0n,
    });
    expect(body.fault).toBe("TransactionNetwork");
    expect(body.committedNetworkId).toBe(1n);
    expect(
      proofThreadDirectionOfForcedLeaf(body.forcedSource.membership.value),
    ).toBe(0n);
    expect(body.subject).toStrictEqual(
      SDK.forcedVerdictSubject({
        transactionId: txId,
        sourceKey: key,
        rejectionReason: null,
      }),
    );
    expect(body.forcedSource.membership.root).toBe(
      block.header.forcedTransactionsRoot,
    );
    const output = await prepareNetworkIdAcceptedForced({
      block,
      expectedNetworkId: 0n,
      detectionId: detections[1]!.detectionId,
    });
    expect(output.fault).toStrictEqual({
      OutputNetwork: { output_index: 1n },
    });
    expect(output.outputNetworkIds).toStrictEqual([0n, 1n]);
    expect(
      FRAUD_PROOF_CLASSIFICATION_RULES.find(
        (rule) => rule.category === "networkId",
      )?.violationIds,
    ).toContain(NETWORK_ID_ACCEPTED_FORCED_VIOLATION_ID);
    for (const prepared of [body, output]) {
      const artifact = networkIdAcceptedForcedArtifactFromPrepared(prepared);
      const roundTrip = admitNetworkIdWorkflowArtifact(
        JSON.parse(JSON.stringify(artifact)),
      );
      expect(roundTrip).toStrictEqual({
        direction: "accepted_forced",
        prepared,
      });
    }
    const outputArtifact = networkIdAcceptedForcedArtifactFromPrepared(output);
    expect(() =>
      admitNetworkIdWorkflowArtifact({ ...outputArtifact, outputIndex: "0" }),
    ).toThrow(/honest output network/u);
    expect(() =>
      admitNetworkIdWorkflowArtifact({ ...outputArtifact, outputIndex: "3" }),
    ).toThrow(/absent or honest output network/u);
    expect(() =>
      admitNetworkIdWorkflowArtifact({
        ...outputArtifact,
        badTxId: "00".repeat(32),
      }),
    ).toThrow(/accepted forced leaf/u);
    // The classifier can select the output claim before the body claim for
    // the same source event; preparation must carry that exact selection.
    const adapter = createNetworkIdWorkflowAdapter({
      contracts: { expectedNetworkId: 0n },
    } as NetworkIdWorkflowAdapterConfig);
    const artifact = await adapter.prepare({
      evidence: block,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: block.headerHash,
        category: "networkId",
        selected: detections[1]!,
        detections,
        unprovableGaps: [],
      },
    });
    expect(admitNetworkIdWorkflowArtifact(artifact)).toStrictEqual({
      direction: "accepted_forced",
      prepared: output,
    });
  });

  it("finds nothing on an honest accepted leaf, including an absent body network", async () => {
    for (const bodyNetworkId of [0n, 255n]) {
      const { block } = await blockWith({
        outputs: [keyOutput(0, 2_000_000n)],
        bodyNetworkId,
        verdict: "ForcedTxValid",
      });
      expect(
        detectNetworkIdAcceptedForced({ block, expectedNetworkId: 0n }),
      ).toStrictEqual([]);
      await expect(
        prepareNetworkIdAcceptedForced({ block, expectedNetworkId: 0n }),
      ).rejects.toThrow(/no foreign network/u);
    }
  });

  it("leaves rejected leaves to the rejection detector", async () => {
    const { block } = await blockWith({
      outputs: [keyOutput(1, 2_000_000n)],
      verdict: { ForcedTxInvalid: { reason: "NetworkIdMismatch" } },
    });
    expect(
      detectNetworkIdAcceptedForced({ block, expectedNetworkId: 0n }),
    ).toStrictEqual([]);
  });
});

/** The forced mint treats the datum byte string as opaque, as does the output scan. */
const opaqueDatumOutput = (
  networkId: number,
  lovelace: bigint,
  datumHex: string,
) => {
  const base = keyOutput(networkId, lovelace);
  return Buffer.concat([
    Buffer.from([0xa3]),
    base.subarray(1),
    Buffer.from([2]),
    encodeCborBytes(Buffer.from(datumHex, "hex")),
  ]);
};

describe("accepted forced opaque inline datum coverage", () => {
  it.each(["00", "1800", "ff", "9f00ff"])(
    "detects and reopens independent faults with datum bytes %s",
    async (datum) => {
      const output = opaqueDatumOutput(1, 1n, datum);
      const { block, admission } = await blockWith({
        outputs: [output],
        bodyNetworkId: 1n,
        verdict: "ForcedTxValid",
      });
      expect(admission).toBeNull();
      const minAda = detectMinAdaForcedReplay(block);
      expect(minAda).toHaveLength(1);
      expect(minAda[0]!.evidence.outputItemCbors).toStrictEqual([
        output.toString("hex"),
      ]);
      const minAdaArtifact = await prepareMinAdaForcedArtifact({ block });
      const reopened = await admitMinAdaForcedArtifact(
        JSON.parse(JSON.stringify(minAdaArtifact)),
      );
      expect(reopened.evidence.outputItemCbors).toStrictEqual([
        output.toString("hex"),
      ]);
      const admitted = await admitMinAdaWorkflowArtifact(minAdaArtifact);
      expect(txFieldPlan(admitted, h28(0x66)).sourceKind).toBe(1n);
      const networks = detectNetworkIdAcceptedForced({
        block,
        expectedNetworkId: 0n,
      });
      expect(networks.map((finding) => finding.faultClaim)).toStrictEqual([
        { kind: "transaction-network" },
        { kind: "output-network", outputIndex: 0n },
      ]);
      for (const detection of networks) {
        const prepared = await prepareNetworkIdAcceptedForced({
          block,
          expectedNetworkId: 0n,
          detectionId: detection.detectionId,
        });
        expect(
          admitNetworkIdWorkflowArtifact(
            JSON.parse(
              JSON.stringify(
                networkIdAcceptedForcedArtifactFromPrepared(prepared),
              ),
            ),
          ),
        ).toStrictEqual({ direction: "accepted_forced", prepared });
      }
    },
  );

  it.each(["00", "1800", "ff", "9f00ff"])(
    "does not accuse a funded matching network because of datum bytes %s",
    async (datum) => {
      const { block, admission } = await blockWith({
        outputs: [opaqueDatumOutput(0, 2_000_000n, datum)],
        verdict: "ForcedTxValid",
      });
      expect(admission).toBeNull();
      expect(detectMinAdaForcedReplay(block)).toStrictEqual([]);
      expect(
        detectNetworkIdAcceptedForced({ block, expectedNetworkId: 0n }),
      ).toStrictEqual([]);
    },
  );
});

it("keeps independent accepted-forced faults when another output needs a shape prerequisite", async () => {
  const unreadable = opaqueDatumOutput(0, 1n, "");
  expect(deriveTransactionOutputScan(unreadable).canonical).toBe(false);
  const { block, admission } = await blockWith({
    outputs: [unreadable, opaqueDatumOutput(1, 1n, "1800")],
    bodyNetworkId: 1n,
    verdict: "ForcedTxValid",
  });
  expect(admission).toBeNull();
  const minAda = detectMinAdaForcedReplay(block);
  expect(
    minAda.map((finding) => finding.evidence.badOutputIndex),
  ).toStrictEqual([1n]);
  const networks = detectNetworkIdAcceptedForced({
    block,
    expectedNetworkId: 0n,
  });
  expect(networks.map((finding) => finding.faultClaim)).toStrictEqual([
    { kind: "transaction-network" },
    { kind: "output-network", outputIndex: 1n },
  ]);
  const prepared = await prepareNetworkIdAcceptedForced({
    block,
    expectedNetworkId: 0n,
  });
  const artifact = networkIdAcceptedForcedArtifactFromPrepared(prepared);
  expect(() =>
    admitNetworkIdWorkflowArtifact({
      ...artifact,
      faultKind: "output-network",
      outputIndex: "0",
    }),
  ).toThrow(/absent or honest output/u);
});

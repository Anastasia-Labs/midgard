import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildCorpusChain } from "@/commands/stress-corpus/build-chain.js";
import {
  canonicalOutrefCborFromLabel,
  decodeCanonicalProbeRow,
} from "@/workers/mpf-engine-probe-corpus.js";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

const seed =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";

describe("Architecture G canonical probe workload", () => {
  it("decodes exact selected-input deletes and every canonical output insert", async () => {
    const wallet = walletFromSeed(seed, { network: "Preprod" });
    const fundingTxHash = "ab".repeat(32);
    const fundingOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(wallet.address),
        assetsToValue({ lovelace: 10_000_000n }),
      ).to_cbor_bytes(),
    );
    const chain = await buildCorpusChain({
      seedPhrase: seed,
      walletId: "stress-wallet-0001",
      fundingUtxo: {
        txHash: fundingTxHash,
        outputIndex: 0,
        outputCborHex: fundingOutput.toString("hex"),
      },
      depth: 2,
      amountLovelace: 1_000_000n,
      feeParams: { minFeeA: 0n, minFeeB: 0n },
      network: "Preprod",
      networkId: 0n,
      maxSubmitTxCborBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
      corpusSliceId: "large",
    });
    const first = decodeCanonicalProbeRow(
      chain.rows[0] as unknown as Record<string, unknown>,
      0,
    );
    const second = decodeCanonicalProbeRow(
      chain.rows[1] as unknown as Record<string, unknown>,
      1,
    );

    expect(first.sourceEvent.ledgerOps).toHaveLength(3);
    expect(second.sourceEvent.ledgerOps).toHaveLength(3);
    expect(first.sourceEvent.ledgerOps[0]).toEqual({
      type: "delete",
      key: canonicalOutrefCborFromLabel(`${fundingTxHash}#0`),
    });
    expect(second.sourceEvent.ledgerOps[0]).toEqual({
      type: "delete",
      key: canonicalOutrefCborFromLabel(`${chain.rows[0]!.txHash}#1`),
    });
    expect(first.sourceEvent.ledgerOps[2]?.key).toEqual(
      second.sourceEvent.ledgerOps[0]?.key,
    );
    expect(first.transactionOp.key.toString("hex")).toBe(chain.rows[0]!.txHash);
    expect(() =>
      decodeCanonicalProbeRow(
        { ...chain.rows[0], unknown: true } as Record<string, unknown>,
        0,
      ),
    ).toThrow("keys must be exact");
    expect(() =>
      decodeCanonicalProbeRow(
        { ...chain.rows[0], corpusSliceId: "" } as Record<string, unknown>,
        0,
      ),
    ).toThrow("corpusSliceId must be a non-empty string");
  });

  it("rejects a corpus row whose selected input is not the signed spend", async () => {
    const wallet = walletFromSeed(seed, { network: "Preprod" });
    const fundingTxHash = "cd".repeat(32);
    const fundingOutput = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(wallet.address),
        assetsToValue({ lovelace: 10_000_000n }),
      ).to_cbor_bytes(),
    );
    const chain = await buildCorpusChain({
      seedPhrase: seed,
      walletId: "stress-wallet-0001",
      fundingUtxo: {
        txHash: fundingTxHash,
        outputIndex: 0,
        outputCborHex: fundingOutput.toString("hex"),
      },
      depth: 1,
      amountLovelace: 1_000_000n,
      feeParams: { minFeeA: 0n, minFeeB: 0n },
      network: "Preprod",
      networkId: 0n,
      maxSubmitTxCborBytes: MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
      corpusSliceId: "large",
    });
    expect(() =>
      decodeCanonicalProbeRow(
        {
          ...chain.rows[0],
          selectedInputOutref: `${"ef".repeat(32)}#0`,
        } as Record<string, unknown>,
        0,
      ),
    ).toThrow("does not spend its declared selected input");
  });
});

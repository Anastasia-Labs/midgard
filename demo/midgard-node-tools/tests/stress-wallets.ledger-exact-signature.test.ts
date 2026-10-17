/**
 * The terminal-drain and consolidation intent checks verify the source
 * signature exactly as the ledger does. A canonical prepared transaction
 * passes; the same transaction with its signature replaced by one the
 * ledger refuses (from the ledger-exact signature vectors: the source key's
 * own scalar with each R in `ED25519_SMALL_ORDER_ENCODINGS` and S = h·a,
 * honest R with S + L, and the honest signature with one bit flipped) is
 * refused at the signature check, with its exact message.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";
import { beforeAll, describe, expect, it } from "vitest";

import {
  ED25519_L,
  ED25519_SMALL_ORDER_ENCODINGS,
  ledgerEd25519Verdict,
  scalarSignatureWithR,
  sOf,
  withS,
} from "../../midgard-validation/tests/ed25519-ledger-vectors.js";
import { assertConsolidationTransferIntent } from "../src/commands/stress-wallets/consolidation.assert-consolidation-transfer-intent.js";
import {
  STRESS_WALLET_RECORD_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
} from "../src/commands/stress-wallets/constants.js";
import { assertTerminalDrainIntent } from "../src/commands/stress-wallets/terminal-drain.assert-terminal-drain-intent.js";
import { terminalSnapshotHash } from "../src/commands/stress-wallets/terminal-drain.terminal-snapshot-hash.js";
import type { StressWalletRecord } from "../src/commands/stress-wallets/types.js";
import {
  prepareCanonicalNativeTransfer,
  prepareCanonicalTerminalDrain,
  TEST_SEEDS,
} from "./stress-wallets.prepare-canonical-native-transfer.js";

beforeAll(() => ledgerEd25519SignatureReady);

const NETWORK = "Preprod" as const;
const sourceSeed = TEST_SEEDS[0]!;
const wallet = walletFromSeed(sourceSeed, { network: NETWORK });
const treasury = walletFromSeed(TEST_SEEDS[2]!, { network: NETWORK }).address;
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const vkey = Buffer.from(privateKey.to_public().to_raw_bytes());
/** The source key's secret scalar: the extended key's first 32 bytes. */
const scalarBytes = Buffer.from(privateKey.to_raw_bytes()).subarray(0, 32);
const source: StressWalletRecord = {
  schemaVersion: STRESS_WALLET_RECORD_SCHEMA_VERSION,
  walletId: "w0",
  index: 0,
  envName: "STRESS_WALLET_SEED_PHRASE_0",
  network: NETWORK,
  seedPhrase: sourceSeed,
  l2Address: wallet.address,
  paymentKeyHash: privateKey.to_public().hash().to_hex(),
  createdAt: "2026-01-01T00:00:00.000Z",
};

/**
 * `signedTxCbor` with its source signature replaced by each signature of the
 * ledger-exact signature vectors over `txHash`, the honest one first, with
 * the ledger's verdict on each.
 */
const signatureVariants = (signedTxCbor: string, txHash: string) => {
  const cbor = Buffer.from(signedTxCbor, "hex");
  const message = Buffer.from(txHash, "hex");
  const honest = Buffer.from(privateKey.sign(message).to_raw_bytes());
  const at = cbor.indexOf(honest);
  expect(at).toBeGreaterThan(0);
  expect(cbor.indexOf(honest, at + 1)).toBe(-1);
  const flipped = Buffer.from(honest);
  flipped[63] = flipped[63]! ^ 1;
  const signatures = [
    { label: "honest", signature: honest },
    ...ED25519_SMALL_ORDER_ENCODINGS.map((encodedR) => ({
      label: `R ${encodedR.toString("hex")}, S h·a`,
      signature: scalarSignatureWithR(scalarBytes, message, encodedR),
    })),
    {
      label: "honest R, S + L",
      signature: withS(honest, sOf(honest) + ED25519_L),
    },
    { label: "honest, one bit flipped", signature: flipped },
  ];
  return signatures.map(({ label, signature }) => {
    const replaced = Buffer.from(cbor);
    signature.copy(replaced, at);
    return {
      label,
      signedTxCbor: replaced.toString("hex"),
      ledger: ledgerEd25519Verdict({ label, vkey, message, signature }),
    };
  });
};

/** The message `check` throws, or undefined when it passes. */
const refusal = (check: () => void): string | undefined => {
  try {
    check();
    return undefined;
  } catch (error) {
    return error instanceof Error ? error.message : String(error);
  }
};

describe("stress-wallet intents and ledger-exact signature vectors", () => {
  it("a terminal drain passes with the honest signature and is refused with each one the ledger refuses", async () => {
    const outRef = "81".repeat(32);
    const utxo = {
      txHash: outRef,
      outputIndex: 0,
      outrefCbor: makeOutRefCbor(outRef, 0),
      outputCbor: Buffer.from(
        makeMidgardTxOutput(
          CML.Address.from_bech32(wallet.address),
          assetsToValue({ lovelace: 5_000_000n }),
        ).to_cbor_bytes(),
      ),
      address: wallet.address,
      assets: { lovelace: 5_000_000n },
    };
    const drain = await prepareCanonicalTerminalDrain({
      sourceSeedPhrase: sourceSeed,
      sourceAddress: wallet.address,
      destinationAddress: treasury,
      utxos: [utxo],
    });
    const variants = signatureVariants(drain.signedTxCbor, drain.txHash);
    expect(variants.map((variant) => variant.ledger)).toEqual(
      variants.map((_, index) => index === 0),
    );
    for (const variant of variants) {
      const error = refusal(() =>
        assertTerminalDrainIntent(
          {
            walletId: "w0",
            address: wallet.address,
            beforeOutrefs: [`${outRef}#0`],
            beforeLovelace: "5000000",
            beforeValueSha256: terminalSnapshotHash([utxo]),
            status: "prepared",
            txHash: drain.txHash,
            signedTxCbor: variant.signedTxCbor,
            selectedInputs: drain.selectedInputs,
            requestedLovelace: drain.requestedLovelace.toString(),
            feeLovelace: drain.feeLovelace.toString(),
            signedTxBytes: drain.signedTxBytes,
          },
          source,
          {
            schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
            scope: {
              count: 1,
              startIndex: 0,
              envPrefix: "STRESS_WALLET_SEED_PHRASE",
              network: NETWORK,
              walletSetSha256: "00".repeat(32),
            },
            scopeSha256: "00".repeat(32),
            nodeEndpoint: "http://127.0.0.1:0",
            network: NETWORK,
            treasuryAddress: treasury,
            treasuryBeforeLovelace: "0",
            minFeeA: "0",
            minFeeB: "0",
            feeCapLovelace: "100000000",
            maxFeeIterations: 4,
            entries: [],
          },
        ),
      );
      expect([variant.label, error]).toEqual([
        variant.label,
        variant.ledger ? undefined : "Terminal drain signature is invalid.",
      ]);
    }
  });

  it("a consolidation transfer passes with the honest signature and is refused with each one the ledger refuses", async () => {
    const transfer = await prepareCanonicalNativeTransfer({
      sourceSeedPhrase: sourceSeed,
      sourceAddress: wallet.address,
      destinationAddress: treasury,
      sourceLovelace: 5_000_000n,
      requestedLovelace: 2_000_000n,
      txHashByte: "83",
    });
    const variants = signatureVariants(transfer.signedTxCbor, transfer.txHash);
    expect(variants.map((variant) => variant.ledger)).toEqual(
      variants.map((_, index) => index === 0),
    );
    for (const variant of variants) {
      const error = refusal(() =>
        assertConsolidationTransferIntent({
          entry: {
            walletId: "w0",
            address: wallet.address,
            beforeLovelace: "5000000",
            beforeOutrefs: [`${"83".repeat(32)}#0`],
            requestedLovelace: "2000000",
            txHash: transfer.txHash,
            signedTxCbor: variant.signedTxCbor,
            selectedInputs: transfer.selectedInputs,
            selectedInputLovelace: "5000000",
          },
          source,
          treasuryAddress: treasury,
          reserveLovelace: 100_000_000n,
          network: NETWORK,
        }),
      );
      expect([variant.label, error]).toEqual([
        variant.label,
        variant.ledger
          ? undefined
          : "Consolidation transaction intent for w0 has an invalid source signature.",
      ]);
    }
  });
});

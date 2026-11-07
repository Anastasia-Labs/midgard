/**
 * `fromTx` imports a signed transaction's address witnesses exactly as the
 * ledger verifies them: a transaction signed by the wallet imports, and the
 * same transaction with its signature replaced by one the ledger refuses
 * (from the ledger-exact signature vectors) is refused with `SigningError`.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  ledgerEd25519Verdict,
  ownKeyIdentityRSignature,
  signerPublicKey,
  signerSeed,
} from "../../midgard-validation/tests/ed25519-ledger-vectors.js";
import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  outRefToCbor,
  SigningError,
} from "../src/index.js";

beforeAll(() => ledgerEd25519SignatureReady());

/** A provider that serves only the protocol parameters building needs. */
const provider: MidgardProvider = {
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
    currentSlot: 0n,
    strictnessProfile: "phase1_midgard",
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://from-tx",
    protocolInfoSource: "node",
  }),
};

describe("fromTx and ledger-exact signature vectors", () => {
  it("refuses a signed transaction whose address witness the ledger refuses", async () => {
    const seed = signerSeed(7);
    const privateKey = CML.PrivateKey.from_normal_bytes(seed);
    expect(Buffer.from(privateKey.to_public().to_raw_bytes())).toEqual(
      signerPublicKey(seed),
    );
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const midgard = await LucidMidgard.new(provider, {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromPrivateKey(privateKey, walletAddress);
    const outRef = { txHash: "22".repeat(32), outputIndex: 0 };
    const input: MidgardUtxo = decodeMidgardUtxo({
      outRef,
      outRefCbor: outRefToCbor(outRef),
      outputCbor: encodeMidgardTxOutput(walletAddress, {
        lovelace: 2_000_000n,
      }),
    });
    const completed = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(walletAddress, { lovelace: 2_000_000n })
      .complete({ fee: 0n });
    const signed = await completed.sign();
    expect(
      midgard.fromTx(signed.txHex, { resolvedSpendInputs: [input] }).txHex,
    ).toBe(signed.txHex);

    const message = signed.txId;
    const honest = Buffer.from(
      privateKey.sign(message).to_raw_bytes(),
    ).toString("hex");
    expect(signed.txHex.split(honest)).toHaveLength(2);
    // The signer's own key, R the identity encoding, S = h·a mod L.
    const refused = ownKeyIdentityRSignature(seed, message);
    expect(
      ledgerEd25519Verdict({
        label: "own key, R identity, S h·a",
        vkey: signerPublicKey(seed),
        message,
        signature: refused,
      }),
    ).toBe(false);
    const refusedHex = signed.txHex.replace(honest, refused.toString("hex"));
    expect(() =>
      midgard.fromTx(refusedHex, { resolvedSpendInputs: [input] }),
    ).toThrow(SigningError);
  });
});

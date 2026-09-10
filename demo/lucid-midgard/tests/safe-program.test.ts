import { MIDGARD_SUPPORTED_SCRIPT_LANGUAGES } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type Address,
  BuilderInvariantError,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  outRefToCbor,
} from "../src/index.js";

const address =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const makeUtxo = (
  ref: OutRef,
  owner: Address,
  assets: Readonly<Record<string, bigint>>,
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(owner, assets),
  });

const makeProvider = (
  utxos: readonly MidgardUtxo[] = [],
  counters?: { protocolParameters: number; utxos: number },
): MidgardProvider => ({
  getUtxos: async () => {
    if (counters !== undefined) {
      counters.utxos += 1;
    }
    return utxos;
  },
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
  getProtocolParameters: async () => {
    if (counters !== undefined) {
      counters.protocolParameters += 1;
    }
    return {
      minFeeA: 0n,
      minFeeB: 0n,
      networkId: 0n,
      currentSlot: 0n,
      strictnessProfile: "phase1_midgard",
    };
  },
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://safe",
    protocolInfoSource: "node",
  }),
});

/**
 * Every scenario below is fully determined: a fixed input outref, one fixed
 * address, fixed values, an explicit `fee: 0n`, and a provider whose protocol
 * parameters are all zero. So the completed transaction — and therefore its
 * V1 transaction id — is a fixed vector, not an incidental value. The ids
 * pinned here were recorded from a reviewed run and are anchored by the
 * metadata asserted alongside them: they exist so that a change to the
 * canonical body encoding or to the balancing decisions cannot pass while the
 * structural assertions still hold.
 */
const SAFE_COMPLETE_TX_ID =
  "9314a2974ff655afac8201c09288ff7b48a092e458dcec5cb7180fb7fb861b46";
const PROGRAM_COMPLETE_TX_ID =
  "49d7825d8a1d6207d09b971eb02c70271a18950b0f07036c9da63e7d2ce5e108";
const CHAIN_COMPLETE_TX_ID =
  "83c94f922a48bb063aebd04d151bee5ec0f7943868243be546aae44bf95ee38b";
const ADDRESS_KEY_HASH =
  "2b218f09113f8c3900e461c3ea9985152b27c410dbc2e1d111eb3262";

describe("safe and Effect builder APIs", () => {
  it("returns completed transactions and structured errors from completeSafe", async () => {
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const input = makeUtxo(makeOutRef(0x11), address, { lovelace: 1_000_000n });

    const success = await midgard
      .newTx()
      .collectFrom([input])
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .completeSafe({ fee: 0n });
    expect(success.ok).toBe(true);
    if (!success.ok) return;
    expect(success.value.txIdHex).toBe(SAFE_COMPLETE_TX_ID);
    // The explicitly collected input funds the single payment exactly, so the
    // builder must not add a wallet input or a change output, and — because
    // nothing was left over to balance — must report the transaction as not
    // balanced rather than silently inventing change.
    expect(success.value.metadata).toEqual({
      fee: 0n,
      inputCount: 1,
      referenceInputCount: 0,
      outputCount: 1,
      requiredSignerCount: 0,
      txByteLength: 208,
      feeIterations: 0,
      balanced: false,
      expectedAddrWitnessCount: 1,
      expectedAddrWitnessKeyHashes: [ADDRESS_KEY_HASH],
      estimatedSignedTxByteLength: 312,
      providerGeneration: 0,
      providerDiagnostics: {
        endpoint: "memory://safe",
        protocolInfoSource: "node",
      },
    });
    // Cross-check the reported metadata against the transaction it describes:
    // the serialized length and the fee are readable from the canonical bytes
    // themselves, so a metadata record that disagrees with the transaction
    // cannot pass.
    expect(success.value.txHex.length / 2).toBe(208);
    expect(success.value.tx.body.fee).toBe(0n);
    expect(success.value.tx.body.networkId).toBe(0n);

    const failure = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1n })
      .completeSafe({ fee: 0n });
    expect(failure.ok).toBe(false);
    if (!failure.ok) {
      expect(failure.error).toBeInstanceOf(BuilderInvariantError);
      // The refusal is caused by the missing spend input, not by any other
      // invariant that happens to share the BUILDER_INVARIANT code.
      expect(failure.error.toJSON()).toEqual({
        name: "BuilderInvariantError",
        code: "BUILDER_INVARIANT",
        message: "Cannot complete a transaction with no spend inputs",
        detail: null,
      });
    }
  });

  it("keeps completeProgram lazy and fails through the Effect error channel", async () => {
    const counters = { protocolParameters: 0, utxos: 0 };
    const input = makeUtxo(makeOutRef(0x22), address, { lovelace: 2_000_000n });
    const midgard = await LucidMidgard.new(makeProvider([input], counters), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromAddress(address);
    counters.protocolParameters = 0;
    counters.utxos = 0;

    const program = midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .completeProgram();
    expect(counters).toEqual({ protocolParameters: 0, utxos: 0 });

    const completed = await Effect.runPromise(program);
    expect(completed.txIdHex).toBe(PROGRAM_COMPLETE_TX_ID);
    // Running the program pulls the wallet's UTxOs from the provider and
    // balances against them: one provider input, the requested payment, and a
    // change output carrying the remaining 1 ADA.
    expect(completed.metadata).toEqual({
      fee: 0n,
      inputCount: 1,
      referenceInputCount: 0,
      outputCount: 2,
      requiredSignerCount: 0,
      txByteLength: 279,
      feeIterations: 1,
      balanced: true,
      changeAddress: address,
      changeAssets: { lovelace: 1_000_000n },
      changeOutputIndex: 1,
      expectedAddrWitnessCount: 1,
      expectedAddrWitnessKeyHashes: [ADDRESS_KEY_HASH],
      estimatedSignedTxByteLength: 383,
      walletInputSource: "provider",
      walletInputCount: 1,
      providerGeneration: 0,
      providerDiagnostics: {
        endpoint: "memory://safe",
        protocolInfoSource: "node",
      },
    });
    // Exactly one fetch of each: laziness must not become repeated I/O.
    expect(counters).toEqual({ protocolParameters: 1, utxos: 1 });

    const failingMidgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    const failure = await Effect.runPromise(
      Effect.either(
        failingMidgard
          .newTx()
          .pay.ToAddress(address, { lovelace: 1n })
          .completeProgram({ fee: 0n }),
      ),
    );
    expect(failure._tag).toBe("Left");
    if (failure._tag === "Left") {
      expect(failure.left).toBeInstanceOf(BuilderInvariantError);
      // Same causal refusal as completeSafe, surfaced through the Effect
      // error channel rather than as a defect.
      expect(failure.left.toJSON()).toEqual({
        name: "BuilderInvariantError",
        code: "BUILDER_INVARIANT",
        message: "Cannot complete a transaction with no spend inputs",
        detail: null,
      });
    }
  });

  it("wraps local chaining with safe and Effect variants", async () => {
    const input = makeUtxo(makeOutRef(0x33), address, { lovelace: 2_000_000n });
    const midgard = await LucidMidgard.new(makeProvider(), {
      network: "Preview",
      networkId: 0,
    });
    midgard.selectWallet.fromAddress(address, [input]);

    const safe = await midgard
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000n })
      .chainSafe({ fee: 0n });
    expect(safe.ok).toBe(true);
    if (safe.ok) {
      const [newWalletUtxos, derivedOutputs, completed] = safe.value;
      expect(completed.txIdHex).toBe(CHAIN_COMPLETE_TX_ID);
      expect(completed.metadata.walletInputSource).toBe("instance-override");
      expect(completed.metadata.changeOutputIndex).toBe(1);
      // Chaining hands back exactly the transaction's own two outputs, keyed
      // by *this* transaction's id — that is the property that makes the next
      // chained transaction spendable before submission.
      const describeUtxo = (utxo: (typeof derivedOutputs)[number]) => ({
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        address: utxo.output.address,
        assets: utxo.output.assets,
      });
      const expectedOutputs = [
        {
          txHash: CHAIN_COMPLETE_TX_ID,
          outputIndex: 0,
          address,
          assets: { lovelace: 1_000_000n },
        },
        {
          txHash: CHAIN_COMPLETE_TX_ID,
          outputIndex: 1,
          address,
          assets: { lovelace: 1_000_000n },
        },
      ];
      expect(derivedOutputs.map(describeUtxo)).toEqual(expectedOutputs);
      // The pre-chain wallet UTxO was consumed, so the new wallet set is the
      // derived set and nothing else.
      expect(newWalletUtxos.map(describeUtxo)).toEqual(expectedOutputs);
    }

    const chained = await Effect.runPromise(
      midgard
        .newTx()
        .pay.ToAddress(address, { lovelace: 1_000_000n })
        .chainProgram({ fee: 0n }),
    );
    // chainProgram must reach the identical decision as chainSafe on the same
    // instance state, not merely produce some transaction.
    expect(chained[2].txIdHex).toBe(CHAIN_COMPLETE_TX_ID);
    expect(chained[0].length).toBe(2);
    expect(chained[1].length).toBe(2);
  });
});

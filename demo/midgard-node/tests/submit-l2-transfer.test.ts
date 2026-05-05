import "./utils.js";

import { afterEach, describe, expect, it, vi } from "vitest";
import { Effect } from "effect";
import {
  CML,
  assetsToValue,
  valueToAssets,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
import {
  DEFAULT_WALLET_SEED_ENV,
  LEGACY_DEFAULT_WALLET_SEED_ENV,
  buildTransferTx,
  buildTransferTxWithMinFee,
  parseSubmitL2TransferConfig,
  resolveWalletSeedPhrase,
  selectTransferInputs,
  submitL2TransferProgram,
  type NodeUtxo,
} from "@/commands/submit-l2-transfer.js";
import {
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type QueuedTx,
} from "@/validation/index.js";
import {
  decodeMidgardTxOutput,
  midgardOutputAddressText,
  midgardValueToCmlValue,
} from "@/validation/midgard-output.js";
import { makeMidgardTxOutput } from "./midgard-output-helpers.js";
import { Database } from "@/services/database.js";
import { NodeConfig } from "@/services/config.js";
import { Lucid as LucidService } from "@/services/lucid.js";

const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";
const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

const mkQueued = (txId: Buffer, txCbor: Buffer): QueuedTx => ({
  txId,
  txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

const mkNodeUtxo = ({
  txHash,
  outputIndex,
  address,
  assets,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly assets: { readonly [unit: string]: bigint };
}): NodeUtxo => {
  const outrefCbor = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );
  const outputCbor = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      assetsToValue(assets),
    ).to_cbor_bytes(),
  );
  return {
    txHash,
    outputIndex,
    outrefCbor,
    outputCbor,
    address,
    assets,
  };
};

const mockLucidService = LucidService.make({
  api: {
    currentSlot: () => 0,
  } as never,
  referenceScriptsApi: {
    currentSlot: () => 0,
  } as never,
  operatorMainAddress: "",
  operatorMergeAddress: "",
  referenceScriptsAddress: "",
  switchToOperatorsMainWallet: Effect.succeed(undefined),
  switchToOperatorsMergingWallet: Effect.succeed(undefined),
  switchToReferenceScriptWallet: Effect.succeed(undefined),
});

describe("submit-l2-transfer config helpers", () => {
  it("parses a valid config and derives a normalized endpoint", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const config = parseSubmitL2TransferConfig({
      l2Address: ` ${wallet.address} `,
      lovelace: "5000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000/",
    });

    expect(config.l2Address).toBe(wallet.address);
    expect(config.lovelace).toBe(5_000_000n);
    expect(config.nodeEndpoint).toBe("http://127.0.0.1:3000");
    expect(config.networkId).toBe(0n);
    expect(config.submissionMode).toBe("api");
  });

  it("resolves USER_WALLET by default and falls back to USER_SEED_PHRASE", () => {
    const direct = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });
    expect(direct.resolvedFrom).toBe("direct-argument");

    const legacy = resolveWalletSeedPhrase({
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {
        [LEGACY_DEFAULT_WALLET_SEED_ENV]: TEST_SEED,
      },
    });
    expect(legacy.resolvedFrom).toBe(LEGACY_DEFAULT_WALLET_SEED_ENV);
    expect(legacy.seedPhrase).toBe(TEST_SEED);
  });
});

describe("submit-l2-transfer tx building", () => {
  it("selects sufficient inputs and builds a valid native transfer with change", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const tokenUnit = `${"ab".repeat(28)}${"cd".repeat(2)}`;

    const utxos: readonly NodeUtxo[] = [
      mkNodeUtxo({
        txHash: "22".repeat(32),
        outputIndex: 1,
        address: sender.address,
        assets: {
          lovelace: 2_500_000n,
          [tokenUnit]: 5n,
        },
      }),
      mkNodeUtxo({
        txHash: "11".repeat(32),
        outputIndex: 0,
        address: sender.address,
        assets: {
          lovelace: 4_000_000n,
        },
      }),
    ];

    const requestedAssets = {
      lovelace: 3_000_000n,
      [tokenUnit]: 5n,
    } as const;

    const selected = selectTransferInputs(utxos, requestedAssets);
    expect(
      selected.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
    ).toEqual([`${"11".repeat(32)}#0`, `${"22".repeat(32)}#1`]);

    const built = await buildTransferTx({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      selectedInputs: selected,
      requestedAssets,
      networkId: 0n,
    });

    expect(built.changeAssets).toEqual({
      lovelace: 3_500_000n,
    });

    const nativeTx = decodeMidgardNativeTxFull(built.txCbor);
    const spendInputs = decodeMidgardNativeByteListPreimage(
      nativeTx.body.spendInputsPreimageCbor,
    ).map((bytes) => {
      const input = CML.TransactionInput.from_cbor_bytes(bytes);
      return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    });
    expect(spendInputs).toEqual([
      `${"11".repeat(32)}#0`,
      `${"22".repeat(32)}#1`,
    ]);

    const outputs = decodeMidgardNativeByteListPreimage(
      nativeTx.body.outputsPreimageCbor,
    ).map((bytes) => {
      expect(bytes[0] >> 5).toBe(5);
      const output = decodeMidgardTxOutput(bytes);
      return {
        address: midgardOutputAddressText(output),
        assets: valueToAssets(midgardValueToCmlValue(output.value)),
      };
    });
    expect(outputs).toHaveLength(2);
    expect(outputs[0]).toEqual({
      address: destination.address,
      assets: {
        lovelace: 3_000_000n,
        [tokenUnit]: 5n,
      },
    });
    expect(outputs[1]).toEqual({
      address: sender.address,
      assets: {
        lovelace: 3_500_000n,
      },
    });

    const validation = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(validation.rejected).toHaveLength(0);
    expect(validation.accepted).toHaveLength(1);
  });

  it("converges fees against signed bytes and passes local Phase A/B", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const minFeeA = 44n;
    const minFeeB = 155_381n;
    const senderUtxo = mkNodeUtxo({
      txHash: "44".repeat(32),
      outputIndex: 0,
      address: sender.address,
      assets: {
        lovelace: 8_000_000n,
      },
    });

    const built = await buildTransferTxWithMinFee({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: [senderUtxo],
      requestedAssets: { lovelace: 3_000_000n },
      networkId: 0n,
      minFeeA,
      minFeeB,
    });

    expect(built.fee).toBe(minFeeA * BigInt(built.txCbor.length) + minFeeB);
    expect(built.changeAssets).toEqual({
      lovelace: 8_000_000n - 3_000_000n - built.fee,
    });

    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA,
        minFeeB,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
      }),
    );
    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);

    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map([
          [senderUtxo.outrefCbor.toString("hex"), senderUtxo.outputCbor],
        ]),
        {
          nowCardanoSlotNo: 0n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
        },
      ),
    );
    expect(phaseB.rejected).toHaveLength(0);
    expect(phaseB.accepted).toHaveLength(1);
  });
});

describe("submit-l2-transfer program", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  it("rejects destination addresses from a different configured node network before fetching UTxOs", async () => {
    vi.stubEnv("NETWORK", "Mainnet");
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const config = parseSubmitL2TransferConfig({
      l2Address: destination.address,
      lovelace: "3000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000",
    });
    const resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });
    const fetchMock = vi.fn();
    vi.stubGlobal("fetch", fetchMock);

    await expect(
      Effect.runPromise(
        submitL2TransferProgram({
          config,
          resolvedWalletSeedPhrase,
        }).pipe(
          Effect.provideService(LucidService, mockLucidService),
          Effect.provide(Database.layer),
          Effect.provide(NodeConfig.layer),
        ),
      ),
    ).rejects.toThrow(
      "Destination address network id 0 does not match configured Midgard node network Mainnet (network id 1).",
    );
    expect(fetchMock).not.toHaveBeenCalled();
  });

  it("queries utxos, builds a transfer, and submits the native tx", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const senderUtxo = mkNodeUtxo({
      txHash: "33".repeat(32),
      outputIndex: 0,
      address: sender.address,
      assets: {
        lovelace: 8_000_000n,
      },
    });

    const config = parseSubmitL2TransferConfig({
      l2Address: destination.address,
      lovelace: "3000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000",
    });
    const resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });

    let expectedTxId = "";
    const fetchMock = vi.fn();
    vi.stubGlobal("fetch", fetchMock);
    fetchMock.mockImplementationOnce(async () => ({
      ok: true,
      status: 200,
      text: async () =>
        JSON.stringify({
          utxos: [
            {
              outref: senderUtxo.outrefCbor.toString("hex"),
              value: senderUtxo.outputCbor.toString("hex"),
            },
          ],
        }),
    }));
    fetchMock.mockImplementationOnce(
      async (_input: string, init?: RequestInit) => {
        const body = JSON.parse(String(init?.body)) as {
          readonly tx_cbor: string;
        };
        const built = decodeMidgardNativeTxFull(
          Buffer.from(body.tx_cbor, "hex"),
        );
        expectedTxId = computeMidgardNativeTxIdFromFull(built).toString("hex");
        return {
          ok: true,
          status: 200,
          text: async () =>
            JSON.stringify({
              txId: expectedTxId,
              status: "queued",
            }),
        };
      },
    );

    const assertWalletAddress = vi.fn();
    const result = await Effect.runPromise(
      submitL2TransferProgram({
        config,
        resolvedWalletSeedPhrase,
        assertWalletAddress,
      }).pipe(
        Effect.provideService(LucidService, mockLucidService),
        Effect.provide(Database.layer),
        Effect.provide(NodeConfig.layer),
      ),
    );

    expect(result.txId).toHaveLength(64);
    expect(result.status).toBe("queued");
    expect(result.senderAddress).toBe(sender.address);
    expect(result.destinationAddress).toBe(destination.address);
    expect(result.selectedInputs).toEqual([`${"33".repeat(32)}#0`]);
    expect(result.changeAssets).toEqual({
      lovelace: 5_000_000n,
    });
    expect(assertWalletAddress).toHaveBeenCalledWith(sender.address);
    expect(fetchMock).toHaveBeenCalledTimes(2);
  });
});

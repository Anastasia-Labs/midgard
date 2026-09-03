import "./utils.js";

import {
  decodeMidgardProofSubmission,
  encodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardAddressFromText,
  midgardValueToCmlValue,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import {
  assetsToValue,
  CML,
  valueToAssets,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  DEFAULT_WALLET_SEED_ENV,
  type NodeUtxo,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  buildTerminalDrainTx,
  buildTransferTx,
  buildTransferTxWithMinFee,
  FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
  makeStaticMidgardProvider,
  parseSubmitL2TransferConfig,
  prepareL2TransferProgram,
  selectTransferInputs,
  submitL2TransferProgram,
  submitNativeTransferTx,
  toQueuedTx,
} from "../src/commands/submit-l2-transfer.js";
import { NodeConfig } from "../src/services/config.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  WriteBehind,
  WriteBehindService,
} from "../src/services/write-behind.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";
const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

const unusedWriteBehind: WriteBehindService = {
  enqueueTxDeltas: () => Effect.void,
  enqueueAddressHistory: () => Effect.void,
  flushNow: Effect.void,
  depths: Effect.succeed({ queueDepth: 0, pendingDepth: 0, totalDepth: 0 }),
  run: Effect.never,
};
const launchDeploymentIdentity = ContractDeploymentIdentity.make({
  kind: "derived" as const,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});

const mkQueued = (txId: Buffer, txCbor: Buffer): QueuedTx => ({
  txId,
  txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
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
  const outrefCbor = makeOutRefCbor(txHash, outputIndex);
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
  referenceScriptsWalletAddress: "",
  referenceScriptsAddress: "",
  submitSlotSnapshot: () =>
    Effect.succeed({
      source: "test",
      currentSlot: 0,
      observedAtMs: 0,
      slotLengthMs: 1_000,
    }),
  switchToOperatorsMainWallet: Effect.succeed(undefined),
  switchToOperatorsMergingWallet: Effect.succeed(undefined),
  switchToReferenceScriptWallet: Effect.succeed(undefined),
});

const unusedSqlClient = new Proxy(
  {},
  {
    get: (_target, property) => {
      throw new Error(
        `Unexpected database access in API-mode transfer test: ${String(property)}`,
      );
    },
  },
) as SqlClient.SqlClient;

describe("submit-l2-transfer config helpers", () => {
  it("preserves a bounded lower submit cap in the static provider", async () => {
    const maxSubmitTxCborBytes =
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes - 1;
    const provider = makeStaticMidgardProvider({
      address: "addr_test1static",
      utxos: [],
      network: "Preprod",
      networkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      maxSubmitTxCborBytes,
    });

    await expect(provider.getProtocolInfo()).resolves.toMatchObject({
      submissionLimits: { maxSubmitTxCborBytes },
    });
  });

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

  it("parses protected Midgard destination addresses with the Midgard codec", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const protectedAddress = encodeMidgardAddressText(
      protectMidgardAddress(midgardAddressFromText(wallet.address)),
    );

    const config = parseSubmitL2TransferConfig({
      l2Address: ` ${protectedAddress} `,
      lovelace: "5000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000/",
    });

    expect(config.l2Address).toBe(protectedAddress);
    expect(config.networkId).toBe(0n);
  });

  it("resolves direct input or USER_WALLET without legacy fallback", () => {
    const direct = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });
    expect(direct.resolvedFrom).toBe("direct-argument");

    const envSeed = resolveWalletSeedPhrase({
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {
        [DEFAULT_WALLET_SEED_ENV]: TEST_SEED,
      },
    });
    expect(envSeed.resolvedFrom).toBe(DEFAULT_WALLET_SEED_ENV);
    expect(envSeed.seedPhrase).toBe(TEST_SEED);

    expect(() =>
      resolveWalletSeedPhrase({
        walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
        env: {
          USER_SEED_PHRASE: TEST_SEED,
        },
      }),
    ).toThrow(
      `Environment variable "${DEFAULT_WALLET_SEED_ENV}" does not contain a wallet seed phrase.`,
    );
  });
});

describe("submit-l2-transfer tx building", () => {
  it("attaches the canonical empty program-material sidecar for local validation", () => {
    const txId = Buffer.from("ab".repeat(32), "hex");
    const txCbor = Buffer.from("80", "hex");
    const queued = toQueuedTx({
      txId,
      txIdHex: txId.toString("hex"),
      txCbor,
      txHex: txCbor.toString("hex"),
      fee: 0n,
      senderAddress: "sender",
      destinationAddress: "destination",
      selectedInputs: [],
      requestedAssets: {},
      changeAssets: {},
    });

    expect(queued.txId).toBe(txId);
    expect(queued.txCbor).toBe(txCbor);
    expect(queued.programMaterialSidecarCbor).toEqual(
      encodeMidgardCekProgramMaterialSidecar([]),
    );
  });

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

    const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor);
    // Field-0 items are §5.3's fixed-index form, so they must be read with the
    // field-item decoder — CML's `TransactionInput` decoder tolerates the
    // non-minimal `19 0000` index but is not the contract these bytes obey.
    const spendInputs = decodeMidgardNativeByteListPreimage(
      nativeTx.body.spendInputsPreimageCbor,
    ).map((bytes) => {
      const input = decodeMidgardSpendInputItem(bytes);
      return `${Buffer.from(input.txId).toString("hex")}#${input.outputIndex.toString()}`;
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
        address: encodeMidgardAddressText(output.address),
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

  it("builds canonical V1 transfers", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, {
      network: "Preprod",
    });
    const senderUtxo = mkNodeUtxo({
      txHash: "45".repeat(32),
      outputIndex: 0,
      address: sender.address,
      assets: { lovelace: 8_000_000n },
    });

    const built = await buildTransferTxWithMinFee({
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: [senderUtxo],
      requestedAssets: { lovelace: 3_000_000n },
      networkId: 0n,
      minFeeA: 44n,
      minFeeB: 155_381n,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    });

    expect(
      decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor).version,
    ).toBe(1n);
    const phaseA = await Effect.runPromise(
      runPhaseAValidation([mkQueued(built.txId, built.txCbor)], {
        expectedNetworkId: 0n,
        minFeeA: 44n,
        minFeeB: 155_381n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);
  });

  it("builds a deterministic exact-zero all-input sweep that passes Phase A/B", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const minFeeA = 44n;
    const minFeeB = 155_381n;
    const inputs = [
      mkNodeUtxo({
        txHash: "62".repeat(32),
        outputIndex: 1,
        address: sender.address,
        assets: { lovelace: 2_000_000n },
      }),
      mkNodeUtxo({
        txHash: "61".repeat(32),
        outputIndex: 0,
        address: sender.address,
        assets: { lovelace: 3_000_000n },
      }),
    ];
    const args = {
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      availableUtxos: inputs,
      networkId: 0n,
      minFeeA,
      minFeeB,
      feeCap: 200_000n,
    } as const;
    const built = await buildTerminalDrainTx(args);
    const repeated = await buildTerminalDrainTx(args);
    expect(repeated.txHex).toBe(built.txHex);
    expect(built.selectedInputs.map((x) => x.txHash)).toEqual([
      "61".repeat(32),
      "62".repeat(32),
    ]);
    expect(built.changeAssets).toEqual({});
    expect((built.requestedAssets.lovelace ?? 0n) + built.fee).toBe(5_000_000n);
    expect(built.fee).toBeGreaterThanOrEqual(
      minFeeA * BigInt(built.txCbor.length) + minFeeB,
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(built.txCbor);
    const outputs = decodeMidgardNativeByteListPreimage(
      decoded.body.outputsPreimageCbor,
    );
    expect(outputs).toHaveLength(1);
    const output = decodeMidgardTxOutput(outputs[0]!);
    expect(encodeMidgardAddressText(output.address)).toBe(destination.address);
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
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map(
          inputs.map((x) => [x.outrefCbor.toString("hex"), x.outputCbor]),
        ),
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

  it("fails terminal drain closed for non-ADA, insufficient, and over-cap sources", async () => {
    const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const base = {
      senderAddress: sender.address,
      destinationAddress: destination.address,
      signer: CML.PrivateKey.from_bech32(sender.paymentKey),
      networkId: 0n,
      minFeeA: 44n,
      minFeeB: 155_381n,
      feeCap: 200_000n,
    };
    await expect(
      buildTerminalDrainTx({
        ...base,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "71".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 2_000_000n, ["aa".repeat(28)]: 1n },
          }),
        ],
      }),
    ).rejects.toThrow("non-ADA");
    await expect(
      buildTerminalDrainTx({
        ...base,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "72".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 1n },
          }),
        ],
      }),
    ).rejects.toThrow("cannot pay fee");
    await expect(
      buildTerminalDrainTx({
        ...base,
        feeCap: 1n,
        availableUtxos: [
          mkNodeUtxo({
            txHash: "73".repeat(32),
            outputIndex: 0,
            address: sender.address,
            assets: { lovelace: 2_000_000n },
          }),
        ],
      }),
    ).rejects.toThrow("exceeds cap");
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
          Effect.provideService(SqlClient.SqlClient, unusedSqlClient),
          Effect.provideService(WriteBehind, unusedWriteBehind),
          Effect.provideService(
            ContractDeploymentIdentity,
            launchDeploymentIdentity,
          ),
          Effect.provide(NodeConfig.layer),
        ),
      ),
    ).rejects.toThrow(
      "Destination address network id 0 does not match configured Midgard node network Mainnet (network id 1).",
    );
    expect(fetchMock).not.toHaveBeenCalled();
  });

  it("aborts a hanging prepare-time UTxO query at the configured request deadline", async () => {
    const destination = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const config = parseSubmitL2TransferConfig({
      l2Address: destination.address,
      lovelace: "3000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000",
      utxoRequestTimeoutMs: 5,
    });
    const resolvedWalletSeedPhrase = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });
    const fetchMock = vi.fn(
      (_input: string | URL | Request, init?: RequestInit): Promise<Response> =>
        new Promise((_resolve, reject) => {
          init?.signal?.addEventListener(
            "abort",
            () => reject(init.signal?.reason),
            { once: true },
          );
        }),
    );
    vi.stubGlobal("fetch", fetchMock);

    await expect(
      Effect.runPromise(
        prepareL2TransferProgram({
          config,
          resolvedWalletSeedPhrase,
        }).pipe(
          Effect.provideService(LucidService, mockLucidService),
          Effect.provideService(SqlClient.SqlClient, unusedSqlClient),
          Effect.provideService(WriteBehind, unusedWriteBehind),
          Effect.provideService(
            ContractDeploymentIdentity,
            launchDeploymentIdentity,
          ),
          Effect.provide(NodeConfig.layer),
        ),
      ),
    ).rejects.toThrow("Failed to fetch Midgard UTxOs");
    expect(fetchMock).toHaveBeenCalledTimes(1);
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
              outputCbor: senderUtxo.outputCbor.toString("hex"),
            },
          ],
        }),
    }));
    fetchMock.mockImplementationOnce(
      async (_input: string, init?: RequestInit) => {
        expect(init?.headers).toMatchObject({
          "content-type": "application/vnd.midgard.v1+cbor",
        });
        const body =
          init?.body instanceof Uint8Array
            ? Buffer.from(init.body)
            : Buffer.from(await new Response(init?.body).arrayBuffer());
        const submission = decodeMidgardProofSubmission(body);
        const built = decodeMidgardNativeTxFullFromCanonicalCbor(
          submission.transactionCbor,
        );
        expectedTxId = computeMidgardNativeTxId(built).toString("hex");
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
        Effect.provideService(SqlClient.SqlClient, unusedSqlClient),
        Effect.provideService(WriteBehind, unusedWriteBehind),
        Effect.provideService(
          ContractDeploymentIdentity,
          launchDeploymentIdentity,
        ),
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

  const runRetryingSubmit = async (
    fetchMock: ReturnType<typeof vi.fn<typeof fetch>>,
    delays: number[],
  ) => {
    vi.stubGlobal("fetch", fetchMock);
    return Effect.runPromise(
      submitNativeTransferTx(
        "http://127.0.0.1:3000",
        "deadbeef",
        "ab".repeat(32),
        undefined,
        {
          ...FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
          sleep: async (delayMs) => {
            delays.push(delayMs);
          },
        },
      ),
    );
  };

  const durableAdmissionFailureResponse = () =>
    new Response(
      JSON.stringify({ error: "durable transaction admission failed" }),
      { status: 500 },
    );

  const acceptedSubmitResponse = (status: 200 | 202) =>
    new Response(
      JSON.stringify({
        txId: "ab".repeat(32),
        status: "queued",
        duplicate: status === 200,
      }),
      { status },
    );

  const submittedBodyHexes = (
    fetchMock: ReturnType<typeof vi.fn<typeof fetch>>,
  ): readonly string[] =>
    fetchMock.mock.calls.map(([, init]) =>
      decodeMidgardProofSubmission(
        Buffer.from(init?.body as Uint8Array),
      ).transactionCbor.toString("hex"),
    );

  it("retries identical CBOR after a no-row admission 500 and accepts a 202", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockResolvedValueOnce(durableAdmissionFailureResponse())
      .mockResolvedValueOnce(acceptedSubmitResponse(202));

    await expect(runRetryingSubmit(fetchMock, delays)).resolves.toEqual({
      txId: "ab".repeat(32),
      status: "queued",
    });
    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(submittedBodyHexes(fetchMock)).toEqual(["deadbeef", "deadbeef"]);
    expect(delays).toEqual([250]);
  });

  it("retries identical CBOR after a commit-ambiguous 500 and accepts a matching 200 duplicate", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockResolvedValueOnce(durableAdmissionFailureResponse())
      .mockResolvedValueOnce(acceptedSubmitResponse(200));

    await expect(runRetryingSubmit(fetchMock, delays)).resolves.toEqual({
      txId: "ab".repeat(32),
      status: "queued",
    });
    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(submittedBodyHexes(fetchMock)).toEqual(["deadbeef", "deadbeef"]);
    expect(delays).toEqual([250]);
  });

  it("retries a transport failure without rebuilding the transfer", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockRejectedValueOnce(new Error("socket closed"))
      .mockResolvedValueOnce(acceptedSubmitResponse(202));

    await expect(runRetryingSubmit(fetchMock, delays)).resolves.toEqual({
      txId: "ab".repeat(32),
      status: "queued",
    });
    expect(submittedBodyHexes(fetchMock)).toEqual(["deadbeef", "deadbeef"]);
    expect(delays).toEqual([250]);
  });

  it("fails after the bounded admission retry budget is exhausted", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockImplementation(async () => durableAdmissionFailureResponse());

    await expect(runRetryingSubmit(fetchMock, delays)).rejects.toThrow(
      "Midgard node transfer submit failed (500)",
    );
    expect(fetchMock).toHaveBeenCalledTimes(3);
    expect(submittedBodyHexes(fetchMock)).toEqual([
      "deadbeef",
      "deadbeef",
      "deadbeef",
    ]);
    expect(delays).toEqual([250, 500]);
  });

  it.each([400, 409, 500, 503])(
    "does not retry a terminal HTTP %s response without the admission-ambiguity body",
    async (status) => {
      const delays: number[] = [];
      const fetchMock = vi
        .fn<typeof fetch>()
        .mockResolvedValue(
          new Response(JSON.stringify({ error: "terminal" }), { status }),
        );

      await expect(runRetryingSubmit(fetchMock, delays)).rejects.toThrow(
        `Midgard node transfer submit failed (${status.toString()})`,
      );
      expect(fetchMock).toHaveBeenCalledTimes(1);
      expect(delays).toEqual([]);
    },
  );

  it("does not retry an invalid successful response", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockResolvedValue(new Response("not-json", { status: 200 }));

    await expect(runRetryingSubmit(fetchMock, delays)).rejects.toThrow(
      "Midgard node submit response must be valid JSON",
    );
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(delays).toEqual([]);
  });

  it("does not retry a successful response with a mismatched transaction id", async () => {
    const delays: number[] = [];
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockResolvedValue(
        new Response(
          JSON.stringify({ txId: "cd".repeat(32), status: "queued" }),
          { status: 200 },
        ),
      );

    await expect(runRetryingSubmit(fetchMock, delays)).rejects.toThrow(
      "Midgard node returned mismatched txId",
    );
    expect(fetchMock).toHaveBeenCalledTimes(1);
    expect(delays).toEqual([]);
  });
});

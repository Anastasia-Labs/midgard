import "./submit-l2-transfer.submit-l2-transfer-tx-building.js";

import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { type Network, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  DEFAULT_WALLET_SEED_ENV,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY,
  parseSubmitL2TransferConfig,
  prepareL2TransferProgram,
  submitL2TransferProgram,
  submitNativeTransferTx,
} from "../src/commands/submit-l2-transfer.js";
import { NodeConfig } from "../src/services/config.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  launchDeploymentIdentity,
  mkNodeUtxo,
  OTHER_TEST_SEED,
  TEST_SEED,
} from "./submit-l2-transfer.submit-l2-transfer-config-helpers.js";

describe("submit-l2-transfer program", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  // The compiled profile fixes the configured network; the other network is
  // the one with the other address network id.
  const compiledNetwork = SELECTED_DEPLOYMENT_PROFILE.network as Network;
  const otherNetwork: Network =
    compiledNetwork === "Mainnet" ? "Preprod" : "Mainnet";
  const networkId = (network: Network) => (network === "Mainnet" ? 1 : 0);

  const expectDestinationNetworkRefusal = async ({
    nodeNetwork,
    destinationNetwork,
  }: {
    readonly nodeNetwork: Network;
    readonly destinationNetwork: Network;
  }) => {
    const destination = walletFromSeed(OTHER_TEST_SEED, {
      network: destinationNetwork,
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
    const nodeConfig = {
      ...(await Effect.runPromise(
        NodeConfig.pipe(Effect.provide(NodeConfig.layer)),
      )),
      NETWORK: nodeNetwork,
    };
    const fetchMock = vi.fn();
    vi.stubGlobal("fetch", fetchMock);

    await expect(
      Effect.runPromise(
        submitL2TransferProgram({
          config,
          resolvedWalletSeedPhrase,
        }).pipe(
          Effect.provideService(
            ContractDeploymentIdentity,
            launchDeploymentIdentity,
          ),
          Effect.provideService(NodeConfig, nodeConfig),
        ),
      ),
    ).rejects.toThrow(
      `Destination address network id ${networkId(destinationNetwork)} does not match configured Midgard node network ${nodeNetwork} (network id ${networkId(nodeNetwork)}).`,
    );
    expect(fetchMock).not.toHaveBeenCalled();
  };

  it("rejects destination addresses from a different configured node network before fetching UTxOs", async () => {
    await expectDestinationNetworkRefusal({
      nodeNetwork: compiledNetwork,
      destinationNetwork: otherNetwork,
    });
  });

  it("derives the node network id from the configured node network", async () => {
    // Bypasses the config refusal of a network other than the compiled
    // profile's, so a node id that ignores NETWORK cannot pass both cases.
    await expectDestinationNetworkRefusal({
      nodeNetwork: otherNetwork,
      destinationNetwork: compiledNetwork,
    });
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

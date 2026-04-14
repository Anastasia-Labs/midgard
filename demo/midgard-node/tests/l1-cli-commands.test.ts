import { describe, expect, it, vi } from "vitest";
import {
  deriveAddressFromSeedPhrase,
  inferNetworkFromBlockfrostApiUrl,
  parseSeedPhraseArgument,
  resolveBlockfrostApiUrl,
} from "@/commands/address-from-seed.js";
import {
  fetchAllBlockfrostAddressUtxos,
  formatL1UtxosResult,
  parseBlockfrostAddressUtxoPage,
  resolveBlockfrostConfig,
} from "@/commands/l1-utxos.js";

const VALID_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";

describe("l1-utxos command helpers", () => {
  it("resolves blockfrost config from explicit values and trims the URL", () => {
    expect(
      resolveBlockfrostConfig({
        apiUrl: " https://cardano-preprod.blockfrost.io/api/v0/ ",
        apiKey: " key ",
      }),
    ).toEqual({
      apiUrl: "https://cardano-preprod.blockfrost.io/api/v0",
      apiKey: "key",
    });
  });

  it("falls back to environment variables for blockfrost config", () => {
    expect(
      resolveBlockfrostConfig({
        env: {
          L1_BLOCKFROST_API_URL: "https://cardano-preprod.blockfrost.io/api/v0",
          L1_BLOCKFROST_KEY: "env-key",
        },
      }),
    ).toEqual({
      apiUrl: "https://cardano-preprod.blockfrost.io/api/v0",
      apiKey: "env-key",
    });
  });

  it("parses a blockfrost address utxo page into bigint-backed assets", () => {
    expect(
      parseBlockfrostAddressUtxoPage([
        {
          tx_hash: "11".repeat(32),
          output_index: 1,
          amount: [
            { unit: "lovelace", quantity: "1500000" },
            { unit: `${"aa".repeat(28)}${"bb".repeat(2)}`, quantity: "7" },
          ],
          block: "22".repeat(32),
          tx_index: 3,
          data_hash: "33".repeat(32),
          inline_datum: "d87980",
          reference_script_hash: "44".repeat(28),
        },
      ]),
    ).toEqual([
      {
        txHash: "11".repeat(32),
        outputIndex: 1,
        assets: {
          lovelace: 1_500_000n,
          [`${"aa".repeat(28)}${"bb".repeat(2)}`]: 7n,
        },
        block: "22".repeat(32),
        txIndex: 3,
        dataHash: "33".repeat(32),
        inlineDatum: "d87980",
        referenceScriptHash: "44".repeat(28),
      },
    ]);
  });

  it("fetches every blockfrost page and formats the result deterministically", async () => {
    const firstPage = Array.from({ length: 100 }, (_, index) => ({
      tx_hash: (1000 - index).toString(16).padStart(64, "0"),
      output_index: index % 2,
      amount: [{ unit: "lovelace", quantity: "1" }],
    }));
    const fetchMock = vi
      .fn<typeof fetch>()
      .mockResolvedValueOnce({
        ok: true,
        json: async () => firstPage,
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => [
          {
            tx_hash: "00".repeat(31) + "01",
            output_index: 0,
            amount: [{ unit: "lovelace", quantity: "5" }],
          },
        ],
      } as Response)

    const result = await fetchAllBlockfrostAddressUtxos({
      address: VALID_ADDRESS,
      apiUrl: "https://cardano-preprod.blockfrost.io/api/v0",
      apiKey: "test-key",
      fetchImpl: fetchMock,
    });

    expect(fetchMock).toHaveBeenCalledTimes(2);
    expect(fetchMock.mock.calls[0]?.[0]).toContain(
      `/addresses/${VALID_ADDRESS}/utxos?page=1&count=100&order=asc`,
    );
    expect(fetchMock.mock.calls[1]?.[0]).toContain(
      `/addresses/${VALID_ADDRESS}/utxos?page=2&count=100&order=asc`,
    );
    expect(fetchMock.mock.calls[0]?.[1]).toEqual({
      headers: { project_id: "test-key" },
    });
    expect(result.utxoCount).toBe(101);
    expect(result.totals).toEqual({ lovelace: 105n });
    expect(result.utxos[0]).toMatchObject({
      txHash: "00".repeat(31) + "01",
      outputIndex: 0,
      assets: { lovelace: 5n },
    });
    expect(formatL1UtxosResult(result)).toContain('"lovelace": "105"');
  });
});

describe("address-from-seed command helpers", () => {
  it("normalizes whitespace in seed phrases", () => {
    expect(
      parseSeedPhraseArgument(
        "  cupboard   digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart  ",
      ),
    ).toBe(
      "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
    );
  });

  it("resolves the blockfrost API URL from the environment", () => {
    expect(
      resolveBlockfrostApiUrl({
        env: {
          L1_BLOCKFROST_API_URL: "https://cardano-preprod.blockfrost.io/api/v0/",
        },
      }),
    ).toBe("https://cardano-preprod.blockfrost.io/api/v0");
  });

  it("infers the network from the configured blockfrost URL", () => {
    expect(
      inferNetworkFromBlockfrostApiUrl(
        "https://cardano-preprod.blockfrost.io/api/v0",
      ),
    ).toBe("Preprod");
    expect(
      inferNetworkFromBlockfrostApiUrl(
        "https://cardano-preview.blockfrost.io/api/v0",
      ),
    ).toBe("Preview");
    expect(
      inferNetworkFromBlockfrostApiUrl(
        "https://cardano-mainnet.blockfrost.io/api/v0",
      ),
    ).toBe("Mainnet");
  });

  it("derives the expected address for the inferred network", () => {
    expect(
      deriveAddressFromSeedPhrase(
        "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
        inferNetworkFromBlockfrostApiUrl(
          "https://cardano-preprod.blockfrost.io/api/v0",
        ),
      ),
    ).toBe(
      "addr_test1qr3uhfdesx9y2uwc77n6ngsehfq45thtqyfr229g7llqawspaec4cgg268cha44uaf5drt8trds96nwexwpjtx93h3uqlc52rh",
    );
  });
});

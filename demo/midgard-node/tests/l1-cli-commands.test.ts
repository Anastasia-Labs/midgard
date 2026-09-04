import { describe, expect, it, vi } from "vitest";

import {
  deriveAddressFromSeedPhrase,
  parseNetworkArgument,
  parseSeedPhraseArgument,
  resolveNetwork,
} from "../src/commands/address-from-seed.js";
import { formatJson } from "../src/commands/command-utils.js";
import {
  fetchKupmiosAddressUtxos,
  lucidUtxoToL1Utxo,
  resolveKupmiosConfig,
} from "../src/commands/l1-utxos.js";

const VALID_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";

describe("l1-utxos command helpers", () => {
  it("resolves local Kupmios config from explicit values and trims URLs", () => {
    expect(
      resolveKupmiosConfig({
        kupoUrl: " http://127.0.0.1:1442/ ",
        ogmiosUrl: " ws://127.0.0.1:1337/ ",
        network: "Preprod",
      }),
    ).toEqual({
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "ws://127.0.0.1:1337",
      network: "Preprod",
    });
  });

  it("falls back to environment variables for local Kupmios config", () => {
    expect(
      resolveKupmiosConfig({
        env: {
          L1_KUPO_KEY: "http://127.0.0.1:1442",
          L1_OGMIOS_KEY: "http://127.0.0.1:1337",
          NETWORK: "Preprod",
        },
      }),
    ).toEqual({
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "http://127.0.0.1:1337",
      network: "Preprod",
    });
  });

  it("maps Lucid UTxOs into deterministic bigint-backed output", () => {
    const assetUnit = `${"aa".repeat(28)}${"bb".repeat(2)}`;
    const parsed = lucidUtxoToL1Utxo({
      txHash: "11".repeat(32),
      outputIndex: 1,
      assets: {
        lovelace: 1_500_000n,
        [assetUnit]: 7n,
      },
      address: VALID_ADDRESS,
      datumHash: "33".repeat(32),
      datum: "d87980",
    });

    expect(Object.keys(parsed.assets)).toEqual([assetUnit, "lovelace"]);
    expect(parsed).toEqual({
      txHash: "11".repeat(32),
      outputIndex: 1,
      assets: {
        [assetUnit]: 7n,
        lovelace: 1_500_000n,
      },
      block: null,
      txIndex: null,
      dataHash: "33".repeat(32),
      inlineDatum: "d87980",
      referenceScriptHash: null,
    });
  });

  it("fetches local Kupmios UTxOs through a Lucid reader", async () => {
    const lucidFactory = vi.fn().mockResolvedValue({
      utxosAt: vi.fn().mockResolvedValue([
        {
          txHash: "22".repeat(32),
          outputIndex: 1,
          assets: { lovelace: 1n },
          address: VALID_ADDRESS,
        },
        {
          txHash: "00".repeat(31) + "01",
          outputIndex: 0,
          assets: { lovelace: 5n },
          address: VALID_ADDRESS,
        },
      ]),
    });

    const result = await fetchKupmiosAddressUtxos({
      address: VALID_ADDRESS,
      kupoUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "http://127.0.0.1:1337",
      network: "Preprod",
      lucidFactory,
    });

    expect(lucidFactory).toHaveBeenCalledTimes(1);
    expect(result.utxoCount).toBe(2);
    expect(result.totals).toEqual({ lovelace: 6n });
    expect(result.utxos[0]).toMatchObject({
      txHash: "00".repeat(31) + "01",
      outputIndex: 0,
      assets: { lovelace: 5n },
    });
    expect(formatJson(result)).toContain('"lovelace": "6"');
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

  it("resolves and validates the explicit network", () => {
    expect(resolveNetwork({ network: "Preprod" })).toBe("Preprod");
    expect(resolveNetwork({ env: { NETWORK: "Preview" } })).toBe("Preview");
    expect(() => parseNetworkArgument("Custom")).toThrow(/Unsupported network/);
  });

  it("derives the expected address for the resolved network", () => {
    expect(
      deriveAddressFromSeedPhrase(
        "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
        resolveNetwork({ network: "Preprod" }),
      ),
    ).toBe(
      "addr_test1qr3uhfdesx9y2uwc77n6ngsehfq45thtqyfr229g7llqawspaec4cgg268cha44uaf5drt8trds96nwexwpjtx93h3uqlc52rh",
    );
  });
});

import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  credentialToAddress,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  reserveFundingRejection,
  selectReserveFundingInput,
} from "../src/reserve-payout/reserve-inputs.js";

const COINS_PER_UTXO_BYTE = 4_310n;
const RESERVE_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: "ab".repeat(28),
});
const TOKEN = `${"cd".repeat(28)}01`;

const reserveUtxo = (
  txHashByte: string,
  assets: Assets,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex: 0,
  address: RESERVE_ADDRESS,
  assets,
  ...extra,
});

const minChangeLovelace = (assets: Assets): bigint =>
  calculateMinLovelaceFromUTxO(COINS_PER_UTXO_BYTE, reserveUtxo("00", assets));

describe("reserve funding input selection", () => {
  const needed: Assets = { lovelace: 4_000_000n };

  it.each([
    ["an inline datum", { datum: "d87980" }, "carries an inline datum"],
    ["a datum hash", { datumHash: "ef".repeat(32) }, "carries a datum hash"],
    [
      "a reference script",
      { scriptRef: { type: "PlutusV3", script: "5900" } },
      "carries a reference script",
    ],
  ] as const)(
    "refuses a reserve UTxO with %s, which the validators never spend",
    (_label, extra, reason) => {
      const planted = reserveUtxo("11", { lovelace: 20_000_000n }, extra);
      expect(
        reserveFundingRejection(planted, needed, COINS_PER_UTXO_BYTE),
      ).toBe(reason);
      expect(
        selectReserveFundingInput([planted], needed, COINS_PER_UTXO_BYTE),
      ).toBeUndefined();
    },
  );

  it("refuses a reserve UTxO that holds none of the still-needed units", () => {
    expect(
      reserveFundingRejection(
        reserveUtxo("11", { lovelace: 5_000_000n, [TOKEN]: 3n }),
        { [TOKEN]: 0n, [`${"cd".repeat(28)}02`]: 1n },
        COINS_PER_UTXO_BYTE,
      ),
    ).toBe("contributes no still-needed payout asset");
  });

  it("refuses reserve change below the minimum UTxO lovelace, at the exact boundary", () => {
    const minimum = minChangeLovelace({ lovelace: 1_000_000n });
    const at = reserveUtxo("11", { lovelace: 4_000_000n + minimum });
    const below = reserveUtxo("22", { lovelace: 4_000_000n + minimum - 1n });
    expect(reserveFundingRejection(at, needed, COINS_PER_UTXO_BYTE)).toBe(
      undefined,
    );
    expect(reserveFundingRejection(below, needed, COINS_PER_UTXO_BYTE)).toBe(
      "leaves reserve change below the minimum UTxO lovelace",
    );
    // Taking all lovelace leaves token-only change, which no output can hold.
    expect(
      reserveFundingRejection(
        reserveUtxo("33", { lovelace: 3_000_000n, [TOKEN]: 1n }),
        needed,
        COINS_PER_UTXO_BYTE,
      ),
    ).toBe("leaves reserve change below the minimum UTxO lovelace");
    // Exhausting the UTxO entirely leaves no change output at all.
    expect(
      reserveFundingRejection(
        reserveUtxo("44", { lovelace: 3_000_000n }),
        needed,
        COINS_PER_UTXO_BYTE,
      ),
    ).toBe(undefined);
  });

  it("skips a larger planted datum UTxO and prefers the largest fundable contribution", () => {
    const planted = reserveUtxo(
      "00",
      { lovelace: 50_000_000n },
      { datum: "d87980" },
    );
    const small = reserveUtxo("11", { lovelace: 2_000_000n });
    const large = reserveUtxo("22", { lovelace: 9_000_000n });
    const tie = reserveUtxo("33", { lovelace: 9_000_000n });
    expect(
      selectReserveFundingInput(
        [planted, small, tie, large],
        needed,
        COINS_PER_UTXO_BYTE,
      ),
    ).toBe(large);
  });
});

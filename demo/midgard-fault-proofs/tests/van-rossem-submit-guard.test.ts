import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertFitsVanRossem,
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
  VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
  VanRossemFitError,
} from "../src/proof-fit/van-rossem.js";
import { network } from "./support/emulator/blueprints.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

/**
 * A signed publication: a plain payment carrying an inline datum of
 * `chunks` 64-byte bytestrings, so it runs no script.
 */
const signedPublication = async (chunks: number) => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const datum = Data.to(
    Array.from({ length: chunks }, (_, index) =>
      index.toString(16).padStart(128, "0"),
    ),
  );
  const unsigned = await lucid
    .newTx()
    .pay.ToAddressWithData(
      account.address,
      { kind: "inline", value: datum },
      { lovelace: 100_000_000n },
    )
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  return { emulator, signedBytes: signed.toCBOR().length / 2, signed };
};

describe("every emulator submission is judged against Van Rossem", () => {
  it("accepts a publication under the publication target", async () => {
    const { emulator, signedBytes, signed } = await signedPublication(234);
    expect(signedBytes).toBeGreaterThan(
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES - 1_000,
    );
    expect(signedBytes).toBeLessThanOrEqual(
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
    );
    await expect(emulator.submitTx(signed.toCBOR())).resolves.toBe(
      signed.toHash(),
    );
  });

  it("refuses a publication inflated past the publication target, naming its margin", async () => {
    const { emulator, signedBytes, signed } = await signedPublication(238);
    // Over the target but under the transaction maximum, so only the
    // publication target can refuse it.
    expect(signedBytes).toBeGreaterThan(VAN_ROSSEM_PUBLICATION_TARGET_BYTES);
    expect(signedBytes).toBeLessThanOrEqual(VAN_ROSSEM_MAX_SIGNED_TX_BYTES);
    const margin = VAN_ROSSEM_PUBLICATION_TARGET_BYTES - signedBytes;
    const refusal = emulator.submitTx(signed.toCBOR());
    await expect(refusal).rejects.toThrow(
      `exceeds the 15872-byte publication target (a transaction with zero execution units is a publication): publication target bytes margin ${margin.toString()}`,
    );
    await expect(refusal).rejects.toMatchObject({
      axis: "publication target bytes",
      margin: BigInt(margin),
    });
    await expect(
      emulator.getUtxosByOutRef([{ txHash: signed.toHash(), outputIndex: 0 }]),
    ).resolves.toEqual([]);
  });
});

describe("assertFitsVanRossem", () => {
  const scriptCost = { memoryUnits: 1n, cpuUnits: 1n };

  it("holds a transaction that runs a script only to the maximum", () => {
    expect(() =>
      assertFitsVanRossem({
        signedBytes: VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
        memoryUnits: VAN_ROSSEM_MAX_MEMORY_UNITS,
        cpuUnits: VAN_ROSSEM_MAX_CPU_UNITS,
      }),
    ).not.toThrow();
  });

  it.each([
    [
      "signed bytes",
      { ...scriptCost, signedBytes: VAN_ROSSEM_MAX_SIGNED_TX_BYTES + 3 },
      -3n,
    ],
    [
      "memory units",
      {
        ...scriptCost,
        signedBytes: 1,
        memoryUnits: VAN_ROSSEM_MAX_MEMORY_UNITS + 5n,
      },
      -5n,
    ],
    [
      "cpu units",
      {
        ...scriptCost,
        signedBytes: 1,
        cpuUnits: VAN_ROSSEM_MAX_CPU_UNITS + 7n,
      },
      -7n,
    ],
    [
      "publication target bytes",
      {
        signedBytes: VAN_ROSSEM_PUBLICATION_TARGET_BYTES + 1,
        memoryUnits: 0n,
        cpuUnits: 0n,
      },
      -1n,
    ],
  ] as const)(
    "refuses a transaction over the %s limit",
    (axis, cost, margin) => {
      let refusal: unknown;
      try {
        assertFitsVanRossem(cost, "tx");
      } catch (cause) {
        refusal = cause;
      }
      expect(refusal).toBeInstanceOf(VanRossemFitError);
      expect(refusal).toMatchObject({ axis, margin });
    },
  );
});

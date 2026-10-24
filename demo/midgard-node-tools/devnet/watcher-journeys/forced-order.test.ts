import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { publishJourneyForcedOrder } from "./forced-order.js";
import type { JourneyFaultPreparationInput } from "./staging.js";

it.each([false, true])(
  "checkpoints and resumes a forced order with published mint reference=%s",
  async (publishedReference) => {
    const directory = await mkdtemp(join(tmpdir(), "forced-order-"));
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const reference: UTxO = {
      txHash: "11".repeat(32),
      outputIndex: 1,
      address: account.address,
      assets: { lovelace: 2_000_000n },
      scriptRef: { type: "PlutusV3", script: "49480100002221200101" },
    };
    const tx = await lucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 3_000_000n })
      .complete({ localUPLCEval: true });
    const orderId = { transactionId: "22".repeat(32), outputIndex: 0n };
    const build = vi
      .spyOn(SDK, "buildUnsignedTxOrderTxWithMetadataProgram")
      .mockReturnValue(
        Effect.succeed({
          tx,
          metadata: {
            txOrderId: orderId,
            txOrderAddress: account.address,
            authNonceCbor: "80",
            txOrderAuthUnit: "33".repeat(28),
            nonceInput: { txHash: "22".repeat(32), outputIndex: 0 },
            validTo: 60_000,
            inclusionTime: 1,
          },
        }),
      );
    const submit = vi.fn(async (bytes: string) => {
      const recorded = JSON.parse(
        await readFile(join(directory, "forced-order.json"), "utf8"),
      );
      expect(recorded.signedCbor).toBe(bytes);
      expect(recorded.confirmed).toBe(false);
      return emulator.submitTx(bytes);
    });
    const input = {
      directory,
      onStage: vi.fn(),
      readSignedCommitRecovery: async (attempt: {
        txHash: string;
        signedCbor: string;
      }) => ({
        transactionHash: attempt.txHash,
        signedTransactionCborHex: attempt.signedCbor,
        status: submit.mock.calls.length === 0 ? "rebroadcast" : "included",
        reason: "isolated forced-order publication fixture",
      }),
      predecessor: { header: { endTime: 0n } },
      context: {
        customNetwork: { slotConfig: { slotLength: 1000 } },
        provider: {
          getUtxosWithUnit: async () => [],
          submitTx: submit,
          awaitTx: (hash: string) => emulator.awaitTx(hash),
        },
        deployment: {
          manifest: { manifestId: "44".repeat(32) },
          operatorLucid: lucid,
          contracts: { txOrder: { spendingScriptAddress: account.address } },
          references: new Map(
            publishedReference ? [["txOrderMint", reference]] : [],
          ),
          chain: {
            now: () => 1,
            delaySlots: async () => undefined,
            awaitLedgerTime: async () => undefined,
          },
        },
      },
    } as unknown as JourneyFaultPreparationInput;
    try {
      const submitted = Buffer.from("80", "hex");
      await expect(
        publishJourneyForcedOrder(input, submitted),
      ).resolves.toEqual(orderId);
      expect(build.mock.calls[0]![2].referenceScripts).toEqual(
        publishedReference ? { txOrderMinting: reference } : undefined,
      );
      await expect(
        publishJourneyForcedOrder(input, submitted),
      ).resolves.toEqual(orderId);
      expect(build).toHaveBeenCalledTimes(1);
      expect(submit).toHaveBeenCalledTimes(1);
    } finally {
      build.mockRestore();
      await rm(directory, { recursive: true, force: true });
    }
  },
);

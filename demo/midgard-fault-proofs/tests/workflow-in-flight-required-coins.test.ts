/** The wallet coins an expired wallet-only transaction spent stay held for
 * its own action's rebuild: a transaction of another action of the
 * objective, recorded in between, neither releases them nor takes them, and
 * the restricted wallet offers them to no build but that rebuild, so the
 * rebuild spends the same coins and a rollback that brings the expired one
 * back lands one of the two, never both. */
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type TxSigned,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  createWorkflowInFlight,
  type InFlightTransaction,
  WalletCoinHeldError,
} from "../src/workflow/in-flight.js";
import { workflowTransactionInputOutRefs } from "../src/workflow/transaction-boundary.js";
import {
  restrictWorkflowWallet,
  withRequiredWalletInputs,
} from "../src/workflow/workflow-wallet.js";

const coin = (index: number) => `${"c7".repeat(32)}#${index.toString()}`;
const sent = (
  actionId: string,
  txHash: string,
  inputs: readonly string[],
): InFlightTransaction => ({
  actionId,
  txHash: txHash.repeat(64 / txHash.length),
  signedTransactionCborHex: "",
  inputs,
  walletInputs: inputs,
  walletOnly: true,
  validToSlot: 100,
  broadcastAtMs: 0,
});

describe("workflow in-flight set: an expired predecessor's coins", () => {
  it("stay required for its action's rebuild while another action of the objective is recorded", () => {
    const inFlight = createWorkflowInFlight();
    const sends = inFlight.forObjective({
      category: "doubleSpend",
      headerHash: "a1".repeat(32),
    });
    const other = inFlight.forObjective({
      category: "doubleSpend",
      headerHash: "b1".repeat(32),
    });
    const expired = sent("init", "a0", [coin(0)]);
    sends.record(expired);
    sends.settle(expired.txHash);
    sends.require("init", [coin(0)]);

    // Another action of the objective, over another coin, is recorded and
    // leaves A's coins required and held.
    sends.record(sent("bind", "b0", [coin(1)]));
    expect(sends.requiredWalletInputs("init")).toEqual([coin(0)]);
    expect(sends.requiredWalletInputs("bind")).toEqual([]);
    expect(inFlight.heldOutRefs()).toEqual(
      expect.arrayContaining([coin(0), coin(1)]),
    );
    // Nor may it, or another objective, take them.
    expect(() => sends.record(sent("bind", "b1", [coin(0)]))).toThrow(
      WalletCoinHeldError,
    );
    expect(sends.requiredWalletInputs("init")).toEqual([coin(0)]);
    expect(other.isHeld(coin(0))).toBe(true);
    expect(() => other.record(sent("init", "c0", [coin(0)]))).toThrow(
      WalletCoinHeldError,
    );

    // A's rebuild spends the same coins, and only then are they no longer
    // required.
    const rebuilt = sent("init", "a2", [coin(0), coin(2)]);
    sends.record(rebuilt);
    expect(sends.requiredWalletInputs("init")).toEqual([]);
    expect(sends.inFlight().map(({ txHash }) => txHash)).toContain(
      rebuilt.txHash,
    );
    sends.settle(rebuilt.txHash);
    expect(inFlight.heldOutRefs()).toEqual([coin(1)]);
  });

  it("are offered by the restricted wallet to that rebuild alone: another action's build selects another coin and records", async () => {
    const prover = generateEmulatorAccount({});
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: prover.seedPhrase,
    });
    // c0, the larger, is the coin selection would take first.
    const emulator = new Emulator(
      [40_000_000n, 30_000_000n].map((lovelace) => ({
        ...prover,
        address: signer.address,
        assets: { lovelace },
      })),
    );
    const [c0, c1] = [0, 1].map(
      (index) => `${"00".repeat(32)}#${index.toString()}`,
    ) as [string, string];
    const inFlight = createWorkflowInFlight();
    const sends = inFlight.forObjective({
      category: "doubleSpend",
      headerHash: "a1".repeat(32),
    });
    const lucid = await Lucid(emulator, "Custom");
    restrictWorkflowWallet({ signer, sends }).selectWallet(lucid);
    const build = async () =>
      await (
        await lucid
          .newTx()
          .pay.ToAddress(signer.address, { lovelace: 5_000_000n })
          .complete()
      ).sign
        .withWallet()
        .complete();
    const recorded = (actionId: string, signed: TxSigned) => {
      const inputs = workflowTransactionInputOutRefs(signed);
      sends.record({
        ...sent(actionId, "00", inputs),
        txHash: signed.toHash(),
      });
      return inputs;
    };
    const expired = sent("init", "a0", [c0]);
    sends.record(expired);
    sends.settle(expired.txHash);
    sends.require("init", [c0]);

    const bind = await withRequiredWalletInputs(
      sends,
      sends.requiredWalletInputs("bind"),
      build,
    );
    expect(recorded("bind", bind)).toEqual([c1]);
    expect(sends.requiredWalletInputs("init")).toEqual([c0]);

    const init = await withRequiredWalletInputs(
      sends,
      sends.requiredWalletInputs("init"),
      build,
    );
    expect(recorded("init", init)).toEqual([c0]);
    expect(sends.requiredWalletInputs("init")).toEqual([]);
  });
});

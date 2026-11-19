/**
 * How the confirmation wait of a submitted transaction ends besides
 * confirmation: phase-2 invalid, expired (its upper bound lies the
 * confirmation depth under the tip with every input unspent), or settled
 * past the bound with an input spent by another transaction. Lucid and its
 * provider are faked; the node's wait is exercised as is. The emulator's
 * tip is its slot, so one poll a slot observes one new tip a slot.
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Emulator } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.await-submitted-transaction-confirmation.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

/** The slot at which a bound at slot 3 lies the confirmation depth deep. */
const SETTLED_SLOT = 3 + DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth - 1;

describe("submitted transaction confirmation outcomes", () => {
  it("fails at once on a transaction that landed phase-2 invalid", async () => {
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: undefined }),
            transactionStatus: async (txHash: string) => ({
              status: "failed" as const,
              txHash,
              reason: "phase-2 script failure",
            }),
            wallet: () => ({}),
          } as never,
          { txHash: "tx-phase-two-invalid", signedTxCbor: "00" },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxConfirmError",
      txHash: "tx-phase-two-invalid",
    });
    expect(String(result.left.cause)).toContain(
      "Transaction tx-phase-two-invalid failed",
    );
  });

  it("reports a bounded transaction expired once its bound lies the confirmation depth under the tip with every input unspent", async () => {
    const emulator = new Emulator([]);
    const inputReadSlots: number[] = [];
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: emulator }),
            currentSlot: () => emulator.slot,
            awaitTx: async () => true,
            transactionStatus: async (txHash: string) => ({
              status: "not_found" as const,
              txHash,
            }),
            utxosByOutRef: async (refs: readonly unknown[]) => {
              inputReadSlots.push(emulator.slot);
              return refs;
            },
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-expired",
            signedTxCbor: signedTxCbor({ invalidHereafterSlot: 3 }),
          },
          {
            confirmationTimeoutMs: 10_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 1_000,
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxExpiredError",
      txHash: "tx-expired",
      invalidHereafterSlot: 3,
      tipSlot: SETTLED_SLOT,
    });
    expect(inputReadSlots).toEqual([SETTLED_SLOT]);
  });

  // Past the bound, an input that is spent means the transaction landed or
  // another one spent it: it is never reported expired (and never
  // rebuilt). Unconfirmed once the bound is settled, it failed to land.
  it("does not report a bounded transaction expired while one of its inputs is spent", async () => {
    const emulator = new Emulator([]);
    const utxosByOutRef = vi.fn(async () => []);
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: emulator }),
            currentSlot: () => emulator.slot,
            awaitTx: async () => true,
            transactionStatus: async (txHash: string) => ({
              status: "not_found" as const,
              txHash,
            }),
            utxosByOutRef,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-input-spent",
            signedTxCbor: signedTxCbor({ invalidHereafterSlot: 3 }),
          },
          {
            confirmationTimeoutMs: 10_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 1_000,
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") throw new Error("expected a failure");
    expect(result.left._tag).toBe("TxConfirmError");
    expect(String(result.left.cause)).toContain(
      "another transaction spent one of its inputs",
    );
    expect(emulator.slot).toBe(SETTLED_SLOT);
    expect(utxosByOutRef).toHaveBeenCalledTimes(1);
  });
});

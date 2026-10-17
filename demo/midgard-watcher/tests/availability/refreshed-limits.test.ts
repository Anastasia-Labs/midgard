import * as SDK from "@al-ft/midgard-sdk";
import * as Lucid from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { watcherAvailabilityExecutionLimits } from "../../src/availability/runtime.execution-limits.js";
import { utxo } from "../support/availability-challenge-fixture.js";
import {
  ADA,
  io,
  observation,
  OPENING,
  PARAMETERS,
  runtime,
  TIMEOUT_COLLATERAL,
  withheld,
} from "./concurrent-challenges.fixture.js";

afterEach(() => vi.restoreAllMocks());

it.each([
  [8620n, 4310n],
  [4310n, 8620n],
])(
  "captures winning rebuild limits when min-ADA changes %s -> %s",
  async (oldCost, newCost) => {
    const actualSdk = await vi.importActual<typeof SDK>("@al-ft/midgard-sdk");
    const deriveLimits = actualSdk.daAvailabilityOperationLimits;
    const actualLucid = await vi.importActual<typeof Lucid>(
      "@lucid-evolution/lucid",
    );
    const protocol = await new actualLucid.Emulator([]).getProtocolParameters();
    const oldProtocol = { ...protocol, coinsPerUtxoByte: oldCost };
    const newProtocol = {
      ...protocol,
      coinsPerUtxoByte: newCost,
      maxTxSize: protocol.maxTxSize - 1,
      maxTxExMem: protocol.maxTxExMem - 1n,
      maxTxExSteps: protocol.maxTxExSteps - 1n,
    };
    const address = actualLucid.CML.EnterpriseAddress.new(
      0,
      actualLucid.CML.Credential.new_pub_key(
        actualLucid.CML.Ed25519KeyHash.from_hex("aa".repeat(28)),
      ),
    )
      .to_address()
      .to_bech32();
    io.walletAddress = address;
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "f1"),
      utxo(1, OPENING, "f2"),
      utxo(2, 100n * ADA, "f3"),
    ];
    const initialized = new WeakSet<Lucid.LucidEvolution>();
    io.limits = (lucid, parameters) => {
      if (!initialized.has(lucid)) {
        const config = lucid.config();
        Object.assign(lucid, {
          config: () => ({ ...config, protocolParameters: oldProtocol }),
        });
        initialized.add(lucid);
      }
      return deriveLimits(lucid, parameters);
    };
    let builds = 0;
    io.openBuild = () =>
      ++builds === 1
        ? Effect.fail(
            new SDK.DaAvailabilityTransactionError("parameter epoch changed"),
          )
        : Effect.succeed({ tx: "rebuilt-open" } as never);
    let checked = false;
    io.run.mockImplementation(async (context, operation) => {
      expect(operation.action).toBe("open");
      const base = io.lucidInstances[0] as Lucid.LucidEvolution;
      const attempt = io.lucidInstances[1] as Lucid.LucidEvolution;
      const before = attempt.config();
      Object.assign(attempt, {
        switchProvider: async () => {
          Object.assign(attempt, {
            config: () => ({ ...before, protocolParameters: newProtocol }),
          });
        },
      });
      await operation.build();
      io.minimumAda = actualLucid.calculateMinLovelaceFromUTxO;
      expect(builds).toBe(2);
      const expected = deriveLimits(attempt, PARAMETERS);
      expect(context.transactionLimits).toEqual(expected);
      expect(base.config().protocolParameters).toEqual(oldProtocol);
      const minAda = (cost: bigint) =>
        actualLucid.calculateMinLovelaceFromUTxO(cost, {
          address,
          assets: { lovelace: 2_000_000n },
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      const intent = (lovelace: bigint) => {
        const outputs = actualLucid.CML.TransactionOutputList.new();
        outputs.add(
          actualLucid.CML.TransactionOutput.new(
            actualLucid.CML.Address.from_bech32(address),
            actualLucid.CML.Value.from_coin(lovelace),
          ),
        );
        const body = actualLucid.CML.TransactionBody.new(
          actualLucid.CML.TransactionInputList.new(),
          outputs,
          1n,
        );
        return {
          action: "prepare" as const,
          txHash: actualLucid.CML.hash_transaction(body).to_hex(),
          signedCbor: actualLucid.CML.Transaction.new(
            body,
            actualLucid.CML.TransactionWitnessSet.new(),
            true,
          ).to_cbor_hex(),
        };
      };
      const fresh = intent(minAda(newCost));
      const oldLimits = deriveLimits(base, PARAMETERS);
      expect(() =>
        actualSdk.assertDaAvailabilitySignedLimits(
          fresh,
          context.transactionLimits,
        ),
      ).not.toThrow();
      if (newCost < oldCost)
        expect(() =>
          actualSdk.assertDaAvailabilitySignedLimits(fresh, oldLimits),
        ).toThrow("minimum ADA");
      else {
        const stale = intent(minAda(oldCost));
        expect(() =>
          actualSdk.assertDaAvailabilitySignedLimits(stale, oldLimits),
        ).not.toThrow();
        expect(() =>
          actualSdk.assertDaAvailabilitySignedLimits(
            stale,
            context.transactionLimits,
          ),
        ).toThrow("minimum ADA");
      }
      // Signed validation keeps the winning snapshot even if a retired instance
      // later changes; it performs no mutable provider read after construction.
      Object.assign(attempt, { config: () => before });
      expect(context.transactionLimits).toEqual(expected);
      checked = true;
      return { status: "submitted", txHash: "checked", expectedOutRefs: [] };
    });
    const header = withheld("6d");
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(checked, JSON.stringify(watcher.status())).toBe(true);
      expect(watcher.status().detail).toBeUndefined();
    } finally {
      await watcher.close();
    }
  },
);

it("refuses to replace captured limits after scope or generation revocation", () => {
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
  const original = {
    coinsPerUtxoByte: 1n,
  } as SDK.DaAvailabilityOperationLimits;
  const context = {
    transactionLimits: original,
  } as SDK.DaAvailabilityOperationContext;
  let current = true;
  const execution = watcherAvailabilityExecutionLimits(
    context,
    PARAMETERS,
    () => {
      if (!current) throw new Error("generation revoked");
    },
  );
  const lucid = {
    config: vi.fn(() => ({
      protocolParameters: {
        maxTxSize: 10_000,
        maxTxExMem: 10_000n,
        maxTxExSteps: 10_000n,
        coinsPerUtxoByte: 2n,
      },
    })),
  } as unknown as Lucid.LucidEvolution;
  current = false;
  expect(() => execution.capture(lucid, scope)).toThrow("generation revoked");
  current = true;
  scope.close();
  expect(() => execution.capture(lucid, scope)).toThrow(
    "Availability read scope closed",
  );
  expect(lucid.config).not.toHaveBeenCalled();
  expect(execution.context.transactionLimits).toBe(original);
});

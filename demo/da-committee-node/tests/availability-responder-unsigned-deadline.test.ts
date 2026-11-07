import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  type DaAvailabilityDeployment,
  type DaAvailabilityOperationContext,
  planDaAvailabilityPublications,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import {
  CML,
  credentialToAddress,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderTransactionOperation } from "../src/availability/factory.discover-availability-responder-challenges.js";
import { challengeFixture, payload } from "./helpers/availability-challenge.js";

const dirs: string[] = [];
const journals: ReturnType<typeof openAvailabilityOperationJournal>[] = [];
beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
  vi.useRealTimers();
});

/** Real signed CML bytes and SQLite reservations; only the wallet's async
 * signing boundary and provider observations are controlled. */
const scene = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-deadline-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(
    join(dir, "operations.sqlite"),
  );
  journals.push(journal);
  const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
  const actor = key.to_public().hash().to_hex();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("ab".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(
        credentialToAddress("Preprod", { type: "Key", hash: actor }),
      ),
      CML.Value.from_coin(5_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 100_000n);
  body.set_validity_interval_start(0n);
  body.set_ttl(1000n);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), key));
  witnesses.set_vkeywitnesses(vkeys);
  const signed = CML.Transaction.new(body, witnesses, true);
  const sign = vi.fn(async () => ({ toCBOR: () => signed.to_cbor_hex() }));
  const tx = {
    toTransaction: () => signed,
    sign: { withWallet: () => ({ complete: sign }) },
  } as unknown as TxSignBuilder;
  const context: DaAvailabilityOperationContext = {
    actor,
    deploymentIdentity: "11".repeat(32),
    stateQueuePolicyId: "22".repeat(28),
    journal,
    minimumConfirmationDepth: 10,
    transactionLimits: {
      maxTxSize: 16_384,
      maxTxExMem: 14_000_000n,
      maxTxExSteps: 10_000_000_000n,
      coinsPerUtxoByte: 4310n,
      feeCeilings: { prepare: 500_000n },
    },
    assertActuationCurrent: vi.fn(async () => {}),
    observe: vi.fn(async () => ({
      status: "unspent" as const,
      currentSlot: 0,
    })),
    submit: vi.fn(async () => CML.hash_transaction(body).to_hex()),
    nowMs: Date.now,
  };
  const build = vi.fn(async (_signal: AbortSignal) => tx);
  const operation = {
    action: "prepare" as const,
    headerHash: "33".repeat(28),
    unsignedDeadlineMs: 1100,
    build,
  };
  return { context, journal, tx, sign, build, operation };
};

describe("absolute unsigned availability deadlines", () => {
  it("uses the authenticated publication deadline without applying it to completion", () => {
    const { challenge } = challengeFixture();
    const publication = planDaAvailabilityPublications({
      commitment: challenge.record.datum.commitment,
      challengeAssetName: challenge.record.datum.challenge_asset_name,
      payload,
    })[0]!.publications[0]!;
    // This checks operation metadata before the provider/build is invoked.
    const lucid = {} as LucidEvolution;
    const deployment = {} as DaAvailabilityDeployment;
    const publicationOperation = availabilityResponderTransactionOperation(
      lucid,
      deployment,
      {
        kind: "publish",
        challenge,
        tranche: challenge.tranches[0]!,
        publication,
      },
    );
    expect(publicationOperation.unsignedDeadlineMs).toBe(
      Number(challenge.record.datum.response_deadline),
    );
    for (const action of [
      { kind: "settle" as const, challenge, tranche: challenge.tranches[0]! },
      { kind: "close" as const, challenge },
    ])
      expect(
        availabilityResponderTransactionOperation(lucid, deployment, action)
          .unsignedDeadlineMs,
      ).toBeUndefined();
  });

  it("forwards cancellation through the production adapter before provider reads", async () => {
    const { challenge } = challengeFixture();
    const controller = new AbortController();
    controller.abort(new Error("operation cancelled"));
    const operation = availabilityResponderTransactionOperation(
      {} as LucidEvolution,
      {} as DaAvailabilityDeployment,
      { kind: "close", challenge },
    );
    await expect(operation.build(controller.signal)).rejects.toThrow(
      "operation cancelled",
    );
  });

  it("refuses fresh work at the deadline without signing or persisting", async () => {
    const s = scene();
    await expect(
      runDaAvailabilityOperation(s.context, {
        ...s.operation,
        unsignedDeadlineMs: 1000,
      }),
    ).rejects.toThrow(/deadline 1000 reached/);
    expect(s.build).not.toHaveBeenCalled();
    expect(s.sign).not.toHaveBeenCalled();
    expect(
      s.journal.pending(s.context.deploymentIdentity, s.context.actor),
    ).toEqual([]);
  });

  it("cancels a stalled build and discards its late result", async () => {
    const s = scene();
    let resolveBuild!: (tx: TxSignBuilder) => void;
    let signal: AbortSignal | undefined;
    const run = runDaAvailabilityOperation(s.context, {
      ...s.operation,
      build: (value) => {
        signal = value;
        return new Promise((resolve) => {
          resolveBuild = resolve;
        });
      },
    });
    const rejected = expect(run).rejects.toThrow(/deadline 1100 reached/);
    await vi.advanceTimersByTimeAsync(100);
    await rejected;
    expect(signal?.aborted).toBe(true);
    resolveBuild(s.tx);
    await vi.advanceTimersByTimeAsync(0);
    expect(s.sign).not.toHaveBeenCalled();
    expect(s.context.submit).not.toHaveBeenCalled();
    expect(
      s.journal.pending(s.context.deploymentIdentity, s.context.actor),
    ).toEqual([]);
  });

  it("keeps the same deadline across build and the final source assertion", async () => {
    const s = scene();
    let sourceChecks = 0;
    const context = {
      ...s.context,
      assertActuationCurrent: async () => {
        sourceChecks += 1;
        if (sourceChecks === 2) vi.setSystemTime(1100);
      },
    };
    const build = async () => {
      vi.setSystemTime(1090);
      return s.tx;
    };
    await expect(
      runDaAvailabilityOperation(context, { ...s.operation, build }),
    ).rejects.toThrow(/deadline 1100 reached/);
    expect(sourceChecks).toBe(2);
    expect(s.sign).not.toHaveBeenCalled();
  });

  it("signs in time and preserves an ambiguous signed intent past the unsigned deadline", async () => {
    const s = scene();
    const first = await runDaAvailabilityOperation(s.context, s.operation);
    expect(first.status).toBe("submitted");
    const before = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    );
    const observe = vi.fn(async () => ({
      status: "unknown" as const,
      reason: "source unavailable",
    }));
    vi.setSystemTime(1200);
    const result = await runDaAvailabilityOperation(
      { ...s.context, observe },
      s.operation,
    );
    expect(result.status).toBe("waiting");
    expect(result.txHash).toBe(first.txHash);
    expect(
      s.journal.pending(s.context.deploymentIdentity, s.context.actor),
    ).toEqual(before);
    expect(s.build).toHaveBeenCalledTimes(1);
    expect(s.sign).toHaveBeenCalledTimes(1);
    expect(s.context.submit).toHaveBeenCalledTimes(1);
  });

  it("rechecks the absolute deadline immediately before signing after body inspection", async () => {
    const s = scene();
    const tx = {
      ...s.tx,
      toTransaction: () => {
        vi.setSystemTime(1100);
        return s.tx.toTransaction();
      },
    } as TxSignBuilder;
    await expect(
      runDaAvailabilityOperation(s.context, {
        ...s.operation,
        build: async () => tx,
      }),
    ).rejects.toThrow(/deadline 1100 reached/);
    expect(s.sign).not.toHaveBeenCalled();
    expect(s.context.submit).not.toHaveBeenCalled();
  });

  it("leaves operations with no protocol upper bound outside this deadline gate", async () => {
    const s = scene();
    vi.setSystemTime(1200);
    expect(
      (
        await runDaAvailabilityOperation(s.context, {
          ...s.operation,
          unsignedDeadlineMs: undefined,
        })
      ).status,
    ).toBe("submitted");
    expect(s.sign).toHaveBeenCalledTimes(1);
  });
});

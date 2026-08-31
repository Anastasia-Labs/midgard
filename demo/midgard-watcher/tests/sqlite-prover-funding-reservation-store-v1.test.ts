import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import type { WatcherProductionProverFundingReservationPlanV1 } from "../src/production-prover-funding-reservation-v1.js";
import {
  isWatcherProductionProverFundingReservationConflictV1,
  unsafeOpenWatcherSqliteProverFundingReservationStoreForTestV1,
} from "../src/sqlite-prover-funding-reservation-store-v1.js";

const temporaryDirectories: string[] = [];
const fundingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x55));
const fundingKeyHash = fundingKey.to_public().hash().to_hex();
const walletAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), Buffer.from(fundingKeyHash, "hex")]),
).to_bech32();

const signedTransition = ({
  inputHash = "11".repeat(32),
  outputLovelace = 99_000_000n,
}: {
  readonly inputHash?: string;
  readonly outputLovelace?: bigint;
} = {}) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(inputHash), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(outputLovelace),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  const transactionHash = CML.hash_transaction(body).to_hex();
  return Object.freeze({
    signedTransactionCborHex: transaction.to_canonical_cbor_hex(),
    transactionHash,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_canonical_cbor_hex(), "hex"))
      .digest("hex"),
    producedInputs: Object.freeze([
      Object.freeze({
        outRef: `${transactionHash}#0`,
        role: "funding" as const,
        lovelace: outputLovelace.toString(),
        assets: Object.freeze([]),
      }),
    ]),
  });
};

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const openStore = async () => {
  const directory = await mkdtemp(
    join(process.cwd(), ".watcher-funding-reservation-test-"),
  );
  temporaryDirectories.push(directory);
  const path = join(directory, "watcher.sqlite");
  return {
    path,
    runtime:
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTestV1(
        { path },
        () => undefined,
      ),
  };
};

const plan = (
  reservationByte: string,
  decisionByte: string,
  fundingOutRef = `${"11".repeat(32)}#0`,
): WatcherProductionProverFundingReservationPlanV1 =>
  Object.freeze({
    schemaVersion:
      "midgard-watcher-production-prover-funding-reservation-plan-v1",
    deploymentFingerprint: "22".repeat(32),
    decisionDigest: decisionByte.repeat(32),
    profileDigest: "33".repeat(32),
    calculationDigest: "44".repeat(32),
    fundingPaymentKeyHash: fundingKeyHash,
    walletAddress,
    inputs: Object.freeze([
      Object.freeze({
        outRef: fundingOutRef,
        role: "funding" as const,
        lovelace: "100000000",
        assets: Object.freeze([]),
      }),
      Object.freeze({
        outRef: `${"12".repeat(32)}#0`,
        role: "collateral" as const,
        lovelace: "5000000",
        assets: Object.freeze([]),
      }),
    ]),
    fundingLovelace: "100000000",
    collateralLovelace: "5000000",
    assets: Object.freeze([]),
    reservationId: reservationByte.repeat(32),
  });

describe("SQLite prover funding reservation store V1", () => {
  it("atomically persists descendant rotation and restart conflict state", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    expect(await opened.runtime.store.reserve(currentPlan)).toBe("reserved");
    expect(await opened.runtime.store.reserve(currentPlan)).toBe("unchanged");

    const signed = signedTransition();
    const prepared = await opened.runtime.store.prepareTransition({
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.step-01",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    });
    expect(prepared).toMatchObject({
      revision: "1",
      state: "active",
      pendingTransition: {
        transactionHash: signed.transactionHash,
      },
    });
    await expect(
      opened.runtime.store.readConfirmedActionOutput({
        reservationId: currentPlan.reservationId,
        sourceActionKind: "proof.step-01",
        sourceOutputIndex: 0,
      }),
    ).rejects.toThrow("is missing");
    await expect(
      opened.runtime.store.release({
        plan: currentPlan,
        expectedRevision: "1",
      }),
    ).rejects.toThrow("release mismatch");

    opened.runtime.close();
    const restarted =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTestV1(
        { path: opened.path },
        () => undefined,
      );
    expect(await restarted.store.readAll()).toEqual([prepared]);

    await expect(
      restarted.store.confirmTransition({
        plan: currentPlan,
        expectedRevision: "0",
        transitionDigest: prepared.pendingTransition!.transitionDigest,
      }),
    ).rejects.toThrow("confirmation mismatch");
    const confirmed = await restarted.store.confirmTransition({
      plan: currentPlan,
      expectedRevision: "1",
      transitionDigest: prepared.pendingTransition!.transitionDigest,
    });
    expect(confirmed).toMatchObject({
      revision: "2",
      pendingTransition: null,
      activeInputs: [
        { outRef: `${"12".repeat(32)}#0`, role: "collateral" },
        { outRef: `${signed.transactionHash}#0`, role: "funding" },
      ],
    });
    await expect(
      restarted.store.readConfirmedActionOutput({
        reservationId: currentPlan.reservationId,
        sourceActionKind: "proof.step-01",
        sourceOutputIndex: 0,
      }),
    ).resolves.toEqual({
      sourceActionKind: "proof.step-01",
      sourceOutputIndex: 0,
      outRef: `${signed.transactionHash}#0`,
      resolvedOutputCborHex: CML.Transaction.from_cbor_hex(
        signed.signedTransactionCborHex,
      )
        .body()
        .outputs()
        .get(0)
        .to_canonical_cbor_hex(),
    });
    const secondSigned = signedTransition({
      inputHash: signed.transactionHash,
      outputLovelace: 98_000_000n,
    });
    const secondPrepared = await restarted.store.prepareTransition({
      plan: currentPlan,
      expectedRevision: "2",
      actionKind: "proof.step-02",
      ...secondSigned,
      consumedOutRefs: [`${signed.transactionHash}#0`],
    });
    const secondConfirmed = await restarted.store.confirmTransition({
      plan: currentPlan,
      expectedRevision: "3",
      transitionDigest: secondPrepared.pendingTransition!.transitionDigest,
    });
    expect(secondConfirmed).toMatchObject({
      revision: "4",
      activeInputs: [
        { outRef: `${"12".repeat(32)}#0`, role: "collateral" },
        { outRef: `${secondSigned.transactionHash}#0`, role: "funding" },
      ],
    });

    let collision: unknown;
    try {
      await restarted.store.reserve(plan("bb", "99", `${"12".repeat(32)}#0`));
    } catch (error) {
      collision = error;
    }
    expect(
      isWatcherProductionProverFundingReservationConflictV1(collision),
    ).toBe(true);
    if (isWatcherProductionProverFundingReservationConflictV1(collision)) {
      expect(collision.conflict).toEqual({
        code: "reservation_collision",
        outRef: `${"12".repeat(32)}#0`,
      });
    }
    expect(
      isWatcherProductionProverFundingReservationConflictV1(
        Object.assign(new Error("lookalike"), {
          conflict: {
            code: "reservation_collision",
            outRef: `${"12".repeat(32)}#0`,
          },
        }),
      ),
    ).toBe(false);

    const conflicted = await restarted.store.markConflict({
      plan: currentPlan,
      expectedRevision: "4",
      code: "unexpected_spend",
    });
    expect(conflicted).toMatchObject({
      revision: "5",
      state: "conflict",
      conflictCode: "unexpected_spend",
    });
    restarted.close();

    const secondRestart =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTestV1(
        { path: opened.path },
        () => undefined,
      );
    expect(await secondRestart.store.readAll()).toEqual([conflicted]);
    await expect(
      secondRestart.store.readConfirmedActionOutput({
        reservationId: currentPlan.reservationId,
        sourceActionKind: "proof.step-01",
        sourceOutputIndex: 0,
      }),
    ).resolves.toMatchObject({ outRef: `${signed.transactionHash}#0` });
    secondRestart.close();
  });

  it("rejects unreserved consumption and substituted produced out-refs", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);

    await expect(
      opened.runtime.store.prepareTransition({
        plan: currentPlan,
        expectedRevision: "0",
        actionKind: "proof.step-01",
        ...signedTransition({ inputHash: "13".repeat(32) }),
        consumedOutRefs: [`${"13".repeat(32)}#0`],
      }),
    ).rejects.toThrow("unreserved input");
    const signed = signedTransition();
    await expect(
      opened.runtime.store.prepareTransition({
        plan: currentPlan,
        expectedRevision: "0",
        actionKind: "proof.step-01",
        signedTransactionCborHex: signed.signedTransactionCborHex,
        transactionHash: signed.transactionHash,
        transactionBodySha256: signed.transactionBodySha256,
        consumedOutRefs: [`${"11".repeat(32)}#0`],
        producedInputs: [
          {
            outRef: `${"78".repeat(32)}#0`,
            role: "funding",
            lovelace: "99000000",
            assets: [],
          },
        ],
      }),
    ).rejects.toThrow("differs from the signed transaction");
    expect(await opened.runtime.store.readAll()).toMatchObject([
      { revision: "0", pendingTransition: null },
    ]);
    opened.runtime.close();
  });
});

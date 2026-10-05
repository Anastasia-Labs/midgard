import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { beforeAll, expect, it, vi } from "vitest";

import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot.js";
import {
  reconcileSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "../src/workflow/signed-transaction-reconciliation.js";

type RecoveryInput = Parameters<typeof reconcileSignedWorkflowTransaction>[0];
let signed: SignedWorkflowTransaction;
beforeAll(async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const lucid = await Lucid(new Emulator([account]), "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const transaction = await (
    await lucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  signed = {
    transactionHash: transaction.toHash(),
    signedTransactionCborHex: transaction.toCBOR(),
  };
});

const observation = (
  status: SignedTransactionRecoveryObservation["status"],
): SignedTransactionRecoveryObservation => {
  const point = {
    slot: "1000",
    blockNo: "50",
    blockHash: "ab".repeat(32),
    pointId: "cd".repeat(32),
  };
  return {
    ...signed,
    status,
    canonicalPoint: {
      ...point,
      slot: "10000",
      blockHash: "ef".repeat(32),
      blockNo: "2211",
      pointId: computeFraudProofRawL1PointId({
        ...point,
        slot: "10000",
        blockHash: "ef".repeat(32),
        blockNo: "2211",
      }),
    },
    releaseFinalPoint: {
      ...point,
      pointId: computeFraudProofRawL1PointId(point),
    },
    inputs: [],
    reason: "Controlled canonical recovery observation",
  };
};

it("retains an authorized rejected intent until canonical recovery resolves its reference conflict", async () => {
  const authorizeResubmission = vi.fn(async () => {});
  const rebroadcast = vi.fn<NonNullable<RecoveryInput["rebroadcast"]>>(
    async ({ authorizeResubmission, ...transaction }) => {
      await authorizeResubmission(transaction);
      throw new Error(
        "Ogmios 3117: competing DA apply spent the reference input",
      );
    },
  );
  const observe = vi
    .fn<NonNullable<RecoveryInput["observe"]>>()
    .mockResolvedValueOnce(observation("rebroadcast"))
    .mockResolvedValueOnce(observation("pending"))
    .mockResolvedValueOnce(observation("invalidated"));
  const input = { ...signed, observe, rebroadcast, authorizeResubmission };
  const pending = { kind: "pending", txHash: signed.transactionHash };

  await expect(reconcileSignedWorkflowTransaction(input)).resolves.toEqual(
    pending,
  );
  await expect(reconcileSignedWorkflowTransaction(input)).resolves.toEqual(
    pending,
  );
  await expect(
    reconcileSignedWorkflowTransaction(input),
  ).resolves.toMatchObject({
    kind: "not_found",
    retirement: {
      reason: "invalidated",
      transactionHash: signed.transactionHash,
    },
  });
  expect(authorizeResubmission).toHaveBeenCalledExactlyOnceWith(signed);
  expect(rebroadcast).toHaveBeenCalledTimes(1);
  expect(observe.mock.calls).toEqual([[signed], [signed], [signed]]);
});

it("does not treat rejected authorization as an authorized submission", async () => {
  const submitted = vi.fn();
  const result = await reconcileSignedWorkflowTransaction({
    ...signed,
    observe: async () => observation("rebroadcast"),
    authorizeResubmission: async () => {
      throw new Error("Actuation permit revoked");
    },
    rebroadcast: async ({ authorizeResubmission, ...transaction }) => {
      await authorizeResubmission(transaction);
      submitted(transaction);
      return transaction.transactionHash;
    },
  });
  expect(result).toMatchObject({ kind: "unknown" });
  expect(submitted).not.toHaveBeenCalled();
});

it("rejects a substituted acknowledgement even after authorization", async () => {
  await expect(
    reconcileSignedWorkflowTransaction({
      ...signed,
      observe: async () => observation("rebroadcast"),
      authorizeResubmission: async () => {},
      rebroadcast: async ({ authorizeResubmission, ...transaction }) => {
        await authorizeResubmission(transaction);
        return "ff".repeat(32);
      },
    }),
  ).rejects.toThrow("Rebroadcast changed recorded transaction hash");
});

for (const status of ["expired", "invalidated"] as const) {
  it(`supersedes ${status} signed attempts inside the recovery horizon and retires them beyond it`, async () => {
    const shallow = {
      ...observation(status),
      canonicalPoint: {
        ...observation(status).canonicalPoint,
        blockNo: "2210",
      },
    };
    // Owner ruling (whichever lands wins): absent and impossible is final for
    // the workflow at once; only the retirement receipt waits for k.
    expect(
      await reconcileSignedWorkflowTransaction({
        ...signed,
        observe: async () => shallow,
      }),
    ).toEqual({ kind: "not_found" });
    expect(
      await reconcileSignedWorkflowTransaction({
        ...signed,
        observe: async () => observation(status),
      }),
    ).toMatchObject({
      kind: "not_found",
      retirement: { transactionHash: signed.transactionHash, reason: status },
    });
    // A rollback that restores input/validity state must preserve the same intent.
    expect(
      await reconcileSignedWorkflowTransaction({
        ...signed,
        observe: async () => observation("pending"),
      }),
    ).toEqual({ kind: "pending", txHash: signed.transactionHash });
  });
}

for (const status of ["expired_at_tip", "invalidated_at_tip"] as const)
  it(`supersedes an attempt ${status.replace("_", " ").replace("_", " ")} without a retirement receipt`, async () => {
    expect(
      await reconcileSignedWorkflowTransaction({
        ...signed,
        observe: async () => observation(status),
      }),
    ).toEqual({ kind: "not_found" });
  });

it("reports a superseded attempt's shallow inclusion as its result only when asked, and retires a deep one", async () => {
  const included = (blockNo: string) => ({
    ...observation("included"),
    inclusionPoint: {
      ...observation("included").releaseFinalPoint,
      blockNo,
      pointId: computeFraudProofRawL1PointId({
        ...observation("included").releaseFinalPoint,
        blockNo,
      }),
    },
  });
  const shallow = async () => included("2000");
  await expect(
    reconcileSignedWorkflowTransaction({
      ...signed,
      observe: shallow,
      reportInclusion: true,
    }),
  ).resolves.toEqual({ kind: "confirmed", txHash: signed.transactionHash });
  await expect(
    reconcileSignedWorkflowTransaction({ ...signed, observe: shallow }),
  ).resolves.toEqual({ kind: "pending", txHash: signed.transactionHash });
  await expect(
    reconcileSignedWorkflowTransaction({
      ...signed,
      observe: async () => included("50"),
      reportInclusion: true,
    }),
  ).resolves.toMatchObject({
    kind: "pending",
    retirement: { reason: "included" },
  });
});

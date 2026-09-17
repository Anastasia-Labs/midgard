import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { beforeAll, expect, it, vi } from "vitest";

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
    canonicalPoint: point,
    releaseFinalPoint: point,
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
  await expect(reconcileSignedWorkflowTransaction(input)).resolves.toEqual({
    kind: "not_found",
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

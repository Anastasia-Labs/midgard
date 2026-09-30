import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  prepareHubOracleOneShotNonceProgram,
  reconcileHubOracleOneShotNonceAttemptProgram,
} from "../src/commands/prepare-hub-oracle-nonce.js";
import { SignedNonceConflictError } from "../src/commands/prepare-hub-oracle-nonce.resume-signed.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { BeforeSignedTransactionSubmission } from "../src/transactions/utils.js";

const signSubmitTransactionMock = vi.hoisted(() => vi.fn());
vi.mock("../src/transactions/utils.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/transactions/utils.js")>()),
  signSubmitTransaction: signSubmitTransactionMock,
  awaitSubmittedTransactionConfirmation: () => Effect.succeed("confirmed"),
}));

const ADDRESS = "addr_test1operatornonce";
const INPUT = { txHash: "11".repeat(32), outputIndex: 3 };

const signedNonceTx = () => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(INPUT.txHash),
      BigInt(INPUT.outputIndex),
    ),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedTxCbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
  };
};

/** Chain view: each status read pops the next status; inputs are live or not. */
const fakeLucid = ({
  statuses,
  inputsLive,
  inlineDatum = "d87980",
  txHash = "",
}: {
  readonly statuses: string[];
  readonly inputsLive: boolean;
  readonly inlineDatum?: string;
  readonly txHash?: string;
}) => {
  const submitTx = vi.fn(async (cbor: string) => cbor);
  const lucid = {
    config: () => ({ provider: { submitTx } }),
    transactionStatus: vi.fn(async (hash: string) => ({
      status: statuses.length > 1 ? statuses.shift()! : statuses[0]!,
      txHash: hash,
      confirmation: { txHash: hash },
    })),
    utxosByOutRef: vi.fn(async () => (inputsLive ? [INPUT as UTxO] : [])),
    awaitTxConfirmation: vi.fn(async (hash: string) => ({ txHash: hash })),
    utxosAt: vi.fn(async () => [
      {
        txHash,
        outputIndex: 0,
        assets: { lovelace: 5_000_000n },
        datum: inlineDatum,
      },
    ]),
  };
  const service = {
    api: lucid as unknown as LucidEvolution,
    switchToOperatorsMainWallet: Effect.void,
  };
  return { lucid, submitTx, service };
};

const reconcileProgram = (
  service: unknown,
  signed: { txHash: string; signedTxCbor: string },
) =>
  reconcileHubOracleOneShotNonceAttemptProgram(
    { ...signed, address: ADDRESS, lovelace: "5000000", inlineDatum: "d87980" },
    { outputLookupTimeoutMs: 0 },
  ).pipe(Effect.provideService(LucidService, service as never));
const reconcile = (...args: Parameters<typeof reconcileProgram>) =>
  Effect.runPromise(reconcileProgram(...args));

describe("hub-oracle nonce signed before submission", () => {
  beforeEach(() => signSubmitTransactionMock.mockReset());

  it("records the signed transaction before submitting, and a failed record blocks submission", async () => {
    const order: string[] = [];
    signSubmitTransactionMock.mockImplementation(() =>
      Effect.gen(function* () {
        const intent = yield* Effect.serviceOption(
          BeforeSignedTransactionSubmission,
        );
        if (Option.isSome(intent))
          yield* intent.value.persist({ txHash: "aa", signedTxCbor: "84a0" });
        order.push("submit");
        return { txHash: "aa", signedTxCbor: "84a0", walletAddress: ADDRESS };
      }),
    );
    const tx = {
      pay: { ToAddressWithData: () => tx },
      complete: async () => ({}),
    };
    const service = {
      api: {
        newTx: () => tx,
        wallet: () => ({ address: async () => ADDRESS }),
        awaitTxConfirmation: async (hash: string) => ({ txHash: hash }),
        utxosAt: async () => [],
      } as unknown as LucidEvolution,
      switchToOperatorsMainWallet: Effect.void,
    };
    const run = (beforeSubmission: () => Effect.Effect<void, unknown>) =>
      Effect.runPromise(
        prepareHubOracleOneShotNonceProgram(5_000_000n, {
          outputLookupTimeoutMs: 0,
          beforeSubmission: (attempt) => {
            order.push(`record ${attempt.txHash} ${attempt.signedTxCbor}`);
            return beforeSubmission();
          },
        }).pipe(Effect.provideService(LucidService, service as never)),
      );

    await expect(run(() => Effect.void)).rejects.toThrow(
      "Expected exactly one marked nonce output",
    );
    expect(order).toEqual(["record aa 84a0", "submit"]);
    order.length = 0;
    await expect(
      run(() => Effect.fail(new Error("disk full"))),
    ).rejects.toThrow("disk full");
    expect(order).toEqual(["record aa 84a0"]);
  });

  it("completes a recorded transaction that landed without resubmitting it", async () => {
    const signed = signedNonceTx();
    const chain = fakeLucid({
      statuses: ["confirmed"],
      inputsLive: false,
      txHash: signed.txHash,
    });
    await expect(reconcile(chain.service, signed)).resolves.toMatchObject({
      outRef: `${signed.txHash}#0`,
    });
    expect(chain.submitTx).not.toHaveBeenCalled();
  });

  it("resubmits exactly the recorded bytes while its inputs are unspent", async () => {
    const signed = signedNonceTx();
    const chain = fakeLucid({
      statuses: ["not_found"],
      inputsLive: true,
      txHash: signed.txHash,
    });
    chain.submitTx.mockRejectedValueOnce(new Error("already in mempool"));
    await expect(reconcile(chain.service, signed)).resolves.toMatchObject({
      outRef: `${signed.txHash}#0`,
    });
    expect(chain.lucid.utxosByOutRef).toHaveBeenCalledWith([INPUT]);
    expect(chain.submitTx).toHaveBeenCalledExactlyOnceWith(signed.signedTxCbor);
  });

  it("fails closed when another transaction spent the recorded inputs", async () => {
    const signed = signedNonceTx();
    const chain = fakeLucid({ statuses: ["not_found"], inputsLive: false });
    const error = await Effect.runPromise(
      Effect.flip(reconcileProgram(chain.service, signed)),
    );
    expect(error).toBeInstanceOf(SignedNonceConflictError);
    expect(String(error)).toContain(
      `another transaction spent its inputs ${INPUT.txHash}#3`,
    );
    expect(String(error)).toContain(signed.txHash);
    expect(chain.submitTx).not.toHaveBeenCalled();
    expect(chain.lucid.awaitTxConfirmation).not.toHaveBeenCalled();
  });

  it("treats inputs spent by the recorded transaction itself as landed", async () => {
    const signed = signedNonceTx();
    const chain = fakeLucid({
      statuses: ["pending", "confirmed"],
      inputsLive: false,
      txHash: signed.txHash,
    });
    await expect(reconcile(chain.service, signed)).resolves.toMatchObject({
      outRef: `${signed.txHash}#0`,
    });
    expect(chain.submitTx).not.toHaveBeenCalled();
  });
});

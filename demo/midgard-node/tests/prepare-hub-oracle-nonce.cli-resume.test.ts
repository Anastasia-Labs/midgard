/**
 * The registered `prepare-hub-oracle-one-shot-nonce` command across a
 * failed broadcast and a resume. The first run reads the local node's tip,
 * records it as the L1 origin with the signed nonce, and its broadcast
 * fails; the resume reconciles the signed nonce and prints the recorded
 * origin as `L1_ORIGIN`, never reading the local node again, though the
 * node's tip has moved on. Only the wallet, the broadcast and the local
 * node's origin source are stubbed; the command, its run state and its
 * output are real.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";
import { CML } from "@lucid-evolution/lucid";
import { Effect, type Exit, Option } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

const seen = vi.hoisted(() => ({
  lucid: undefined as unknown,
  originReads: 0,
  /** The nonce output's datum the chain holds after the broadcast. */
  datum: undefined as string | undefined,
  run: undefined as Promise<Exit.Exit<unknown, unknown>> | undefined,
}));

const TIP: L1Origin = { slot: 9_000, blockHash: "a1".repeat(32) };
const LATER_TIP: L1Origin = { slot: 9_020, blockHash: "b2".repeat(32) };
const ADDRESS = "addr_test1operatornonce";
const INPUT = { txHash: "11".repeat(32), outputIndex: 3 };

/** A signed nonce transaction whose bytes a resume can resubmit. */
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
const SIGNED = signedNonceTx();

vi.mock("../src/runtime-env.js", () => ({ loadRuntimeDotenv: () => {} }));
// The broadcast persists the signed bytes ahead of it, as the node's does,
// then fails.
vi.mock("../src/transactions/utils.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/transactions/utils.js")>();
  return {
    ...original,
    signSubmitTransaction: () =>
      Effect.gen(function* () {
        const intent = yield* Effect.serviceOption(
          original.BeforeSignedTransactionSubmission,
        );
        if (Option.isSome(intent))
          yield* intent.value.persist({ ...SIGNED, journal: Effect.void });
        return yield* Effect.fail(new Error("submission refused"));
      }),
    awaitSubmittedTransactionConfirmation: () => Effect.succeed("confirmed"),
  };
});
// The local node: its tip is TIP on the first read and LATER_TIP after.
vi.mock(
  "../src/commands/prepare-hub-oracle-nonce.l1-origin.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/commands/prepare-hub-oracle-nonce.l1-origin.js")
      >();
    return {
      ...original,
      nodeHubOracleNonceOriginSource: () =>
        Effect.sync(() => {
          const tip = seen.originReads === 0 ? TIP : LATER_TIP;
          seen.originReads += 1;
          return {
            observer: {
              tip: async () => tip,
              settle: async () => "settled" as const,
            },
            depth: 10,
            settleBoundMs: 60_000,
          };
        }),
    };
  },
);
vi.mock("../src/commands/cli-runtime.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/commands/cli-runtime.js")>();
  const { Effect: E } = await import("effect");
  const Services = await import("../src/services/index.js");
  const { IntentJournalWithoutFollower } = await import(
    "../src/services/intent-journal.js"
  );
  return {
    ...original,
    provideLucidOnlyServices: (
      effect: Effect.Effect<unknown, unknown, never>,
    ) =>
      effect.pipe(
        E.provide(IntentJournalWithoutFollower),
        E.provideService(Services.NodeConfig, {
          NETWORK: "Preprod",
        } as never),
        E.provideService(Services.Lucid, seen.lucid as never),
      ),
    runCliEffect: (effect: Effect.Effect<unknown, unknown>) => {
      seen.run = E.runPromiseExit(effect);
    },
  };
});

import "../src/index.registration-6.js";

import { program } from "../src/index.registration.js";

/** The operator's wallet; after the broadcast, the chain holds the nonce. */
const wallet = (broadcast: boolean) => {
  const tx = {
    pay: { ToAddressWithData: () => tx },
    complete: async () => ({}),
  };
  return {
    api: {
      newTx: () => tx,
      wallet: () => ({ address: async () => ADDRESS }),
      config: () => ({ provider: { submitTx: async (cbor: string) => cbor } }),
      transactionStatus: async (hash: string) => ({
        status: "confirmed",
        txHash: hash,
        confirmation: { txHash: hash },
      }),
      utxosByOutRef: async () => [],
      awaitTxConfirmation: async (hash: string) => ({ txHash: hash }),
      utxosAt: async () =>
        broadcast
          ? [
              {
                txHash: SIGNED.txHash,
                outputIndex: 0,
                assets: { lovelace: 5_000_000n },
                datum: seen.datum,
              },
            ]
          : [],
    },
    switchToOperatorsMainWallet: Effect.void,
  };
};

/** Runs the registered command; returns its exit and what it printed. */
const command = async (args: readonly string[]) => {
  const printed: string[] = [];
  const write = vi
    .spyOn(process.stdout, "write")
    .mockImplementation((chunk: string | Uint8Array) => {
      printed.push(String(chunk));
      return true;
    });
  try {
    seen.run = undefined;
    await program.parseAsync([
      "node",
      "midgard-node",
      "prepare-hub-oracle-one-shot-nonce",
      ...args,
    ]);
    const exit = await seen.run!;
    return { exit, printed: printed.join("") };
  } finally {
    write.mockRestore();
  }
};

describe("prepare-hub-oracle-one-shot-nonce resumed after a failed broadcast", () => {
  it("D-g: prints the origin recorded before the nonce was signed as L1_ORIGIN, never reading the local node again", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-nonce-cli-"));
    onTestFinished(() => rm(directory, { recursive: true, force: true }));
    const runState = ["--run-state", join(directory, "run-state.json")];

    seen.lucid = wallet(false);
    const first = await command(runState);
    expect(first.exit._tag).toBe("Failure");
    expect(seen.originReads).toBe(1);

    const { loadPendingHubOracleNonceAttempt } = await import(
      "../src/commands/deployment-run-state.js"
    );
    const pending = await loadPendingHubOracleNonceAttempt({
      options: { runStatePath: runState[1]!, freshRedeploy: false },
    });
    expect(pending).toMatchObject({ txHash: SIGNED.txHash, l1Origin: TIP });
    seen.datum = pending!.inlineDatum;

    seen.lucid = wallet(true);
    const resumed = await command(runState);
    expect(resumed.exit._tag).toBe("Success");
    const lines = resumed.printed.split("\n");
    expect(lines).toContain(`HUB_ORACLE_ONE_SHOT_TX_HASH=${SIGNED.txHash}`);
    expect(lines).toContain(`L1_ORIGIN=${formatL1Origin(TIP)}`);
    expect(seen.originReads).toBe(1);
  });
});

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import { CML, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { beforeEach, describe, expect, it, onTestFinished, vi } from "vitest";

import {
  loadPendingHubOracleNonceAttempt,
  recordHubOracleNonce,
} from "../src/commands/deployment-run-state.js";
import { hubOracleNonceCliOutput } from "../src/commands/prepare-hub-oracle-nonce.cli-output.js";
import {
  type PrepareHubOracleNonceOptions,
  prepareHubOracleOneShotNonceProgram,
  reconcileHubOracleOneShotNonceAttemptProgram,
} from "../src/commands/prepare-hub-oracle-nonce.js";
import {
  type L1ChainObserver,
  nodeHubOracleNonceOriginSource,
  ORIGIN_SETTLE_ATTEMPTS,
} from "../src/commands/prepare-hub-oracle-nonce.l1-origin.js";
import { hubOracleNonceRunStateHooks } from "../src/commands/prepare-hub-oracle-nonce.run-state-hooks.js";
import { loadDeploymentRunState } from "../src/e2e/run-state.js";
import { Lucid as LucidService } from "../src/services/lucid.js";
import { BeforeSignedTransactionSubmission } from "../src/transactions/utils.js";
import { settledOriginSource } from "./helpers/hub-oracle-nonce-origin.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";

const signSubmitTransactionMock = vi.hoisted(() => vi.fn());
vi.mock("../src/transactions/utils.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/transactions/utils.js")>()),
  signSubmitTransaction: signSubmitTransactionMock,
  awaitSubmittedTransactionConfirmation: () => Effect.succeed("confirmed"),
}));

const ADDRESS = "addr_test1operatornonce";
const INPUT = { txHash: "11".repeat(32), outputIndex: 3 };
const TIP: L1Origin = { slot: 9_000, blockHash: "a1".repeat(32) };
const LATER_TIP: L1Origin = { slot: 9_020, blockHash: "b2".repeat(32) };

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

const runStateFile = async () => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-nonce-origin-"));
  onTestFinished(() => rm(directory, { recursive: true, force: true }));
  return join(directory, "run-state.json");
};

/** A chain observer over scripted tips and settle answers, journaling calls. */
const scriptedObserver = (
  journal: string[],
  tips: readonly L1Origin[],
  settles: readonly ("settled" | "rolled_back")[],
): L1ChainObserver => {
  let tipReads = 0;
  let settleCalls = 0;
  return {
    tip: async () => {
      const tip = tips[Math.min(tipReads, tips.length - 1)]!;
      tipReads += 1;
      journal.push(`tip ${tip.slot.toString()}`);
      return tip;
    },
    settle: async (point, depth) => {
      const answer = settles[Math.min(settleCalls, settles.length - 1)]!;
      settleCalls += 1;
      journal.push(`settle ${point.slot.toString()} ${depth.toString()}`);
      return answer;
    },
  };
};

/**
 * The node's submission: runs the write-ahead, journals what run state holds
 * at that moment, then fails as a broadcast the node refused would.
 */
const failingSubmission = (
  journal: string[],
  runStatePath: string,
  signed = signedNonceTx(),
) =>
  signSubmitTransactionMock.mockImplementation(() =>
    Effect.gen(function* () {
      const intent = yield* Effect.serviceOption(
        BeforeSignedTransactionSubmission,
      );
      if (Option.isSome(intent))
        yield* intent.value.persist({ ...signed, journal: Effect.void });
      const state = yield* Effect.promise(() =>
        loadDeploymentRunState(runStatePath),
      );
      journal.push(
        `submit with recorded origin ${state?.identity.l1Origin?.slot.toString() ?? "none"}`,
      );
      return yield* Effect.fail(new Error("submission refused"));
    }),
  );

const operator = () => {
  const journal: string[] = [];
  const tx = {
    pay: { ToAddressWithData: () => tx },
    complete: async () => ({}),
  };
  const service = {
    api: {
      newTx: () => tx,
      wallet: () => ({ address: async () => ADDRESS }),
      utxosAt: async () => [],
    } as unknown as LucidEvolution,
    switchToOperatorsMainWallet: Effect.sync(() => {
      journal.push("wallet");
    }),
  };
  return { journal, service };
};

const prepare = (
  service: unknown,
  runStatePath: string,
  options: PrepareHubOracleNonceOptions & { freshRedeploy?: boolean },
) =>
  runWithoutFollower(
    prepareHubOracleOneShotNonceProgram(5_000_000n, {
      ...hubOracleNonceRunStateHooks(
        {
          runStatePath,
          freshRedeploy: options.freshRedeploy ?? false,
          ...(options.freshRedeploy === true
            ? { freshRedeployReason: "replacing the test deployment" }
            : {}),
        },
        "Preprod",
      ),
      outputLookupTimeoutMs: 0,
      ...options,
    }).pipe(Effect.provideService(LucidService, service as never)),
  );

describe("the L1 origin the nonce command records", () => {
  beforeEach(() => signSubmitTransactionMock.mockReset());

  it("D-a: records the settled tip with the signed bytes before any broadcast, so a failed broadcast leaves it in run state", async () => {
    const runStatePath = await runStateFile();
    const { journal, service } = operator();
    failingSubmission(journal, runStatePath);
    await expect(
      prepare(service, runStatePath, {
        l1Origin: {
          observer: scriptedObserver(journal, [TIP], ["settled"]),
          depth: 30,
        },
      }),
    ).rejects.toThrow("submission refused");
    expect(journal).toEqual([
      "wallet",
      "tip 9000",
      "settle 9000 30",
      "submit with recorded origin 9000",
    ]);
    const state = await loadDeploymentRunState(runStatePath);
    expect(state?.identity.l1Origin).toEqual(TIP);
    expect(state?.steps.hubOracleNonceSigned?.status).toBe("submitted");
  });

  it("D-b: a resume keeps the recorded origin, never reading the node again", async () => {
    const runStatePath = await runStateFile();
    const signed = signedNonceTx();
    const first = operator();
    failingSubmission(first.journal, runStatePath, signed);
    await expect(
      prepare(first.service, runStatePath, {
        l1Origin: settledOriginSource(TIP),
      }),
    ).rejects.toThrow("submission refused");

    const options = { runStatePath, freshRedeploy: false };
    const pending = await loadPendingHubOracleNonceAttempt({ options });
    expect(pending).toMatchObject({ txHash: signed.txHash, l1Origin: TIP });
    const lucid = {
      config: () => ({ provider: { submitTx: async (cbor: string) => cbor } }),
      transactionStatus: async (hash: string) => ({
        status: "confirmed",
        txHash: hash,
        confirmation: { txHash: hash },
      }),
      utxosByOutRef: async () => [] as UTxO[],
      awaitTxConfirmation: async (hash: string) => ({ txHash: hash }),
      utxosAt: async () => [
        {
          txHash: signed.txHash,
          outputIndex: 0,
          assets: { lovelace: 5_000_000n },
          datum: pending!.inlineDatum,
        },
      ],
    };
    const result = await Effect.runPromise(
      reconcileHubOracleOneShotNonceAttemptProgram(pending!, {
        outputLookupTimeoutMs: 0,
      }).pipe(
        Effect.provideService(LucidService, {
          api: lucid as unknown as LucidEvolution,
          switchToOperatorsMainWallet: Effect.void,
        } as never),
      ),
    );
    expect(result.l1Origin).toEqual(TIP);
    await recordHubOracleNonce({
      options,
      network: "Preprod",
      txHash: result.txHash,
      outputIndex: result.outputIndex,
      outRef: result.outRef,
    });
    const state = await loadDeploymentRunState(runStatePath);
    expect(state?.identity.l1Origin).toEqual(TIP);
    expect(state?.steps.hubOracleNonce?.status).toBe("complete");
  });

  it("D-c: the command prints the recorded origin as L1_ORIGIN, in text and in JSON", () => {
    const result = {
      txHash: "cd".repeat(32),
      outputIndex: 0,
      outRef: `${"cd".repeat(32)}#0`,
      address: ADDRESS,
      lovelace: "5000000",
      inlineDatum: "d87980",
      confirmationStatus: "confirmed" as const,
      l1Origin: TIP,
    };
    const text = hubOracleNonceCliOutput(result, {
      json: false,
      verb: "prepared",
    });
    expect(text.split("\n")).toContain(`L1_ORIGIN=9000.${"a1".repeat(32)}`);
    expect(text).toContain(`HUB_ORACLE_ONE_SHOT_TX_HASH=${"cd".repeat(32)}`);
    expect(
      JSON.parse(
        hubOracleNonceCliOutput(result, { json: true, verb: "prepared" }),
      ),
    ).toMatchObject({ l1Origin: TIP });
    const { l1Origin: _recorded, ...unrecorded } = result;
    expect(
      hubOracleNonceCliOutput(unrecorded, { json: false, verb: "reconciled" }),
    ).not.toContain("L1_ORIGIN=");
  });

  it("D-d: a fresh redeploy records the new tip in place of the old origin", async () => {
    const runStatePath = await runStateFile();
    const first = operator();
    failingSubmission(first.journal, runStatePath);
    await expect(
      prepare(first.service, runStatePath, {
        l1Origin: settledOriginSource(TIP),
      }),
    ).rejects.toThrow("submission refused");
    const second = operator();
    failingSubmission(second.journal, runStatePath);
    await expect(
      prepare(second.service, runStatePath, {
        freshRedeploy: true,
        l1Origin: settledOriginSource(LATER_TIP),
      }),
    ).rejects.toThrow("submission refused");
    expect(second.journal).toContain("submit with recorded origin 9020");
    const state = await loadDeploymentRunState(runStatePath);
    expect(state?.identity.l1Origin).toEqual(LATER_TIP);
  });

  it("D-e: with no local node configured, refuses before the wallet is used or anything is signed", async () => {
    const runStatePath = await runStateFile();
    const { journal, service } = operator();
    failingSubmission(journal, runStatePath);
    const source = await Effect.runPromise(
      Effect.scoped(
        nodeHubOracleNonceOriginSource({
          L1_NATIVE_LEDGER: undefined,
          NETWORK: "Preprod",
        }),
      ),
    );
    expect(source).toBeUndefined();
    const refusal = prepare(service, runStatePath, {});
    await expect(refusal).rejects.toThrow("No local node is configured");
    await expect(refusal).rejects.toThrow(
      "L1_NODE_SOCKET_PATH, L1_NODE_CONFIG_PATH, L1_NODE_TRANSPORT_BINARY_PATH",
    );
    expect(journal).toEqual([]);
    expect(signSubmitTransactionMock).not.toHaveBeenCalled();
    expect(await loadDeploymentRunState(runStatePath)).toBeNull();
  });

  it("D-f: a tip that leaves the chain before it settles is read again, and the settled tip is the one recorded", async () => {
    const runStatePath = await runStateFile();
    const { journal, service } = operator();
    failingSubmission(journal, runStatePath);
    await expect(
      prepare(service, runStatePath, {
        l1Origin: {
          observer: scriptedObserver(
            journal,
            [TIP, LATER_TIP],
            ["rolled_back", "settled"],
          ),
          depth: 10,
        },
      }),
    ).rejects.toThrow("submission refused");
    expect(journal).toEqual([
      "wallet",
      "tip 9000",
      "settle 9000 10",
      "tip 9020",
      "settle 9020 10",
      "submit with recorded origin 9020",
    ]);
    expect(
      (await loadDeploymentRunState(runStatePath))?.identity.l1Origin,
    ).toEqual(LATER_TIP);
  });

  it("D-f: a tip that keeps leaving the chain is refused after the bound, with nothing signed or broadcast", async () => {
    const runStatePath = await runStateFile();
    const { journal, service } = operator();
    failingSubmission(journal, runStatePath);
    await expect(
      prepare(service, runStatePath, {
        l1Origin: {
          observer: scriptedObserver(journal, [TIP], ["rolled_back"]),
          depth: 10,
        },
      }),
    ).rejects.toThrow(
      `left its chain ${ORIGIN_SETTLE_ATTEMPTS.toString()} times before 10 blocks extended it; no hub-oracle nonce was signed`,
    );
    expect(journal.filter((entry) => entry.startsWith("settle"))).toHaveLength(
      ORIGIN_SETTLE_ATTEMPTS,
    );
    expect(signSubmitTransactionMock).not.toHaveBeenCalled();
    expect(await loadDeploymentRunState(runStatePath)).toBeNull();
  });
});

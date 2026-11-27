import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import {
  CML,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { beforeEach, expect, it, vi } from "vitest";

import {
  submitUnattestedTimeoutCorrection,
  TimeoutCorrectionAttemptInFlightError,
} from "../src/remove-unattested-block.js";
import { DEFAULT_CONFIRMATION_POLL_MS } from "../src/runtime.js";
import {
  address,
  h,
  node,
  utxo,
} from "./remove-unattested-block-submit.fixture.js";
import { emulatorDoubleSpendRefusal } from "./support/emulator/double-spend-refusal.js";

const seams = vi.hoisted(() => ({
  queue: vi.fn(),
  lock: vi.fn(),
  build: vi.fn(),
  contracts: vi.fn(),
  reference: vi.fn(),
  hub: vi.fn(),
}));
vi.mock("@al-ft/midgard-sdk/core", async (original) => {
  const actual = await original<typeof import("@al-ft/midgard-sdk/core")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    fetchCorrectionLockUTxOProgram: () => Effect.succeed(seams.lock()),
    getStateQueueNodeFromStateQueueDatum: (datum: SDK.LinkedListNodeView) =>
      Effect.succeed({
        proven_fraud: null,
        block_time: 1n,
        da_attestation:
          datum.data === "attested"
            ? { Attested: { commitment_hash: "aa".repeat(32) } }
            : SDK.NO_DA_ATTESTATION,
      }),
  };
});
vi.mock("@al-ft/midgard-sdk/state-queue", async (original) => {
  const actual =
    await original<typeof import("@al-ft/midgard-sdk/state-queue")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    fetchSortedStateQueueUTxOsProgram: () => Effect.succeed(seams.queue()),
    incompletePruneUnattestedBlockDescendantTxProgram: (...args: unknown[]) =>
      seams.build(...args),
    incompleteRemoveLastUnattestedBlockTxProgram: (...args: unknown[]) =>
      seams.build(...args),
  };
});
vi.mock("../src/inspect-contracts.js", () => ({
  parseContractDeploymentInfo: () => seams.contracts(),
}));
vi.mock("../src/runtime.js", async (original) => ({
  ...(await original<typeof import("../src/runtime.js")>()),
  requireDeploymentReferenceScript: (...args: unknown[]) =>
    seams.reference(...args),
  requireSingletonUtxo: (...args: unknown[]) => seams.hub(...args),
}));

const root: SDKStateQueue.StateQueueUTxO = {
  ...node("aa", "01"),
  assetName: SDKStateQueue.STATE_QUEUE_ROOT_ASSET_NAME,
  datum: { key: "Empty", next: { Key: { key: h("01") } }, data: "root" },
};

const setup = () => {
  const wallet = [utxo("dd")];
  const submit = vi.fn(async (signedHash: string) => signedHash);
  const ordinary = [root, node("01", "11", true), node("11")];
  seams.queue.mockImplementation(() => ordinary);
  seams.lock.mockImplementation(() => ({
    utxo: utxo("cc"),
    datum: "Idle",
    assetName: SDK.CORRECTION_LOCK_ASSET_NAME,
  }));
  const script: Script = { type: "PlutusV3", script: "00" };
  const scriptHash = validatorToScriptHash(script);
  seams.contracts.mockReturnValue(
    Object.fromEntries(
      [
        "stateQueueMint",
        "stateQueueSpend",
        "stateQueueUnattestedTimeoutWithdraw",
        "correctionLockSpend",
        "hubOracleMint",
      ].map((name) => [
        name,
        { scriptHash, contract: { type: script.type, cborHex: script.script } },
      ]),
    ),
  );
  seams.reference.mockResolvedValue(utxo("ee"));
  seams.hub.mockResolvedValue(utxo("ff"));
  seams.build.mockImplementation(
    (
      _lucid: unknown,
      _config: unknown,
      params: {
        timedOutBlockUTxO: SDKStateQueue.StateQueueUTxO;
        predecessorUTxO: SDKStateQueue.StateQueueUTxO;
        additionalInputs: UTxO[];
        correctionLockInput: { utxo: UTxO };
        validTo: bigint;
      },
    ) => {
      const inputs = CML.TransactionInputList.new();
      const refs = [
        params.predecessorUTxO.utxo,
        params.timedOutBlockUTxO.utxo,
        params.correctionLockInput.utxo,
        ...params.additionalInputs,
      ].sort((a, b) => a.txHash.localeCompare(b.txHash));
      for (const ref of refs)
        inputs.add(
          CML.TransactionInput.new(
            CML.TransactionHash.from_hex(ref.txHash),
            BigInt(ref.outputIndex),
          ),
        );
      const body = CML.TransactionBody.new(
        inputs,
        CML.TransactionOutputList.new(),
        1n,
      );
      body.set_validity_interval_start(10n);
      body.set_ttl(params.validTo / 1000n);
      const txHash = CML.hash_transaction(body).to_hex();
      const ready = {
        toHash: () => txHash,
        submit: () => submit(txHash),
      };
      const builder = {
        addSignerKey: () => builder,
        complete: async () => ({
          sign: { withWallet: () => ({ complete: async () => ready }) },
        }),
      };
      return builder;
    },
  );
  const awaitTx = vi.fn(async () => true);
  const lucid = {
    wallet: () => ({ address: async () => address }),
    utxosAt: async () => wallet,
    awaitTx,
    unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1000),
    slotToUnixTime: (slot: number) => slot * 1000,
  } as unknown as LucidEvolution;
  const params = {
    lucid,
    deploymentInfo: {},
    network: "Preprod" as const,
    signer: {
      source: "test",
      address,
      proofMaterialsAddress: "addr_test1_unused_no_proof_material",
      paymentKeyHash: h("dd"),
      selectWallet: () => undefined,
    },
    nowMs: () => 4_000_000,
    awaitConfirmation: false,
  };
  return {
    params,
    submit,
    awaitTx,
    removeTarget: () => {
      seams.queue.mockReturnValue([
        root,
        { ...node("01", undefined, true), utxo: utxo("99") },
      ]);
    },
  };
};
beforeEach(() => vi.clearAllMocks());

const lockedOn = (target: string) => ({
  utxo: utxo("cc"),
  datum: {
    Locked: {
      correction_identity: "AttestationTimeout",
      target_header_hash: h(target),
    },
  },
});
const leaseCoordinator = () => {
  const release = vi.fn(async () => undefined);
  const fail = vi.fn(async () => undefined);
  const acquire = vi.fn(async () => ({
    token: "t",
    source: "test",
    release,
    fail,
    renew: vi.fn(async () => undefined),
  }));
  return { coordinator: { acquire }, acquire, release, fail };
};

it("submits the step the queue calls for, funded by one wallet input", async () => {
  const f = setup();
  const result = await submitUnattestedTimeoutCorrection(f.params);
  expect(result).toMatchObject({
    status: "pending",
    targetHeaderHash: h("11"),
  });
  expect(seams.build).toHaveBeenCalledOnce();
  expect(seams.build.mock.calls[0]![2]).toMatchObject({
    additionalInputs: [utxo("dd")],
  });
  expect(f.submit).toHaveBeenCalledOnce();
  expect(result.pendingTxHash).toBe(f.submit.mock.calls[0]![0]);
});

it("names the ledger's refusal of a step whose inputs an in-flight step spent, releases the lease, and finds nothing left to correct once it lands", async () => {
  const f = setup();
  const lease = leaseCoordinator();
  const params = {
    ...f.params,
    stateQueueMutationLeaseCoordinator: lease.coordinator,
  };
  expect((await submitUnattestedTimeoutCorrection(params)).status).toBe(
    "pending",
  );
  // The next run reads the same queue, so it builds the same step again.
  const refusal = await emulatorDoubleSpendRefusal();
  f.submit.mockRejectedValueOnce(refusal);
  const refused = await submitUnattestedTimeoutCorrection(params).catch(
    (error: unknown) => error,
  );
  expect(refused).toBeInstanceOf(TimeoutCorrectionAttemptInFlightError);
  expect((refused as Error).cause).toBe(refusal);
  expect(lease.release).toHaveBeenCalledTimes(2);
  expect(lease.fail).not.toHaveBeenCalled();
  // The first step lands: the queue no longer holds the target.
  f.removeTarget();
  const done = await submitUnattestedTimeoutCorrection(params);
  expect(done.status).toBe("empty");
  expect(seams.build).toHaveBeenCalledTimes(2);
});

it("leaves a lock of another correction kind alone, without taking the lease or building", async () => {
  const f = setup();
  seams.lock.mockReturnValue({
    utxo: utxo("cc"),
    datum: {
      Locked: {
        correction_identity: "UnavailableData",
        target_header_hash: h("33"),
      },
    },
  });
  const lease = leaseCoordinator();
  expect(
    await submitUnattestedTimeoutCorrection({
      ...f.params,
      stateQueueMutationLeaseCoordinator: lease.coordinator,
    }),
  ).toMatchObject({ status: "pending", targetHeaderHash: null });
  expect(lease.acquire).not.toHaveBeenCalled();
  expect(seams.build).not.toHaveBeenCalled();
});

it("releases its lease when another correction acquires the lock after selection", async () => {
  const f = setup();
  const lease = leaseCoordinator();
  lease.acquire.mockImplementation(async () => {
    seams.lock.mockReturnValue(lockedOn("33"));
    return {
      token: "t",
      source: "test",
      release: lease.release,
      fail: lease.fail,
      renew: vi.fn(async () => undefined),
    };
  });
  const result = await submitUnattestedTimeoutCorrection({
    ...f.params,
    stateQueueMutationLeaseCoordinator: lease.coordinator,
  });
  expect(result.status).toBe("pending");
  expect(lease.release).toHaveBeenCalledOnce();
  expect(lease.fail).not.toHaveBeenCalled();
  expect(seams.build).not.toHaveBeenCalled();
});

it("waits for each submitted step to land and builds the next from the chain", async () => {
  const f = setup();
  f.awaitTx.mockImplementationOnce(async () => {
    f.removeTarget();
    return true;
  });
  const result = await submitUnattestedTimeoutCorrection({
    ...f.params,
    awaitConfirmation: true,
  });
  expect(result).toMatchObject({ status: "complete", pendingTxHash: null });
  expect(f.awaitTx).toHaveBeenCalledWith(
    f.submit.mock.calls[0]![0],
    DEFAULT_CONFIRMATION_POLL_MS,
  );
  expect(seams.build).toHaveBeenCalledOnce();
});

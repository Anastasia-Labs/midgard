import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { beforeEach, expect, it, vi } from "vitest";

import {
  createFileTimeoutCorrectionJournalStore,
  submitUnattestedTimeoutCorrection,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionRecovery,
} from "../src/remove-unattested-block.js";

const seams = vi.hoisted(() => ({
  queue: vi.fn(),
  lock: vi.fn(),
  build: vi.fn(),
  contracts: vi.fn(),
  reference: vi.fn(),
  hub: vi.fn(),
}));
vi.mock("@al-ft/midgard-sdk", async (original) => {
  const actual = await original<typeof SDK>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    fetchSortedStateQueueUTxOsProgram: () => Effect.succeed(seams.queue()),
    fetchCorrectionLockUTxOProgram: () => Effect.succeed(seams.lock()),
    getStateQueueNodeFromStateQueueDatum: (datum: SDK.LinkedListNodeView) =>
      Effect.succeed({
        proven_fraud: null,
        header: { endTime: 1n },
        da_attestation:
          datum.data === "attested"
            ? { Attested: { da_bond_asset_name: "aa".repeat(32) } }
            : SDK.NO_DA_ATTESTATION,
      }),
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

const h = (byte: string) => byte.repeat(28);
const tx = (byte: string) => byte.repeat(32);
const address = credentialToAddress("Preprod", { type: "Key", hash: h("dd") });
const utxo = (byte: string): UTxO => ({
  txHash: tx(byte),
  outputIndex: 0,
  address,
  assets: { lovelace: 20_000_000n },
});
const node = (
  byte: string,
  next?: string,
  attested = false,
): SDK.StateQueueUTxO => ({
  assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + h(byte),
  utxo: utxo(byte),
  datum: {
    key: { Key: { key: h(byte) } },
    next: next === undefined ? "Empty" : { Key: { key: h(next) } },
    data: attested ? "attested" : "unattested",
  },
});
const root: SDK.StateQueueUTxO = {
  ...node("aa", "01"),
  assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  datum: { key: "Empty", next: { Key: { key: h("01") } }, data: "root" },
};
const point = {
  pointId: "test:9999",
  blockHash: tx("77"),
  slot: "9999",
  blockNo: "100",
};

const setup = () => {
  let retained: TimeoutCorrectionJournal | undefined;
  let wallet = [utxo("dd")];
  const save = vi.fn(async (journal: TimeoutCorrectionJournal) => {
    retained = structuredClone(journal);
  });
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
        timedOutBlockUTxO: SDK.StateQueueUTxO;
        predecessorUTxO: SDK.StateQueueUTxO;
        additionalInputs: UTxO[];
        correctionLockInput: { utxo: UTxO };
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
      body.set_ttl(20n);
      const signed = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      );
      const txHash = CML.hash_transaction(body).to_hex();
      const ready = {
        toHash: () => txHash,
        toCBOR: () => signed.to_cbor_hex(),
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
  const lucid = {
    wallet: () => ({
      address: async () => address,
      getUtxos: async () => wallet,
    }),
    utxosAt: async () => wallet,
    overrideUTxOs: (values: UTxO[]) => {
      wallet = values;
    },
    transactionStatus: async () => ({ status: "not_found" }),
    unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1000),
    slotToUnixTime: (slot: number) => slot * 1000,
  } as unknown as LucidEvolution;
  const observed = vi.fn<TimeoutCorrectionRecovery["observeSignedTransaction"]>(
    async (signed) => ({
      ...signed,
      status: "unknown",
      reason: "unresolved",
      canonicalPoint: point,
      releaseFinalPoint: point,
      inputs: [],
    }),
  );
  const rebroadcast =
    vi.fn<TimeoutCorrectionRecovery["rebroadcastSignedTransaction"]>();
  const params = {
    lucid,
    deploymentInfo: {},
    network: "Preprod" as const,
    signer: {
      source: "test",
      address,
      paymentKeyHash: h("dd"),
      selectWallet: () => undefined,
    },
    journalStore: { load: async () => retained, save },
    nowMs: () => 4_000_000,
    awaitConfirmation: false,
    recovery: {
      observeSignedTransaction: observed,
      rebroadcastSignedTransaction: rebroadcast,
    },
  };
  return {
    params,
    submit,
    save,
    observed,
    retained: () => retained,
    changeFee: () => {
      wallet = [utxo("de")];
    },
    removeTarget: () => {
      seams.queue.mockReturnValue([
        root,
        { ...node("01", undefined, true), utxo: utxo("99") },
      ]);
    },
  };
};
beforeEach(() => vi.clearAllMocks());

it("journals signed bytes before an ambiguous submit and resumes exact inclusion without rebuilding", async () => {
  const f = setup();
  f.submit.mockImplementationOnce(async () => {
    expect(f.retained()?.steps[0]).toMatchObject({
      status: "prepared",
      validFromSlot: "10",
      validToSlot: "20",
    });
    expect(f.retained()?.steps[0]?.signedCbor).toMatch(/^[0-9a-f]+$/);
    throw new Error("socket closed after submission");
  });
  const first = await submitUnattestedTimeoutCorrection(f.params);
  expect(first.status).toBe("pending");
  expect(seams.build).toHaveBeenCalledOnce();
  const retainedBytes = f.retained()!.steps[0]!.signedCbor;
  f.removeTarget();
  f.observed.mockImplementation(async (signed) => ({
    ...signed,
    status: "included",
    reason: "exact canonical body",
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
  }));
  const resumed = await submitUnattestedTimeoutCorrection(f.params);
  expect(resumed.status).toBe("complete");
  expect(f.retained()?.completed).toBe(true);
  expect(f.retained()?.steps[0]?.signedCbor).toBe(retainedBytes);
  expect(seams.build).toHaveBeenCalledOnce();
  expect(f.submit).toHaveBeenCalledOnce();
});

it("keeps unknown attempts intact, then refreshes the fee input only after canonical invalidation", async () => {
  const f = setup();
  f.submit.mockRejectedValueOnce(new Error("unknown acknowledgement"));
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained()!.steps[0]!);
  await submitUnattestedTimeoutCorrection(f.params);
  expect(seams.build).toHaveBeenCalledOnce();
  expect(f.retained()!.steps).toEqual([original]);
  f.changeFee();
  f.observed.mockImplementation(async (signed) => ({
    ...signed,
    status: "invalidated",
    reason: "fee input canonically spent",
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
  }));
  const retried = await submitUnattestedTimeoutCorrection(f.params);
  expect(retried.status).toBe("pending");
  expect(seams.build).toHaveBeenCalledTimes(2);
  expect(f.retained()!.steps[0]).toEqual({ ...original, status: "superseded" });
  expect(f.retained()!.steps[1]?.txHash).not.toBe(original.txHash);
  expect(f.retained()!.steps[1]?.inputOutRefs).toContain(`${tx("de")}#0`);
  expect(f.retained()!.steps[1]?.inputOutRefs).not.toContain(`${tx("dd")}#0`);
});

it("waits for a competing timeout lock without replacing the retained objective or poisoning its lease", async () => {
  const f = setup();
  await submitUnattestedTimeoutCorrection(f.params);
  const retained = structuredClone(f.retained());
  const acquire = vi.fn();
  seams.lock.mockReturnValue({
    utxo: utxo("cc"),
    datum: {
      Locked: {
        correction_identity: "AttestationTimeout",
        target_header_hash: h("33"),
      },
    },
  });
  const result = await submitUnattestedTimeoutCorrection({
    ...f.params,
    stateQueueMutationLeaseCoordinator: { acquire },
  });
  expect(result.status).toBe("pending");
  expect(acquire).not.toHaveBeenCalled();
  expect(f.retained()).toEqual(retained);
  expect(seams.build).toHaveBeenCalledOnce();
});

it("releases its lease when another correction acquires the lock after selection", async () => {
  const f = setup();
  const release = vi.fn(async () => undefined);
  const fail = vi.fn(async () => undefined);
  const acquire = vi.fn(async () => {
    seams.lock.mockReturnValue({
      utxo: utxo("cc"),
      datum: {
        Locked: {
          correction_identity: "AttestationTimeout",
          target_header_hash: h("33"),
        },
      },
    });
    return {
      token: "test-token",
      source: "test",
      release,
      fail,
      renew: vi.fn(async () => undefined),
    };
  });
  const result = await submitUnattestedTimeoutCorrection({
    ...f.params,
    stateQueueMutationLeaseCoordinator: { acquire },
  });
  expect(result.status).toBe("pending");
  expect(release).toHaveBeenCalledOnce();
  expect(fail).not.toHaveBeenCalled();
  expect(seams.build).not.toHaveBeenCalled();
});

const competingTimeout = () => {
  const f = setup();
  const takeLock = () => {
    seams.queue.mockReturnValue([
      root,
      node("01", "11", true),
      node("11", "33"),
      node("33"),
    ]);
    seams.lock.mockReturnValue({
      utxo: utxo("cc"),
      datum: {
        Locked: {
          correction_identity: "AttestationTimeout",
          target_header_hash: h("33"),
        },
      },
    });
  };
  return { ...f, takeLock };
};

it("archives resolved displaced attempts before writing the locked target and resumes that target", async () => {
  const f = competingTimeout();
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained()!);
  f.takeLock();
  f.observed.mockImplementation(async (signed) => ({
    ...signed,
    status: "invalidated",
    reason: "correction input canonically consumed",
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
  }));
  const archive = vi.fn(async (journal: TimeoutCorrectionJournal) => {
    expect(f.retained()?.targetHeaderHash).toBe(h("11"));
    expect(journal.steps).toEqual([
      { ...original.steps[0], status: "superseded" },
    ]);
  });
  const result = await submitUnattestedTimeoutCorrection({
    ...f.params,
    journalStore: { ...f.params.journalStore, archive },
  });
  expect(archive).toHaveBeenCalledOnce();
  expect(result).toMatchObject({
    status: "pending",
    targetHeaderHash: h("33"),
  });
  expect(f.retained()?.targetHeaderHash).toBe(h("33"));
  expect(seams.build).toHaveBeenCalledTimes(2);
});

it("does not switch objectives when archival fails", async () => {
  const f = competingTimeout();
  await submitUnattestedTimeoutCorrection(f.params);
  f.takeLock();
  f.observed.mockImplementation(async (signed) => ({
    ...signed,
    status: "expired",
    reason: "stable expiry",
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
  }));
  const archive = vi.fn(async () => {
    throw new Error("archive disk unavailable");
  });
  await expect(
    submitUnattestedTimeoutCorrection({
      ...f.params,
      journalStore: { ...f.params.journalStore, archive },
    }),
  ).rejects.toThrow("archive disk unavailable");
  expect(f.retained()?.targetHeaderHash).toBe(h("11"));
  expect(seams.build).toHaveBeenCalledOnce();
});

it("never rebroadcasts an unresolved displaced attempt or switches to another correction kind", async () => {
  const f = competingTimeout();
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained());
  f.takeLock();
  f.observed.mockImplementation(async (signed) => ({
    ...signed,
    status: "rebroadcast",
    reason: "inputs still available",
    canonicalPoint: point,
    releaseFinalPoint: point,
    inputs: [],
  }));
  const archive = vi.fn(async () => undefined);
  const params = {
    ...f.params,
    journalStore: { ...f.params.journalStore, archive },
  };
  expect((await submitUnattestedTimeoutCorrection(params)).status).toBe(
    "pending",
  );
  expect(f.params.recovery.rebroadcastSignedTransaction).not.toHaveBeenCalled();
  expect(archive).not.toHaveBeenCalled();
  expect(f.retained()).toEqual(original);
  seams.lock.mockReturnValue({
    utxo: utxo("cc"),
    datum: {
      Locked: {
        correction_identity: "UnavailableData",
        target_header_hash: h("33"),
      },
    },
  });
  f.observed.mockClear();
  expect((await submitUnattestedTimeoutCorrection(params)).status).toBe(
    "pending",
  );
  expect(f.observed).not.toHaveBeenCalled();
  expect(archive).not.toHaveBeenCalled();
  expect(seams.build).toHaveBeenCalledOnce();
});

it("archives the exact journal idempotently and refuses to overwrite conflicting bytes", async () => {
  const f = setup();
  await submitUnattestedTimeoutCorrection(f.params);
  const directory = await mkdtemp(join(tmpdir(), "timeout-journal-archive-"));
  try {
    const path = join(directory, "journal.json");
    const store = createFileTimeoutCorrectionJournalStore(path);
    const retained = f.retained()!;
    const bytes = `${JSON.stringify(retained, null, 2)}\n`;
    const digest = createHash("sha256").update(bytes).digest("hex");
    const archivedPath = `${path}.archive-${digest}.json`;
    await store.archive!(retained);
    await store.archive!(retained);
    expect(await readFile(archivedPath, "utf8")).toBe(bytes);
    await writeFile(archivedPath, "conflicting retained evidence");
    await expect(store.archive!(retained)).rejects.toThrow("does not match");
    expect(await readFile(archivedPath, "utf8")).toBe(
      "conflicting retained evidence",
    );
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

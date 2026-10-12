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

import { STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS } from "../src/remove-fraudulent-block.js";
import {
  createFileTimeoutCorrectionJournalStore,
  submitUnattestedTimeoutCorrection,
  TimeoutCorrectionAttemptInFlightError,
  type TimeoutCorrectionAttemptObservation,
  type TimeoutCorrectionJournal,
  type TimeoutCorrectionRecovery,
} from "../src/remove-unattested-block.js";
import { assertTimeoutCorrectionExclusion } from "../src/remove-unattested-block.supersede-timeout-correction-attempts.js";
import { DEFAULT_CONFIRMATION_POLL_MS } from "../src/runtime.js";
import { createSupersededAttemptReadSchedule } from "../src/workflow/superseded-attempt-read-schedule.js";
import { emulatorDoubleSpendRefusal } from "./support/emulator/double-spend-refusal.js";

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
            ? { Attested: { commitment_hash: "aa".repeat(32) } }
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
const point = { slot: 9999, hash: tx("77") };
type Status = TimeoutCorrectionAttemptObservation["status"];
/** An observation; `final` marks a dead reason beyond every legal rollback. */
const observation = (
  status: Status,
  final = false,
  reason = "unresolved",
): TimeoutCorrectionAttemptObservation => ({
  status,
  final,
  canonicalPoint: point,
  releaseFinalPoint: point,
  reason,
});

/** `ttl: "validity"` signs each attempt valid to the upper bound it was
 * built with (on the mocked 1 s slot clock); otherwise to slot 20. */
const setup = ({ ttl = 20n }: { readonly ttl?: bigint | "validity" } = {}) => {
  let retained: TimeoutCorrectionJournal | undefined;
  let wallet = [utxo("dd")];
  let txStatus: (txHash: string) => string = () => "not_found";
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
      body.set_ttl(ttl === "validity" ? params.validTo / 1000n : ttl);
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
    transactionStatus: async (txHash: string) => ({
      status: txStatus(txHash),
    }),
    unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1000),
    slotToUnixTime: (slot: number) => slot * 1000,
  } as unknown as LucidEvolution;
  const observed = vi.fn<TimeoutCorrectionRecovery["observeAttempt"]>();
  const observe = (status: Status, final = false, reason = "unresolved") =>
    observed.mockImplementation(async () => observation(status, final, reason));
  observe("unknown");
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
    recovery: { observeAttempt: observed } as TimeoutCorrectionRecovery,
  };
  const { recovery: _recovery, ...cliParams } = params;
  return {
    params,
    /** A CLI run: no follower, so no recovery reader. */
    cliParams,
    setTxStatus: (status: (txHash: string) => string) => {
      txStatus = status;
    },
    submit,
    save,
    observed,
    observe,
    retained: () => retained,
    changeFee: () => {
      wallet = [utxo("de")];
    },
    setWallet: (values: UTxO[]) => {
      wallet = values;
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
  f.observe("included", false, "landed at depth 1");
  const resumed = await submitUnattestedTimeoutCorrection(f.params);
  expect(resumed.status).toBe("complete");
  expect(f.retained()?.completed).toBe(true);
  expect(f.retained()?.steps[0]?.signedCbor).toBe(retainedBytes);
  expect(seams.build).toHaveBeenCalledOnce();
  expect(f.submit).toHaveBeenCalledOnce();
});

it("keeps unknown attempts intact, then re-signs at once when an invalidation is still within k", async () => {
  const f = setup();
  f.submit.mockRejectedValueOnce(new Error("unknown acknowledgement"));
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained()!.steps[0]!);
  await submitUnattestedTimeoutCorrection(f.params);
  expect(seams.build).toHaveBeenCalledOnce();
  expect(f.retained()!.steps).toEqual([original]);
  f.changeFee();
  f.observe("invalidated", false, "fee input spent within k");
  // No wait for a retirement receipt: the attempt is abandoned and replaced.
  const replaced = await submitUnattestedTimeoutCorrection(f.params);
  expect(replaced.status).toBe("pending");
  expect(seams.build).toHaveBeenCalledTimes(2);
  expect(f.retained()!.steps[0]).toEqual({ ...original, status: "abandoned" });
  const replacement = f.retained()!.steps[1]!;
  expect(replacement.txHash).not.toBe(original.txHash);
  expect(replacement.inputOutRefs).toContain(`${tx("de")}#0`);
  expect(replacement.inputOutRefs).not.toContain(`${tx("dd")}#0`);
  // Mutually exclusive through the shared node and correction-lock inputs.
  expect(
    replacement.inputOutRefs.filter((outRef) =>
      original.inputOutRefs.includes(outRef),
    ),
  ).toEqual([`${tx("01")}#0`, `${tx("11")}#0`, `${tx("cc")}#0`]);
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
  f.observe("invalidated", true, "correction input consumed beyond k");
  const archive = vi.fn(async (journal: TimeoutCorrectionJournal) => {
    expect(f.retained()?.targetHeaderHash).toBe(h("11"));
    expect(journal.steps).toEqual([
      { ...original.steps[0], status: "retired" },
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
  const signed = f.retained()!.steps[0]!.signedCbor;
  const ttl = CML.Transaction.from_cbor_hex(signed).body().ttl();
  expect(ttl).toBeLessThan(BigInt(point.slot));
  f.takeLock();
  f.observe("expired", true, "expiry beyond k");
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

it("never resubmits an unresolved displaced attempt or switches to another correction kind", async () => {
  const f = competingTimeout();
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained());
  f.takeLock();
  f.observed.mockImplementation(async () => ({
    ...observation("pending", false, "live, inputs unspent"),
    inputsAvailable: true,
  }));
  const archive = vi.fn(async () => undefined);
  const params = {
    ...f.params,
    journalStore: { ...f.params.journalStore, archive },
  };
  expect((await submitUnattestedTimeoutCorrection(params)).status).toBe(
    "pending",
  );
  expect(f.submit).toHaveBeenCalledOnce();
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

const statusByHash = (
  f: ReturnType<typeof setup>,
  statuses: Readonly<Record<string, Status>>,
  fallback: Status = "unknown",
) =>
  f.observed.mockImplementation(async (signed) =>
    observation(
      statuses[signed.transactionHash] ?? fallback,
      false,
      "per attempt",
    ),
  );
const funded = (byte: string, lovelace: bigint): UTxO => ({
  ...utxo(byte),
  assets: { lovelace },
});
/** Another correction moved the lock and the nodes, so no protocol input of
 * the first attempt is left for a replacement to share. */
const moveProtocolInputs = () => {
  seams.lock.mockImplementation(() => ({
    utxo: utxo("c1"),
    datum: "Idle",
    assetName: SDK.CORRECTION_LOCK_ASSET_NAME,
  }));
  seams.queue.mockReturnValue([
    root,
    { ...node("01", "11", true), utxo: utxo("a1") },
    { ...node("11"), utxo: utxo("b1") },
  ]);
};

it("draws a replacement onto an abandoned attempt's wallet input when no protocol input is shared, topping up from the wallet", async () => {
  const f = setup();
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained()!.steps[0]!);
  expect(original.inputOutRefs).toContain(`${tx("dd")}#0`);
  moveProtocolInputs();
  // The old fee input is still unspent but no longer the largest.
  f.setWallet([funded("dd", 2_000_000n), funded("de", 90_000_000n)]);
  statusByHash(f, { [original.txHash]: "expired" });
  await submitUnattestedTimeoutCorrection(f.params);
  expect(seams.build).toHaveBeenCalledTimes(2);
  expect(f.retained()!.steps[0]!.status).toBe("abandoned");
  const replacement = f.retained()!.steps[1]!;
  // The shared input is too small on its own; the largest wallet input pays.
  expect(replacement.inputOutRefs).toContain(`${tx("dd")}#0`);
  expect(replacement.inputOutRefs).toContain(`${tx("de")}#0`);
});

it("admits a replacement with no shared input only when none of the abandoned attempt's inputs is left", async () => {
  const f = setup();
  await submitUnattestedTimeoutCorrection(f.params);
  const original = structuredClone(f.retained()!.steps[0]!);
  moveProtocolInputs();
  f.changeFee();
  statusByHash(f, { [original.txHash]: "invalidated" });
  await submitUnattestedTimeoutCorrection(f.params);
  const replacement = f.retained()!.steps[1]!;
  expect(
    replacement.inputOutRefs.some((outRef) =>
      original.inputOutRefs.includes(outRef),
    ),
  ).toBe(false);
  // Negative: had the old wallet input been left, the same replacement is refused.
  expect(() =>
    assertTimeoutCorrectionExclusion({
      journal: f.retained()!,
      inputOutRefs: replacement.inputOutRefs,
      walletUtxos: [utxo("dd"), utxo("de")],
    }),
  ).toThrow("must share an input with each abandoned attempt");
});

it("adopts an abandoned attempt that a rollback lands, and completes without waiting for k", async () => {
  const f = setup();
  const schedule = createSupersededAttemptReadSchedule();
  const params = { ...f.params, attemptReadSchedule: schedule };
  await submitUnattestedTimeoutCorrection(params);
  const original = structuredClone(f.retained()!.steps[0]!);
  f.changeFee();
  statusByHash(f, { [original.txHash]: "expired" });
  await submitUnattestedTimeoutCorrection(params);
  const replacement = structuredClone(f.retained()!.steps[1]!);
  expect(f.retained()!.steps.map(({ status }) => status)).toEqual([
    "abandoned",
    "submitted",
  ]);
  // A rollback lands the original; the replacement, which shares its node
  // inputs, is now impossible at the tip.
  f.removeTarget();
  statusByHash(f, {
    [original.txHash]: "included",
    [replacement.txHash]: "invalidated",
  });
  const result = await submitUnattestedTimeoutCorrection(params);
  expect(result.status).toBe("complete");
  expect(f.retained()!.steps.map(({ status }) => status)).toEqual([
    "confirmed",
    "abandoned",
  ]);
  expect(result.removedHeaderHashes).toEqual([original.removedHeaderHash]);
  expect(seams.build).toHaveBeenCalledTimes(2);
  expect(f.submit).toHaveBeenCalledTimes(2);
});

it("re-reads abandoned attempts on a backoff schedule, a bounded number per pass", async () => {
  let now = 4_000_000;
  const f = setup();
  const schedule = createSupersededAttemptReadSchedule({ perPass: 1 });
  const params = {
    ...f.params,
    nowMs: () => now,
    attemptReadSchedule: schedule,
  };
  await submitUnattestedTimeoutCorrection(params);
  const original = structuredClone(f.retained()!.steps[0]!);
  f.changeFee();
  statusByHash(f, { [original.txHash]: "expired" }, "pending");
  await submitUnattestedTimeoutCorrection(params);
  const reads = () =>
    f.observed.mock.calls.filter(
      ([signed]) => signed.transactionHash === original.txHash,
    ).length;
  const before = reads();
  // First pass after abandonment reads it once; then it backs off.
  for (let pass = 0; pass < 3; pass += 1)
    await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 1);
  now += 19_999;
  await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 1);
  now += 1;
  await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 2);
  // The delay doubles after another unresolved read.
  now += 20_000;
  await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 2);
  now += 20_000;
  await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 3);
  // Past k the read retires it and it leaves the schedule.
  f.observed.mockImplementation(async (signed) =>
    signed.transactionHash === original.txHash
      ? observation("expired", true, "expiry beyond k")
      : observation("pending"),
  );
  now += 80_000;
  await submitUnattestedTimeoutCorrection(params);
  expect(f.retained()!.steps[0]!.status).toBe("retired");
  now += 600_000;
  await submitUnattestedTimeoutCorrection(params);
  expect(reads()).toBe(before + 4);
});

it("a CLI run abandons a prior run's attempt it cannot observe, and the ledger's refusal of the replacement is the named in-flight error", async () => {
  const f = setup();
  const release = vi.fn(async () => undefined);
  const fail = vi.fn(async () => undefined);
  const lease = {
    acquire: async () => ({
      token: "t",
      source: "test",
      release,
      fail,
      renew: vi.fn(async () => undefined),
    }),
  };
  const params = {
    ...f.cliParams,
    stateQueueMutationLeaseCoordinator: lease,
    attemptReadSchedule: createSupersededAttemptReadSchedule(),
  };
  expect((await submitUnattestedTimeoutCorrection(params)).status).toBe(
    "pending",
  );
  const prior = structuredClone(f.retained()!.steps[0]!);
  expect(prior.status).toBe("submitted");
  // The next run cannot tell whether the prior attempt is in flight. Its
  // replacement shares the prior's inputs; the ledger refuses it.
  f.changeFee();
  const refusal = await emulatorDoubleSpendRefusal();
  f.submit.mockRejectedValueOnce(refusal);
  const refused = await submitUnattestedTimeoutCorrection(params).catch(
    (error: unknown) => error,
  );
  expect(refused).toBeInstanceOf(TimeoutCorrectionAttemptInFlightError);
  expect((refused as Error).cause).toBe(refusal);
  expect(f.submit).toHaveBeenCalledTimes(2);
  const steps = f.retained()!.steps;
  expect(steps.map(({ status }) => status)).toEqual(["abandoned", "abandoned"]);
  expect(steps[0]).toEqual({ ...prior, status: "abandoned" });
  expect(
    steps[1]!.inputOutRefs.filter((outRef) =>
      prior.inputOutRefs.includes(outRef),
    ),
  ).toEqual([`${tx("01")}#0`, `${tx("11")}#0`, `${tx("cc")}#0`]);
  // A named refusal, not a failed run: the lease is released, not failed.
  expect(release).toHaveBeenCalledTimes(2);
  expect(fail).not.toHaveBeenCalled();
  // The prior attempt lands: the provider confirms it and the queue shows its
  // effect, so it is adopted and the correction completes with no new build.
  f.removeTarget();
  f.setTxStatus((txHash) =>
    txHash === prior.txHash ? "confirmed" : "not_found",
  );
  const done = await submitUnattestedTimeoutCorrection(params);
  expect(done.status).toBe("complete");
  expect(done.removedHeaderHashes).toEqual([prior.removedHeaderHash]);
  expect(seams.build).toHaveBeenCalledTimes(2);
});

it("a CLI run waits on its own submission while it is valid, and abandons it once its validity has passed", async () => {
  vi.useFakeTimers({ toFake: ["setTimeout"] });
  try {
    const f = setup({ ttl: "validity" });
    let now = 4_000_000;
    const run = submitUnattestedTimeoutCorrection({
      ...f.cliParams,
      attemptReadSchedule: createSupersededAttemptReadSchedule(),
      nowMs: () => now,
      awaitConfirmation: true,
    });
    await vi.advanceTimersByTimeAsync(DEFAULT_CONFIRMATION_POLL_MS * 3);
    expect(seams.build).toHaveBeenCalledOnce();
    expect(f.retained()!.steps.map(({ status }) => status)).toEqual([
      "submitted",
    ]);
    // Past its validity it can no longer land: replaced, sharing its inputs.
    now += Number(STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS);
    f.changeFee();
    await vi.advanceTimersByTimeAsync(DEFAULT_CONFIRMATION_POLL_MS);
    expect(seams.build).toHaveBeenCalledTimes(2);
    expect(f.retained()!.steps.map(({ status }) => status)).toEqual([
      "abandoned",
      "submitted",
    ]);
    const replacement = f.retained()!.steps[1]!.txHash;
    f.removeTarget();
    f.setTxStatus((txHash) =>
      txHash === replacement ? "confirmed" : "not_found",
    );
    await vi.advanceTimersByTimeAsync(DEFAULT_CONFIRMATION_POLL_MS);
    expect((await run).status).toBe("complete");
  } finally {
    vi.useRealTimers();
  }
});

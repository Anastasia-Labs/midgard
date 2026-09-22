import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { publishJourneyForcedOrder } from "./forced-order.js";
import type { SignedCommitAttempt } from "./signed-commit-reconciliation.js";
import type { JourneyFaultPreparationInput } from "./staging.js";

type Recovery = Awaited<
  ReturnType<JourneyFaultPreparationInput["readSignedCommitRecovery"]>
>;
const directories: string[] = [];
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});
const signed = (ttl: number) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  body.set_ttl(BigInt(ttl));
  const tx = CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    signedCbor: tx.to_cbor_hex(),
  };
};
const observation = (
  attempt: SignedCommitAttempt,
  status: Recovery["status"],
): Recovery => ({
  transactionHash: attempt.txHash,
  signedTransactionCborHex: attempt.signedCbor,
  status,
  reason: `authenticated ${status}`,
  canonicalPoint: {
    pointId: "aa".repeat(32),
    blockHash: "aa".repeat(32),
    blockNo: "100",
    slot: "1000",
  },
  releaseFinalPoint: {
    pointId: "bb".repeat(32),
    blockHash: "bb".repeat(32),
    blockNo: "70",
    slot: "700",
  },
  inputs: [],
});
const fixture = async (confirmed = false) => {
  const directory = await mkdtemp(join(tmpdir(), "forced-order-recovery-"));
  directories.push(directory);
  const old = signed(500);
  const address = credentialToAddress("Preprod", {
    type: "Key",
    hash: "22".repeat(28),
  });
  const metadata = (
    attempt: SignedCommitAttempt,
    inclusionTime: number,
  ): SDK.TxOrderBuildMetadata => ({
    txOrderId: { transactionId: attempt.txHash, outputIndex: 0n },
    txOrderAddress: address,
    authNonceCbor: "80",
    txOrderAuthUnit: "33".repeat(28),
    nonceInput: { txHash: "11".repeat(32), outputIndex: 0 },
    validTo: inclusionTime + 60_000,
    inclusionTime,
  });
  const saved = {
    deploymentFingerprint: "44".repeat(32),
    submittedTxCbor: "80",
    ...old,
    metadata: metadata(old, 100),
    confirmed,
  };
  const path = join(directory, "forced-order.json");
  await writeJourneyArtifact(path, saved);
  const funding = {
    txHash: "11".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 50_000_000n },
  };
  const refresh = vi.fn(async () => [funding]);
  const override = vi.fn();
  const fresh = signed(1500);
  const build = vi
    .spyOn(SDK, "buildUnsignedTxOrderTxWithMetadataProgram")
    .mockReturnValue(
      Effect.succeed({
        tx: {
          sign: {
            withWallet: () => ({
              complete: async () => ({
                toHash: () => fresh.txHash,
                toCBOR: () => fresh.signedCbor,
              }),
            }),
          },
        },
        metadata: metadata(fresh, 200),
      }) as unknown as ReturnType<
        typeof SDK.buildUnsignedTxOrderTxWithMetadataProgram
      >,
    );
  const readRecovery = vi.fn(async (attempt: SignedCommitAttempt) =>
    observation(attempt, "included"),
  );
  const submit = vi.fn(async (bytes: string) => {
    const persisted = await readJourneyArtifact<typeof saved>(path);
    expect(persisted.signedCbor).toBe(bytes);
    expect(persisted.confirmed).toBe(false);
    return persisted.txHash;
  });
  const delaySlots = vi.fn(async (_slots: number) => undefined);
  const input = {
    directory,
    onStage: vi.fn(),
    readSignedCommitRecovery: readRecovery,
    predecessor: { header: { endTime: 0n } },
    context: {
      customNetwork: { slotConfig: { slotLength: 1000 } },
      provider: { submitTx: submit },
      deployment: {
        manifest: { manifestId: saved.deploymentFingerprint },
        operatorLucid: {
          wallet: () => ({
            address: async () => address,
            getUtxos: async () => [funding],
          }),
          utxosAt: refresh,
          overrideUTxOs: override,
        },
        contracts: {},
        references: new Map(),
        chain: {
          now: () => 300,
          delaySlots,
          awaitLedgerTime: vi.fn(async () => {}),
        },
      },
    },
  } as unknown as JourneyFaultPreparationInput;
  return {
    directory,
    path,
    old,
    fresh,
    saved,
    build,
    refresh,
    readRecovery,
    submit,
    delaySlots,
    input,
  };
};

it.each(["expired", "invalidated"] as const)(
  "rebuilds a forced order only after authenticated %s and persists before broadcast",
  async (status) => {
    const f = await fixture();
    f.readRecovery.mockImplementationOnce(async (attempt) =>
      observation(attempt, status),
    );
    await expect(
      publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
    ).resolves.toEqual({ transactionId: f.fresh.txHash, outputIndex: 0n });
    expect(f.build).toHaveBeenCalledOnce();
    expect(f.refresh).toHaveBeenCalledTimes(2);
    expect(f.submit).toHaveBeenCalledExactlyOnceWith(f.fresh.signedCbor);
    expect(await readJourneyArtifact(f.path)).toMatchObject({
      ...f.fresh,
      confirmed: true,
      submittedTxCbor: "80",
    });
  },
);

it.each(["pending", "unknown"] as const)(
  "retains %s bytes without constructing another order",
  async (status) => {
    const f = await fixture();
    let finish!: () => void;
    const paused = new Promise<void>((resolve) => {
      finish = resolve;
    });
    let entered!: () => void;
    const enteredPromise = new Promise<void>((resolve) => {
      entered = resolve;
    });
    f.readRecovery.mockImplementationOnce(async (attempt) =>
      observation(attempt, status),
    );
    f.delaySlots.mockImplementationOnce(async () => {
      entered();
      await paused;
    });
    const work = publishJourneyForcedOrder(f.input, Buffer.from("80", "hex"));
    await enteredPromise;
    expect(f.build).not.toHaveBeenCalled();
    expect(f.submit).not.toHaveBeenCalled();
    expect(await readJourneyArtifact(f.path)).toEqual(f.saved);
    finish();
    await expect(work).resolves.toEqual(f.saved.metadata.txOrderId);
    expect(
      f.readRecovery.mock.calls.every(
        ([attempt]) => attempt.signedCbor === f.old.signedCbor,
      ),
    ).toBe(true);
  },
);

it("reauthenticates a previously confirmed order and propagates recovery transport failure", async () => {
  const f = await fixture(true);
  const failure = new Error("authenticated recovery transport unavailable");
  f.readRecovery.mockRejectedValueOnce(failure);
  await expect(
    publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
  ).rejects.toBe(failure);
  expect(f.readRecovery).toHaveBeenCalledWith(f.old);
  expect(f.build).not.toHaveBeenCalled();
  expect(f.submit).not.toHaveBeenCalled();
  expect(await readJourneyArtifact(f.path)).toEqual(f.saved);
  await expect(
    publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
  ).resolves.toEqual(f.saved.metadata.txOrderId);
});

it.each(["signedCommit", "commitTxHash"] as const)(
  "does not replace an order already bound by a fault's %s",
  async (field) => {
    const f = await fixture();
    await writeJourneyArtifact(join(f.directory, "staged.json"), {
      deploymentFingerprint: f.saved.deploymentFingerprint,
      [field]: field === "signedCommit" ? f.old : f.old.txHash,
    });
    f.readRecovery.mockImplementationOnce(async (attempt) =>
      observation(attempt, "expired"),
    );
    await expect(
      publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
    ).rejects.toThrow("signed or published fault commitment");
    expect(f.build).not.toHaveBeenCalled();
    expect(f.submit).not.toHaveBeenCalled();
    expect(await readJourneyArtifact(f.path)).toEqual(f.saved);
  },
);

it("publishes fresh persisted bytes before recovery and keeps an ambiguous submission for canonical reconciliation", async () => {
  const f = await fixture();
  await rm(f.path);
  f.submit.mockImplementationOnce(async (bytes) => {
    expect(await readJourneyArtifact(f.path)).toMatchObject({
      signedCbor: bytes,
      confirmed: false,
    });
    throw new Error("submission acknowledgement lost");
  });
  f.readRecovery.mockImplementation(async (attempt) => {
    expect(f.submit).toHaveBeenCalledExactlyOnceWith(f.fresh.signedCbor);
    return observation(attempt, "included");
  });
  await expect(
    publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
  ).resolves.toEqual({ transactionId: f.fresh.txHash, outputIndex: 0n });
  expect(f.build).toHaveBeenCalledOnce();
  expect(await readJourneyArtifact(f.path)).toMatchObject({
    ...f.fresh,
    confirmed: true,
  });
});

it("rejects a changed fresh submission hash without discarding its signed checkpoint", async () => {
  const f = await fixture();
  await rm(f.path);
  f.submit.mockResolvedValueOnce("ff".repeat(32));
  await expect(
    publishJourneyForcedOrder(f.input, Buffer.from("80", "hex")),
  ).rejects.toThrow(
    "Provider changed the signed forced-order transaction hash",
  );
  expect(f.readRecovery).not.toHaveBeenCalled();
  expect(await readJourneyArtifact(f.path)).toMatchObject({
    ...f.fresh,
    confirmed: false,
  });
});

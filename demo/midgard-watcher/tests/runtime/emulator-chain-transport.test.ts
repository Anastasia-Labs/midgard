import { readFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { createEmulatorChainTransport } from "../support/emulator-chain-transport.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticNativeTip,
  type SyntheticUserEventOriginFixture,
} from "../support/user-event-origin-fixture.js";

let deployment: SyntheticUserEventOriginFixture["deployment"];
beforeAll(async () => {
  // Only the native transport is under test; reuse the synthetic fixture's
  // script configuration while its activation frame carries a real wallet tx.
  const seed = await createSyntheticUserEventOriginFixture();
  deployment = seed.deployment;
  await seed.close();
}, 60_000);

const open = async (confirmationBlocksPerSubmission?: number) => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account]);
  const signedCbors = new Map<string, string>();
  const originalSubmit = emulator.submitTx.bind(emulator);
  const recordedSubmit = vi
    .spyOn(emulator, "submitTx")
    .mockImplementation(async (cbor) => {
      const txHash = await originalSubmit(cbor);
      signedCbors.set(txHash, cbor);
      return txHash;
    });
  const recorder = { signedCbors, restore: () => recordedSubmit.mockRestore() };
  const lucid = await Lucid(emulator, "Custom");
  emulator.awaitSlot(100);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const build = async (validForMs = 120_000) => {
    const completed = await lucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 3_000_000n })
      .validFrom(emulator.now() - 60_000)
      .validTo(emulator.now() + validForMs)
      .complete({ localUPLCEval: true });
    return await completed.sign.withWallet().complete();
  };
  const submitRecorded = async () => {
    const transaction = await build();
    const cbor = transaction.toTransaction().to_cbor_hex();
    const txHash = await emulator.submitTx(cbor);
    await emulator.awaitTx(txHash);
    return { txHash, cbor };
  };
  let native: SyntheticUserEventOriginFixture | undefined;
  try {
    const initialization = await submitRecorded();
    const inclusion = await emulator.getTransactionStatus(
      initialization.txHash,
    );
    if (
      inclusion.status !== "confirmed" ||
      inclusion.confirmation.slot === undefined
    )
      throw new Error("Initialization transaction was not actually included");
    const history = await submitRecorded();
    native = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
      blockSlotInterval: 20,
      published: {
        deployment,
        transactionCbor: initialization.cbor,
        inclusionSlot: inclusion.confirmation.slot,
        creatingTransactions: [],
      },
    });
    const growNativeTip = native.growNativeTip.bind(native);
    const grow = vi.spyOn(native, "growNativeTip");
    const chain = await createEmulatorChainTransport({
      emulator,
      native,
      recorder,
      initializationTxHash: initialization.txHash,
      ...(confirmationBlocksPerSubmission === undefined
        ? {}
        : { confirmationBlocksPerSubmission }),
    });
    return {
      emulator,
      native,
      recorder,
      chain,
      grow,
      growNativeTip,
      build,
      history,
      args: {
        emulator,
        native,
        recorder,
        initializationTxHash: initialization.txHash,
      },
      close: async () => {
        try {
          await chain.close();
        } finally {
          await native!.close();
          recorder.restore();
        }
      },
    };
  } catch (cause) {
    await native?.close();
    recorder.restore();
    throw cause;
  }
};

type NativeBlock = Readonly<{
  point: SyntheticNativeTip;
  nativeBlock: Readonly<{
    transactionIds: readonly string[];
    transactionCbors: readonly string[];
  }>;
}>;
const blocks = async (native: SyntheticUserEventOriginFixture) =>
  JSON.parse(
    await readFile(
      join(dirname(native.nativeChainSyncBinaryPath), "blocks.json"),
      "utf8",
    ),
  ) as NativeBlock[];

describe("emulator confirmation block transport", () => {
  it("pauses background ticks during construction without changing actual inclusion or confirmation growth", async () => {
    const owner = await open(40);
    try {
      const { chain, emulator, native, build, grow } = owner;
      chain.start({ intervalMs: 10, blocksPerTick: 1 });
      await chain.withPausedBackgroundGrowth(async () => {
        const startSlot = emulator.slot;
        const transaction = await build(40_000);
        const cbor = transaction.toTransaction().to_cbor_hex();
        const body = transaction.toTransaction().body();
        const end = body.ttl()!;
        body.free();
        expect(end).toBe(BigInt(startSlot + 40));
        // Cross several real timer ticks after the transaction has fixed its
        // validity interval. Each unpaused tick would consume 20 slots.
        await pause(50);
        expect(emulator.slot).toBe(startSlot);
        expect(grow).not.toHaveBeenCalled();

        const txHash = await emulator.submitTx(cbor);
        const inclusion = await emulator.getTransactionStatus(txHash);
        if (inclusion.status !== "confirmed")
          throw new Error("Submitted transaction was not confirmed");
        expect(inclusion.confirmation.slot).toBe(startSlot + 20);
        expect(BigInt(inclusion.confirmation.slot!)).toBeLessThan(end);
        const registry = await blocks(native);
        const position = registry.findIndex((block) =>
          block.nativeBlock.transactionIds.includes(txHash),
        );
        const accepted = registry[position]!;
        expect(accepted.point.slot).toBe(String(inclusion.confirmation.slot));
        expect(accepted.nativeBlock.transactionCbors).toEqual([cbor]);
        expect(registry.slice(position + 1)).toHaveLength(40);
        expect(grow).toHaveBeenCalledExactlyOnceWith(40, startSlot + 20);
      });
      await vi.waitFor(() =>
        expect(grow.mock.calls.some(([count]) => count === 1)).toBe(true),
      );
      chain.assertHealthy();
    } finally {
      await owner.close();
    }
  });

  it("drains pending growth before the producer starts and resumes ticks after a producer failure", async () => {
    const owner = await open();
    try {
      const { chain, emulator, grow, growNativeTip } = owner;
      let release!: () => void;
      let entered!: () => void;
      const pending = new Promise<void>((resolve) => {
        release = resolve;
      });
      const started = new Promise<void>((resolve) => {
        entered = resolve;
      });
      grow.mockImplementationOnce(async (...args) => {
        entered();
        await pending;
        return await growNativeTip(...args);
      });
      const before = emulator.slot;
      const growth = chain.grow();
      await started;
      let producerStarted = false;
      const failure = new Error("producer interrupted");
      chain.start({ intervalMs: 10, blocksPerTick: 1 });
      const producer = chain.withPausedBackgroundGrowth(async () => {
        producerStarted = true;
        expect(emulator.slot).toBeGreaterThan(before);
        const stableSlot = emulator.slot;
        await pause(30);
        expect(emulator.slot).toBe(stableSlot);
        throw failure;
      });
      const rejected = producer.catch((cause: unknown) => cause);
      await pause(20);
      expect(producerStarted).toBe(false);
      release();
      await growth;
      expect(await rejected).toBe(failure);
      const after = emulator.slot;
      await vi.waitFor(() => expect(emulator.slot).toBeGreaterThan(after));
      chain.assertHealthy();
    } finally {
      await owner.close();
    }
  });

  it.each([undefined, 0, 40])(
    "preserves exact inclusion with confirmation growth %s",
    async (count) => {
      const owner = await open(count);
      try {
        const { emulator, chain, native, grow, build, history } = owner;
        // Previously confirmed history is imported without acceleration.
        expect(grow).not.toHaveBeenCalled();
        expect(
          (await blocks(native)).at(-1)?.nativeBlock.transactionIds,
        ).toEqual([history.txHash]);
        for (let submission = 0; submission < 2; submission++) {
          const clockBeforeBuild = emulator.slot;
          const transaction = await build();
          const body = transaction.toTransaction().body();
          try {
            expect(body.validity_interval_start()).toBe(
              BigInt(Math.max(0, clockBeforeBuild - 60)),
            );
            expect(body.ttl()).toBe(BigInt(clockBeforeBuild + 120));
          } finally {
            body.free();
          }
          const cbor = transaction.toTransaction().to_cbor_hex();
          const txHash = await emulator.submitTx(cbor);
          const inclusion = await emulator.getTransactionStatus(txHash);
          if (inclusion.status !== "confirmed")
            throw new Error("Submitted transaction was not confirmed");
          const registry = await blocks(native);
          const position = registry.findIndex((block) =>
            block.nativeBlock.transactionIds.includes(txHash),
          );
          expect(position).toBeGreaterThanOrEqual(0);
          const accepted = registry[position]!;
          expect(accepted.nativeBlock.transactionIds).toEqual([txHash]);
          expect(accepted.nativeBlock.transactionCbors).toEqual([cbor]);
          expect(accepted.point.slot).toBe(String(inclusion.confirmation.slot));
          const successors = registry.slice(position + 1);
          expect(successors).toHaveLength(count ?? 0);
          expect(
            successors.every(
              (block) =>
                block.nativeBlock.transactionIds.length === 0 &&
                block.nativeBlock.transactionCbors.length === 0,
            ),
          ).toBe(true);
          expect(
            BigInt(chain.tip().blockNo) - BigInt(accepted.point.blockNo),
          ).toBe(BigInt(count ?? 0));
          expect(BigInt(chain.tip().slot) - BigInt(accepted.point.slot)).toBe(
            BigInt((count ?? 0) * 20),
          );
          expect(emulator.slot).toBe(Number(chain.tip().slot));
          expect(await emulator.getTransactionStatus(txHash)).toEqual(
            inclusion,
          );
        }
        expect(grow).toHaveBeenCalledTimes((count ?? 0) === 0 ? 0 : 2);
        chain.assertHealthy();
      } finally {
        await owner.close();
      }
    },
  );

  it("rejects invalid growth counts before changing the active transport", async () => {
    const owner = await open();
    try {
      const submit = owner.emulator.submitTx;
      const tip = owner.chain.tip();
      for (const count of [
        -1,
        0.5,
        101,
        Number.NaN,
        Number.POSITIVE_INFINITY,
      ]) {
        await expect(
          createEmulatorChainTransport({
            ...owner.args,
            confirmationBlocksPerSubmission: count,
          }),
        ).rejects.toThrow("integer from 0 to 100");
        expect(owner.emulator.submitTx).toBe(submit);
        expect(owner.chain.tip()).toEqual(tip);
      }
      expect(owner.grow).not.toHaveBeenCalled();
    } finally {
      await owner.close();
    }
  });

  it("retains the transport failure if empty-block growth fails after real acceptance", async () => {
    const owner = await open(40);
    const failure = new Error("native growth interrupted");
    owner.grow.mockRejectedValueOnce(failure);
    try {
      const signed = await owner.build();
      const cbor = signed.toTransaction().to_cbor_hex();
      await expect(owner.emulator.submitTx(cbor)).rejects.toBe(failure);
      expect(
        await owner.emulator.getTransactionStatus(signed.toHash()),
      ).toMatchObject({ status: "confirmed" });
      expect(() => owner.chain.assertHealthy()).toThrow(failure);
      const submissions = owner.recorder.signedCbors.size;
      await expect(owner.emulator.submitTx(cbor)).rejects.toBe(failure);
      expect(owner.recorder.signedCbors.size).toBe(submissions);
    } finally {
      await expect(owner.close()).rejects.toBe(failure);
    }
  });
});

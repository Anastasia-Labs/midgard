import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { submitPublishedInitialization } from "./helpers/published-workflow-deployment.js";

it("records initialization before node I/O and resumes an accepted transaction after acknowledgement loss", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const [nonce] = await lucid.wallet().getUtxos();
  const tx = await lucid
    .newTx()
    .collectFrom([nonce!])
    .pay.ToAddress(account.address, { lovelace: 100_000_000n })
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  let recorded: string | undefined;
  let submissions = 0;
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    expect(recorded).toBe(cbor);
    submissions += 1;
    await submit(cbor);
    throw new Error(
      "Node accepted initialization but acknowledgement was lost",
    );
  };
  const params = {
    lucid,
    nonce: nonce!,
    signedCbor,
    onPrepared: (cbor: string) => {
      recorded = cbor;
    },
    synchronize: async () => emulator.slot,
  };
  const txHash = await submitPublishedInitialization(params);
  expect(recorded).toBe(signedCbor);
  expect(submissions).toBe(1);
  expect(await lucid.utxosByOutRef([nonce!])).toEqual([]);
  expect(await submitPublishedInitialization(params)).toBe(txHash);
  expect(submissions).toBe(1);
});

it("does not submit initialization when persisting its signed identity fails", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const [nonce] = await lucid.wallet().getUtxos();
  const tx = await lucid
    .newTx()
    .collectFrom([nonce!])
    .pay.ToAddress(account.address, { lovelace: 100_000_000n })
    .complete({ localUPLCEval: true });
  const signedCbor = (await tx.sign.withWallet().complete()).toCBOR();
  await expect(
    submitPublishedInitialization({
      lucid,
      nonce: nonce!,
      signedCbor,
      onPrepared: () => {
        throw new Error("journal write failed");
      },
      synchronize: async () => emulator.slot,
    }),
  ).rejects.toThrow("journal write failed");
  expect(await lucid.utxosByOutRef([nonce!])).toHaveLength(1);
});

it("waits against authoritative Ogmios slots and resubmits identical initialization bytes after a too-early rejection", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const [nonce] = await lucid.wallet().getUtxos();
  const tx = await lucid
    .newTx()
    .collectFrom([nonce!])
    .pay.ToAddress(account.address, { lovelace: 100_000_000n })
    .validFrom(lucid.slotToUnixTime(1))
    .validTo(lucid.slotToUnixTime(10))
    .complete({ localUPLCEval: true });
  const signedCbor = (await tx.sign.withWallet().complete()).toCBOR();
  const submitted: string[] = [];
  const waits: number[] = [];
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    submitted.push(cbor);
    if (submitted.length === 1)
      throw {
        code: 3118,
        data: {
          validityInterval: { invalidBefore: 1, invalidAfter: 10 },
          currentSlot: 0,
        },
      };
    return submit(cbor);
  };
  await submitPublishedInitialization({
    lucid,
    nonce: nonce!,
    signedCbor,
    onPrepared: () => {},
    synchronize: async () => emulator.slot,
    waitForRetry: async (milliseconds) => {
      waits.push(milliseconds);
      emulator.awaitSlot(milliseconds / 1000);
    },
  });
  expect(submitted).toEqual([signedCbor, signedCbor]);
  expect(waits).toEqual([3000]);
  expect(await lucid.utxosByOutRef([nonce!])).toEqual([]);
});

it("does not retry a known validity rejection beyond the recorded initialization TTL", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const [nonce] = await lucid.wallet().getUtxos();
  const tx = await lucid
    .newTx()
    .collectFrom([nonce!])
    .pay.ToAddress(account.address, { lovelace: 100_000_000n })
    .validFrom(lucid.slotToUnixTime(1))
    .validTo(lucid.slotToUnixTime(3))
    .complete({ localUPLCEval: true });
  const signedCbor = (await tx.sign.withWallet().complete()).toCBOR();
  let attempts = 0;
  emulator.submitTx = async () => {
    attempts += 1;
    throw {
      code: 3118,
      data: {
        validityInterval: { invalidBefore: 1, invalidAfter: 100 },
        currentSlot: 0,
      },
    };
  };
  await expect(
    submitPublishedInitialization({
      lucid,
      nonce: nonce!,
      signedCbor,
      onPrepared: () => {},
      synchronize: async () => emulator.slot,
      waitForRetry: async () => {
        throw new Error("Must not wait past signed TTL");
      },
    }),
  ).rejects.toThrow(/recorded initialization validity/);
  expect(attempts).toBe(1);
  expect(await lucid.utxosByOutRef([nonce!])).toHaveLength(1);
});

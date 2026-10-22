/** The seeds of the plutus-execution block fixture: the deposits whose
 * canonical insertions are the outputs its transactions spend or read, and
 * the owner key that signs them (plutus-execution-block-fixture.ts). */
import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import {
  hashScriptWitness,
  makeMintPreimageCbor,
  nativeScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML } from "@lucid-evolution/lucid";

import type { LedgerEntry } from "../../src/ledger-at.js";

export const SEED_LOVELACE = 20_000_000n;
export const ownerKey = () =>
  CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x63));
export const ownerKeyHash = (): string => {
  const key = ownerKey();
  try {
    return computeHash28(Buffer.from(key.to_public().to_raw_bytes())).toString(
      "hex",
    );
  } finally {
    key.free();
  }
};

export type SeedCredential =
  | { PublicKeyCredential: [string] }
  | { ScriptCredential: [string] };

/** The deposit event whose canonical insertion is a scenario's seed. */
export const seedDepositEvent = (
  byte: number,
  paymentCredential: SeedCredential,
) =>
  ({
    id: {
      transactionId: byte.toString(16).padStart(2, "0").repeat(32),
      outputIndex: 0n,
    },
    info: {
      l2_address: {
        paymentCredential,
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: null,
    },
  }) satisfies SDK.DepositEvent;

/** A deposit's canonical ledger insertion, exactly as block production derives it. */
export const depositSeed = (
  byte: number,
  paymentCredential: SeedCredential,
): LedgerEntry => {
  const event = seedDepositEvent(byte, paymentCredential);
  const effect = deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: "Custom",
    eventId: event.id,
    l2Address: event.info.l2_address,
    l2NetworkId: event.info.l2_network_id,
    l2DatumCbor: null,
    originalAssets: { lovelace: SEED_LOVELACE },
  });
  const operation = effect.operations[0];
  if (operation?.type !== "insert")
    throw new Error("seed deposit has no canonical insertion");
  return { outRef: operation.outRefCbor, output: operation.outputCbor };
};

export const keyCredential = (): SeedCredential => ({
  PublicKeyCredential: [ownerKeyHash()],
});

/** A native script that always holds, and the deposit byte of the output it
 * holds, which sorts after every scenario's seed. */
export const NATIVE_SPEND_SCRIPT = nativeScriptWitness({
  type: "all",
  scripts: [],
});
const NATIVE_SPEND_SEED_BYTE = 0x66;

/** The asset a transaction mints under the native script's policy: the
 * value it adds to an output (none when it does not mint), and its mint
 * field. */
const NATIVE_MINT_ASSET = "4d";
export const nativeMintAssets = (nativeMint: boolean) =>
  new Map(
    nativeMint
      ? [
          [
            hashScriptWitness(NATIVE_SPEND_SCRIPT),
            new Map([[NATIVE_MINT_ASSET, 1n]]),
          ],
        ]
      : [],
  );
export const nativeMintPreimageCbor = () =>
  makeMintPreimageCbor(
    new Map(
      [...nativeMintAssets(true)].map(([policy, assets]) => [
        Buffer.from(policy, "hex"),
        new Map(
          [...assets].map(([name, quantity]) => [
            Buffer.from(name, "hex"),
            quantity,
          ]),
        ),
      ]),
    ),
  );
const nativeSpendCredential = (): SeedCredential => ({
  ScriptCredential: [hashScriptWitness(NATIVE_SPEND_SCRIPT)],
});

/** The output the native script holds, which a failing spend can also
 * spend, and its deposit, as a committed deposit block carries it. */
export const nativeSpendSeed = () =>
  depositSeed(NATIVE_SPEND_SEED_BYTE, nativeSpendCredential());
export const nativeSpendSeedDeposit = () => ({
  event: seedDepositEvent(NATIVE_SPEND_SEED_BYTE, nativeSpendCredential()),
  originalAssets: { lovelace: SEED_LOVELACE },
  honest: true,
});

/** The deposits of `count` key-owned outputs a transaction can read as
 * reference inputs, and those outputs' out-refs in ascending order. */
export const referenceSeedDeposits = (count: number) => {
  const transactionId = "72".repeat(32);
  const deposits = Array.from({ length: count }, (_, index) => ({
    event: {
      ...seedDepositEvent(0x72, keyCredential()),
      id: { transactionId, outputIndex: BigInt(index) },
    },
    originalAssets: { lovelace: SEED_LOVELACE },
    honest: true,
  }));
  const outRefs = deposits.map(({ event }) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.from(transactionId, "hex"),
      outputIndex: Number(event.id.outputIndex),
    }),
  );
  return { deposits, outRefs };
};

/** The seeds of the plutus-execution block fixture: the deposits whose
 * canonical insertions are the outputs its transactions spend or read, and
 * the owner key that signs them (plutus-execution-block-fixture.ts). */
import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import { protectMidgardAddress } from "@al-ft/midgard-core/codec/address";
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

/** The canonical ledger insertion of a deposit of `lovelace`, exactly as
 * block production derives it. */
export const depositSeed = (
  byte: number,
  paymentCredential: SeedCredential,
  lovelace: bigint = SEED_LOVELACE,
): LedgerEntry => {
  const event = seedDepositEvent(byte, paymentCredential);
  const effect = deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: "Custom",
    eventId: event.id,
    l2Address: event.info.l2_address,
    l2NetworkId: event.info.l2_network_id,
    l2DatumCbor: null,
    originalAssets: { lovelace },
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

/** What a transaction mints: how many policies, the first the native
 * script's and each other one an always-satisfied native script of its own
 * (`mintPolicyScript`), and how many assets under each, under the last
 * `lastAssets` when given. With `shortNames`, each policy names its assets
 * from the shortest names up. `true` mints one asset under the native
 * script's policy. */
export type ScenarioMint =
  | boolean
  | Readonly<{
      policies?: number;
      assets?: number;
      lastAssets?: number;
      shortNames?: boolean;
    }>;

/** The `index`th minting policy beyond the native script's, which any
 * transaction satisfies. */
export const mintPolicyScript = (index: number) =>
  nativeScriptWitness({
    type: "atLeast",
    required: 0n,
    scripts: [{ type: "after", slot: BigInt(MINT_POLICY_SLOT_BASE + index) }],
  });
const MINT_POLICY_SLOT_BASE = 1_000_000;

/** The native scripts whose policies `mint` mints under, the native script
 * `script` first. */
export const mintPolicies = (
  mint: ScenarioMint,
  script: MidgardVersionedScript = NATIVE_SPEND_SCRIPT,
): readonly MidgardVersionedScript[] =>
  mint === false
    ? []
    : [
        script,
        ...Array.from(
          { length: (mint === true ? 1 : (mint.policies ?? 1)) - 1 },
          (_, index) => mintPolicyScript(index),
        ),
      ];

/** The name of the `index`th asset a policy mints: one byte for the first,
 * two past it; or, with `short`, the empty name first, then each one-byte
 * name, then two-byte ones. */
const NATIVE_MINT_ASSET = "4d";
const mintAssetName = (index: number, short: boolean) =>
  short
    ? index === 0
      ? ""
      : index <= 256
        ? (index - 1).toString(16).padStart(2, "0")
        : index.toString(16).padStart(4, "0")
    : index === 0
      ? NATIVE_MINT_ASSET
      : index.toString(16).padStart(4, "0");

/** How many assets `mint` mints under its `index`th policy of `count`. */
const policyAssets = (mint: ScenarioMint, index: number, count: number) =>
  mint === true || mint === false
    ? 1
    : index === count - 1
      ? (mint.lastAssets ?? mint.assets ?? 1)
      : (mint.assets ?? 1);

/** The assets `mint` mints under the native script `script`'s policy and
 * the others: the value it adds to an output (none when it does not mint),
 * and its mint field. */
export const nativeMintAssets = (
  mint: ScenarioMint,
  script: MidgardVersionedScript = NATIVE_SPEND_SCRIPT,
) => {
  const policies = mintPolicies(mint, script);
  const short = typeof mint === "object" && mint.shortNames === true;
  return new Map(
    policies.map((policy, at) => [
      hashScriptWitness(policy),
      new Map(
        Array.from(
          { length: policyAssets(mint, at, policies.length) },
          (_, index) => [mintAssetName(index, short), 1n] as const,
        ),
      ),
    ]),
  );
};
export const nativeMintPreimageCbor = (
  mint: ScenarioMint,
  script: MidgardVersionedScript = NATIVE_SPEND_SCRIPT,
) =>
  makeMintPreimageCbor(
    new Map(
      [...nativeMintAssets(mint, script)].map(([policy, assets]) => [
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

/** The protected address whose payment credential is `script`. */
export const protectedScriptAddress = (script: MidgardVersionedScript) =>
  protectMidgardAddress(
    Buffer.from(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(
          CML.ScriptHash.from_hex(hashScriptWitness(script)),
        ),
      )
        .to_address()
        .to_raw_bytes(),
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

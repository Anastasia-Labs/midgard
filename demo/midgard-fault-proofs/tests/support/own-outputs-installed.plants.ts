/**
 * The plants of the installed own-outputs tests (`own-outputs-installed.ts`):
 * transactions of another party that put outputs at the addresses the
 * application's own carriage goes to, or take one of ours off the chain.
 */
import { startScriptDecodeWalk } from "@al-ft/midgard-validation/script-decode.step-walk";
import {
  Data,
  type Emulator,
  getAddressDetails,
  type LucidEvolution,
  mintingPolicyToId,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";

import { spendProofMaterials } from "../../src/funding/proof-materials-address.js";
import type { plutusItemScript } from "../../src/script-decode/item-record.js";
import { stageScriptDecodeBytes } from "../../src/script-decode/stage.js";
import { bindScriptDecodeRun } from "../../src/script-decode/submit.js";
import type { LedgerWatch, OwnOutputRole } from "./own-outputs-installed.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { honestScript } from "./script-decode-plutus-max-shape.js";

/**
 * A Plutus V3 script of over 2 KB, carried as a reference script by a shape
 * plant: a transaction that read the plant would pay its reference-script
 * fee (15 lovelace a byte), well past the fee spread the cost check allows.
 */
const REFERENCE_SCRIPT = honestScript(2_048).toString("hex");

/** Outputs per transaction of the mass plant. */
const MASS_BATCH = 25;

/** Distinct tokens in the token-bearing coin plant. */
const TOKEN_BUNDLE = 40;

const submitAs = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  build: (lucid: LucidEvolution) => ReturnType<LucidEvolution["newTx"]>,
): Promise<string> =>
  await watch.asOtherParty(async () => {
    lucid.clearUTxOOverride();
    const unsigned = await build(lucid).complete();
    const signed = await unsigned.sign.withWallet().complete();
    return await signed.submit();
  });

/** A native-script token the other party can mint. */
const otherPartyToken = (address: string) => {
  const keyHash = getAddressDetails(address).paymentCredential!.hash;
  const policy = scriptFromNative({ type: "sig", keyHash });
  return { policy, unit: `${mintingPolicyToId(policy)}cafe` };
};

/**
 * Outputs holding `role`'s datums in other shapes: with a token, with a
 * reference script, and as a datum hash.
 */
export const plantShapes = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
) => {
  const { policy, unit } = otherPartyToken(await lucid.wallet().address());
  await submitAs(watch, lucid, (other) => {
    let tx = other
      .newTx()
      .mintAssets({ [unit]: BigInt(role.datums.length) })
      .attach.MintingPolicy(policy);
    for (const datum of role.datums)
      tx = tx.pay
        .ToAddressWithData(
          role.address,
          { kind: "inline", value: datum },
          { [unit]: 1n },
        )
        .pay.ToAddressWithData(
          role.address,
          { kind: "inline", value: datum },
          {},
          { type: "PlutusV3", script: REFERENCE_SCRIPT },
        )
        .pay.ToAddressWithData(
          role.address,
          { kind: "asHash", value: datum },
          {},
        );
    return tx;
  });
};

/**
 * One coin at `address` with no datum and no script, more ada than the
 * largest coin there, and a bundle of `TOKEN_BUNDLE` tokens: the coin a
 * wallet's coin selection would reach for first were tokens no bar.
 */
export const plantTokenCoin = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  address: string,
) => {
  const { policy } = otherPartyToken(await lucid.wallet().address());
  const policyId = mintingPolicyToId(policy);
  const bundle: Record<string, bigint> = {};
  for (let index = 0; index < TOKEN_BUNDLE; index += 1)
    bundle[`${policyId}${index.toString(16).padStart(4, "0")}`] = 1n;
  const largest = (await lucid.utxosAt(address)).reduce(
    (max, { assets }) => (assets.lovelace! > max ? assets.lovelace! : max),
    0n,
  );
  await submitAs(watch, lucid, (other) =>
    other
      .newTx()
      .mintAssets(bundle)
      .attach.MintingPolicy(policy)
      .pay.ToAddress(address, {
        lovelace: largest + 1_000_000_000n,
        ...bundle,
      }),
  );
};

/** Exact copies of `role`'s outputs: the same datums, ada only, inline. */
export const plantCopies = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
) => {
  await submitAs(watch, lucid, (other) => {
    let tx = other.newTx();
    for (const datum of role.datums)
      tx = tx.pay.ToAddressWithData(
        role.address,
        { kind: "inline", value: datum },
        {},
      );
    return tx;
  });
};

/**
 * An exact copy of `datum` at `address` whose transaction id sorts before
 * `below`: submitted ahead of the transaction it copies, into the same block.
 */
export const plantCopyBefore = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  { address, datum, below }: { address: string; datum: string; below: string },
) => {
  lucid.clearUTxOOverride();
  for (let attempt = 0; attempt < 4_096; attempt += 1) {
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        { kind: "inline", value: datum },
        { lovelace: 20_000_000n + BigInt(attempt) },
      )
      .complete();
    const signed = await unsigned.sign.withWallet().complete();
    if (signed.toHash() < below) {
      return await watch.asOtherParty(async () => await signed.submit());
    }
  }
  throw new Error("no copy sorting first was found");
};

/**
 * `count` datum-bearing outputs at `role`'s address, in transactions of
 * `MASS_BATCH`, a block each.
 */
export const plantMass = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
  { emulator, count }: { emulator: Emulator; count: number },
) => {
  for (let start = 0; start < count; start += MASS_BATCH) {
    await submitAs(watch, lucid, (other) => {
      let tx = other.newTx();
      for (
        let index = start;
        index < Math.min(count, start + MASS_BATCH);
        index += 1
      )
        tx = tx.pay.ToAddressWithData(
          role.address,
          { kind: "inline", value: Data.to(BigInt(index)) },
          {},
        );
      return tx;
    });
    emulator.awaitBlock();
  }
};

/**
 * The other party's own record of the same script, begun: it stages the
 * bytes at its own address and Binds a run of them.
 */
export const plantForeignRun = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  {
    emulator,
    script,
  }: {
    emulator: Emulator;
    script: NonNullable<ReturnType<typeof plutusItemScript>>;
  },
): Promise<UTxO> =>
  await watch.asOtherParty(async () => {
    const staging = await stageScriptDecodeBytes({
      lucid,
      scriptBytes: script.scriptBytes,
    });
    return await bindScriptDecodeRun({
      lucid,
      deployment: scriptDecodeDeploymentOf(emulator),
      language: script.language,
      scriptHashHex: script.scriptHashHex,
      initialRun: startScriptDecodeWalk(script.language, script.scriptBytes)
        .run,
      staging,
    });
  });

/** Takes our coin or proof material `utxo` off the chain, ada to `to`. */
export const removeOutput = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  utxo: UTxO,
  to: string,
) => {
  await watch.asOtherParty(async () => {
    lucid.clearUTxOOverride();
    const wallet = getAddressDetails(await lucid.wallet().address());
    const spend = lucid.newTx().collectFrom([utxo]);
    const unsigned = await (
      utxo.address === wallet.address.bech32
        ? spend
        : spendProofMaterials(spend, wallet.paymentCredential!.hash)
    ).complete({
      presetWalletInputs: [utxo],
      coinSelection: false,
      changeAddress: to,
    });
    await (await unsigned.sign.withWallet().complete()).submit();
    lucid.clearUTxOOverride();
  });
};

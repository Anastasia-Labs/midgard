import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  Emulator,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

/**
 * **The adversarial route matrix for the Option B complete-item chain (#621),
 * against the applied validators** — hostile probes red, honest continuations
 * green, on one emulator ledger.
 *
 * Since Option B (#619/#620) the committed `evidence_hash` is transition-only
 * and the observe stage's §8.8 door is the sole content gate, so two things
 * become claims that need falsifiers rather than assumptions:
 *
 * 1. **The door really is a gate.** Corrupted inline bytes, a publication
 *    whose commitment binding is wrong, and a publication whose preimage is
 *    not the committed field's are each refused by the applied validator —
 *    and then the same machinery passes with honest material, so the reds
 *    are attributable to the mutation and nothing else.
 * 2. **The routes and the drivers are interchangeable.** The staged datums
 *    are route-independent, so the inline door and the reference door must
 *    write byte-identical observations — and because `continue()` ignores
 *    `fraud_prover`, a third party who is not the prover must be able to
 *    drive a stage to that same state with valid data (and must fail with
 *    invalid data). Both are proved here by driving two threads over the
 *    same content, one per route, one stage by a non-prover wallet.
 *
 * The retired wires are replayed too: the pre-#620 four-field `Verify`
 * (transition + carriage) and a thread whose datum still commits the old
 * two-part `(transition, auxiliary)` evidence hash are both refused at the
 * authenticate boundary — the pins that Option B's commitment change is
 * enforced on chain, not merely spoken off chain.
 *
 * Harness notes: like `complete-item-carriage-tiers-emulator.test.ts` the
 * computation-thread tokens are seeded (thread authenticity is the
 * fault-proofs lifecycle suites' job); unlike it, everything here speaks the
 * Option B wire, so against a pre-Option-B blueprint this file skips loudly
 * (see the gate below) instead of manufacturing the recorded rows'
 * unfalsifiable `Spend[0]` red.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

export const blueprintJson = JSON.parse(
  readFileSync(blueprintPath, "utf8"),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
    readonly parameters?: readonly { readonly title: string }[];
  }[];
};

const ITEM_SEMANTIC_SPEND_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend";

/**
 * The Option B gate (#621): #620 removed the carriage parameter from
 * `canonical_decode_item_semantic_v1`, taking its declared parameter list
 * from three entries to two. Against the three-parameter deployed build every
 * journey here would red out exactly like the recorded expected-red rows in
 * the fault-proofs suite, proving nothing — so skip, and say why.
 */
export const blueprintSpeaksOptionB = (() => {
  const itemSemantic = blueprintJson.validators.find(
    (validator) => validator.title === ITEM_SEMANTIC_SPEND_TITLE,
  );
  if (itemSemantic === undefined) {
    throw new Error(
      `blueprint has no "${ITEM_SEMANTIC_SPEND_TITLE}" validator to probe`,
    );
  }
  return (itemSemantic.parameters ?? []).length === 2;
})();

// ## Emulator harness — two wallets, three seeded threads

export type Harness = {
  readonly emulator: Emulator;
  readonly proverLucid: LucidEvolution;
  readonly thirdPartyLucid: LucidEvolution;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly threadUnit: string;
};

export const walletAddress = (key: CML.PrivateKey): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  )
    .to_address()
    .to_bech32();

export const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

export const submitAndAwait = async (
  lucid: LucidEvolution,
  unsigned: Awaited<
    ReturnType<ReturnType<LucidEvolution["newTx"]>["complete"]>
  >,
): Promise<{ readonly txHash: string; readonly signedCbor: string }> => {
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return { txHash, signedCbor };
};

const feeInputFor = async (
  lucid: LucidEvolution,
  threadUnit: string,
): Promise<UTxO> => {
  const candidates = (await lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[threadUnit] === undefined,
  );
  return candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
};

// ## Stage submission, parameterised by the driving wallet

type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

export const submitStage = async ({
  harness,
  driver,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  extraReferences,
}: {
  readonly harness: Harness;
  /** Who builds, funds, and signs — the prover or the third party. */
  readonly driver: { readonly lucid: LucidEvolution; readonly hash: string };
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex: (target: UTxO) => bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly extraReferences?: readonly UTxO[];
}): Promise<{
  readonly nextThreadUtxo: UTxO;
  readonly signedBytes: number;
}> => {
  const makeRedeemer: BuildTxWithRedeemer = (ctx) =>
    encode({
      inputIndex: requireInputIndex(ctx, inputUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          output.address === outputContract.spendingScriptAddress &&
          output.datum != null &&
          sameDatumValue(output.datum, outputDatum) &&
          output.assets[harness.threadUnit] === 1n,
        label,
      ),
      referenceInputIndex: (target) =>
        requireReferenceInputIndex(ctx, target, label),
    });
  let tx = driver.lucid
    .newTx()
    .collectFrom([await feeInputFor(driver.lucid, harness.threadUnit)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (extraReferences !== undefined && extraReferences.length > 0) {
    tx = tx.readFrom([...extraReferences]);
  }
  tx = tx.pay
    .ToContract(
      outputContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: inputUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(driver.hash);
  if (scriptReference === undefined) {
    tx = tx.attach.SpendingValidator(inputContract.spendingScript);
  }
  let unsigned;
  try {
    unsigned = await tx.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(
      `${label} local evaluation failed: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  const { txHash, signedCbor } = await submitAndAwait(driver.lucid, unsigned);
  const nextThreadUtxo = (
    await driver.lucid.utxosAt(outputContract.spendingScriptAddress)
  ).find(
    (utxo) => utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (nextThreadUtxo === undefined) {
    throw new Error(`${label} did not hand the thread on`);
  }
  return { nextThreadUtxo, signedBytes: signedCbor.length / 2 };
};

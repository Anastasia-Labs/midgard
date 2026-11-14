/**
 * A spend back of a staging output other than the one its builder built,
 * presented to the prover funding permit: the builder's signed spend back
 * with its body edited (an input added, an output paid elsewhere, a mint),
 * signed again by the prover and granted its staging authority again, or
 * also another funding authority; or, in place of our spend back, a spend
 * back of an output at our staging address that no staging of ours left.
 * Bytes the permit accepts reach the ledger as presented.
 */
import {
  buildUnsignedMidgardCekStagingReclaimProgram,
  MidgardCekStagingPins,
  midgardCekStagingScript,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Lucid,
  type TxSigned,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../../src/validation-trace-dispute/one-step-staging-journal.js";
import { validationTraceReclaimAction } from "../../src/validation-trace-dispute/one-step-staging-sweep.js";
import {
  grantValidationDisputeStagingAuthority,
  readValidationDisputeStagingAuthority,
} from "../../src/workflow/funding-reservation-permit.validation-dispute-staging-authority.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
} from "../../src/workflow/orchestrator.js";
import { parseOwnOutRef } from "../../src/workflow/own-outputs.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  workflowPreflightTransaction,
} from "../../src/workflow/transaction-boundary.js";
import { rebodied } from "./validation-trace-dispute-funded-move-walk.js";
import type { AdapterWrap } from "./validation-trace-dispute-funded-runner.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";
import { withCoin } from "./validation-trace-dispute-tampered-verify-source.js";

/** What an edited spend back pays in fee above the built one, taken from
 * its output to our wallet, so the larger body still meets the minimum. */
const FEE_BUMP = 20_000n;

/** Rewrites the spend back's body, given our wallet's address; it spends
 * the staging output and pays our wallet its one output. */
export type ReclaimEdit = (
  body: CML.TransactionBody,
  wallet: string,
) => CML.TransactionBody;

/** How a presented spend back differs from the built one. */
export type ReclaimPresentation = Readonly<{
  edit?: ReclaimEdit;
  /** Grants another funding authority over the presented bytes as well. */
  beside?: (signed: TxSigned, staging: UTxO) => void;
}>;

const isReclaim = (action: FraudProofWorkflowAction) =>
  action.input.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM;

/** `body`'s one output (to our wallet) less `less` lovelace. */
const walletOutputLess = (body: CML.TransactionBody, less: bigint) => {
  const outputs = body.outputs();
  if (outputs.len() !== 1) throw new Error("the spend back pays no one output");
  return withCoin(outputs.get(0), outputs.get(0).amount().coin() - less);
};

const listOf = (outputs: readonly CML.TransactionOutput[]) => {
  const list = CML.TransactionOutputList.new();
  for (const output of outputs) list.add(output);
  return list;
};

/** The spend back also spending `input`, its lovelace paid to our wallet. */
export const spendingAlso =
  (input: UTxO): ReclaimEdit =>
  (body) => {
    const inputs = CML.TransactionInputList.new();
    for (let index = 0; index < body.inputs().len(); index += 1)
      inputs.add(body.inputs().get(index));
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(input.txHash),
        BigInt(input.outputIndex),
      ),
    );
    return rebodied(body, {
      inputs,
      outputs: listOf([
        walletOutputLess(body, FEE_BUMP - (input.assets.lovelace ?? 0n)),
      ]),
      fee: body.fee() + FEE_BUMP,
    });
  };

/** The spend back paying its exact min-Ada to `address` from our wallet's
 * output. */
export const payingAlso =
  (address: string, coinsPerUtxoByte: bigint): ReclaimEdit =>
  (body) => {
    let paid = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(0n),
    );
    // The minimum grows with the coin's own encoding; settle on its fixpoint.
    while (
      paid.amount().coin() !== CML.min_ada_required(paid, coinsPerUtxoByte)
    )
      paid = withCoin(paid, CML.min_ada_required(paid, coinsPerUtxoByte));
    return rebodied(body, {
      outputs: listOf([
        walletOutputLess(body, FEE_BUMP + paid.amount().coin()),
        paid,
      ]),
      fee: body.fee() + FEE_BUMP,
    });
  };

/** The spend back minting one token under our staging script, paid to our
 * wallet with its output. */
export const minting =
  (fundingPaymentKeyHash: string): ReclaimEdit =>
  (body) => {
    const policy = CML.ScriptHash.from_hex(
      validatorToScriptHash(midgardCekStagingScript(fundingPaymentKeyHash)),
    );
    const name = CML.AssetName.from_hex("00");
    const mint = CML.Mint.new();
    mint.set(policy, name, 1n);
    const output = walletOutputLess(body, FEE_BUMP);
    const assets = CML.MultiAsset.new();
    assets.set(policy, name, 1n);
    const next = rebodied(body, {
      outputs: listOf([
        CML.TransactionOutput.new(
          output.address(),
          CML.Value.new(output.amount().coin(), assets),
          output.datum() ?? null,
          output.script_ref() ?? null,
        ),
      ]),
      fee: body.fee() + FEE_BUMP,
    });
    next.set_mint(mint);
    return next;
  };

/** `body` under `signed`'s witnesses less their signatures, signed again by
 * the prover. */
const signedAgain = async (
  scene: Scene,
  signed: TxSigned,
  body: CML.TransactionBody,
): Promise<TxSigned> => {
  const transaction = signed.toTransaction();
  const witnesses = CML.TransactionWitnessSet.from_cbor_hex(
    transaction.witness_set().to_cbor_hex(),
  );
  witnesses.set_vkeywitnesses(CML.VkeywitnessList.new());
  return await (
    await proverLucid(scene)
  )
    .fromTx(
      CML.Transaction.new(
        body,
        witnesses,
        true,
        transaction.auxiliary_data(),
      ).to_cbor_hex(),
    )
    .sign.withWallet()
    .complete();
};

/** A Lucid over the emulator holding the prover's whole wallet. */
const proverLucid = async ({ h }: Scene) => {
  const lucid = await Lucid(h.emulator, "Custom", {
    slotConfig: h.proverLucid.config().slotConfig,
  });
  h.proverSigner.selectWallet(lucid);
  return lucid;
};

/** The preflight presenting `signed` in place of `preflight`'s bytes. */
const presenting = (
  preflight: FraudProofWorkflowPreflight,
  signed: TxSigned,
): FraudProofWorkflowPreflight =>
  bindWorkflowPreflightTransaction(
    {
      ...preflight,
      txHash: signed.toHash().toLowerCase(),
      durableRecovery: {
        ...preflight.durableRecovery,
        signedTransactionCborHex: signed.toCBOR(),
      },
    },
    signed,
  );

/**
 * Presents a spend back of ours edited as `presentation` says, granted its
 * staging authority again over the presented bytes.
 */
export const presentReclaim =
  (scene: Scene, presentation: ReclaimPresentation): AdapterWrap =>
  (adapter) => {
    const presented = new Map<string, TxSigned>();
    return {
      ...adapter,
      preflight: async (context) => {
        const preflight = await adapter.preflight(context);
        if (!isReclaim(context.action)) return preflight;
        const signed = workflowPreflightTransaction(preflight)!;
        const { shape } = readValidationDisputeStagingAuthority(signed)!;
        if (shape.kind !== "reclaim_staging")
          throw new Error("the spend back carries no spend back authority");
        const [staging] = await scene.h.emulator.getUtxosByOutRef([
          parseOwnOutRef(shape.outRef),
        ]);
        const body = CML.TransactionBody.from_cbor_hex(
          signed.toTransaction().body().to_cbor_hex(),
        );
        const again = await signedAgain(
          scene,
          signed,
          presentation.edit?.(body, scene.h.proverSigner.address) ?? body,
        );
        grantValidationDisputeStagingAuthority({
          signed: again,
          deploymentFingerprint:
            readValidationDisputeStagingAuthority(signed)!
              .deploymentFingerprint,
          headerHash: readValidationDisputeStagingAuthority(signed)!.headerHash,
          shape: { output: staging! },
        });
        presentation.beside?.(again, staging!);
        presented.set(again.toHash().toLowerCase(), again);
        return presenting(preflight, again);
      },
      submit: async (input) => {
        const signed = presented.get(input.preflight.txHash);
        if (signed === undefined) return await adapter.submit(input);
        return { kind: "submitted", txHash: await signed.submit() };
      },
    };
  };

/**
 * In place of the spend back our workflow takes next, a spend back of
 * `output` at our staging address, built and signed by the prover and
 * granted the staging authority over `output` under our deployment and
 * header.
 */
export const reclaimInstead =
  (
    scene: Scene,
    output: UTxO,
    terms: Readonly<{ deploymentFingerprint: string; headerHash: string }>,
  ): AdapterWrap =>
  (adapter) => {
    const action = validationTraceReclaimAction(
      `${output.txHash}#${output.outputIndex.toString()}`,
    );
    const presented = new Map<string, TxSigned>();
    return {
      ...adapter,
      observe: async (context) => {
        const observed = await adapter.observe(context);
        return observed.kind === "action_required" && isReclaim(observed.action)
          ? { kind: "action_required", action }
          : observed;
      },
      preflight: async (context) => {
        if (context.action.actionId !== action.actionId)
          return await adapter.preflight(context);
        const unsigned = await Effect.runPromise(
          buildUnsignedMidgardCekStagingReclaimProgram(
            await proverLucid(scene),
            { outputs: [output], pins: new MidgardCekStagingPins() },
          ),
        );
        const signed = await unsigned.sign.withWallet().complete();
        grantValidationDisputeStagingAuthority({
          signed,
          ...terms,
          shape: { output },
        });
        const txHash = signed.toHash().toLowerCase();
        presented.set(txHash, signed);
        return bindWorkflowPreflightTransaction(
          {
            actionId: action.actionId,
            txHash,
            scriptExecution: "none",
            localUplcEvaluation: {
              status: "passed",
              evaluator: LOCAL_UPLC_EVALUATOR,
            },
            referenceScripts: [],
            durableRecovery: { signedTransactionCborHex: signed.toCBOR() },
          },
          signed,
        );
      },
      submit: async (input) => {
        const signed = presented.get(input.preflight.txHash);
        if (signed === undefined) return await adapter.submit(input);
        return { kind: "submitted", txHash: await signed.submit() };
      },
    };
  };

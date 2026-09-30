import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import { buildMintAuthorizationStep02Evidence } from "../../src/mint-authorization/evidence.js";
import {
  requireMintAuthorizationReferenceScript,
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "../../src/mint-authorization/submit-common.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type MintAuthorizationHarness,
  publishRawFieldPreimageCarriage,
} from "./mint-authorization-emulator.build-mint-authorization-subject.js";
import { type DecodingBlockFixture } from "./native-script-decoding-emulator.js";

/**
 * The honest step-02 transaction shape with the mint door opened against a
 * caller-planted TAMPERED tier-2 publication. Every local twin is omitted, so
 * the recomputed §8.8 field commitment reaches the validator, which aborts
 * when it disagrees with the anchored committed slot.
 */
export const submitRawMintAuthorizationStep02TamperedMint = async ({
  harness,
  threadOutRef,
  block,
  tamperedPreimageBytes,
  referenceScriptUtxo,
}: {
  readonly harness: MintAuthorizationHarness;
  readonly threadOutRef: string;
  readonly block: DecodingBlockFixture;
  readonly tamperedPreimageBytes: Buffer;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireMintAuthorizationThreadUtxo({
    lucid,
    contracts,
    categoryId: harness.category.categoryId,
    stepIndex: 1,
    threadOutRef,
  });
  const anchorState = requireMintAuthorizationStepState({
    threadUtxo,
    signer,
    schema: SDK.MintAuthorizationStep02Datum,
    stepIndex: 1,
  });
  const evidence = await buildMintAuthorizationStep02Evidence({
    reconstruction: block.reconstruction,
    eventKey: { L2TransactionEventKey: { tx_id: anchorState.bad_tx_id } },
  });
  const tamperedUtxo = await publishRawFieldPreimageCarriage({
    lucid,
    signer,
    bytes: tamperedPreimageBytes,
  });
  const referenceInputs = [
    tamperedUtxo,
    requireMintAuthorizationReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
  ];
  const step03State: SDK.MintAuthorizationStep03State = {
    policy_id: "ab".repeat(28),
    direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
    bad_tx_id: anchorState.bad_tx_id,
    bad_tx_witness_set_hash: anchorState.bad_tx_witness_set_hash,
    validity_interval_start: anchorState.validity_interval_start,
    validity_interval_end: anchorState.validity_interval_end,
    prior_ledger_root: evidence.transitionStepMembership.value.pre_utxos_root,
  };
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    SDK.MintAuthorizationStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = excludeUtxo(walletUtxos, tamperedUtxo);
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw mint-authorization step-02",
    );
    const refIndex = SDK.requireReferenceInputIndex(
      ctx,
      tamperedUtxo,
      "raw mint-authorization tampered publication",
    );
    const mintOpening = SDK.fieldOpeningForField({
      fieldIndex: SDK.MIDGARD_FIELD_INDEX.mint,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      carriage: { RawUtxo: { ref_input_index: refIndex } },
    });
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw mint-authorization step-02",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              step03OutputMatches,
              "raw mint-authorization step-02 output",
            ),
            header: block.reconstruction.header,
            event_to_step_membership: evidence.eventToStepMembership,
            transition_step_membership: evidence.transitionStepMembership,
            policy_index: 0n,
            direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
            mint_opening: mintOpening,
          },
        ],
      },
      SDK.MintAuthorizationStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({
      localUPLCEval: true,
      presetWalletInputs: walletUtxosSansCarriage as UTxO[],
    });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

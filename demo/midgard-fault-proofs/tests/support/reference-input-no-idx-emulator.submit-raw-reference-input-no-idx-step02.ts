import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  referenceInputNoIdxStep03StateFromBadInput,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireFaultProofStepReferenceScript,
  type ResolvedProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
} from "../../src/runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { type ReferenceInputNoIdxHarness } from "./reference-input-no-idx-emulator.build-reference-input-no-idx-block-fixture.js";
import {
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

/**
 * Publishes all four step validators as reference scripts, the deployment
 * shape the standing owner ruling requires: a fault proof is always read from
 * a published reference script, never inline-attached.
 */
export const publishReferenceInputNoIdxReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: ReferenceInputNoIdxHarness["contracts"]["fraudProofContracts"]["referenceInputNoIdx"];
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `reference-input-no-idx step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  const [step01, step02, step03, step04, ...unexpected] = published;
  if (
    step01 === undefined ||
    step02 === undefined ||
    step03 === undefined ||
    step04 === undefined ||
    unexpected.length !== 0
  ) {
    throw new Error(
      `Expected exactly four reference-input-no-idx step scripts, published ${published.length.toString()}.`,
    );
  }
  return [step01, step02, step03, step04];
};

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite watches the VALIDATOR refuse.
// Production code never takes these paths.
// ---------------------------------------------------------------------------

export type RawStepConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /**
   * The published step reference script. Required here, not optional: §8.7's
   * positional carriage indices count into the transaction's COMPLETE
   * canonically-sorted reference-input set, so a raw builder that dropped the
   * step script would resolve different indices than the production one.
   */
  readonly referenceScriptUtxo: UTxO;
};

/**
 * A raw step-02: the honest submitter's field-1 opening transaction with a
 * caller-chosen `bad_reference_input_index` and a caller-chosen forwarded
 * reference input, and none of the submitter's local range check.
 *
 * The opening itself stays honest — the door authenticates it — so the only
 * thing a refusal can be attributed to is the on-chain selection rule
 * (`spend_input_at`'s §7.3 abort-never-clamp range guard).
 */
export const submitRawReferenceInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  badReferenceInputIndex,
  forwardedReferenceInput,
  nativeTxCompactCbor,
  referenceScriptUtxo,
}: RawStepConfig & {
  readonly referenceInputsPreimage: readonly MidgardTxInput[];
  readonly badReferenceInputIndex: number;
  readonly forwardedReferenceInput: MidgardTxInput;
  readonly nativeTxCompactCbor: string;
}): Promise<string> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.referenceInputNoIdx;
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "raw reference-input-no-idx step-02 thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = Data.from(
    threadUtxo.datum!,
    ReferenceInputNoIdxStep02Datum,
  );
  if (inputDatum.data === null) {
    throw new Error("raw step-02 thread carries no §2.5 anchor");
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.referenceInputs,
    anchorTxId: inputDatum.data.verified_tx_id,
    nativeTxCompactCbor,
    itemCbors: referenceInputsPreimage.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    label: "Raw reference-input-no-idx step 02 reference-inputs",
  });
  signer.selectWallet(lucid);
  const published = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Raw reference-input-no-idx step 02 reference-inputs field",
  });
  const stepReference = requireFaultProofStepReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: chain.steps[1].spendingScriptHash,
    label: "raw reference-input-no-idx step 02",
  });
  const referenceInputs = [...published, stepReference];
  const referenceInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    label: "Raw reference-input-no-idx step 02 reference-inputs",
  });
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: referenceInputNoIdxStep03StateFromBadInput(forwardedReferenceInput),
    },
    ReferenceInputNoIdxStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: chain.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw reference-input-no-idx step 02",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw reference-input-no-idx step 02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              step03OutputMatches,
              "raw reference-input-no-idx step 02 output",
            ),
            reference_inputs_opening: referenceInputsOpening,
            bad_reference_input_index: BigInt(badReferenceInputIndex),
          },
        ],
      },
      ReferenceInputNoIdxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([...referenceInputs])
    .pay.ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

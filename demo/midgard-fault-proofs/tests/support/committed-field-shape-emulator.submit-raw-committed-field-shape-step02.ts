import { type MidgardNativeTxCanonical } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type CommittedFieldClaim,
  CommittedFieldShapeStep02SpendRedeemer,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContracts } from "../../src/committed-field-shape/contracts.js";
import type { PreparedCommittedFieldShape } from "../../src/committed-field-shape/prepare-committed-field-shape.js";
import { requireCommittedFieldShapeThreadUtxo } from "../../src/committed-field-shape/submit-common.js";
import { type ResolvedProverSigner } from "../../src/runtime.js";
import { selectFeeInput } from "../../src/step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../../src/witness-reference-scripts.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  type CommittedFieldShapeScenario,
} from "./committed-field-shape-emulator.committed-field-shape-scenario-material.js";

/** Raw finalization without the production submitter's predicate guard. */
export const submitRawCommittedFieldShapeStep02 = async ({
  harness,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarness;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const fraudProofUnit = toUnit(
    harness.committedFieldShape.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.committedFieldShape.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-02",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw committed-field-shape step-02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw committed-field-shape fraud-proof output",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              harness.committedFieldShape.fraudProof.policyId,
              "raw committed-field-shape fraud-proof mint",
            ),
          },
        ],
      },
      CommittedFieldShapeStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.committedFieldShape.computationThread.policyId,
      "raw committed-field-shape thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.committedFieldShape.fraudProof.policyId,
      "raw committed-field-shape fraud mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          harness.committedFieldShape.computationThread.policyId,
          "raw committed-field-shape thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.committedFieldShape.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw committed-field-shape step-02 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: harness.committedFieldShape.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw committed-field-shape step-02 fraud-proof mint",
  });
  const base = harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudMintRedeemer)
    .pay.ToContract(
      harness.committedFieldShape.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

const RawCancelSchemaValue = SDK.faultProofStepRedeemerSchema(Data.Any());

type RawCancelSchema = Data.Static<typeof RawCancelSchemaValue>;

const RawCancelSchema = RawCancelSchemaValue as unknown as RawCancelSchema;

/** Raw outsider cancel, bypassing only the off-chain signer guard. */
export const submitRawCommittedFieldShapeCancel = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  threadAssetName,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CommittedFieldShapeContracts;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 1;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly threadAssetName: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw new Error("raw cancel thread is not at the named family step");
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw committed-field-shape cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw committed-field-shape cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw committed-field-shape cancel burn",
          ),
        },
      },
      RawCancelSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw committed-field-shape cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: threadAssetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw committed-field-shape cancel computation-thread mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
    ])
    .mintAssets({ [threadUnit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await computationThreadCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** Exact evaluator-failure assertion: off-chain errors are not security proof. */
export const expectCommittedFieldShapeOnchainRefusal = async (
  build: () => Promise<unknown>,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined) {
    throw new Error("expected an on-chain refusal, but the transaction landed");
  }
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution/u.test(text)) {
    throw new Error(
      `expected failed script execution, got an off-chain failure: ${text}`,
    );
  }
  return text;
};

/** Convenience inline claim for raw adversarial builders. */
export const committedFieldShapeInlineClaim = ({
  fieldIndex,
  preimage,
}: {
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): CommittedFieldClaim => ({
  BodyFieldClaim: {
    field_index: BigInt(fieldIndex),
    carriage: {
      Inline: { preimage: Buffer.from(preimage).toString("hex") },
    },
  },
});

export const preparedFromScenario = (
  scenario: CommittedFieldShapeScenario,
  prepare: (tx: MidgardNativeTxCanonical) => PreparedCommittedFieldShape,
): PreparedCommittedFieldShape => {
  if (scenario.canonicalTx === null) {
    throw new Error("scenario has no canonical transaction for prepare");
  }
  return prepare(scenario.canonicalTx);
};

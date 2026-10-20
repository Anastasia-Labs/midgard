/** The close of our convicted case (validation-case-v1 CaseClose): once the
 * defense deadline has passed with nobody defending, the case and its bond
 * are spent together; our computation thread burns into the hub's
 * fraud-proof record, the case tokens are locked beside it, and the bond
 * and the case's remaining lovelace return to us. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  type RedeemerContext,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { witnessMintingPolicyCarriage } from "../witness-reference-scripts.js";
import { validationTraceRecovery } from "./actions.js";
import {
  captured,
  type CapturedValidationTraceStage,
  caseConfigUtxo,
  hubOracleUtxo,
  ours,
  type StageInput,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";
import { validationTraceDefenseDeadlineMs } from "./waiting.js";

/** L_d: every dispute move's validity range. */
const MAX_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms;

/** `env.fraud_proof_output_lovelace`: what the fraud-proof record holds. */
const FRAUD_PROOF_OUTPUT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

/**
 * The lovelace of the close's two small outputs: the case tokens locked at
 * the fraud-proof address, which case-v1 caps at the fraud-proof output's
 * lovelace, and the remainder when the case holds less. It covers the
 * minimum of an output holding two tokens or a tag datum.
 */
export const VALIDATION_TRACE_CLOSE_SMALL_OUTPUT_LOVELACE = 1_500_000n;

/** `case_outcome` (validation-case-v1) is Convicted. */
const convicts = (caseDatum: SDK.Case): boolean =>
  !caseDatum.defender_won &&
  caseDatum.open_threads === 0n &&
  !caseDatum.neither_claim_valid;

/** An output reference as the plain `OutputReference` data case-v1 tags. */
const tagOf = (utxo: UTxO): string =>
  Data.to(new Constr(0, [utxo.txHash, BigInt(utxo.outputIndex)]));

const keyAddress = (
  deployment: ValidationTraceDisputeDeployment,
  hash: string,
): string => credentialToAddress(deployment.network, { type: "Key", hash });

/** The fraud-proof record a conviction of `headerHash` mints. */
export const validationTraceFraudProofUnit = (
  deployment: ValidationTraceDisputeDeployment,
  headerHash: string,
): string =>
  toUnit(
    deployment.fraudProof.policyId,
    deployment.init.category.categoryId + headerHash,
  );

/**
 * The CaseClose of the convicted case at `caseOutRef` with its bond at
 * `bondOutRef`, valid from `validFrom` for L_d, before balancing. The case
 * validator decides whether it may close then.
 */
export const buildValidationTraceClose = async ({
  lucid,
  signer,
  deployment,
  artifact,
  caseOutRef,
  bondOutRef,
  validFrom,
}: StageInput & {
  readonly caseOutRef: string;
  readonly bondOutRef: string;
  readonly validFrom: number;
}): Promise<
  Readonly<{
    tx: TxBuilder;
    caseDatum: SDK.Case;
    proof: Readonly<{ address: string; unit: string }>;
    candidates: Parameters<typeof captured>[1];
  }>
> => {
  const { chain, references } = deployment;
  const caseUtxo = await ours(lucid, caseOutRef, "case");
  const bond = await ours(lucid, bondOutRef, "bond");
  if (typeof caseUtxo.datum !== "string")
    throw new Error("validation-trace case carries no inline datum");
  const caseDatum = Data.from(caseUtxo.datum, SDK.Case);
  if (!convicts(caseDatum))
    throw new Error("validation-trace case does not convict");
  const [hubOracle, caseConfig] = await Promise.all([
    hubOracleUtxo(lucid, deployment),
    caseConfigUtxo(lucid, deployment),
  ]);
  const witnesses = deployment.init.witnessReferenceScripts;
  const computationThread = deployment.init.contracts.computationThread;
  const threadBurn = witnessMintingPolicyCarriage({
    script: computationThread.mintingScript,
    referenceUtxo: witnesses?.computationThreadMint,
    label: "validation-trace close thread burn",
  });
  const proofMint = witnessMintingPolicyCarriage({
    script: deployment.fraudProof.mintingScript,
    referenceUtxo: witnesses?.fraudProofMint,
    label: "validation-trace close proof mint",
  });
  const assetName = deployment.init.category.categoryId + artifact.headerHash;
  const threadUnit = toUnit(computationThread.policyId, assetName);
  const proofUnit = validationTraceFraudProofUnit(
    deployment,
    artifact.headerHash,
  );
  // The case NFT, and the bond's marker: case-v1 locks both beside the record.
  const caseTokens = Object.fromEntries(
    Object.entries(caseUtxo.assets).filter(([unit]) =>
      unit.startsWith(chain.case.policyId),
    ),
  );
  const markers = Object.fromEntries(
    Object.entries(bond.assets).filter(([unit]) =>
      unit.startsWith(chain.case.policyId),
    ),
  );
  const claimant = keyAddress(deployment, caseDatum.bounty_claimant);
  const own = caseUtxo.assets.lovelace ?? 0n;
  const left =
    own -
    FRAUD_PROOF_OUTPUT_LOVELACE -
    VALIDATION_TRACE_CLOSE_SMALL_OUTPUT_LOVELACE;
  const close = (ctx: RedeemerContext) =>
    Data.to(
      {
        CaseClose: {
          bond_input_index: SDK.requireInputIndex(ctx, bond, "bond"),
          bond_output_index: 0n,
          remainder_output_index: 1n,
        },
      },
      SDK.CaseRedeemer,
    );
  signer.selectWallet(lucid);
  const tx = lucid
    .newTx()
    .collectFrom([caseUtxo], close)
    .collectFrom([bond], (ctx) =>
      Data.to(
        { case_input_index: SDK.requireInputIndex(ctx, caseUtxo, "case") },
        SDK.DisputeBondSpendRedeemer,
      ),
    )
    .mintAssets({ [threadUnit]: -1n }, () =>
      Data.to(
        { Success: { burning_token_asset_name: assetName } },
        SDK.FraudProofComputationThreadRedeemer,
      ),
    )
    .mintAssets({ [proofUnit]: 1n }, (ctx) =>
      Data.to(
        {
          computation_thread_token_asset_name: assetName,
          computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            computationThread.policyId,
            "validation-trace close thread burn",
          ),
        },
        SDK.FraudProofTokenMintRedeemer,
      ),
    )
    .readFrom([
      caseConfig,
      hubOracle,
      references.case,
      references.bond,
      ...threadBurn.referenceInputs,
      ...proofMint.referenceInputs,
    ])
    .pay.ToAddressWithData(
      claimant,
      { kind: "inline", value: tagOf(bond) },
      { lovelace: bond.assets.lovelace ?? 0n },
    )
    .pay.ToAddressWithData(
      claimant,
      { kind: "inline", value: tagOf(caseUtxo) },
      {
        lovelace:
          left > VALIDATION_TRACE_CLOSE_SMALL_OUTPUT_LOVELACE
            ? left
            : VALIDATION_TRACE_CLOSE_SMALL_OUTPUT_LOVELACE,
      },
    )
    .pay.ToContract(
      deployment.fraudProof.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          { fraud_prover: caseDatum.bounty_claimant },
          SDK.FraudProofTokenDatum,
        ),
      },
      { lovelace: FRAUD_PROOF_OUTPUT_LOVELACE, [proofUnit]: 1n },
    )
    .pay.ToAddress(deployment.fraudProof.spendingScriptAddress, {
      lovelace: VALIDATION_TRACE_CLOSE_SMALL_OUTPUT_LOVELACE,
      ...caseTokens,
      ...markers,
    })
    .validFrom(validFrom)
    .validTo(validFrom + MAX_RANGE_MS);
  return {
    tx,
    caseDatum,
    proof: {
      address: deployment.fraudProof.spendingScriptAddress,
      unit: proofUnit,
    },
    candidates: [
      { role: "validation-case", utxo: references.case },
      { role: "validation-dispute-bond", utxo: references.bond },
      {
        role: "validation-trace-close-thread-burn",
        utxo: witnesses?.computationThreadMint,
        expectedScript: computationThread.mintingScript,
      },
      {
        role: "validation-trace-close-proof-mint",
        utxo: witnesses?.fraudProofMint,
        expectedScript: deployment.fraudProof.mintingScript,
      },
    ],
  };
};

/**
 * Close our convicted case, now: only once the current slot's time is past
 * the case's defense deadline, the earliest a close may start.
 */
export const captureValidationTraceClose = async (
  input: StageInput & {
    readonly caseOutRef: string;
    readonly bondOutRef: string;
  },
): Promise<CapturedValidationTraceStage> => {
  const { lucid } = input;
  const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
  const { tx, caseDatum, proof, candidates } = await buildValidationTraceClose({
    ...input,
    validFrom,
  });
  const deadline = validationTraceDefenseDeadlineMs(caseDatum);
  if (validFrom <= deadline)
    throw new Error(
      `validation-trace case cannot close before its defense deadline ${deadline.toString()}`,
    );
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const transaction = captured(signed, candidates);
  return {
    transaction,
    durableRecovery: validationTraceRecovery({
      stage: "close",
      transaction,
      slots: [proof],
    }),
  };
};

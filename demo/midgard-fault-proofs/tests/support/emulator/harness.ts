import {} from "node:path";

import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  createReferenceScriptAuthPolicy,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  type FraudProofCatalogueCategoryDeploymentInfo,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  type MidgardValidators,
  Proof,
  type ReferenceScriptAuthPolicy,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
} from "../../../src/index.js";
import { type FabricatedDepositContracts } from "../../../src/submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContracts } from "../../../src/submit-fabricated-withdrawal-step-01.js";
import { computationThreadOutputPredicate } from "../../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../../../src/witness-reference-scripts.js";
import {
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  getCompiledScript,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./blueprints.js";
import { buildCatalogueDeploymentInfo, categoryIdSchema } from "./catalogue.js";
import { buildMinimalFaultProofContracts } from "./contracts.js";
import {
  fundedProverEmulatorAccount,
  registerPhasMembershipRewardAccount,
} from "./emulator-context.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";
import {
  type MinAdaYieldReferenceScripts,
  type OperatorLifecycleReferenceScripts,
  publishFaultProofWitnessReferenceScripts,
  publishHarnessFaultProofReferenceScripts,
  publishMinAdaYieldReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
} from "./reference-scripts.js";

const encodeCatalogueMembershipRedeemer = ({
  root,
  categoryId: id,
  categoryScriptHash,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly categoryId: string;
  readonly categoryScriptHash: string;
  readonly membershipProofCbor: string;
}): string =>
  encodePhasMembershipProofRedeemer({
    root,
    keyCbor: Data.to(id, asLucidSchema(categoryIdSchema)),
    valueCbor: Data.to(categoryScriptHash, asLucidSchema(ScriptHashSchema)),
    membershipProofCbor,
  });

/**
 * Init transaction for a fabricated family (Q39/Q40): mints the computation
 * thread under the family's canonical catalogue category and locks it at
 * step-01.
 *
 * This mirrors the generic tail of `src/submit-init.ts` exactly — catalogue,
 * hub-oracle and fraudulent-block reference inputs, the PHAS membership
 * withdrawal carrying the category proof, and the `Init` mint redeemer. It
 * remains here as the focused Q39/Q40 emulator adapter.
 */
export const submitFabricatedFamilyInit = async ({
  lucid,
  realBlueprint,
  contracts,
  catalogueRoot,
  category,
  family,
  familyLabel,
  signer,
  fraudulentBlockOutRef,
  witnessReferenceScripts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly realBlueprint: Blueprint;
  readonly contracts: Pick<
    MidgardValidators,
    "fraudProofCatalogue" | "hubOracle"
  >;
  readonly catalogueRoot: string;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly family: FabricatedDepositContracts | FabricatedWithdrawalContracts;
  readonly familyLabel: string;
  readonly signer: ReturnType<typeof resolveProverSigner>;
  readonly fraudulentBlockOutRef: string;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}): Promise<{
  readonly txHash: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly threadOutRef: string;
}> => {
  // The deployed step-01 must be the very script the catalogue category
  // registers; a divergence would mint a thread the family cannot spend.
  expect(category.categoryId).toBe(family.categoryId);
  expect(category.scriptHash).toBe(family.steps[0].spendingScriptHash);

  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: contracts.fraudProofCatalogue.spendingScriptAddress,
        unit: toUnit(
          contracts.fraudProofCatalogue.policyId,
          FRAUD_PROOF_CATALOGUE_ASSET_NAME,
        ),
        label: `${familyLabel} init fraud-proof catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOracle.policyId),
        ),
        unit: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
        label: `${familyLabel} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          `${familyLabel} fraudulent block out-ref`,
        ),
        label: `${familyLabel} fraudulent block UTxO`,
      }),
    ],
  );
  const fraudulentHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: family.stateQueuePolicyId,
    fraudulentBlockUtxo,
  });
  const computationThreadAssetName = `${family.categoryId}${fraudulentHeaderHash}`;
  const computationThreadUnit = toUnit(
    family.computationThread.policyId,
    computationThreadAssetName,
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, "phas.membership.withdraw"),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const firstStepAddress = family.steps[0].spendingScriptAddress;
  const firstStepDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const firstStepOutputMatches = computationThreadOutputPredicate({
    address: firstStepAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let firstStepOutputIndex: bigint | undefined;
  const computationThreadMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      family.computationThread.policyId,
      `${familyLabel} init computation-thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${familyLabel} init first step`,
    );
    firstStepOutputIndex = outputIndex;
    return Data.to(
      {
        Init: {
          first_step_output_index: outputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: category.scriptHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${familyLabel} init fraud-proof catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${familyLabel} init PHAS membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${familyLabel} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${familyLabel} init fraudulent block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  // Owner ruling 2026-08-26: witness scripts resolve from published reference
  // scripts wherever the scenario requires them; missing entries fail closed.
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: family.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${familyLabel} init computation-thread mint`,
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${familyLabel} init phas membership withdrawal`,
  });
  const chainedTx = lucid
    .newTx()
    .readFrom([
      catalogueUtxo,
      hubOracleUtxo,
      fraudulentBlockUtxo,
      ...computationThreadMintCarriage.referenceInputs,
      ...phasMembershipCarriage.referenceInputs,
    ])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeCatalogueMembershipRedeemer({
        root: catalogueRoot,
        categoryId: category.categoryId,
        categoryScriptHash: category.scriptHash,
        membershipProofCbor: category.membershipProofCbor,
      }),
    )
    .mintAssets({ [computationThreadUnit]: 1n }, computationThreadMintRedeemer)
    .pay.ToContract(
      firstStepAddress,
      { kind: "inline", value: firstStepDatum },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasMembershipCarriage
    .attach(computationThreadMintCarriage.attach(chainedTx))
    .complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw new Error(
      `BuildTxWithRedeemer did not resolve ${familyLabel} init output index.`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  return {
    txHash,
    fraudulentHeaderHash,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress,
    threadOutRef: `${txHash}#${firstStepOutputIndex.toString()}`,
  };
};

export type FaultProofEmulatorHarness = {
  readonly realBlueprint: Blueprint;
  readonly alwaysBlueprint: Blueprint;
  readonly emulator: Emulator;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly nonceUtxo: UTxO;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  > & {
    readonly referenceScriptAuth: ReferenceScriptAuthPolicy;
    readonly operatorLifecycleReferenceScripts: OperatorLifecycleReferenceScripts;
    readonly minAdaYieldReferenceScripts?: MinAdaYieldReferenceScripts;
  };
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly faultProofReferenceScripts: Awaited<
    ReturnType<typeof publishHarnessFaultProofReferenceScripts>
  >;
};

/**
 * The journey preamble every fault-proof emulator suite opens with, in the
 * exact order the suites performed it: read both blueprints, stand up the
 * funder/prover party, register the PHAS membership reward account (then any
 * family-specific reward accounts the caller registers, in the caller's own
 * order), take the funder's first UTxO as the parameterizing nonce, build the
 * minimal contract set for the family under test, then derive the catalogue
 * deployment info.
 *
 * Shared witness scripts are published here exactly once per scenario harness
 * so every downstream submitter sees the same immutable reference UTxOs.
 */
export const makeFaultProofEmulatorHarness = async ({
  contractOptions = {},
  accounts,
  emulatorTimeMs,
  registerAdditionalRewardAccounts,
  lucidOptions,
}: {
  readonly contractOptions?: Parameters<
    typeof buildMinimalFaultProofContracts
  >[3];
  readonly accounts?: {
    readonly funder: EmulatorAccount;
    readonly prover: EmulatorAccount;
  };
  readonly emulatorTimeMs?: number;
  readonly lucidOptions?: Parameters<typeof Lucid>[2];
  readonly registerAdditionalRewardAccounts?: (
    funderLucid: Awaited<ReturnType<typeof Lucid>>,
    realBlueprint: Blueprint,
  ) => Promise<void>;
} = {}): Promise<FaultProofEmulatorHarness> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder =
    accounts?.funder ?? generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover =
    accounts?.prover ?? fundedProverEmulatorAccount(20_000_000_000n);
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  if (emulatorTimeMs !== undefined) {
    emulator.time = emulatorTimeMs;
  }
  const funderLucid = await Lucid(emulator, "Custom", lucidOptions);
  const proverLucid = await Lucid(emulator, "Custom", lucidOptions);
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  // Selected through the signer so the prover Lucid instance and every
  // `signer.selectWallet(lucid)` call site address the same funded wallet.
  proverSigner.selectWallet(proverLucid);

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  if (registerAdditionalRewardAccounts !== undefined) {
    await registerAdditionalRewardAccounts(funderLucid, realBlueprint);
  }
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const referenceScriptAuth = createReferenceScriptAuthPolicy(
    proverLucid,
    emulator.now(),
  );
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        ...contractOptions,
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
      },
    )),
    // Test/dev scaffold: production deployments use the same native timelock
    // policy and persist its SDK-derived deployment info. Keeping the complete
    // policy here lets strict manifest consumers validate the policy id and
    // canonical role-token map rather than accepting an empty sidecar.
    referenceScriptAuth,
  };
  const operatorLifecycleReferenceScripts =
    await publishOperatorLifecycleReferenceScripts({
      // Keep the funder's deployment nonce unspent. These immutable reference
      // scripts are chain-global and the prover wallet is already the harness
      // publisher for the fraud-proof witness roster below.
      lucid: proverLucid,
      contracts: baseContracts,
    });
  const stagedContracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    stagedContracts.fraudProofs,
  );
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript:
        stagedContracts.computationThread.mintingScript,
      fraudProofMintingScript: stagedContracts.fraudProof.mintingScript,
      includeChunkedVerify: true,
      includePexcludes: true,
    });
  const contractsWithWitnesses = {
    ...stagedContracts,
    faultProofWitnessReferenceScripts: witnessReferenceScripts,
  };
  // Publish authenticated min-Ada yield scripts before fixture code samples
  // the block-header clock. Setup then only registers their reward accounts,
  // so deployment traffic cannot expire the bounded commit validity window.
  const minAdaYieldReferenceScripts =
    contractsWithWitnesses.minAda === undefined
      ? undefined
      : await publishMinAdaYieldReferenceScripts({
          lucid: proverLucid,
          contracts: contractsWithWitnesses,
        });
  const contracts = {
    ...contractsWithWitnesses,
    ...(minAdaYieldReferenceScripts === undefined
      ? {}
      : { minAdaYieldReferenceScripts }),
  };
  const faultProofReferenceScripts =
    await publishHarnessFaultProofReferenceScripts({
      lucid: proverLucid,
      contracts,
    });
  return {
    realBlueprint,
    alwaysBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
    witnessReferenceScripts,
    faultProofReferenceScripts,
  };
};

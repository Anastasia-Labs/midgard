import {} from "node:path";

import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  type FraudProofCatalogueCategoryDeploymentInfo,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  type MidgardValidators,
  Proof,
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
import { type FabricatedDepositContractsV1 } from "../../../src/submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContractsV1 } from "../../../src/submit-fabricated-withdrawal-step-01.js";
import { computationThreadOutputPredicate } from "../../../src/tx-layout.js";
import {
  alwaysSucceedsBlueprintPath,
  type Blueprint,
  getCompiledScript,
  network,
  readBlueprint,
  realBlueprintPath,
} from "./blueprints.js";
import {
  buildCatalogueDeploymentInfo,
  categoryIdSchema,
  type LucidDataSchema,
} from "./catalogue.js";
import { buildMinimalFaultProofContracts } from "./contracts.js";
import { registerPhasMembershipRewardAccount } from "./emulator-context.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";

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
    keyCbor: Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    valueCbor: Data.to(
      categoryScriptHash,
      ScriptHashSchema as unknown as LucidDataSchema,
    ),
    membershipProofCbor,
  });

/**
 * Init transaction for a fabricated family (Q39/Q40): mints the computation
 * thread under the family's extra catalogue category and locks it at step-01.
 *
 * This mirrors the generic tail of `src/submit-init.ts` exactly — catalogue,
 * hub-oracle and fraudulent-block reference inputs, the PHAS membership
 * withdrawal carrying the category proof, and the `Init` mint redeemer — but
 * lives here because the production `submitInit` category union is parent-owned
 * and does not register these families yet.
 */
export const submitFabricatedFamilyInitV1 = async ({
  lucid,
  realBlueprint,
  contracts,
  catalogueRoot,
  category,
  family,
  familyLabel,
  signer,
  fraudulentBlockOutRef,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly realBlueprint: Blueprint;
  readonly contracts: Pick<
    MidgardValidators,
    "fraudProofCatalogue" | "hubOracle"
  >;
  readonly catalogueRoot: string;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly family:
    | FabricatedDepositContractsV1
    | FabricatedWithdrawalContractsV1;
  readonly familyLabel: string;
  readonly signer: ReturnType<typeof resolveProverSigner>;
  readonly fraudulentBlockOutRef: string;
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
  const unsigned = await lucid
    .newTx()
    .readFrom([catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo])
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
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(family.computationThread.mintingScript)
    .attach.WithdrawalValidator(phasMembershipScript)
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

export type FaultProofEmulatorHarnessV1 = {
  readonly realBlueprint: Blueprint;
  readonly alwaysBlueprint: Blueprint;
  readonly emulator: Emulator;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly nonceUtxo: UTxO;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
};

/**
 * Reserved test-harness id for committed-field-shape. It is intentionally not
 * a production catalogue allocation and is wired only through extraCategories.
 */
export const COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1 = "00000012";

/**
 * The journey preamble every fault-proof emulator suite opens with, in the
 * exact order the suites performed it: read both blueprints, stand up the
 * funder/prover party, register the PHAS membership reward account (then any
 * family-specific reward accounts the caller registers, in the caller's own
 * order), take the funder's first UTxO as the parameterizing nonce, build the
 * minimal contract set for the family under test, then derive the catalogue
 * deployment info.
 *
 * Reference-script publication is deliberately NOT part of this helper: the
 * suites publish at different points in the timeline, and the emulator clock
 * they sample afterwards is what their measured byte counts are anchored to.
 */
/**
 * Test-only catalogue id for the `native-script-decoding` family (#635).
 * The production id is assigned only at catalogue registration (design §10
 * Q2); `0000000d` is the expected-but-not-promised next slot after the two
 * Q39/Q40 families, and the emulator suites register it as an extra
 * category exactly the way those families do.
 */
export const NATIVE_SCRIPT_DECODING_TEST_CATEGORY_ID_V1 = "0000000d";

export const makeFaultProofEmulatorHarnessV1 = async ({
  contractOptions = {},
  accounts,
  emulatorTimeMs,
  registerAdditionalRewardAccounts,
}: {
  readonly contractOptions?: Parameters<
    typeof buildMinimalFaultProofContracts
  >[3];
  readonly accounts?: {
    readonly funder: EmulatorAccount;
    readonly prover: EmulatorAccount;
  };
  readonly emulatorTimeMs?: number;
  readonly registerAdditionalRewardAccounts?: (
    funderLucid: Awaited<ReturnType<typeof Lucid>>,
    realBlueprint: Blueprint,
  ) => Promise<void>;
} = {}): Promise<FaultProofEmulatorHarnessV1> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder =
    accounts?.funder ?? generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover =
    accounts?.prover ?? generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  if (emulatorTimeMs !== undefined) {
    emulator.time = emulatorTimeMs;
  }
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  if (registerAdditionalRewardAccounts !== undefined) {
    await registerAdditionalRewardAccounts(funderLucid, realBlueprint);
  }
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    contractOptions,
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs, {
    ...(contracts.fabricatedDeposit === undefined
      ? {}
      : {
          fabricatedDeposit: {
            categoryId: contracts.fabricatedDeposit.categoryId,
            scriptHash: contracts.fabricatedDeposit.steps[0].spendingScriptHash,
          },
        }),
    ...(contracts.fabricatedWithdrawal === undefined
      ? {}
      : {
          fabricatedWithdrawal: {
            categoryId: contracts.fabricatedWithdrawal.categoryId,
            scriptHash:
              contracts.fabricatedWithdrawal.steps[0].spendingScriptHash,
          },
        }),
    ...(contracts.nativeScriptDecoding === undefined
      ? {}
      : {
          nativeScriptDecoding: {
            categoryId: NATIVE_SCRIPT_DECODING_TEST_CATEGORY_ID_V1,
            scriptHash:
              contracts.nativeScriptDecoding.steps[0].spendingScriptHash,
          },
        }),
    ...(contracts.committedFieldShape === undefined
      ? {}
      : {
          committedFieldShape: {
            categoryId: COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1,
            scriptHash:
              contracts.committedFieldShape.steps[0].spendingScriptHash,
          },
        }),
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
  };
};

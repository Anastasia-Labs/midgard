import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  CML,
  coreToTxOutput,
  type TxSigned,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { readFraudSlashFundingAuthority } from "../remove-fraudulent-block.js";
import { assertWorkflowActuationPermitIdentity } from "./actuation-permit.js";
import {
  assertAuthenticatedFraudSlashReward,
  isExactFraudSlashRewardOutput,
} from "./funding-reservation-permit.assert-authenticated-fraud-slash-reward.js";
import {
  addAssets,
  isProtocolFundingContract,
} from "./funding-reservation-permit.begin-workflow-funding-reservation-action.js";
import {
  actionKind,
  exactResolvedOutputCbor,
  parseConfirmedActionOutput,
  parseProtocolInputAuthority,
  resolveExactOutRefs,
} from "./funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { type PermitState } from "./funding-reservation-permit.workflow-funding-reservation-port.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  readWorkflowRuntimeFundingPolicy,
  workflowRuntimeFundingMinimumFee,
} from "./runtime-funding-policy.js";
import { workflowTransactionReferenceInputOutRefs } from "./transaction-boundary.js";
export const assertRuntimeTransactionBound = async ({
  state,
  action,
  signed,
  bodyInputs,
  fundingOutRefs,
  collateralOutRefs,
}: {
  readonly state: PermitState;
  readonly action: FraudProofWorkflowAction;
  readonly signed: TxSigned;
  readonly bodyInputs: readonly string[];
  readonly fundingOutRefs: readonly string[];
  readonly collateralOutRefs: readonly string[];
}): Promise<void> => {
  if (state.policy === undefined)
    throw new Error("runtime funding policy is missing");
  const policy = readWorkflowRuntimeFundingPolicy(state.policy);
  const parameters = policy.protocolParameters;
  const transaction = signed.toTransaction();
  const body = transaction.body();
  const witnesses = transaction.witness_set();
  const signedBytes = BigInt(transaction.to_cbor_hex().length / 2);
  const contracts = new Map(
    policy.contracts.map((contract) => [contract.address, contract]),
  );
  const slash = readFraudSlashFundingAuthority(signed);
  if (slash !== null) {
    const actuation = assertWorkflowActuationPermitIdentity({
      permit: state.actuationPermit,
      category: state.category,
      rollbackGeneration: state.snapshot.rollbackGeneration,
    });
    const economics = policy.economics.policy;
    const bond =
      BigInt(economics.requiredBondLovelace) -
      (slash.tranche === "full"
        ? 0n
        : BigInt(economics.inactivitySlashingPenaltyLovelace));
    const reward = BigInt(economics.fraudProverRewardLovelace);
    // The removal action names the out-refs it spends and references under
    // the shared `nextRemovalOutRef` / `fraudProofOutRef` vocabulary; its kind
    // is read through `actionKind` (either `actionKind` or `stage`). A refusal
    // names the differing checks so an operator can act on it.
    const differing = (
      [
        ["current action kind", state.currentActionKind === "remove"],
        ["action kind", actionKind(action) === "remove"],
        [
          "action digest",
          state.currentActionDigest ===
            computeDeploymentManifestJsonDigest(action),
        ],
        [
          "nextRemovalOutRef",
          action.input.nextRemovalOutRef === slash.removedStateQueueOutRef,
        ],
        [
          "fraudProofOutRef",
          action.input.fraudProofOutRef === slash.fraudProofOutRef,
        ],
        ["category", slash.category === state.category],
        ["header hash", slash.headerHash === actuation.headerHash],
        [
          "deployment fingerprint",
          slash.deploymentFingerprint === policy.deploymentFingerprint,
        ],
        [
          "economics policy digest",
          slash.economicsPolicyDigest === policy.economicsPolicyDigest,
        ],
        ["operator bond", BigInt(slash.operatorBondLovelace) === bond],
        ["reward", BigInt(slash.rewardLovelace) === reward],
        ["exact fee", BigInt(slash.exactFeeLovelace) === bond - reward],
        ["body fee", body.fee() === bond - reward],
        ["funding inputs", fundingOutRefs.length === 0],
        [
          "authority inputs",
          slash.inputs.length === bodyInputs.length &&
            slash.inputs.every(
              (input, index) => input.outRef === bodyInputs[index],
            ),
        ],
        ["operator input", bodyInputs.includes(slash.operatorOutRef)],
        [
          "removed state-queue input",
          bodyInputs.includes(slash.removedStateQueueOutRef),
        ],
        [
          "fraud proof reference input",
          workflowTransactionReferenceInputOutRefs(signed).includes(
            slash.fraudProofOutRef,
          ),
        ],
      ] as const
    )
      .filter(([, holds]) => !holds)
      .map(([name]) => name);
    if (differing.length > 0)
      throw new Error(
        `fraud slash funding authority differs from its exact removal action or economics (${differing.join(", ")})`,
      );
  }
  if (signedBytes > BigInt(parameters.maxTxSize))
    throw new Error("signed transaction exceeds protocol maxTxSize");
  if (!transaction.is_valid())
    throw new Error("funding transaction declares script failure");
  const bodyHash = CML.hash_transaction(body);
  if (signed.toHash().toLowerCase() !== bodyHash.to_hex())
    throw new Error("funding transaction hash differs from its actual body");
  const vkeys = witnesses.vkeywitnesses();
  let fundingSignature = false;
  for (let index = 0; index < (vkeys?.len() ?? 0); index += 1) {
    const witness = vkeys!.get(index);
    if (
      witness.vkey().hash().to_hex() === policy.fundingPaymentKeyHash &&
      witness
        .vkey()
        .verify(bodyHash.to_raw_bytes(), witness.ed25519_signature())
    )
      fundingSignature = true;
  }
  if (!fundingSignature)
    throw new Error("funding transaction lacks the reserved wallet signature");
  if (
    (witnesses.native_scripts()?.len() ?? 0) +
      (witnesses.plutus_v1_scripts()?.len() ?? 0) +
      (witnesses.plutus_v2_scripts()?.len() ?? 0) +
      (witnesses.plutus_v3_scripts()?.len() ?? 0) !==
    0
  )
    throw new Error("funding transaction embeds executable script witnesses");
  let memory = 0n,
    steps = 0n;
  const redeemers = witnesses.redeemers()?.to_flat_format();
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    memory += redeemers!.get(index).ex_units().mem();
    steps += redeemers!.get(index).ex_units().steps();
  }
  if (
    memory > BigInt(parameters.maxTxExUnits.memory) ||
    steps > BigInt(parameters.maxTxExUnits.steps)
  )
    throw new Error("funding transaction exceeds protocol maxTxExUnits");
  const references = await resolveExactOutRefs({
    port: state.port,
    outRefs: [...workflowTransactionReferenceInputOutRefs(signed)].sort(),
    label: "runtime funding reference inputs",
  });
  if (slash !== null)
    assertAuthenticatedFraudSlashReward(
      slash,
      references,
      contracts,
      state.snapshot.walletAddress,
    );
  let referenceScriptBytes = 0n;
  const scriptIdentities = new Map(
    policy.referenceScripts.map(({ outRef, scriptHash }) => [
      outRef,
      scriptHash,
    ]),
  );
  for (const [outRef, reference] of references) {
    const expected = scriptIdentities.get(outRef);
    if (reference.scriptRef == null) {
      if (expected !== undefined)
        throw new Error(
          "funding transaction lost its deployed reference script",
        );
      continue;
    }
    if (expected !== validatorToScriptHash(reference.scriptRef))
      throw new Error(
        "funding transaction uses an ungoverned reference script",
      );
    referenceScriptBytes += BigInt(reference.scriptRef.script.length / 2);
  }
  if (
    referenceScriptBytes >
    BigInt(parameters.referenceScriptFee.maximumSizeBytes)
  )
    throw new Error("funding transaction exceeds reference-script byte limit");
  const minimumFee = workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: signedBytes,
    memory,
    steps,
    referenceScriptBytes,
  });
  if (
    body.fee() < minimumFee ||
    (slash === null && body.fee() > BigInt(policy.maximumFeeLovelace))
  )
    throw new Error(
      "funding transaction fee is outside the live protocol funding bounds",
    );
  const scriptExecution = (redeemers?.len() ?? 0) !== 0;
  const collateral = await resolveExactOutRefs({
    port: state.port,
    outRefs: collateralOutRefs,
    label: "runtime funding collateral inputs",
  });
  const collateralValue = [...collateral.values()].reduce((total, input) => {
    if (
      input.address !== state.snapshot.walletAddress ||
      Object.keys(input.assets).some((unit) => unit !== "lovelace")
    )
      throw new Error("funding collateral is not reserved-wallet pure Ada");
    return total + (input.assets.lovelace ?? 0n);
  }, 0n);
  const declaredCollateral = body.total_collateral();
  const collateralReturn = body.collateral_return();
  if (scriptExecution) {
    const percentage =
      (body.fee() * BigInt(parameters.collateralPercentage) + 99n) / 100n;
    const floor = BigInt(policy.economics.policy.proverCollateralFloorLovelace);
    const reservedCollateral = state.snapshot.activeInputs
      .filter(({ role }) => role === "collateral")
      .reduce((sum, input) => sum + BigInt(input.lovelace), 0n);
    if (reservedCollateral < floor)
      throw new Error(
        "funding collateral reservation is below the release floor",
      );
    if (
      collateralOutRefs.length === 0 ||
      collateralOutRefs.length > state.maximumCollateralInputs ||
      declaredCollateral === undefined ||
      declaredCollateral < percentage ||
      declaredCollateral >
        BigInt(
          slash === null
            ? policy.maximumCollateralLovelace
            : policy.maximumSlashCollateralLovelace,
        ) ||
      collateralValue < declaredCollateral
    )
      throw new Error(
        "funding transaction collateral is outside the exact funding bounds",
      );
    if (
      collateralReturn !== undefined &&
      (collateralReturn.address().to_bech32() !==
        state.snapshot.walletAddress ||
        collateralReturn.amount().has_multiassets())
    )
      throw new Error("funding collateral return escapes the reserved wallet");
    if (
      collateralReturn !== undefined &&
      collateralReturn.amount().coin() <
        CML.min_ada_required(
          collateralReturn,
          BigInt(parameters.coinsPerUtxoByte),
        )
    )
      throw new Error("funding collateral return is below exact min-Ada");
    if (
      collateralValue !==
      declaredCollateral + (collateralReturn?.amount().coin() ?? 0n)
    )
      throw new Error("funding collateral value is not conserved");
  } else if (
    collateralOutRefs.length !== 0 ||
    declaredCollateral !== undefined ||
    collateralReturn !== undefined
  )
    throw new Error(
      "non-script funding transaction unexpectedly declares collateral",
    );
  const allInputs = await resolveExactOutRefs({
    port: state.port,
    outRefs: bodyInputs,
    label: "runtime funding ordinary inputs",
  });
  const inputAssets = new Map<string, bigint>();
  const walletAssets = new Map<string, bigint>();
  let releasedCustody = 0n;
  let protocolInputLovelace = 0n;
  for (const [outRef, input] of allInputs) {
    addAssets(inputAssets, input.assets);
    if (input.address === state.snapshot.walletAddress) {
      if (slash !== null)
        throw new Error("fraud slash cannot consume ordinary wallet funding");
      if (!fundingOutRefs.includes(outRef))
        throw new Error(
          "signed transaction consumed an unreserved wallet input",
        );
      addAssets(walletAssets, input.assets);
      continue;
    }
    const contract = contracts.get(input.address);
    if (contract === undefined)
      throw new Error(
        "funding transaction consumed an ungoverned contract input",
      );
    if (
      slash !== null &&
      slash.inputs.find((value) => value.outRef === outRef)
        ?.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
    ) {
      throw new Error(
        "fraud slash protocol input changed after local evaluation",
      );
    }
    if (
      slash !== null &&
      outRef === slash.operatorOutRef &&
      (input.assets.lovelace ?? 0n).toString() !== slash.operatorBondLovelace
    ) {
      throw new Error(
        "fraud slash operator bond differs from its authenticated tranche",
      );
    }
    // Slashing always reacquires live protocol authority, even for an output
    // whose earlier transaction already appears in this workflow's lineage.
    const lineage =
      slash === null
        ? await state.port.resolveConfirmedInput({ outRef })
        : null;
    if (lineage !== null) {
      const confirmed = parseConfirmedActionOutput(lineage);
      if (
        confirmed.outRef !== outRef ||
        confirmed.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
      )
        throw new Error(
          "funding released custody differs from its exact confirmed lineage",
        );
      if (!isProtocolFundingContract(contract))
        releasedCustody += input.assets.lovelace ?? 0n;
      else protocolInputLovelace += input.assets.lovelace ?? 0n;
      continue;
    }
    if (!isProtocolFundingContract(contract))
      throw new Error(
        "funding released custody lacks confirmed workflow lineage",
      );
    const authority = parseProtocolInputAuthority(
      await state.port.resolveProtocolInputAuthority({
        deploymentFingerprint: policy.deploymentFingerprint,
        outRef,
        semanticRole: "protocol_state",
      }),
    );
    if (
      authority.deploymentFingerprint !== policy.deploymentFingerprint ||
      authority.outRef !== outRef ||
      authority.resolvedOutputCborHex !== exactResolvedOutputCbor(input)
    )
      throw new Error(
        "funding protocol input differs from its deployment-bound authority",
      );
    protocolInputLovelace += input.assets.lovelace ?? 0n;
  }
  const outputAssets = new Map<string, bigint>();
  const walletOutputAssets = new Map<string, bigint>();
  const outputs = body.outputs();
  let custody = 0n,
    custodyAllocation = 0n,
    protocolOutputs = 0n,
    protocolMinimum = 0n;
  let rewardOutputs = 0;
  for (let index = 0; index < outputs.len(); index += 1) {
    const raw = outputs.get(index);
    const output = coreToTxOutput(raw);
    const minimum = CML.min_ada_required(
      raw,
      BigInt(parameters.coinsPerUtxoByte),
    );
    if (
      raw.amount().coin() < minimum ||
      BigInt(raw.amount().to_canonical_cbor_hex().length / 2) >
        BigInt(parameters.maxValueSize)
    )
      throw new Error("funding output violates exact min-Ada or maxValueSize");
    addAssets(outputAssets, output.assets);
    if (slash !== null && isExactFraudSlashRewardOutput(raw, slash)) {
      rewardOutputs += 1;
      if (output.address === state.snapshot.walletAddress)
        addAssets(walletOutputAssets, output.assets);
      continue;
    }
    if (output.address === state.snapshot.walletAddress) {
      if (slash !== null)
        throw new Error("fraud slash cannot pay unrelated caller change");
      addAssets(walletOutputAssets, output.assets);
      continue;
    }
    const contract = contracts.get(output.address);
    if (contract === undefined)
      throw new Error("funding output escapes the governed contract roster");
    if (isProtocolFundingContract(contract)) {
      protocolOutputs += raw.amount().coin();
      protocolMinimum += minimum;
    } else {
      custody += raw.amount().coin();
      // Operator stake and slashing rewards never authorize prover topups.
      // Existing custody is accounted from exact confirmed lineage above.
      custodyAllocation += minimum;
    }
  }
  if (custody > releasedCustody + custodyAllocation)
    throw new Error(
      `funding transaction exceeds its governed custody allocation: actual=${custody.toString()} allowed=${(releasedCustody + custodyAllocation).toString()}`,
    );
  const protocolTopup =
    protocolOutputs > protocolInputLovelace
      ? protocolOutputs - protocolInputLovelace
      : 0n;
  if (
    slash !== null &&
    (rewardOutputs !== 1 ||
      custody !== 0n ||
      releasedCustody !== 0n ||
      protocolTopup !== 0n ||
      protocolInputLovelace !==
        protocolOutputs + body.fee() + BigInt(slash.rewardLovelace))
  )
    throw new Error(
      "fraud slash must preserve protocol state and fund only its exact fee and reward",
    );
  if (protocolTopup > protocolMinimum)
    throw new Error(
      "funding transaction exceeds exact protocol min-Ada topups",
    );
  const walletSpent =
    (walletAssets.get("lovelace") ?? 0n) -
    (walletOutputAssets.get("lovelace") ?? 0n);
  const custodyIncrease =
    custody > releasedCustody ? custody - releasedCustody : 0n;
  if (walletSpent > body.fee() + custodyIncrease + protocolTopup)
    throw new Error(
      "signed transaction loses reserved wallet value outside fees and governed custody",
    );
  for (const [unit, quantity] of walletAssets) {
    if (unit !== "lovelace" && (walletOutputAssets.get(unit) ?? 0n) < quantity)
      throw new Error("signed transaction loses reserved wallet native assets");
  }
  const mint = body.mint();
  if (mint !== undefined) {
    const policies = mint.keys();
    for (let i = 0; i < policies.len(); i += 1) {
      const policyHash = policies.get(i);
      const assets = mint.get_assets(policyHash)!;
      const names = assets.keys();
      for (let j = 0; j < names.len(); j += 1) {
        const name = names.get(j),
          unit = policyHash.to_hex() + name.to_hex();
        inputAssets.set(
          unit,
          (inputAssets.get(unit) ?? 0n) + assets.get(name)!,
        );
      }
    }
  }
  // Withdraw-zero yielding never authorizes stake withdrawals or deposits.
  const withdrawals = body.withdrawals();
  if (withdrawals !== undefined) {
    const keys = withdrawals.keys();
    for (let index = 0; index < keys.len(); index += 1)
      if (withdrawals.get(keys.get(index)) !== 0n)
        throw new Error("funding transaction includes a nonzero withdrawal");
  }
  if ((body.certs()?.len() ?? 0) !== 0 || body.donation() !== undefined)
    throw new Error("funding transaction changes unrelated ledger accounting");
  outputAssets.set(
    "lovelace",
    (outputAssets.get("lovelace") ?? 0n) + body.fee(),
  );
  for (const unit of new Set([...inputAssets.keys(), ...outputAssets.keys()]))
    if ((inputAssets.get(unit) ?? 0n) !== (outputAssets.get(unit) ?? 0n))
      throw new Error(
        "funding transaction does not conserve exact ledger value",
      );
};

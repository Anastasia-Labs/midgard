import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  castConfirmedStateToData,
  castStateQueueNodeToData,
  ConfirmedState,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofTokenDatum,
  hashBlockHeader,
  makeGenesisConfirmedState,
  NO_DA_ATTESTATION,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  keyHashToCredential,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1Utxo,
  type VerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../../src/workflow/index.js";
import { makeHeader } from "./emulator/header-fixtures.js";

export const hash32 = (byte: string): string => byte.repeat(32);
export const policy = (byte: string): string => byte.repeat(28);
export const OPERATOR = policy("11");
export const PROVER = policy("12");
export const DEPLOYMENT = hash32("13");
export const RELEASE = hash32("14");
export const finalityPolicy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
export const FINALITY =
  computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy);
export const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: FINALITY,
  policy: finalityPolicy,
};
export const SOURCE = "local-kupmios-family-test";

export const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

export const releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
  policy: economicsPolicy,
};

export const scriptAddress = (byte: string): string =>
  credentialToAddress("Preview", scriptHashToCredential(policy(byte)));

export const value = (assets: Readonly<Record<string, bigint>>): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit === "lovelace") continue;
    multiasset.set(
      CML.ScriptHash.from_hex(unit.slice(0, 56)),
      CML.AssetName.from_hex(unit.slice(56)),
      quantity,
    );
  }
  return CML.Value.new(assets.lovelace ?? 0n, multiasset);
};

export const output = ({
  address,
  assets,
  datum,
}: {
  readonly address: string;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly datum?: string;
}): CML.TransactionOutput =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    value(assets),
    datum === undefined
      ? undefined
      : CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
  );

export const raw = (
  outRef: string,
  transactionOutput: CML.TransactionOutput,
): FraudProofRawL1Utxo => ({
  outRef,
  outputCbor: transactionOutput.to_canonical_cbor_hex(),
  datumCbor:
    transactionOutput.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: null,
});

export const input = (outRef: string): CML.TransactionInput => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

export const fixture = async ({
  header: suppliedHeader,
  deploymentFingerprint = DEPLOYMENT,
  blueprintHash = RELEASE,
  verifiedFinality = releaseFinality,
  proverCredential = PROVER,
  verifiedEconomics = releaseEconomics,
  proofCreation = false,
  descendant = false,
  partial = false,
  duplicateReward = false,
  bondStatus = "active",
  burnBond = true,
  continueBond = false,
}: {
  header?: ReturnType<typeof makeHeader>;
  deploymentFingerprint?: string;
  blueprintHash?: string;
  verifiedFinality?: VerifiedFraudProofReleaseFinalityPolicy;
  proverCredential?: string;
  verifiedEconomics?: VerifiedFraudProofReleaseEconomicsPolicy;
  proofCreation?: boolean;
  descendant?: boolean;
  partial?: boolean;
  duplicateReward?: boolean;
  bondStatus?: "active" | "retired";
  burnBond?: boolean;
  continueBond?: boolean;
} = {}) => {
  const header = suppliedHeader ?? makeHeader(policy("11"), 1_789_500_000_000);
  const OPERATOR = header.operatorVkey;
  const PROVER = proverCredential;
  const DEPLOYMENT = deploymentFingerprint;
  const RELEASE = blueprintHash;
  const FINALITY = verifiedFinality.policyDigest;
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const statePolicy = policy("21");
  const threadPolicy = policy("22");
  const proofPolicy = policy("23");
  const activePolicy = policy("24");
  const retiredPolicy = policy("25");
  const stateAddress = scriptAddress("31");
  const proofAddress = scriptAddress("32");
  const activeAddress = scriptAddress("33");
  const retiredAddress = scriptAddress("34");
  const stateUnit = toUnit(
    statePolicy,
    `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  );
  const rootUnit = toUnit(statePolicy, STATE_QUEUE_ROOT_ASSET_NAME);
  const assetName = `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${headerHash}`;
  const threadUnit = toUnit(threadPolicy, assetName);
  const proofUnit = toUnit(proofPolicy, assetName);
  const bondUnit = toUnit(
    bondStatus === "active" ? activePolicy : retiredPolicy,
    `${bondStatus === "active" ? ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX : RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${OPERATOR}`,
  );
  const targetDatum = encodeLinkedListNodeView({
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: NO_DA_ATTESTATION,
    }) as never,
  });
  const rootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: castConfirmedStateToData(makeGenesisConfirmedState(0n)) as never,
  });
  const proofDatum = Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum);
  const targetOutputs = CML.TransactionOutputList.new();
  targetOutputs.add(
    output({
      address: stateAddress,
      assets: { lovelace: 3_000_000n, [stateUnit]: 1n },
      datum: targetDatum,
    }),
  );
  const targetBody = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    targetOutputs,
    0n,
  );
  const targetMint = CML.Mint.new();
  targetMint.set(
    CML.ScriptHash.from_hex(statePolicy),
    CML.AssetName.from_hex(stateUnit.slice(56)),
    1n,
  );
  targetBody.set_mint(targetMint);
  const targetHash = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(targetBody.to_canonical_cbor_hex()),
  ).to_hex();
  const targetOutRef = `${proofCreation ? targetHash : hash32("41")}#0`;
  const bondOutRef = `${hash32("42")}#0`;
  const proofOutputs = CML.TransactionOutputList.new();
  proofOutputs.add(
    output({
      address: proofAddress,
      assets: { lovelace: 3_000_000n, [proofUnit]: 1n },
      datum: proofDatum,
    }),
  );
  const proofBody = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    proofOutputs,
    0n,
  );
  const proofMint = CML.Mint.new();
  proofMint.set(
    CML.ScriptHash.from_hex(proofPolicy),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  proofBody.set_mint(proofMint);
  const proofHash = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(proofBody.to_canonical_cbor_hex()),
  ).to_hex();
  const proofOutRef = `${proofCreation ? proofHash : hash32("43")}#0`;
  const target = raw(
    targetOutRef,
    output({
      address: stateAddress,
      assets: { lovelace: 3_000_000n, [stateUnit]: 1n },
      datum: targetDatum,
    }),
  );
  const bond = raw(
    bondOutRef,
    output({
      address: bondStatus === "active" ? activeAddress : retiredAddress,
      assets: {
        lovelace: partial
          ? BigInt(verifiedEconomics.policy.requiredBondLovelace) -
            BigInt(verifiedEconomics.policy.inactivitySlashingPenaltyLovelace)
          : BigInt(verifiedEconomics.policy.requiredBondLovelace),
        [bondUnit]: 1n,
      },
      datum: Data.to("" as never, Data.Bytes()),
    }),
  );
  const proof = raw(
    proofOutRef,
    output({
      address: proofAddress,
      assets: { lovelace: 3_000_000n, [proofUnit]: 1n },
      datum: proofDatum,
    }),
  );
  const rewardAddress = credentialToAddress(
    "Preview",
    keyHashToCredential(PROVER),
  );
  const slashOutputs = CML.TransactionOutputList.new();
  slashOutputs.add(
    output({
      address: stateAddress,
      assets: descendant
        ? { lovelace: 3_000_000n, [stateUnit]: 1n }
        : { lovelace: 3_000_000n, [rootUnit]: 1n },
      datum: descendant ? targetDatum : rootDatum,
    }),
  );
  if (duplicateReward) {
    slashOutputs.add(
      output({
        address: rewardAddress,
        assets: {
          lovelace: BigInt(verifiedEconomics.policy.fraudProverRewardLovelace),
        },
      }),
    );
  }
  slashOutputs.add(
    output({
      address: rewardAddress,
      assets: {
        lovelace: BigInt(verifiedEconomics.policy.fraudProverRewardLovelace),
      },
    }),
  );
  if (continueBond)
    slashOutputs.add(CML.TransactionOutput.from_cbor_hex(bond.outputCbor));
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(targetOutRef));
  inputs.add(input(bondOutRef));
  const slashBody = CML.TransactionBody.new(
    inputs,
    slashOutputs,
    partial
      ? BigInt(verifiedEconomics.policy.slashingPenaltyLovelace) -
          BigInt(verifiedEconomics.policy.inactivitySlashingPenaltyLovelace)
      : BigInt(verifiedEconomics.policy.slashingPenaltyLovelace),
  );
  const references = CML.TransactionInputList.new();
  references.add(input(proofOutRef));
  slashBody.set_reference_inputs(references);
  const mint = CML.Mint.new();
  if (burnBond)
    mint.set(
      CML.ScriptHash.from_hex(bondUnit.slice(0, 56)),
      CML.AssetName.from_hex(bondUnit.slice(56)),
      -1n,
    );
  if (!descendant) {
    mint.set(
      CML.ScriptHash.from_hex(statePolicy),
      CML.AssetName.from_hex(stateUnit.slice(56)),
      -1n,
    );
  }
  slashBody.set_mint(mint);
  const slashTxHash = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(slashBody.to_canonical_cbor_hex()),
  ).to_hex();
  const continuedTarget = raw(`${slashTxHash}#0`, slashOutputs.get(0));
  let removalBody = slashBody;
  let removalTxHash = slashTxHash;
  let root = continuedTarget;
  if (descendant) {
    const finalInputs = CML.TransactionInputList.new();
    finalInputs.add(input(continuedTarget.outRef));
    const finalOutputs = CML.TransactionOutputList.new();
    finalOutputs.add(
      output({
        address: stateAddress,
        assets: { lovelace: 3_000_000n, [rootUnit]: 1n },
        datum: rootDatum,
      }),
    );
    removalBody = CML.TransactionBody.new(finalInputs, finalOutputs, 200_000n);
    const finalReferences = CML.TransactionInputList.new();
    finalReferences.add(input(proofOutRef));
    removalBody.set_reference_inputs(finalReferences);
    const finalMint = CML.Mint.new();
    finalMint.set(
      CML.ScriptHash.from_hex(statePolicy),
      CML.AssetName.from_hex(stateUnit.slice(56)),
      -1n,
    );
    removalBody.set_mint(finalMint);
    removalTxHash = CML.hash_transaction(
      CML.TransactionBody.from_cbor_hex(removalBody.to_canonical_cbor_hex()),
    ).to_hex();
    root = raw(`${removalTxHash}#0`, finalOutputs.get(0));
  }
  const pointInput = {
    slot: "1000",
    blockHash: hash32("51"),
    blockNo: "71",
  };
  const point = {
    ...pointInput,
    pointId: computeFraudProofRawL1PointId(pointInput),
  };
  const tipInput = {
    slot: "1030",
    blockHash: hash32("52"),
    blockNo: "100",
  };
  const tip = {
    ...tipInput,
    pointId: computeFraudProofRawL1PointId(tipInput),
  };
  const stepAddresses = ["35", "36", "37", "38"].map(scriptAddress);
  const definition: FraudProofRawL1FamilyDefinition = {
    category: "doubleSpend",
    categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend,
    headerHash,
    proverCredential: PROVER,
    stateQueue: { policyId: statePolicy, address: stateAddress },
    computationThread: {
      policyId: threadPolicy,
      steps: stepAddresses.map((address, index) => ({
        role: `computation_thread_step_0${(index + 1).toString()}` as
          | "computation_thread_step_01"
          | "computation_thread_step_02"
          | "computation_thread_step_03"
          | "computation_thread_step_04",
        address,
        datumSchema: FraudProofTokenDatum,
      })),
    },
    proofToken: { policyId: proofPolicy, address: proofAddress },
    operatorDirectory: {
      activePolicyId: activePolicy,
      activeAddress,
      retiredPolicyId: retiredPolicy,
      retiredAddress,
    },
    schedulerAddress: scriptAddress("39"),
  };
  const snapshot: FraudProofRawL1Snapshot = {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: FINALITY,
    headerHash,
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: SOURCE,
      grade: "security",
      sourceMode: "local_kupo_ogmios",
      kupoCheckpoint: point,
      ogmiosTip: tip,
    },
    cursor: {
      point,
      tip,
      confirmationDepth: 30,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        finalityPolicyDigest: FINALITY,
        sourceId: SOURCE,
        pointId: point.pointId,
      }),
    },
    scopes: [
      { role: "state_queue", address: stateAddress, utxos: [root] },
      ...stepAddresses.map((address, index) => ({
        role: `computation_thread_step_0${(index + 1).toString()}` as const,
        address,
        utxos: [],
      })),
      { role: "permanent_proof_token", address: proofAddress, utxos: [proof] },
      { role: "active_operator_directory", address: activeAddress, utxos: [] },
      {
        role: "retired_operator_directory",
        address: retiredAddress,
        utxos: [],
      },
      { role: "scheduler", address: definition.schedulerAddress, utxos: [] },
    ] as FraudProofRawL1Snapshot["scopes"],
    historyUnits: [stateUnit, threadUnit, proofUnit],
    history: [
      {
        unit: stateUnit,
        fromGenesis: true,
        completeThroughPointId: point.pointId,
        transactionHashes: [
          ...(proofCreation ? [targetHash] : []),
          ...(descendant ? [slashTxHash, removalTxHash] : [removalTxHash]),
        ],
      },
      {
        unit: threadUnit,
        fromGenesis: true,
        completeThroughPointId: point.pointId,
        transactionHashes: [],
      },
      {
        unit: proofUnit,
        fromGenesis: true,
        completeThroughPointId: point.pointId,
        transactionHashes: proofCreation ? [proofHash] : [],
      },
    ],
    transactions: [
      {
        txHash: slashTxHash,
        bodyCbor: slashBody.to_canonical_cbor_hex(),
        witnessSetCbor: CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
        redeemersCbor: null,
        isValid: true,
        inclusionPoint: point,
        confirmationDepth: 30,
        resolvedInputs: [target, bond],
        resolvedReferenceInputs: [proof],
      },
      ...(descendant
        ? [
            {
              txHash: removalTxHash,
              bodyCbor: removalBody.to_canonical_cbor_hex(),
              witnessSetCbor:
                CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
              redeemersCbor: null,
              isValid: true as const,
              inclusionPoint: point,
              confirmationDepth: 30,
              resolvedInputs: [continuedTarget],
              resolvedReferenceInputs: [proof],
            },
          ]
        : []),
      ...(proofCreation
        ? [
            {
              txHash: targetHash,
              bodyCbor: targetBody.to_canonical_cbor_hex(),
              witnessSetCbor:
                CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
              redeemersCbor: null,
              isValid: true as const,
              inclusionPoint: point,
              confirmationDepth: 30,
              resolvedInputs: [],
              resolvedReferenceInputs: [],
            },
          ]
        : []),
      ...(proofCreation
        ? [
            {
              txHash: proofHash,
              bodyCbor: proofBody.to_canonical_cbor_hex(),
              witnessSetCbor:
                CML.TransactionWitnessSet.new().to_canonical_cbor_hex(),
              redeemersCbor: null,
              isValid: true as const,
              inclusionPoint: point,
              confirmationDepth: 30,
              resolvedInputs: [],
              resolvedReferenceInputs: [],
            },
          ]
        : []),
    ],
  };
  return {
    snapshot,
    definition,
    removalTxHash,
    rewardOutRef: `${slashTxHash}#1`,
    binding: {
      deploymentFingerprint,
      definition,
      releaseFinality: verifiedFinality,
      releaseEconomics: verifiedEconomics,
    },
  };
};

/** Canonical fixture rewind retaining the authenticated header/proof mints. */
export const rollBackTerminalFixture = ({
  snapshot,
  definition,
}: Awaited<ReturnType<typeof fixture>>): FraudProofRawL1Snapshot => {
  const removal = snapshot.transactions[0]!;
  const [target, bond] = removal.resolvedInputs;
  if (target === undefined || bond === undefined)
    throw new Error("fixture requires its exact removal inputs");
  const root = raw(
    `${hash32("63")}#0`,
    output({
      address: definition.stateQueue.address,
      assets: {
        lovelace: 3_000_000n,
        [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
          1n,
      },
      datum: encodeLinkedListNodeView({
        key: "Empty",
        next: { Key: { key: definition.headerHash } },
        data: Data.from(Data.to(makeGenesisConfirmedState(0n), ConfirmedState)),
      }),
    }),
  );
  return {
    ...snapshot,
    transactions: snapshot.transactions.filter(
      ({ txHash }) => txHash !== removal.txHash,
    ),
    history: snapshot.history.map((entry) => ({
      ...entry,
      transactionHashes: entry.transactionHashes.filter(
        (txHash) => txHash !== removal.txHash,
      ),
    })),
    scopes: snapshot.scopes.map((scope) =>
      scope.role === "state_queue"
        ? { ...scope, utxos: [root, target] }
        : scope.role === "active_operator_directory"
          ? { ...scope, utxos: [bond] }
          : scope,
    ),
  };
};

import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofTokenDatum,
  hashBlockHeader,
  makeGenesisConfirmedState,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  keyHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1Utxo,
  type VerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../../src/workflow/index.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import {
  descendantStateQueueFixture,
  stateQueueNodeFixtureDatum,
} from "./raw-l1-terminal-fixture.descendant-state-queue.js";
import { finalDescendantRemoval } from "./raw-l1-terminal-fixture.final-descendant-removal.js";
import {
  DEPLOYMENT,
  PROVER,
  RELEASE,
} from "./raw-l1-terminal-fixture.output.js";
import {
  hash32,
  input,
  output,
  policy,
  raw,
  releaseEconomics,
  releaseFinality,
  scriptAddress,
  SOURCE,
} from "./raw-l1-terminal-fixture.output.js";
import { tokenCreationBody } from "./raw-l1-terminal-fixture.token-creation.js";

export const fixture = async ({
  header: suppliedHeader,
  deploymentFingerprint = DEPLOYMENT,
  blueprintHash = RELEASE,
  verifiedFinality = releaseFinality,
  proverCredential = PROVER,
  verifiedEconomics = releaseEconomics,
  proofCreation = false,
  confirmationDepth = 30,
  descendant = false,
  descendantOperatorCredential,
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
  confirmationDepth?: number;
  descendant?: boolean;
  descendantOperatorCredential?: string;
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
    `${bondStatus === "active" ? ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX : RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX}${descendantOperatorCredential ?? OPERATOR}`,
  );
  const descendantState = descendant
    ? await descendantStateQueueFixture({
        header,
        headerHash,
        statePolicy,
        stateAddress,
        descendantOperatorCredential,
      })
    : undefined;
  const targetDatum = stateQueueNodeFixtureDatum(
    headerHash,
    header,
    descendantState?.childHash,
  );
  const rootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: castConfirmedStateToData(makeGenesisConfirmedState(0n)) as never,
  });
  const proofDatum = Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum);
  const targetBody = tokenCreationBody(
    output({
      address: stateAddress,
      assets: { lovelace: 3_000_000n, [stateUnit]: 1n },
      datum: targetDatum,
    }),
    stateUnit,
  );
  const targetHash = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(targetBody.to_canonical_cbor_hex()),
  ).to_hex();
  const targetOutRef = `${proofCreation ? targetHash : hash32("41")}#0`;
  const bondOutRef = `${hash32("42")}#0`;
  const proofBody = tokenCreationBody(
    output({
      address: proofAddress,
      assets: { lovelace: 3_000_000n, [proofUnit]: 1n },
      datum: proofDatum,
    }),
    proofUnit,
  );
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
      datum: descendantState?.continuedTargetDatum ?? rootDatum,
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
  if (descendantState !== undefined)
    inputs.add(input(descendantState.child.outRef));
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
  } else {
    descendantState!.burnChild(mint);
  }
  slashBody.set_mint(mint);
  const slashTxHash = CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(slashBody.to_canonical_cbor_hex()),
  ).to_hex();
  const continuedTarget = raw(`${slashTxHash}#0`, slashOutputs.get(0));
  let removalBody = slashBody;
  let removalTxHash = slashTxHash;
  let root = continuedTarget;
  let finalTargetBond: FraudProofRawL1Utxo | undefined;
  if (descendant) {
    const final = finalDescendantRemoval({
      continuedTargetOutRef: continuedTarget.outRef,
      stateAddress,
      rootUnit,
      rootDatum,
      proofOutRef,
      stateUnit,
      verifiedEconomics,
      activePolicy,
      activeAddress,
      operatorCredential: OPERATOR,
      rewardAddress,
      distinctOperator: descendantOperatorCredential !== undefined,
      duplicateReward,
    });
    removalBody = final.body;
    finalTargetBond = final.targetBond;
    removalTxHash = CML.hash_transaction(
      CML.TransactionBody.from_cbor_hex(removalBody.to_canonical_cbor_hex()),
    ).to_hex();
    root = raw(`${removalTxHash}#0`, final.rootOutput);
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
    slot: (BigInt(point.slot) + BigInt(confirmationDepth)).toString(),
    blockHash: hash32("52"),
    blockNo: (
      BigInt(point.blockNo) +
      BigInt(confirmationDepth) -
      1n
    ).toString(),
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
      sourceMode: "local_chain_follower",
      boundaryPoint: point,
      tipPoint: tip,
    },
    cursor: {
      point,
      tip,
      confirmationDepth,
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
        confirmationDepth,
        resolvedInputs: [
          target,
          bond,
          ...(descendantState === undefined ? [] : [descendantState.child]),
        ],
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
              confirmationDepth,
              resolvedInputs: [
                continuedTarget,
                ...(finalTargetBond === undefined ? [] : [finalTargetBond]),
              ],
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
              confirmationDepth,
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
              confirmationDepth,
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
    rewardOutRef: `${descendantOperatorCredential === undefined ? slashTxHash : removalTxHash}#1`,
    binding: {
      deploymentFingerprint,
      definition,
      releaseFinality: verifiedFinality,
      releaseEconomics: verifiedEconomics,
    },
  };
};

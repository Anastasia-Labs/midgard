import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  castConfirmedStateToData,
  castStateQueueNodeV1ToData,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofTokenDatum,
  hashBlockHeaderV1,
  makeGenesisConfirmedStateV1,
  NO_DA_ATTESTATION,
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
import { describe, expect, it } from "vitest";

import {
  computeFraudProofRawL1PointIdV1,
  computeFraudProofRawL1RollbackCursorV1,
  computeFraudProofReleaseEconomicsPolicyDigestV1,
  deriveFraudProofRawL1FamilyStageV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_V1_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION,
  type FraudProofRawL1FamilyDefinitionV1,
  type FraudProofRawL1SnapshotV1,
  type FraudProofRawL1UtxoV1,
  type VerifiedFraudProofReleaseEconomicsPolicyV1,
} from "../src/workflow/index.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";

const hash32 = (byte: string): string => byte.repeat(32);
const policy = (byte: string): string => byte.repeat(28);
const OPERATOR = policy("11");
const PROVER = policy("12");
const DEPLOYMENT = hash32("13");
const RELEASE = hash32("14");
const FINALITY = hash32("15");
const SOURCE = "local-kupmios-family-test";

const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

const releaseEconomics: VerifiedFraudProofReleaseEconomicsPolicyV1 = {
  schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_V1_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  releaseIdentityDigest: RELEASE,
  policyDigest:
    computeFraudProofReleaseEconomicsPolicyDigestV1(economicsPolicy),
  policy: economicsPolicy,
};

const scriptAddress = (byte: string): string =>
  credentialToAddress("Preview", scriptHashToCredential(policy(byte)));

const value = (assets: Readonly<Record<string, bigint>>): CML.Value => {
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

const output = ({
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

const raw = (
  outRef: string,
  transactionOutput: CML.TransactionOutput,
): FraudProofRawL1UtxoV1 => ({
  outRef,
  outputCbor: transactionOutput.to_canonical_cbor_hex(),
  datumCbor:
    transactionOutput.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
  referenceScriptCbor: null,
});

const input = (outRef: string): CML.TransactionInput => {
  const [txHash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(txHash!),
    BigInt(index!),
  );
};

const fixture = async ({
  descendant = false,
  partial = false,
  duplicateReward = false,
} = {}) => {
  const header = makeHeader(OPERATOR, Date.now());
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
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
  const activeUnit = toUnit(
    activePolicy,
    `${ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX}${OPERATOR}`,
  );
  const targetDatum = encodeLinkedListNodeView({
    key: { Key: { key: headerHash } },
    next: "Empty",
    data: castStateQueueNodeV1ToData({
      header,
      da_attestation: NO_DA_ATTESTATION,
    }) as never,
  });
  const rootDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: castConfirmedStateToData(makeGenesisConfirmedStateV1(0n)) as never,
  });
  const proofDatum = Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum);
  const targetOutRef = `${hash32("41")}#0`;
  const bondOutRef = `${hash32("42")}#0`;
  const proofOutRef = `${hash32("43")}#0`;
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
      address: activeAddress,
      assets: {
        lovelace: partial ? 800_000_000n : 900_000_000n,
        [activeUnit]: 1n,
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
        assets: { lovelace: 400_000_000n },
      }),
    );
  }
  slashOutputs.add(
    output({
      address: rewardAddress,
      assets: { lovelace: 400_000_000n },
    }),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(targetOutRef));
  inputs.add(input(bondOutRef));
  const slashBody = CML.TransactionBody.new(
    inputs,
    slashOutputs,
    partial ? 400_000_000n : 500_000_000n,
  );
  const references = CML.TransactionInputList.new();
  references.add(input(proofOutRef));
  slashBody.set_reference_inputs(references);
  if (!descendant) {
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(statePolicy),
      CML.AssetName.from_hex(stateUnit.slice(56)),
      -1n,
    );
    slashBody.set_mint(mint);
  }
  const slashTxHash = CML.hash_transaction(slashBody).to_hex();
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
    removalTxHash = CML.hash_transaction(removalBody).to_hex();
    root = raw(`${removalTxHash}#0`, finalOutputs.get(0));
  }
  const pointInput = {
    slot: "1000",
    blockHash: hash32("51"),
    blockNo: "71",
  };
  const point = {
    ...pointInput,
    pointId: computeFraudProofRawL1PointIdV1(pointInput),
  };
  const tipInput = {
    slot: "1030",
    blockHash: hash32("52"),
    blockNo: "100",
  };
  const tip = {
    ...tipInput,
    pointId: computeFraudProofRawL1PointIdV1(tipInput),
  };
  const stepAddresses = ["35", "36", "37", "38"].map(scriptAddress);
  const definition: FraudProofRawL1FamilyDefinitionV1 = {
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
  const snapshot: FraudProofRawL1SnapshotV1 = {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    releaseIdentityDigest: RELEASE,
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
      rollbackCursor: computeFraudProofRawL1RollbackCursorV1({
        deploymentIdentityDigest: DEPLOYMENT,
        releaseIdentityDigest: RELEASE,
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
    ] as FraudProofRawL1SnapshotV1["scopes"],
    historyUnits: [stateUnit, threadUnit, proofUnit],
    history: [
      {
        unit: stateUnit,
        fromGenesis: true,
        completeThroughPointId: point.pointId,
        transactionHashes: descendant
          ? [slashTxHash, removalTxHash]
          : [removalTxHash],
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
        transactionHashes: [],
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
    ],
  };
  return {
    snapshot,
    definition,
    removalTxHash,
    rewardOutRef: `${slashTxHash}#1`,
  };
};

describe("raw L1 family terminal economics", () => {
  it("derives a live sixth computation step from exact scoped bytes", async () => {
    const value = await fixture();
    const extraAddresses = [scriptAddress("3a"), scriptAddress("3b")];
    const definition: FraudProofRawL1FamilyDefinitionV1 = {
      ...value.definition,
      computationThread: {
        ...value.definition.computationThread,
        steps: [
          ...value.definition.computationThread.steps,
          {
            role: "computation_thread_step_05",
            address: extraAddresses[0]!,
            datumSchema: FraudProofTokenDatum,
          },
          {
            role: "computation_thread_step_06",
            address: extraAddresses[1]!,
            datumSchema: FraudProofTokenDatum,
          },
        ],
      },
    };
    const threadUnit = toUnit(
      definition.computationThread.policyId,
      `${definition.categoryId}${definition.headerHash}`,
    );
    const thread = raw(
      `${hash32("61")}#0`,
      output({
        address: extraAddresses[1]!,
        assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
        datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
      }),
    );
    const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
    const root = raw(
      `${hash32("62")}#0`,
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
          data: castConfirmedStateToData(
            makeGenesisConfirmedStateV1(0n),
          ) as never,
        }),
      }),
    );
    const snapshot: FraudProofRawL1SnapshotV1 = {
      ...value.snapshot,
      scopes: value.snapshot.scopes
        .map((scope) => {
          if (scope.role === "state_queue") {
            return { ...scope, utxos: [root, target] };
          }
          if (scope.role === "permanent_proof_token") {
            return { ...scope, utxos: [] };
          }
          return scope;
        })
        .concat([
          {
            role: "computation_thread_step_05",
            address: extraAddresses[0]!,
            utxos: [],
          },
          {
            role: "computation_thread_step_06",
            address: extraAddresses[1]!,
            utxos: [thread],
          },
        ]),
    };
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot,
        definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "step",
      step: 6,
      threadOutRef: thread.outRef,
    });
  });

  it("derives a live ninth transition-trace final without truncating the exact role set", async () => {
    const value = await fixture();
    const extraAddresses = ["3a", "3b", "3c", "3d", "3e"].map(scriptAddress);
    const extraSteps = extraAddresses.map((address, index) => ({
      role: `computation_thread_step_0${(index + 5).toString()}` as
        | "computation_thread_step_05"
        | "computation_thread_step_06"
        | "computation_thread_step_07"
        | "computation_thread_step_08"
        | "computation_thread_step_09",
      address,
      datumSchema: FraudProofTokenDatum,
    }));
    const definition: FraudProofRawL1FamilyDefinitionV1 = {
      ...value.definition,
      computationThread: {
        ...value.definition.computationThread,
        steps: [...value.definition.computationThread.steps, ...extraSteps],
      },
    };
    const threadUnit = toUnit(
      definition.computationThread.policyId,
      `${definition.categoryId}${definition.headerHash}`,
    );
    const thread = raw(
      `${hash32("63")}#0`,
      output({
        address: extraAddresses[4]!,
        assets: { lovelace: 3_000_000n, [threadUnit]: 1n },
        datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
      }),
    );
    const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
    const root = raw(
      `${hash32("64")}#0`,
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
          data: castConfirmedStateToData(
            makeGenesisConfirmedStateV1(0n),
          ) as never,
        }),
      }),
    );
    const snapshot: FraudProofRawL1SnapshotV1 = {
      ...value.snapshot,
      scopes: value.snapshot.scopes
        .map((scope) =>
          scope.role === "state_queue"
            ? { ...scope, utxos: [root, target] }
            : scope.role === "permanent_proof_token"
              ? { ...scope, utxos: [] }
              : scope,
        )
        .concat(
          extraSteps.map((step, index) => ({
            role: step.role,
            address: step.address,
            utxos: index === 4 ? [thread] : [],
          })),
        ),
    };
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot,
        definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "step",
      step: 9,
      threadOutRef: thread.outRef,
    });
  });

  it("rejects a reordered computation-step authority definition", async () => {
    const value = await fixture();
    const [first, second, ...rest] = value.definition.computationThread.steps;
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: {
          ...value.definition,
          computationThread: {
            ...value.definition.computationThread,
            steps: [second!, first!, ...rest],
          },
        },
        releaseEconomics,
      }),
    ).rejects.toThrow(/canonically ordered computation steps/u);
  });

  it("derives the exact release-bound slash/reward from transaction bytes", async () => {
    const value = await fixture();
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        correction: { removalTxHash: value.removalTxHash },
        economics: {
          operatorBondInputLovelace: "900000000",
          slashedLovelace: "500000000",
          proverRewardOutputOutRef: value.rewardOutRef,
          proverRewardLovelace: "400000000",
          removalFeeLovelace: "500000000",
          duplicateRewardAbsent: true,
        },
      },
    });
  });

  it("rejects a substituted release economics identity", async () => {
    const value = await fixture();
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics: {
          ...releaseEconomics,
          releaseIdentityDigest: hash32("99"),
        },
      }),
    ).rejects.toThrow(/economics identity does not match/u);
  });

  it("keeps final removal separate from an earlier descendant slash", async () => {
    const value = await fixture({ descendant: true });
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        correction: { removalTxHash: value.removalTxHash },
        economics: {
          proverRewardOutputOutRef: value.rewardOutRef,
          slashedLovelace: "500000000",
          removalFeeLovelace: "500000000",
        },
      },
    });
  });

  it("accepts only the exact partially inactivity-slashed tranche", async () => {
    const value = await fixture({ partial: true });
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).resolves.toMatchObject({
      kind: "removed",
      terminal: {
        economics: {
          operatorBondInputLovelace: "800000000",
          slashedLovelace: "400000000",
          removalFeeLovelace: "400000000",
        },
      },
    });
  });

  it("rejects duplicate reward outputs", async () => {
    const value = await fixture({ duplicateReward: true });
    await expect(
      deriveFraudProofRawL1FamilyStageV1({
        snapshot: value.snapshot,
        definition: value.definition,
        releaseEconomics,
      }),
    ).rejects.toThrow(/one exact ADA-only enterprise reward/u);
  });
});

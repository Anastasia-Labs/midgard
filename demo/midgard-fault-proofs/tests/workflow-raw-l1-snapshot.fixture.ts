import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import {
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1PointId,
  computeFraudProofRawL1RollbackCursor,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Point,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotRequest,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/index.js";

const DEPLOYMENT = "aa".repeat(32);

const RELEASE = "bb".repeat(32);

const HEADER = "cc".repeat(28);

export const UNIT = `${"dd".repeat(28)}00`;

export const OTHER_UNIT = `${"ee".repeat(28)}01`;

const SOURCE = "local-kupmios-release-v1";

const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

export const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

export const chainPoint = ({
  slot,
  blockNo,
  blockHash,
}: {
  readonly slot: string;
  readonly blockNo: string;
  readonly blockHash: string;
}): FraudProofRawL1Point => ({
  slot,
  blockNo,
  blockHash,
  pointId: computeFraudProofRawL1PointId({ slot, blockNo, blockHash }),
});

const rawOutput = (
  address: string,
  assets?: Readonly<{ unit: string; quantity: bigint }>,
): string => {
  const multiasset = assets === undefined ? undefined : CML.MultiAsset.new();
  if (assets !== undefined) {
    multiasset!.set(
      CML.ScriptHash.from_hex(assets.unit.slice(0, 56)),
      CML.AssetName.from_hex(assets.unit.slice(56)),
      assets.quantity,
    );
  }
  return CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    multiasset === undefined
      ? CML.Value.from_coin(3_000_000n)
      : CML.Value.new(3_000_000n, multiasset),
  ).to_canonical_cbor_hex();
};

export const fixture = (): {
  readonly request: FraudProofRawL1SnapshotRequest;
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly alternateAddressOutput: string;
} => {
  const address = credentialToAddress(
    "Preview",
    scriptHashToCredential("11".repeat(28)),
  );
  const alternateAddress = credentialToAddress(
    "Preview",
    scriptHashToCredential("22".repeat(28)),
  );
  const spentOutputCbor = rawOutput(address);
  const referenceOutputCbor = rawOutput(address);
  const createdOutputCbor = rawOutput(address, { unit: UNIT, quantity: 1n });
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("31".repeat(32)), 0n),
  );
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("32".repeat(32)), 1n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(CML.TransactionOutput.from_cbor_hex(createdOutputCbor));
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_reference_inputs(referenceInputs);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(UNIT.slice(0, 56)),
    CML.AssetName.from_hex(UNIT.slice(56)),
    1n,
  );
  body.set_mint(mint);
  const witnesses = CML.TransactionWitnessSet.new();
  const txHash = CML.hash_transaction(body).to_hex();
  const included = chainPoint({
    slot: "1070",
    blockNo: "70",
    blockHash: "41".repeat(32),
  });
  const cursorPoint = chainPoint({
    slot: "1071",
    blockNo: "71",
    blockHash: "42".repeat(32),
  });
  const tip = chainPoint({
    slot: "1100",
    blockNo: "100",
    blockHash: "43".repeat(32),
  });
  const request = {
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    headerHash: HEADER,
    scopes: [{ role: "state_queue", address }],
    historyUnits: [UNIT],
  } as const satisfies FraudProofRawL1SnapshotRequest;
  const snapshot = {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    blueprintHash: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    headerHash: HEADER,
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: SOURCE,
      grade: "security",
      sourceMode: "local_kupo_ogmios",
      kupoCheckpoint: cursorPoint,
      ogmiosTip: tip,
    },
    cursor: {
      point: cursorPoint,
      tip,
      confirmationDepth: 30,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        finalityPolicyDigest: releaseFinality.policyDigest,
        sourceId: SOURCE,
        pointId: cursorPoint.pointId,
      }),
    },
    scopes: [
      {
        role: "state_queue",
        address,
        utxos: [
          {
            outRef: `${txHash}#0`,
            outputCbor: createdOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
      },
    ],
    historyUnits: [UNIT],
    history: [
      {
        unit: UNIT,
        fromGenesis: true,
        completeThroughPointId: cursorPoint.pointId,
        transactionHashes: [txHash],
      },
    ],
    transactions: [
      {
        txHash,
        bodyCbor: body.to_canonical_cbor_hex(),
        witnessSetCbor: witnesses.to_canonical_cbor_hex(),
        redeemersCbor: null,
        isValid: true,
        inclusionPoint: included,
        confirmationDepth: 31,
        resolvedInputs: [
          {
            outRef: `${"31".repeat(32)}#0`,
            outputCbor: spentOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
        resolvedReferenceInputs: [
          {
            outRef: `${"32".repeat(32)}#1`,
            outputCbor: referenceOutputCbor,
            datumCbor: null,
            referenceScriptCbor: null,
          },
        ],
      },
    ],
  } as const satisfies FraudProofRawL1Snapshot;
  return {
    request,
    snapshot,
    alternateAddressOutput: rawOutput(alternateAddress, {
      unit: UNIT,
      quantity: 1n,
    }),
  };
};

export const admit = (
  snapshot: unknown,
  request: FraudProofRawL1SnapshotRequest,
): FraudProofRawL1Snapshot =>
  admitFraudProofRawL1Snapshot({
    value: snapshot,
    request,
    releaseFinality,
  });

type Mutable<T> = T extends readonly (infer Item)[]
  ? Mutable<Item>[]
  : T extends object
    ? { -readonly [Key in keyof T]: Mutable<T[Key]> }
    : T;

export const mutable = <T>(value: T): Mutable<T> =>
  structuredClone(value) as Mutable<T>;

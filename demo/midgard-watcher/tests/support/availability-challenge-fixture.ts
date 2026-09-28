import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";

/** Availability challenge snapshots and parameters for watcher unit suites. */
export const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "11".repeat(28),
});
export const utxo = (
  index: number,
  lovelace = 10_000_000n,
  txByte = "22",
): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex: index,
  address,
  assets: { lovelace },
});

export const header = (endTime: bigint): SDK.Header => ({
  prevUtxosRoot: "01".repeat(32),
  utxosRoot: "02".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: "03".repeat(32),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "04".repeat(32),
  eventToStepRoot: "05".repeat(32),
  validationTracesRoot: "08".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: endTime - 1n,
  endTime,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "06".repeat(28),
  operatorVkey: "07".repeat(28),
  protocolVersion: 1n,
});

export const queueNode = (input: {
  headerHash: string;
  status: SDK.DaAvailabilityStateQueueStatus;
  endTime?: bigint;
  index?: number;
}): SDK.StateQueueUTxO => ({
  utxo: utxo(input.index ?? 3),
  datum: {
    key: { Key: { key: input.headerHash } },
    next: "Empty",
    data: Data.castTo(
      {
        proven_fraud: null,
        header: header(input.endTime ?? HEADER_END_TIME),
        da_attestation: input.status,
      },
      SDK.StateQueueNode,
    ),
  },
  assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + input.headerHash,
});

export const parametersFixture = () =>
  SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(
      SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
    ...SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
    challengerBondLovelace: 10_000_000_000n,
    maxOpenFeeLovelace: 500_000n,
    maxPublicationFeeLovelace: 500_000n,
    maxSettlementFeeLovelace: 500_000n,
    maxCloseFeeLovelace: 1_000_000n,
    maxTimeoutFeeLovelace: 1_200_000n,
  });

/** The pooled DA bond: its policy, address and one pool UTxO. */
export const DA_BOND_POOL_POLICY_ID = "9a".repeat(28);
export const DA_BOND_POOL_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: DA_BOND_POOL_POLICY_ID,
});
export const daBondPoolUtxo = (
  lovelace: bigint,
  datum: SDK.DaBondPoolDatum = "Bonded",
): UTxO => ({
  txHash: "77".repeat(32),
  outputIndex: 0,
  address: DA_BOND_POOL_ADDRESS,
  assets: { lovelace, [SDK.daBondPoolUnit(DA_BOND_POOL_POLICY_ID)]: 1n },
  datum: SDK.encodeDaBondPoolDatum(datum),
});

export const idleLock = () => ({
  ...utxo(4),
  datum: Data.to("Idle", SDK.CorrectionLockDatum),
});

/** Default header end time. */
export const HEADER_END_TIME = 10_000n;

/**
 * One withheld header. `attested` is the snapshot before any Open (queue node
 * `Attested{commitment_hash}`, no record); `challenged` is the snapshot after
 * Open (queue node `Challenged{..}` and the record, terminal and tranche),
 * whose response deadline follows `openedAt`.
 */
export const fixture = (
  headerByte = "44",
  endTime = HEADER_END_TIME,
  openedAt = 1_000n,
) => {
  const parameters = parametersFixture();
  const bytes = new Uint8Array(16_000).fill(7);
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "33".repeat(28),
    headerHash: headerByte.repeat(28),
    payload: bytes,
    responseGeometry: parameters.response_geometry,
  });
  const commitmentHash = SDK.daAvailabilityCommitmentHash(commitment);
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    commitment,
    challengerFundingOutRef: {
      transactionId: headerByte.repeat(32),
      outputIndex: 0n,
    },
    challenger: "11".repeat(28),
    openedAt,
    parameters,
  });
  const confirmedState: SDK.StateQueueUTxO = {
    utxo: utxo(9),
    datum: {
      key: "Empty",
      next: { Key: { key: commitment.header_hash } },
      data: "",
    },
    assetName: "",
  };
  const attested: SDK.DaAvailabilityChallengeSnapshot = {
    headerHash: commitment.header_hash,
    queue: queueNode({
      headerHash: commitment.header_hash,
      status: { Attested: { commitment_hash: commitmentHash } },
      endTime,
    }),
    confirmedState,
    correctionLock: idleLock(),
    tranches: [],
  };
  const challenged: SDK.DaAvailabilityChallengeSnapshot = {
    ...attested,
    queue: queueNode({
      headerHash: commitment.header_hash,
      status: {
        Challenged: {
          commitment_hash: commitmentHash,
          challenge_asset_name: plan.challengeAssetName,
        },
      },
      endTime,
    }),
    record: utxo(0),
    recordDatum: plan.record,
    terminal: utxo(1),
    terminalDatum: plan.terminalAccumulator,
    tranches: [{ utxo: utxo(2), datum: plan.trancheThreads[0]! }],
  };
  return { parameters, bytes, plan, commitment, attested, challenged };
};

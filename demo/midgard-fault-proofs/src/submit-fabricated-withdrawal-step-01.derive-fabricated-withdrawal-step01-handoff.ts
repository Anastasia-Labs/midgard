import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytes,
  type FabricatedWithdrawalStep02State,
  type Header,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  type RootMembershipProof,
  withdrawalContentCommitmentCbor,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { Data, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FabricatedHistoryEnvironment } from "./fabricated-history-witness.js";
import { parseHex, requireRecord } from "./json-file.js";
import { type SubmitProviderConfig } from "./runtime.js";

/** Human-readable family label used in every local failure message. */
export const FABRICATED_WITHDRAWAL_CATEGORY_LABEL = "fabricated-withdrawal";

/** One deployed step of the `fabricated-withdrawal` chain. */
export type FabricatedWithdrawalStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `fabricated-withdrawal` submission needs.
 *
 * Passed in explicitly after canonical deployment resolution. This keeps the
 * #609 arity guard in `applyBlueprintParams` the single place where scripts are
 * parameterized.
 */
export type FabricatedWithdrawalContracts = {
  readonly history: FabricatedHistoryEnvironment;
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    FabricatedWithdrawalStepContract,
    FabricatedWithdrawalStepContract,
    FabricatedWithdrawalStepContract,
    FabricatedWithdrawalStepContract,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  /** Catalogue category id of `fabricatedWithdrawal`, as deployed. */
  readonly categoryId: string;
};

/**
 * Prepared committed-withdrawal inclusion produced by
 * `prepare-fabricated-withdrawal`.
 */
export type SubmitFabricatedWithdrawalInclusion = {
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalInfoCbor: string;
  readonly withdrawalsPhasRoot: string;
  readonly withdrawalMembershipProof: Proof;
  readonly withdrawalMembershipProofCbor: string;
};

export const parseSubmitFabricatedWithdrawalInclusion = (
  value: unknown,
): SubmitFabricatedWithdrawalInclusion => {
  const record = requireRecord(value, "--withdrawal-inclusion");
  const committedWithdrawalIdCbor = parseHex(
    record.committedWithdrawalIdCbor,
    "--withdrawal-inclusion.committedWithdrawalIdCbor",
  );
  const committedWithdrawalInfoCbor = parseHex(
    record.committedWithdrawalInfoCbor,
    "--withdrawal-inclusion.committedWithdrawalInfoCbor",
  );
  const withdrawalsPhasRoot = parseHex(
    record.withdrawalsPhasRoot,
    "--withdrawal-inclusion.withdrawalsPhasRoot",
    32,
  );
  const withdrawalMembershipProofCbor = parseHex(
    record.withdrawalMembershipProofCbor,
    "--withdrawal-inclusion.withdrawalMembershipProofCbor",
  );
  return {
    committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor,
    withdrawalsPhasRoot,
    withdrawalMembershipProof: Data.from(withdrawalMembershipProofCbor, Proof),
    withdrawalMembershipProofCbor,
  };
};

/** The membership witness the step-01 redeemer carries, plus its handoff state. */
export type FabricatedWithdrawalStep01Handoff = {
  readonly committedWithdrawal: RootMembershipProof<
    OutputReference,
    WithdrawalInfo
  >;
  readonly step02State: FabricatedWithdrawalStep02State;
};

/**
 * Re-derives the step-01 handoff from the **on-chain** header.
 *
 * Fails closed when the supplied raw PHAS root and the header's own
 * `withdrawal_count` do not commit the header's `withdrawals_root`: that is exactly
 * the counted-root equality the L1 step re-establishes, so a witness that cannot
 * satisfy it locally can never satisfy it on chain. Fails closed as well when the
 * supplied leaf bytes are not the `serialiseData` bytes the on-chain re-serialisation
 * will produce.
 */
export const deriveFabricatedWithdrawalStep01Handoff = async ({
  stateQueuePolicyId,
  header,
  headerHash,
  inclusion,
}: {
  readonly stateQueuePolicyId: string;
  readonly header: Header;
  readonly headerHash: string;
  readonly inclusion: SubmitFabricatedWithdrawalInclusion;
}): Promise<FabricatedWithdrawalStep01Handoff> => {
  const countedWithdrawalsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: inclusion.withdrawalsPhasRoot,
      count: header.withdrawalCount,
    }),
  );
  if (countedWithdrawalsRoot !== header.withdrawalsRoot) {
    throw new Error(
      `--withdrawal-inclusion.withdrawalsPhasRoot does not open the committed withdrawals_root: derived=${countedWithdrawalsRoot}, header=${header.withdrawalsRoot}.`,
    );
  }
  const key = Data.from(inclusion.committedWithdrawalIdCbor, OutputReference);
  const value = Data.from(
    inclusion.committedWithdrawalInfoCbor,
    WithdrawalInfo,
  );
  if (
    committedWithdrawalKeyBytes(key) !== inclusion.committedWithdrawalIdCbor
  ) {
    throw new Error(
      `--withdrawal-inclusion.committedWithdrawalIdCbor is not in serialiseData form: the on-chain membership check will hash ${committedWithdrawalKeyBytes(key)}, not ${inclusion.committedWithdrawalIdCbor}.`,
    );
  }
  if (
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      inclusion.committedWithdrawalInfoCbor,
    ) !== inclusion.committedWithdrawalInfoCbor
  ) {
    throw new Error(
      `--withdrawal-inclusion.committedWithdrawalInfoCbor is not in serialiseData form: the on-chain membership check will hash ${aikenSerialisedPlutusDataCborPreservingMapOrder(inclusion.committedWithdrawalInfoCbor)}, not ${inclusion.committedWithdrawalInfoCbor}.`,
    );
  }
  const committedWithdrawal: RootMembershipProof<
    OutputReference,
    WithdrawalInfo
  > = {
    domain: ROOT_DOMAINS.withdrawals,
    root: header.withdrawalsRoot,
    phas_root: inclusion.withdrawalsPhasRoot,
    count: header.withdrawalCount,
    key,
    value,
    proof: inclusion.withdrawalMembershipProof,
  };
  const step02State: FabricatedWithdrawalStep02State = {
    state_queue_policy: stateQueuePolicyId,
    challenged_header_hash: headerHash,
    header_start_time: header.startTime,
    header_end_time: header.endTime,
    committed_withdrawal_id: key,
    committed_withdrawal_content_hash: await Effect.runPromise(
      withdrawalContentCommitmentCbor(inclusion.committedWithdrawalInfoCbor),
    ),
  };
  return { committedWithdrawal, step02State };
};

export type SubmitFabricatedWithdrawalStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly withdrawalInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedWithdrawalStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly secondStepAddress: string;
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalContentHash: string;
  readonly withdrawalsPhasRoot: string;
  readonly committedWithdrawalsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type FabricatedWithdrawalStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  commitCountedRootProgram,
  DepositInfo,
  depositInfoCommitmentCbor,
  type FabricatedDepositStep02State,
  type Header,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FabricatedHistoryEnvironment } from "./fabricated-history-witness.js";
import { parseHex, requireRecord } from "./json-file.js";
import { type SubmitProviderConfig } from "./runtime.js";

/** Human-readable family label used in every local failure message. */
export const FABRICATED_DEPOSIT_CATEGORY_LABEL = "fabricated-deposit";

/** One deployed step of the `fabricated-deposit` chain. */
export type FabricatedDepositStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `fabricated-deposit` submission needs.
 *
 * Passed in explicitly after canonical deployment resolution. This keeps the
 * #609 arity guard in `applyBlueprintParams` the single place where scripts are
 * parameterized.
 */
export type FabricatedDepositContracts = {
  readonly history: FabricatedHistoryEnvironment;
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    FabricatedDepositStepContract,
    FabricatedDepositStepContract,
    FabricatedDepositStepContract,
    FabricatedDepositStepContract,
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
  /** Catalogue category id of `fabricatedDeposit`, as deployed. */
  readonly categoryId: string;
};

/** Prepared committed-deposit inclusion produced by `prepare-fabricated-deposit`. */
export type SubmitFabricatedDepositInclusion = {
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoCbor: string;
  readonly depositsPhasRoot: string;
  readonly depositMembershipProof: Proof;
  readonly depositMembershipProofCbor: string;
};

export const parseSubmitFabricatedDepositInclusion = (
  value: unknown,
): SubmitFabricatedDepositInclusion => {
  const record = requireRecord(value, "--deposit-inclusion");
  const committedDepositIdCbor = parseHex(
    record.committedDepositIdCbor,
    "--deposit-inclusion.committedDepositIdCbor",
  );
  const committedDepositInfoCbor = parseHex(
    record.committedDepositInfoCbor,
    "--deposit-inclusion.committedDepositInfoCbor",
  );
  const depositsPhasRoot = parseHex(
    record.depositsPhasRoot,
    "--deposit-inclusion.depositsPhasRoot",
    32,
  );
  const depositMembershipProofCbor = parseHex(
    record.depositMembershipProofCbor,
    "--deposit-inclusion.depositMembershipProofCbor",
  );
  return {
    committedDepositIdCbor,
    committedDepositInfoCbor,
    depositsPhasRoot,
    depositMembershipProof: Data.from(depositMembershipProofCbor, Proof),
    depositMembershipProofCbor,
  };
};

/** The membership witness the step-01 redeemer carries, plus its handoff state. */
export type FabricatedDepositStep01Handoff = {
  readonly committedDeposit: RootMembershipProof<OutputReference, DepositInfo>;
  readonly step02State: FabricatedDepositStep02State;
};

/**
 * Re-derives the step-01 handoff from the **on-chain** header.
 *
 * Fails closed when the supplied raw PHAS root and the header's own
 * `deposit_count` do not commit the header's `deposits_root`: that is exactly the
 * counted-root equality the L1 step re-establishes, so a witness that cannot
 * satisfy it locally can never satisfy it on chain.
 */
export const deriveFabricatedDepositStep01Handoff = async ({
  stateQueuePolicyId,
  header,
  headerHash,
  inclusion,
}: {
  readonly stateQueuePolicyId: string;
  readonly header: Header;
  readonly headerHash: string;
  readonly inclusion: SubmitFabricatedDepositInclusion;
}): Promise<FabricatedDepositStep01Handoff> => {
  const countedDepositsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.deposits,
      phasRoot: inclusion.depositsPhasRoot,
      count: header.depositCount,
    }),
  );
  if (countedDepositsRoot !== header.depositsRoot) {
    throw new Error(
      `--deposit-inclusion.depositsPhasRoot does not open the committed deposits_root: derived=${countedDepositsRoot}, header=${header.depositsRoot}.`,
    );
  }
  const key = Data.from(inclusion.committedDepositIdCbor, OutputReference);
  const value = Data.from(inclusion.committedDepositInfoCbor, DepositInfo);
  if (
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      inclusion.committedDepositInfoCbor,
    ) !== inclusion.committedDepositInfoCbor
  )
    throw new Error(
      "--deposit-inclusion.committedDepositInfoCbor is not in serialiseData form",
    );
  const committedDeposit: RootMembershipProof<OutputReference, DepositInfo> = {
    domain: ROOT_DOMAINS.deposits,
    root: header.depositsRoot,
    phas_root: inclusion.depositsPhasRoot,
    count: header.depositCount,
    key,
    value,
    proof: inclusion.depositMembershipProof,
  };
  const step02State: FabricatedDepositStep02State = {
    state_queue_policy: stateQueuePolicyId,
    challenged_header_hash: headerHash,
    header_start_time: header.startTime,
    header_end_time: header.endTime,
    committed_deposit_id: key,
    committed_deposit_info_hash: await Effect.runPromise(
      depositInfoCommitmentCbor(inclusion.committedDepositInfoCbor),
    ),
  };
  return { committedDeposit, step02State };
};

export type SubmitFabricatedDepositStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly depositInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositStep01Result = {
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
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoHash: string;
  readonly depositsPhasRoot: string;
  readonly committedDepositsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type FabricatedDepositStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

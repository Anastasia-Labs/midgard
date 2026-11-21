import { Proof } from "@al-ft/midgard-sdk/core";
import { daHashPreimageEvidenceFromCommittedLeaf } from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { parseHex, requireRecord } from "./json-file.js";
import { type SubmitProviderConfig } from "./runtime.js";
import type { LocallyEvaluatedTransaction } from "./workflow/transaction-boundary.js";

/** Prepared committed-leaf inclusion produced by `prepare-da-hash-preimage`. */
export type SubmitDaHashPreimageTxInclusion = {
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProof: Proof;
  readonly txMembershipProofCbor: string;
};

export const parseSubmitDaHashPreimageTxInclusion = (
  value: unknown,
): SubmitDaHashPreimageTxInclusion => {
  const record = requireRecord(value, "--tx-inclusion");
  const committedTxId = parseHex(
    record.committedTxId,
    "--tx-inclusion.committedTxId",
    32,
  );
  const committedLeafValueCbor = parseHex(
    record.committedLeafValueCbor,
    "--tx-inclusion.committedLeafValueCbor",
  );
  const transactionsPhasRoot = parseHex(
    record.transactionsPhasRoot,
    "--tx-inclusion.transactionsPhasRoot",
    32,
  );
  const txMembershipProofCbor = parseHex(
    record.txMembershipProofCbor,
    "--tx-inclusion.txMembershipProofCbor",
  );
  return {
    committedTxId,
    committedLeafValueCbor,
    transactionsPhasRoot,
    txMembershipProof: Data.from(txMembershipProofCbor, Proof),
    txMembershipProofCbor,
  };
};

export type SubmitDaHashPreimageStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

/** The signed step-01 transaction and what its builder derived for it. */
export type DaHashPreimageStep01Build = {
  readonly transaction: LocallyEvaluatedTransaction;
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
  readonly committedTxId: string;
  readonly verdict: ReturnType<
    typeof daHashPreimageEvidenceFromCommittedLeaf
  >["verdict"];
  readonly embeddedTxId: string | null;
  readonly derivedTxId: string | null;
  readonly committedLeafByteCount: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

/** What the CLI reports after it submits step 01. */
export type SubmitDaHashPreimageStep01Result = {
  readonly txHash: string;
} & Omit<DaHashPreimageStep01Build, "transaction"> & {
    readonly awaitedConfirmation: boolean;
  };

export type DaHashPreimageStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

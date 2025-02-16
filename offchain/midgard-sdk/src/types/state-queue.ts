import { Address, PolicyId } from "@lucid-evolution/lucid";
import { MerkleRoot, POSIXTime } from "@/types/contracts/common.js";

export type FetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

export type CommitBlockParams = {
  newUTxOsRoot: MerkleRoot;
  transactionsRoot: MerkleRoot;
  endTime: POSIXTime;
};

import { type NetworkIdStep02State } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import type {
  PreparedNetworkIdPostUtxoProof,
  PreparedNetworkIdProof,
} from "./prepare.js";
import { networkIdStepLabel } from "./submit-common.js";
import { type PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

export const STEP_LABEL = networkIdStepLabel(1);

export type PreparedNetworkIdFinalProof =
  | PreparedNetworkIdProof
  | PreparedNetworkIdPostUtxoProof
  | PreparedNetworkIdWrongfulRejection;

export const isPostUtxoPrepared = (
  prepared: PreparedNetworkIdFinalProof,
): prepared is PreparedNetworkIdPostUtxoProof =>
  prepared.faultClaim.kind === "post-utxo-network";

export const isForcedPrepared = (
  prepared: PreparedNetworkIdFinalProof,
): prepared is PreparedNetworkIdWrongfulRejection =>
  prepared.faultClaim.kind === "forced-network-mismatch";

export const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};

export const sameFault = (
  left: NetworkIdStep02State["fault"],
  right: NetworkIdStep02State["fault"],
): boolean => {
  if (
    left === "TransactionNetwork" ||
    right === "TransactionNetwork" ||
    left === "ForcedNetworkIdMismatch" ||
    right === "ForcedNetworkIdMismatch"
  ) {
    return left === right;
  }
  if ("OutputNetwork" in left || "OutputNetwork" in right) {
    return (
      "OutputNetwork" in left &&
      "OutputNetwork" in right &&
      left.OutputNetwork.output_index === right.OutputNetwork.output_index
    );
  }
  return (
    left.OutputNetworkUtxo.observed_network_id ===
    right.OutputNetworkUtxo.observed_network_id
  );
};

export type SubmitNetworkIdStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly state: NetworkIdStep02State;
  readonly outputOpeningTier: string | null;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

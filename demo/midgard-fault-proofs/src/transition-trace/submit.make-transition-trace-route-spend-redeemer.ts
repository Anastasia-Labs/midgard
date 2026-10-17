import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  TransitionTraceRouteSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "../runtime.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import { type FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type TransitionDepositOpening } from "./history-opening.js";
import {
  readTransitionProof,
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";

export type SubmitTransitionTraceProofConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly proof: TransitionProofInput;
  readonly additionalReferenceInputs?: readonly UTxO[];
  /** Persist before capture; subsequent stages authenticate it against their checkpoint. */
  readonly depositOpening?: TransitionDepositOpening;
  /** Required published shared minting witnesses for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
};

export type SubmitTransitionTraceProofFromFilesConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly proof: TransitionProofInput;
  readonly awaitConfirmation?: boolean;
};

export type SubmitTransitionTraceProofResult = {
  readonly txHash: string;
  readonly routeTxHash: string;
  readonly routeOutRef: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly transitionTraceProofAddress: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type SubmitTransitionTraceRouteResult = Readonly<{
  txHash: string;
  routeOutRef: string;
  fraudulentHeaderHash: string;
  finalIndex: number;
  awaitedConfirmation: boolean;
}>;

export type SubmitTransitionTraceFinalResult = Readonly<{
  inputIndex: number;
  outputIndex: number;
  hubOracleRefInputIndex: number;
  computationThreadMintRedeemerIndex: number;
  fraudProofMintRedeemerIndex: number;
  txHash: string;
  fraudProofOutRef: string;
  fraudulentHeaderHash: string;
  computationThreadUnit: string;
  fraudProofUnit: string;
  finalIndex: number;
  awaitedConfirmation: boolean;
}>;

export type TransitionTraceRouteSpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export type TransitionTraceFinalSpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export type TransitionTraceFinalResolvedLayout =
  TransitionTraceFinalSpendLayout & {
    readonly computationThreadMintRedeemerIndex: bigint;
  };

export const TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES = [
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const;

export const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

export const makeTransitionTraceRouteSpendRedeemer = ({
  threadUtxo,
  routeAddress,
  routeDatum,
  computationThreadUnit,
  proof,
  proofReferences,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly routeAddress: string;
  readonly routeDatum: string;
  readonly computationThreadUnit: string;
  readonly proof: TransitionProofInput;
  readonly proofReferences: readonly UTxO[];
  readonly onLayout: (layout: TransitionTraceRouteSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "transition-trace route");
    const layout: TransitionTraceRouteSpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "transition-trace route"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: routeAddress,
          datum: routeDatum,
          unit: computationThreadUnit,
        }),
        "transition-trace routed computation-thread output",
      ),
    };
    onLayout(layout);
    const encoded = Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            proof:
              proofReferences.length === 0 ? readTransitionProof(proof) : null,
            proof_ref_indices: proofReferences.map((utxo) =>
              requireReferenceInputIndex(
                ctx,
                utxo,
                "transition-trace proof chunk",
              ),
            ),
          },
        ],
      },
      TransitionTraceRouteSpendRedeemer,
    );
    return proofReferences.length === 0
      ? replacePlutusConstrFieldCbor(
          encoded,
          [0, 2, 0],
          transitionProofCbor(proof),
        )
      : encoded;
  }) satisfies BuildTxWithRedeemer;

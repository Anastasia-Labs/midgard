import {
  encodeMidgardTxOutputCanonical,
  type FieldOpening,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  type MidgardTxOutput,
  ReferenceInputNoIdxStep04Datum,
  ReferenceInputNoIdxStep04SpendRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseHex, requireRecord } from "./json-file.js";
import { midgardTxOutputFromCanonicalCbor } from "./prepare-input-no-idx.js";
import {
  outRefLabel,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

/** Prepared outputs preimage produced by `prepare-reference-input-no-idx`. */
export type SubmitReferenceInputNoIdxOutputsPreimage = {
  readonly outputsPreimage: readonly MidgardTxOutput[];
};

/**
 * The prepared file carries the canonical `encode_midgard_tx_output` bytes, so
 * the structured redeemer value is re-projected here rather than trusted: any
 * item the canonical decoder cannot invert, or whose re-encoding is not
 * byte-identical, is rejected before a transaction is built.
 *
 * `prepare-reference-input-no-idx` writes its `outputsPreimageCbor` artifact as
 * a bare JSON array of canonical items, so both that shape and the enclosing
 * `{ outputsPreimageCbor }` record are accepted.
 */
export const parseSubmitReferenceInputNoIdxOutputsPreimage = (
  value: unknown,
): SubmitReferenceInputNoIdxOutputsPreimage => {
  const rawOutputs = Array.isArray(value)
    ? value
    : requireRecord(value, "--outputs-preimage").outputsPreimageCbor;
  if (!Array.isArray(rawOutputs)) {
    throw new Error(
      "--outputs-preimage must be a JSON array, or a JSON object with an outputsPreimageCbor array.",
    );
  }
  const outputsPreimage = rawOutputs.map((item, index) => {
    const label = `--outputs-preimage.outputsPreimageCbor[${index.toString()}]`;
    const canonicalCbor = Buffer.from(parseHex(item, label), "hex");
    const projected = midgardTxOutputFromCanonicalCbor(canonicalCbor);
    if (!encodeMidgardTxOutputCanonical(projected).equals(canonicalCbor)) {
      throw new Error(`${label} is not a canonical Midgard output encoding.`);
    }
    return projected;
  });
  return { outputsPreimage };
};

export type SubmitReferenceInputNoIdxStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly outputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the **producing** transaction's
   * compact structure. New in #604: the door authenticates its field 2 against
   * the `producing_tx_id` the thread anchored.
   */
  readonly nativeTxCompactPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitReferenceInputNoIdxStep04Result = {
  readonly txHash: string;
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
  readonly fourthStepAddress: string;
  /** §4's flat commitment for the producing transaction's field 2. */
  readonly producingTxOutputsHash: string;
  /** The §2.5 anchor the thread carried for the producing transaction. */
  readonly producingTxId: string;
  readonly producingTxOutputCount: number;
  readonly badReferenceInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type ReferenceInputNoIdxStep04DatumWithState =
  ReferenceInputNoIdxStep04Datum & {
    readonly data: NonNullable<ReferenceInputNoIdxStep04Datum["data"]>;
  };

export type ReferenceInputNoIdxStep04ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export type ReferenceInputNoIdxStep04SpendLayout = Omit<
  ReferenceInputNoIdxStep04ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

export const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): ReferenceInputNoIdxStep04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Reference-input-no-idx step 04 input datum must carry the producing outputs commitment.",
    );
  }
  return datum as ReferenceInputNoIdxStep04DatumWithState;
};

const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}): ((output: TxOutput) => boolean) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

export const makeReferenceInputNoIdxStep04SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  outputsOpening,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly outputsOpening: FieldOpening;
  readonly onLayout: (layout: ReferenceInputNoIdxStep04SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 04");
    const layout: ReferenceInputNoIdxStep04SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "reference-input-no-idx step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "reference-input-no-idx step 04 fraud-proof",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            outputs_opening: outputsOpening,
          },
        ],
      },
      ReferenceInputNoIdxStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "reference-input-no-idx step 04 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "reference-input-no-idx step 04 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

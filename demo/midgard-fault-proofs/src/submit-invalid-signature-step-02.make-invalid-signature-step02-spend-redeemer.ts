import {
  type FieldOpening,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  InvalidSignatureStep02Datum,
  InvalidSignatureStep02SpendRedeemer,
  type MidgardAddressWitness,
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
import {
  outRefLabel,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

/**
 * Complete positional address-witness list, as `prepare-invalid-signature`
 * writes it to `invalid-signature-addr-tx-wits-preimage.json`. The commitment
 * fixes the item count as well as each item's content, so the list is only ever
 * accepted whole — a partial list can never open it.
 */
export const parseSubmitInvalidSignatureAddrTxWitsPreimage = (
  value: unknown,
): readonly MidgardAddressWitness[] => {
  if (!Array.isArray(value)) {
    throw new Error("--addr-tx-wits-preimage must be a JSON array.");
  }
  return value.map((item, index) => {
    const label = `--addr-tx-wits-preimage[${index.toString()}]`;
    const entry = requireRecord(item, label);
    return {
      verification_key: parseHex(
        entry.verificationKey,
        `${label}.verificationKey`,
        32,
      ),
      signature: parseHex(entry.signature, `${label}.signature`, 64),
    };
  });
};

export type SubmitInvalidSignatureStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly addrTxWitsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604.
   */
  readonly nativeTxCompactPath: string;
  /**
   * The bad transaction's compact witness set, the same file step-01 takes. The
   * door authenticates it against the `witness_set_hash` the thread anchored
   * before reading field 7 out of it.
   */
  readonly witnessSetCompactPath: string;
  readonly badAddrTxWitIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInvalidSignatureStep02Result = {
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
  readonly secondStepAddress: string;
  readonly badTxId: string;
  /** §4's flat commitment for field 7 — re-derived here and by the door. */
  readonly badAddrTxWitsHash: string;
  /** The witness-set half of `WitnessAnchor`, as the thread carried it. */
  readonly badTxWitnessSetHash: string;
  readonly addrTxWitsPreimageItemCount: number;
  readonly badAddrTxWitIndex: number;
  readonly badAddrTxWitVerificationKey: string | null;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InvalidSignatureStep02DatumWithState = InvalidSignatureStep02Datum & {
  readonly data: NonNullable<InvalidSignatureStep02Datum["data"]>;
};

export type InvalidSignatureStep02ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export type InvalidSignatureStep02SpendLayout = Omit<
  InvalidSignatureStep02ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

export const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InvalidSignatureStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InvalidSignatureStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Invalid-signature step 02 input datum must carry the bad transaction id and its address-witness collection hash.",
    );
  }
  return datum as InvalidSignatureStep02DatumWithState;
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

export const makeInvalidSignatureStep02SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  addrTxWitsOpening,
  badAddrTxWitIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly addrTxWitsOpening: FieldOpening;
  readonly badAddrTxWitIndex: bigint;
  readonly onLayout: (layout: InvalidSignatureStep02SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-signature step 02");
    const layout: InvalidSignatureStep02SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "invalid-signature step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "invalid-signature step 02 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "invalid-signature step 02 fraud-proof",
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
            addr_tx_wits_opening: addrTxWitsOpening,
            bad_addr_tx_wit_index: badAddrTxWitIndex,
          },
        ],
      },
      InvalidSignatureStep02SpendRedeemer,
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
      "invalid-signature step 02 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "invalid-signature step 02 computation-thread burn",
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
      "invalid-signature step 02 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

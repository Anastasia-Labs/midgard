import {
  type MidgardTxInput,
  ReferenceInputNoIdxStep02Datum,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import {
  parseHex,
  parseSafeNonNegativeInteger,
  requireRecord,
} from "./json-file.js";
import {
  outRefLabel,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

/** Prepared reference-inputs preimage produced by `prepare-reference-input-no-idx`. */
export type SubmitReferenceInputNoIdxReferenceInputsPreimage = {
  readonly referenceInputsPreimage: readonly MidgardTxInput[];
  readonly badReferenceInputIndex: number;
};

/**
 * `prepare-reference-input-no-idx` writes its `referenceInputsPreimage`
 * artifact as a bare JSON array of `{ txId, index }` entries and keeps the
 * challenged position in the sibling plan file, so both the artifact shape and
 * the canonical `{ tx_id, output_index }` shape are accepted here. The
 * challenged position is a caller selection, not evidence: it is only bounds
 * checked, and the violation itself is re-run against the producing
 * transaction's committed outputs in step 04.
 */
export const parseSubmitReferenceInputNoIdxReferenceInputsPreimage = ({
  value,
  badReferenceInputIndex,
}: {
  readonly value: unknown;
  readonly badReferenceInputIndex?: string | number;
}): SubmitReferenceInputNoIdxReferenceInputsPreimage => {
  const record = Array.isArray(value)
    ? undefined
    : requireRecord(value, "--reference-inputs-preimage");
  const rawEntries =
    record === undefined ? value : record.referenceInputsPreimage;
  if (!Array.isArray(rawEntries)) {
    throw new Error(
      "--reference-inputs-preimage must be a JSON array, or a JSON object with a referenceInputsPreimage array.",
    );
  }
  const referenceInputsPreimage = rawEntries.map((item, index) => {
    const label = `--reference-inputs-preimage[${index.toString()}]`;
    const entry = requireRecord(item, label);
    return {
      tx_id: parseHex(entry.tx_id ?? entry.txId, `${label}.tx_id`, 32),
      output_index: parseSafeNonNegativeInteger(
        entry.output_index ?? entry.index,
        `${label}.output_index`,
      ),
    };
  });
  const rawBadReferenceInputIndex =
    badReferenceInputIndex ?? record?.badReferenceInputIndex;
  if (rawBadReferenceInputIndex === undefined) {
    throw new Error(
      "--bad-reference-input-index is required: the reference-inputs preimage artifact does not carry the challenged position.",
    );
  }
  return {
    referenceInputsPreimage,
    badReferenceInputIndex: Number(
      parseSafeNonNegativeInteger(
        rawBadReferenceInputIndex,
        "--bad-reference-input-index",
      ),
    ),
  };
};

export type SubmitReferenceInputNoIdxStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly referenceInputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604: the door authenticates field 1 against it.
   */
  readonly nativeTxCompactPath: string;
  readonly badReferenceInputIndex?: string | number;
  readonly awaitConfirmation?: boolean;
};

export type SubmitReferenceInputNoIdxStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  /** §4's flat commitment for field 1 — re-derived here and by the door. */
  readonly verifiedTxReferenceInputsHash: string;
  /** The §2.5 anchor the thread carried, and the id these compact bytes derive to. */
  readonly verifiedTxId: string;
  readonly referenceInputsPreimageItemCount: number;
  readonly badReferenceInputIndex: number;
  readonly badReferenceInputTxId: string;
  readonly badReferenceInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type ReferenceInputNoIdxStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type ReferenceInputNoIdxStep02DatumWithState =
  ReferenceInputNoIdxStep02Datum & {
    readonly data: NonNullable<ReferenceInputNoIdxStep02Datum["data"]>;
  };

export const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): ReferenceInputNoIdxStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Reference-input-no-idx step 02 input datum must carry the disputed transaction's §2.5 anchor.",
    );
  }
  return datum as ReferenceInputNoIdxStep02DatumWithState;
};

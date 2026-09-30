import { InputNoIdxStep02Datum, type MidgardTxInput } from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { parseHex, requireRecord } from "./json-file.js";
import {
  outRefLabel,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

/** Prepared spend-inputs preimage produced by `prepare-input-no-idx`. */
export type SubmitInputNoIdxInputsPreimage = {
  readonly inputsPreimage: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
};

const parseNonNegativeInteger = (value: unknown, label: string): number => {
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return parsed;
};

export const parseSubmitInputNoIdxInputsPreimage = (
  value: unknown,
): SubmitInputNoIdxInputsPreimage => {
  const record = requireRecord(value, "--inputs-preimage");
  const rawInputs = record.inputsPreimage;
  if (!Array.isArray(rawInputs)) {
    throw new Error("--inputs-preimage.inputsPreimage must be a JSON array.");
  }
  const inputsPreimage = rawInputs.map((item, index) => {
    const entry = requireRecord(
      item,
      `--inputs-preimage.inputsPreimage[${index.toString()}]`,
    );
    return {
      tx_id: parseHex(
        entry.tx_id,
        `--inputs-preimage.inputsPreimage[${index.toString()}].tx_id`,
        32,
      ),
      output_index: BigInt(
        parseNonNegativeInteger(
          entry.output_index,
          `--inputs-preimage.inputsPreimage[${index.toString()}].output_index`,
        ),
      ),
    };
  });
  return {
    inputsPreimage,
    badInputsIndex: parseNonNegativeInteger(
      record.badInputsIndex,
      "--inputs-preimage.badInputsIndex",
    ),
  };
};

export type SubmitInputNoIdxStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly inputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604: the door re-derives the anchored id from
   * these bytes and authenticates field 0 against them.
   */
  readonly nativeTxCompactPath: string;
  /**
   * Force §8 tier 2 for field 0's preimage: publish the bytes as raw carriage
   * and reference them, instead of carrying them in this step's own redeemer.
   *
   * **Programmatic only — `bin.ts` parses no `--publish-carriage` flag**, so a
   * config assembled from argv never sets it and the shipped CLI always lets
   * the ladder decide. It is settable only by a caller that builds this config
   * in process, and it is forwarded from here to the same-named option on
   * {@link submitInputNoIdxStep02}, which is what the emulator leg exercises
   * directly. That the CLI does not expose it is deliberate: it is the **only**
   * tier choice §8 leaves open, and it changes which transaction pays rather
   * than what the door authenticates, so there is nothing an operator gains by
   * naming it. Above the tier-1 bound the ladder publishes on its own and the
   * option is redundant.
   */
  readonly publishCarriage?: boolean;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInputNoIdxStep02Result = {
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
  /** The §2.5 anchor the thread carried, and the id these compact bytes derive to. */
  readonly verifiedTxId: string;
  /** §4's flat commitment for field 0 — re-derived here and by the door. */
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimageItemCount: number;
  readonly badInputsIndex: number;
  readonly badInputTxId: string;
  readonly badInputOutputIndex: number;
  /** Which §8 tier field 0's preimage travelled under. */
  readonly carriageTier: string;
  /** Out-refs of the §8.5 raw carriage this submission referenced, in §8.4 order. */
  readonly carriageOutRefs: readonly string[];
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type InputNoIdxStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type InputNoIdxStep02DatumWithState = InputNoIdxStep02Datum & {
  readonly data: NonNullable<InputNoIdxStep02Datum["data"]>;
};

export const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InputNoIdxStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Input-no-idx step 02 input datum must carry the disputed transaction's §2.5 anchor.",
    );
  }
  return datum as InputNoIdxStep02DatumWithState;
};

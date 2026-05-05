import type { Assets } from "./assets.js";
import type { AuthoredOutput } from "./output.js";
import type { OutRef } from "./out-ref.js";
import type { BuilderScriptState } from "./scripts.js";
import type { LedgerEntry } from "@al-ft/midgard-validation";
import type { ProtocolScriptLanguage } from "../provider.js";

export type Address = string;
export type TxHash = string;
export type Datum = string;

export type MidgardResult<T, E = unknown> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: E };

export type MidgardOutRef = OutRef & {
  readonly txHash: TxHash;
};

export type MidgardDatum =
  | {
      /** Inline-only PlutusData CBOR hex. Midgard rejects datum hashes. */
      readonly kind: "inline";
      readonly cbor: Datum;
    }
  | null;

/** Builder/provider-facing script reference metadata. */
export type MidgardScript =
  | {
      readonly type: "Native";
      readonly script: string;
    }
  | {
      readonly type: "PlutusV3";
      readonly script: string;
    }
  | {
      readonly type: "MidgardV1";
      readonly script: string;
    };

export type MidgardTxOutput = {
  readonly address: Address;
  readonly assets: Assets;
  readonly datum?: MidgardDatum;
  readonly scriptRef?: MidgardScript | null;
};

export type MidgardUtxo = MidgardOutRef & {
  readonly output: MidgardTxOutput;
  readonly cbor?: {
    readonly outRef?: Uint8Array;
    readonly output?: Uint8Array;
  };
};

export type MidgardProtocolParameters = {
  readonly apiVersion?: number;
  readonly network?: string;
  readonly midgardNativeTxVersion?: number;
  readonly currentSlot?: bigint;
  readonly supportedScriptLanguages?: readonly ProtocolScriptLanguage[];
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly networkId: bigint;
  readonly maxSubmitTxCborBytes?: number;
  readonly strictnessProfile?: string;
};

export type WalletInputSource =
  | "provider"
  | "instance-override"
  | "completion-preset";

export type LocalValidationPreStateSource =
  | "explicit"
  | "instance-override"
  | "completion-preset";

export type CompleteOptions = {
  readonly fee?: bigint | number;
  readonly changeAddress?: Address;
  readonly presetWalletInputs?: readonly MidgardUtxo[];
  readonly localValidation?: "none" | "phase-a" | "phase-b";
  readonly localPreState?: LocalValidationPreState;
  readonly localPreStateSource?: LocalValidationPreStateSource;
  readonly nowCardanoSlotNo?: bigint | number;
  readonly validationConcurrency?: number;
  readonly enforceScriptBudget?: boolean;
  readonly maxFeeIterations?: number;
  readonly feePolicy?:
    | "provider"
    | {
        readonly minFeeA: bigint;
        readonly minFeeB: bigint;
      };
};

export type LocalValidationPreState =
  | ReadonlyMap<string, Uint8Array>
  | readonly LedgerEntry[];

export type LocalValidationRejectedTx = {
  readonly txId: string;
  readonly code: string;
  readonly detail: string | null;
};

export type LocalValidationReport = {
  readonly phase: "phase-a" | "phase-b";
  readonly acceptedTxIds: readonly string[];
  readonly rejected: readonly LocalValidationRejectedTx[];
  readonly preStateSource?: LocalValidationPreStateSource;
  readonly preStateAuthoritative?: false;
  readonly statePatch?: {
    readonly deletedOutRefs: readonly string[];
    readonly upsertedOutRefs: readonly (readonly [string, string])[];
  };
};

export type BuilderSnapshot = {
  readonly spendInputs: readonly MidgardUtxo[];
  readonly referenceInputs: readonly MidgardUtxo[];
  readonly outputs: readonly AuthoredOutput[];
  readonly requiredSigners: readonly string[];
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly minimumFee?: bigint;
  readonly networkId?: bigint;
  readonly scripts: BuilderScriptState;
  readonly providerGeneration?: number;
  readonly utxoOverrideGeneration?: number;
  readonly hasUtxoOverrides?: boolean;
  readonly composition?: {
    readonly fragmentCount: number;
  };
};

export const emptyBuilderScriptState = (): BuilderScriptState => ({
  spendRedeemers: [],
  referenceScriptMetadata: [],
  scripts: [],
  datumWitnesses: [],
  mints: [],
  observers: [],
  receiveRedeemers: [],
});

export const SUBMIT_ADMISSION_STATUS_VALUES = [
  "queued",
  "validating",
  "accepted",
  "rejected",
] as const;

export type SubmitAdmissionStatus =
  (typeof SUBMIT_ADMISSION_STATUS_VALUES)[number];

const SUBMIT_ADMISSION_STATUS_SET: ReadonlySet<string> = new Set(
  SUBMIT_ADMISSION_STATUS_VALUES,
);

export const isSubmitAdmissionStatus = (
  status: string,
): status is SubmitAdmissionStatus =>
  SUBMIT_ADMISSION_STATUS_SET.has(status);

export type SubmitTxResult = {
  readonly txId: string;
  readonly status: SubmitAdmissionStatus;
  readonly httpStatus: 200 | 202;
  readonly firstSeenAt?: string;
  readonly lastSeenAt?: string;
  readonly duplicate: boolean;
};

export type TxStatus =
  | {
      readonly kind:
        | "queued"
        | "validating"
        | "accepted"
        | "committed"
        | "pending_commit"
        | "awaiting_local_recovery"
        | "not_found";
      readonly txId: string;
    }
  | {
      readonly kind: "rejected";
      readonly txId: string;
      readonly code: string;
      readonly detail: string | null;
      readonly createdAt?: string;
    };

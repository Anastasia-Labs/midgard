import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type Address,
  Data,
  fromText,
  type LucidEvolution,
  type PolicyId,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { type GenericErrorFields, LucidError } from "./common.js";
import {
  authenticateUTxOs,
  type AuthenticUTxO,
  fetchSingleAuthenticUTxOProgram,
} from "./internals.js";
import { HeaderHashSchema } from "./ledger-state.js";

export const CORRECTION_LOCK_ASSET_NAME = fromText("MIDGARD_CORRECTION_LOCK");

export const CorrectionIdentitySchema = Data.Enum([
  Data.Object({
    FraudProof: Data.Object({
      fraud_proof_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
  Data.Literal("AttestationTimeout"),
  Data.Object({
    AvailabilityChallenge: Data.Object({
      challenge_asset_name: Data.Bytes({ minLength: 32, maxLength: 32 }),
    }),
  }),
]);
export type CorrectionIdentity = Data.Static<typeof CorrectionIdentitySchema>;
export const CorrectionIdentity = asDataType<CorrectionIdentity>(
  CorrectionIdentitySchema,
);

export const CorrectionLockDatumSchema = Data.Enum([
  Data.Literal("Idle"),
  Data.Object({
    Locked: Data.Object({
      target_header_hash: HeaderHashSchema,
      correction_identity: CorrectionIdentitySchema,
    }),
  }),
]);
export type CorrectionLockDatum = Data.Static<typeof CorrectionLockDatumSchema>;
export const CorrectionLockDatum = asDataType<CorrectionLockDatum>(
  CorrectionLockDatumSchema,
);

export const CorrectionLockRedeemerSchema = Data.Enum([
  Data.Object({
    Correct: Data.Object({ hub_oracle_ref_input_index: Data.Integer() }),
  }),
  Data.Object({
    Deinit: Data.Object({ hub_oracle_input_index: Data.Integer() }),
  }),
]);
export type CorrectionLockRedeemer = Data.Static<
  typeof CorrectionLockRedeemerSchema
>;
export const CorrectionLockRedeemer = asDataType<CorrectionLockRedeemer>(
  CorrectionLockRedeemerSchema,
);

export type CorrectionLockConfig = {
  readonly correctionLockAddress: Address;
  readonly hubOraclePolicyId: PolicyId;
};

export type CorrectionLockUTxO = AuthenticUTxO<CorrectionLockDatum>;

export const correctionLockUnit = (hubOraclePolicyId: PolicyId): string =>
  toUnit(hubOraclePolicyId, CORRECTION_LOCK_ASSET_NAME);

export const utxosToCorrectionLockUTxOs = (
  utxos: UTxO[],
  hubOraclePolicyId: PolicyId,
): Effect.Effect<CorrectionLockUTxO[], LucidError> =>
  authenticateUTxOs<CorrectionLockDatum>(
    utxos,
    hubOraclePolicyId,
    CorrectionLockDatum,
  ).pipe(
    Effect.map((authentic) =>
      authentic.filter(
        ({ assetName }) => assetName === CORRECTION_LOCK_ASSET_NAME,
      ),
    ),
  );

export class CorrectionLockError extends EffectData.TaggedError(
  "CorrectionLockError",
)<GenericErrorFields> {}

/** Fetches the one deployment-bound lock token at its dedicated validator. */
export const fetchCorrectionLockUTxOProgram = (
  lucid: LucidEvolution,
  config: CorrectionLockConfig,
): Effect.Effect<CorrectionLockUTxO, CorrectionLockError | LucidError> =>
  fetchSingleAuthenticUTxOProgram<
    CorrectionLockUTxO,
    LucidError,
    CorrectionLockError
  >(lucid, {
    address: config.correctionLockAddress,
    policyId: config.hubOraclePolicyId,
    utxoLabel: "correction lock",
    conversionFunction: utxosToCorrectionLockUTxOs,
    onUnexpectedAuthenticUTxOCount: () =>
      new CorrectionLockError({
        message: "Failed to fetch the correction-lock UTxO",
        cause:
          "Exactly one authentic correction-lock UTxO was expected, but none or more were found",
      }),
  });

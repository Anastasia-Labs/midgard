import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type Address,
  calculateMinLovelaceFromUTxO,
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
  type UnexpectedAuthenticUTxOCountFields,
  unexpectedAuthenticUTxOCountFields,
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

/** Reserve rent at creation: Correct conserves the lock's value exactly.
 * FraudProof and AvailabilityChallenge have equally large 32-byte identities;
 * AttestationTimeout is smaller. Every target header hash is 28 bytes. */
export const calculateCorrectionLockMinLovelace = (
  lucid: LucidEvolution,
  config: CorrectionLockConfig,
): bigint => {
  const coinsPerUtxoByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (coinsPerUtxoByte === undefined || coinsPerUtxoByte <= 0n) {
    throw new Error(
      "Correction-lock creation requires live UTxO cost parameters",
    );
  }
  return calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
    address: config.correctionLockAddress,
    assets: { [correctionLockUnit(config.hubOraclePolicyId)]: 1n },
    txHash: "00".repeat(32),
    outputIndex: 0,
    datum: Data.to(
      {
        Locked: {
          target_header_hash: "00".repeat(28),
          correction_identity: {
            FraudProof: { fraud_proof_asset_name: "00".repeat(32) },
          },
        },
      },
      CorrectionLockDatum,
    ),
  });
};

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
)<GenericErrorFields & UnexpectedAuthenticUTxOCountFields> {}

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
    onUnexpectedAuthenticUTxOCount: (count) =>
      new CorrectionLockError({
        message: "Failed to fetch the correction-lock UTxO",
        ...unexpectedAuthenticUTxOCountFields("correction lock", count),
      }),
  });

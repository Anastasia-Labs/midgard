/**
 * `mint-authorization` family — off-chain codec twins.
 *
 * Proves that an operator-ACCEPTED committed L2 transaction mints or burns
 * under a policy id that never authorized it, in either direction:
 * direction A (script absent — no script with that hash among the
 * transaction's machine-consulted script sources) or direction B (script
 * present but its native payload evaluates unsatisfied against the committed
 * signer set and validity interval).
 *
 * Violation: `mint-authorization`.
 * Catalogue category: **not registered yet** — this module is reached by
 * direct import rather than through `fraud-proof/catalogue.ts`, and the
 * asset-name helper is parameterized on the category id instead of pinning
 * one. Emulator wiring reserves `0000001b`.
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/mint-authorization/
 * step-0{1,2,3,4,5}.ak` field for field and constructor index for
 * constructor index.
 */

import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "../common.js";
import "../ledger-state.js";
import "../transition-trace.js";
import "./field-opening.js";
import "./native.js";
import "./mint-authorization.mint-authorization-thread-token-asset-name.js";
import "./mint-authorization.mint-authorization-step03-args-schema.js";
export {
  MINT_AUTHORIZATION_STEP_NAMES,
  MintAuthorizationEvaluateArgsSchema,
  MintAuthorizationEvaluateDatum,
  MintAuthorizationEvaluateDatumSchema,
  type MintAuthorizationEvaluateOperation,
  MintAuthorizationEvaluateOperationSchema,
  MintAuthorizationEvaluateSpendRedeemer,
  MintAuthorizationEvaluateSpendRedeemerSchema,
  type MintAuthorizationEvaluateState,
  MintAuthorizationEvaluateStateSchema,
  type MintAuthorizationFrame,
  MintAuthorizationFrameSchema,
  MintAuthorizationStep03Args,
  MintAuthorizationStep03ArgsSchema,
  MintAuthorizationStep03SpendRedeemer,
  MintAuthorizationStep03SpendRedeemerSchema,
  MintAuthorizationStep04Args,
  MintAuthorizationStep04ArgsSchema,
  MintAuthorizationStep04Datum,
  MintAuthorizationStep04DatumSchema,
  MintAuthorizationStep04SpendRedeemer,
  MintAuthorizationStep04SpendRedeemerSchema,
  MintAuthorizationStep05Args,
  MintAuthorizationStep05ArgsSchema,
  MintAuthorizationStep05Datum,
  MintAuthorizationStep05DatumSchema,
  MintAuthorizationStep05SpendRedeemer,
  MintAuthorizationStep05SpendRedeemerSchema,
  mintAuthorizationStepDatumSchema,
  type MintAuthorizationStepName,
  MintAuthorizationWitnessScanArgsSchema,
  MintAuthorizationWitnessScanDatum,
  MintAuthorizationWitnessScanDatumSchema,
  MintAuthorizationWitnessScanSpendRedeemer,
  MintAuthorizationWitnessScanSpendRedeemerSchema,
  type MintAuthorizationWitnessScanState,
  MintAuthorizationWitnessScanStateSchema,
} from "./mint-authorization.mint-authorization-step03-args-schema.js";
export {
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED,
  MINT_AUTHORIZATION_VIOLATION_ID,
  MintAuthorizationClaimEvidence,
  MintAuthorizationClaimEvidenceSchema,
  MintAuthorizationEventToStepMembershipSchema,
  MintAuthorizationMintScanControl,
  MintAuthorizationMintScanControlSchema,
  MintAuthorizationMintScanState,
  MintAuthorizationMintScanStateSchema,
  MintAuthorizationStep01Args,
  MintAuthorizationStep01ArgsSchema,
  MintAuthorizationStep01Datum,
  MintAuthorizationStep01DatumSchema,
  MintAuthorizationStep01SpendRedeemer,
  MintAuthorizationStep01SpendRedeemerSchema,
  MintAuthorizationStep02Args,
  MintAuthorizationStep02ArgsSchema,
  MintAuthorizationStep02Datum,
  MintAuthorizationStep02DatumSchema,
  MintAuthorizationStep02PublishedSpendRedeemer,
  MintAuthorizationStep02PublishedSpendRedeemerSchema,
  MintAuthorizationStep02SpendRedeemer,
  MintAuthorizationStep02SpendRedeemerSchema,
  MintAuthorizationStep02State,
  MintAuthorizationStep02StateSchema,
  MintAuthorizationStep02ThreadDatum,
  MintAuthorizationStep02ThreadDatumSchema,
  MintAuthorizationStep03Datum,
  MintAuthorizationStep03DatumSchema,
  MintAuthorizationStep03State,
  MintAuthorizationStep03StateSchema,
  MintAuthorizationStep04State,
  MintAuthorizationStep04StateSchema,
  MintAuthorizationStep05State,
  MintAuthorizationStep05StateSchema,
  mintAuthorizationThreadTokenAssetName,
  MintAuthorizationTransitionStepMembershipSchema,
} from "./mint-authorization.mint-authorization-thread-token-asset-name.js";

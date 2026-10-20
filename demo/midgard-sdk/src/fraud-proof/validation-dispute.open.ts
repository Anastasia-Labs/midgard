/**
 * The two transactions that open a validation case: Open, at the dispute
 * opener (`dispute_v1`), and VerifySource, at the source validator
 * (`source_v1`), which mints the case and its bond marker under the case
 * policy and locks the challenger bond.
 */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  hashMidgardValidationMachineState,
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  fromText,
  type LucidEvolution,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AuthenticatedValidator,
  hashHexWithBlake2b,
  type SpendingValidator,
} from "../common.js";
import { EMPTY_MERKLE_TREE_ROOT } from "../ledger-constants.js";
import {
  EventKey,
  getStateQueueNodeFromStateQueueDatum,
  type Header,
} from "../ledger-state.js";
import { utxoToStateQueueUTxO } from "../state-queue.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "../tx-context-redeemer.js";
import { Case, CaseRedeemer } from "./validation-case.js";
import { DisputeBond } from "./validation-dispute.dispute-bond-schema.js";
import {
  PendingValidationClaimDatum,
  type ValidationClaimWitness,
  ValidationDisputeOpenSpendRedeemer,
  ValidationSourceSpendRedeemer,
} from "./validation-dispute.validation-game-action-schema.js";
import {
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "./validation-dispute.validation-machine-state-data-from-core.js";

const limits = SELECTED_DEPLOYMENT_PROFILE.limits;

/**
 * `validation_case_v1.case_floor_lovelace`: the fraud-proof output and one
 * capped move fee, the least a case output holds.
 */
export const VALIDATION_CASE_FLOOR_LOVELACE =
  BigInt(limits.fraud_proof_output_lovelace) +
  BigInt(limits.max_dispute_move_fee_lovelace);

const blake2b256 = (hex: string): string =>
  Effect.runSync(hashHexWithBlake2b(hex, 32));

/**
 * `validation_case_v1.case_nft_name`: hash(header hash ‖ cbor(event key) ‖
 * cbor(opener out-ref)).
 */
export const validationCaseId = (
  headerHash: string,
  eventKey: EventKey,
  opener: Pick<UTxO, "txHash" | "outputIndex">,
): string =>
  blake2b256(
    headerHash +
      Data.to(eventKey, EventKey) +
      Data.to(new Constr(0, [opener.txHash, BigInt(opener.outputIndex)])),
  );

/** `validation_case_v1.bond_marker_name`: hash("bond" ‖ case id). */
export const validationCaseBondMarkerName = (caseId: string): string =>
  blake2b256(fromText("bond") + caseId);

/** The state-queue block a validation dispute challenges. */
export type ValidationDisputeChallengedBlock = {
  readonly utxo: UTxO;
  readonly header: Header;
  readonly headerHash: string;
  /** The node's `block_time`, from which the case open guard runs. */
  readonly blockTime: bigint;
};

/** The state-queue node `utxo`, decoded as the block Open challenges. */
export const validationDisputeChallengedBlock = (
  utxo: UTxO,
  stateQueuePolicyId: string,
) =>
  Effect.gen(function* () {
    const node = yield* utxoToStateQueueUTxO(utxo, stateQueuePolicyId);
    if (node.datum.key === "Empty")
      return yield* Effect.fail(
        new Error("validation dispute: the state-queue root is no block"),
      );
    const decoded = yield* getStateQueueNodeFromStateQueueDatum(node.datum);
    return {
      utxo,
      header: decoded.header,
      headerHash: node.datum.key.Key.key,
      blockTime: decoded.block_time,
    } satisfies ValidationDisputeChallengedBlock;
  });

/** The challenger's committed claim: its event, trace root and step count. */
export type ValidationDisputeClaim = {
  readonly claimWitness: ValidationClaimWitness;
  readonly challengerRoot: string;
  readonly stepCount: bigint;
};

const singleThreadUnit = (thread: UTxO, policyId: string): string => {
  const units = Object.keys(thread.assets).filter(
    (unit) => unit !== "lovelace",
  );
  if (units.length !== 1 || !units[0]!.startsWith(policyId))
    throw new Error(
      `validation dispute: ${thread.txHash}#${thread.outputIndex.toString()} does not hold exactly one computation-thread token`,
    );
  return units[0]!;
};

const requireReferenceScript = (
  utxo: UTxO,
  scriptHash: string,
  label: string,
) => {
  if (
    utxo.scriptRef == null ||
    validatorToScriptHash(utxo.scriptRef) !== scriptHash
  )
    throw new Error(
      `validation dispute: the ${label} reference ${utxo.txHash}#${utxo.outputIndex.toString()} does not carry script ${scriptHash}`,
    );
};

/**
 * Open: spends the thread `Init` left at the opener and continues it to the
 * source validator with the challenger's pending claim over
 * `challenged`. The fraud prover named in the thread datum signs; the
 * thread keeps its lovelace, raised to the output's minimum if needed.
 */
export const appendValidationDisputeOpen = (
  tx: TxBuilder,
  params: {
    readonly contracts: {
      readonly opener: SpendingValidator;
      readonly source: SpendingValidator;
    };
    readonly computationThreadPolicyId: string;
    readonly openerReferenceScript: UTxO;
    /** The thread at the opener, as `Init` left it. */
    readonly thread: UTxO;
    readonly fraudProver: string;
    readonly hubOracle: UTxO;
    readonly challenged: ValidationDisputeChallengedBlock;
    readonly claim: ValidationDisputeClaim;
  },
): TxBuilder => {
  const { contracts, thread, hubOracle, challenged, claim } = params;
  requireReferenceScript(
    params.openerReferenceScript,
    contracts.opener.spendingScriptHash,
    "opener",
  );
  const threadUnit = singleThreadUnit(thread, params.computationThreadPolicyId);
  const pending = Data.to(
    {
      fraud_prover: params.fraudProver,
      data: {
        challenged_header_hash: challenged.headerHash,
        challenged_header: challenged.header,
        claim: claim.claimWitness,
        challenger_root: claim.challengerRoot,
        step_count: claim.stepCount,
        block_end: challenged.blockTime,
      },
    },
    PendingValidationClaimDatum,
  );
  return tx
    .collectFrom([thread], ((ctx) =>
      Data.to(
        {
          Continue: [
            {
              input_index: requireInputIndex(ctx, thread, "open thread"),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === contracts.source.spendingScriptAddress &&
                  (output.assets[threadUnit] ?? 0n) === 1n,
                "open pending claim",
              ),
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracle,
                "open hub oracle",
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                challenged.utxo,
                "open challenged block",
              ),
              claim: claim.claimWitness,
              challenger_root: claim.challengerRoot,
              step_count: claim.stepCount,
            },
          ],
        },
        ValidationDisputeOpenSpendRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .readFrom([params.openerReferenceScript, hubOracle, challenged.utxo])
    .pay.ToContract(
      contracts.source.spendingScriptAddress,
      { kind: "inline", value: pending },
      { lovelace: thread.assets.lovelace ?? 0n, [threadUnit]: 1n },
    )
    .addSignerKey(params.fraudProver);
};

/** The case and bond VerifySource creates. */
export type ValidationCaseOpening = {
  readonly caseId: string;
  readonly caseUnit: string;
  readonly bondMarkerUnit: string;
  readonly case: Case;
  readonly bond: DisputeBond;
};

/**
 * VerifySource: spends the pending claim at the source validator, mints the
 * case NFT and its bond marker, and pays the case (the thread token, the case
 * NFT, the case datum) and the bond (the marker and `bondLovelace`). The
 * transaction's upper bound is the start of the slot holding `validTo`,
 * exclusive, and the case starts at its inclusive form, one millisecond
 * before; the range is at most L_d long and must end by `block_end + M/2`.
 * `agreedInitialState` is state 0 of the challenger's trace, the state
 * VerifySource derives from the committed header and source.
 */
export const appendValidationSourceVerification = (
  tx: TxBuilder,
  params: {
    readonly contracts: {
      readonly source: SpendingValidator;
      readonly case: AuthenticatedValidator;
      readonly disputeBond: SpendingValidator;
    };
    readonly computationThreadPolicyId: string;
    readonly sourceReferenceScript: UTxO;
    readonly caseReferenceScript: UTxO;
    /** The case configuration UTxO, read by the case mint. */
    readonly caseConfig: UTxO;
    /** The pending claim Open left at the source validator. */
    readonly opener: UTxO;
    readonly challengerTerminal: MidgardValidationMachineState;
    readonly challengerTerminalProof: MidgardValidationTraceProof;
    readonly agreedInitialState: MidgardValidationMachineState;
    readonly bondLovelace: bigint;
    readonly validFrom: number;
    readonly validTo: number;
    /** The slot conversion the transaction's bounds are set with. */
    readonly slots: Pick<LucidEvolution, "unixTimeToSlot" | "slotToUnixTime">;
  },
): { readonly tx: TxBuilder; readonly opening: ValidationCaseOpening } => {
  const { contracts, opener } = params;
  requireReferenceScript(
    params.sourceReferenceScript,
    contracts.source.spendingScriptHash,
    "source",
  );
  requireReferenceScript(
    params.caseReferenceScript,
    contracts.case.policyId,
    "case",
  );
  if (opener.datum == null)
    throw new Error("validation dispute: the pending claim has no datum");
  const pending = Data.from(opener.datum, PendingValidationClaimDatum);
  if (pending.data === null)
    throw new Error("validation dispute: the pending claim has no state");
  const state = pending.data;
  const threadUnit = singleThreadUnit(opener, params.computationThreadPolicyId);
  const eventKey = state.claim.event_to_step_membership.key;
  const caseId = validationCaseId(
    state.challenged_header_hash,
    eventKey,
    opener,
  );
  const caseUnit = toUnit(contracts.case.policyId, caseId);
  const bondMarkerUnit = toUnit(
    contracts.case.policyId,
    validationCaseBondMarkerName(caseId),
  );
  const hex = (bytes: Buffer) => bytes.toString("hex");
  const caseDatum: Case = {
    version: 1n,
    block_header_hash: state.challenged_header_hash,
    event_key: eventKey,
    challenger_root: state.challenger_root,
    step_count: state.step_count,
    challenger_terminal_hash: hex(
      hashMidgardValidationMachineState(params.challengerTerminal),
    ),
    bounty_claimant: pending.fraud_prover,
    agreed_initial_hash: hex(
      hashMidgardValidationMachineState(params.agreedInitialState),
    ),
    started_at: BigInt(
      params.slots.slotToUnixTime(params.slots.unixTimeToSlot(params.validTo)) -
        1,
    ),
    open_threads: 0n,
    defenders_root: EMPTY_MERKLE_TREE_ROOT,
    defender_won: false,
    neither_claim_valid: false,
  };
  const bond: DisputeBond = {
    challenged_header_hash: state.challenged_header_hash,
    fraud_prover: pending.fraud_prover,
    case_id: caseId,
  };
  const openerLovelace = opener.assets.lovelace ?? 0n;
  const caseLovelace =
    openerLovelace > VALIDATION_CASE_FLOOR_LOVELACE
      ? openerLovelace
      : VALIDATION_CASE_FLOOR_LOVELACE;
  const isCase = (output: TxOutput) =>
    output.address === contracts.case.spendingScriptAddress &&
    (output.assets[caseUnit] ?? 0n) === 1n;
  const built = tx
    .collectFrom([opener], ((ctx) =>
      Data.to(
        {
          Continue: [
            {
              input_index: requireInputIndex(ctx, opener, "source claim"),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                isCase,
                "source case",
              ),
              challenger_terminal: validationMachineStateDataFromCore(
                params.challengerTerminal,
              ),
              challenger_terminal_proof: validationTraceProofDataFromCore(
                params.challengerTerminalProof,
              ),
            },
          ],
        },
        ValidationSourceSpendRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .mintAssets({ [caseUnit]: 1n, [bondMarkerUnit]: 1n }, ((ctx) =>
      Data.to(
        {
          VerifySource: {
            opener_input_index: requireInputIndex(ctx, opener, "case opener"),
            case_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              isCase,
              "case output",
            ),
          },
        },
        CaseRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .readFrom([
      params.sourceReferenceScript,
      params.caseReferenceScript,
      params.caseConfig,
    ])
    .pay.ToContract(
      contracts.case.spendingScriptAddress,
      { kind: "inline", value: Data.to(caseDatum, Case) },
      { lovelace: caseLovelace, [caseUnit]: 1n, [threadUnit]: 1n },
    )
    .pay.ToContract(
      contracts.disputeBond.spendingScriptAddress,
      { kind: "inline", value: Data.to(bond, DisputeBond) },
      { lovelace: params.bondLovelace, [bondMarkerUnit]: 1n },
    )
    .validFrom(params.validFrom)
    .validTo(params.validTo);
  return {
    tx: built,
    opening: { caseId, caseUnit, bondMarkerUnit, case: caseDatum, bond },
  };
};
